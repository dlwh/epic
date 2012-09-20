package epic.sequences

import java.io.File
import breeze.config.{Configuration, CommandLineParser}
import epic.ontonotes.ConllOntoReader
import epic.everything.{NERType, DSpan}
import collection.mutable.ArrayBuffer
import breeze.util._
import breeze.text.analyze.{EnglishWordClassGenerator, WordShapeGenerator}
import breeze.linalg.{Counter, DenseVector, SparseVector}
import collection.immutable
import epic.framework.{ModelObjective, Feature}
import breeze.optimize._
import breeze.collection.mutable.{TriangularArray, OpenAddressHashArray}
import breeze.optimize.FirstOrderMinimizer.OptParams
import epic.trees.Span
import epic.everything.DSpan
import epic.parser.features.WordShapeFeaturizer

/**
 *
 * @author dlwh
 */
object SemiNERPipeline {

  def makeSegmentation(ner: Map[DSpan, NERType.Value],
                       words: IndexedSeq[String],
                       id: String): Segmentation[NERType.Value, String]  = {
    val sorted = ner.toIndexedSeq.sortBy((_: (DSpan, NERType.Value))._1.begin)
    var out = new ArrayBuffer[(NERType.Value, Span)]()
    var last = 0
    for( (dspan, label) <- sorted ) {
      assert(last <= dspan.begin)
      while(dspan.begin != last) {
        out += (NERType.NotEntity -> Span(last,last+1))
        last += 1
      }
      out += (label -> Span(dspan.begin, dspan.end))
      last = dspan.end
    }
    while(words.length != last) {
      out += (NERType.NotEntity -> Span(last,last+1))
      last += 1
    }

    Segmentation(out, words, id)
  }

  case class SFeature(w: String, kind: Symbol) extends Feature
  case class BeginFeature[L](w: Feature, cur: L) extends Feature
  case class EndFeature[L](w: Feature, cur: L) extends Feature
  case class SpanFeature[L](distance: Feature, cur: L) extends Feature
  case class UnigramFeature[L](w: Feature, cur: L) extends Feature
  case class CFeature(component: Int, f: Feature) extends Feature
  case class DistanceFeature(distanceBin: Int) extends Feature

  // features that can be quickly computed
  class StandardFeaturizer(wordCounts: Counter[String, Double],
                           gazetteer: Map[IndexedSeq[String], String]) {

    val flattenedGazetteer = {
      val justWords = for((seq, kind) <- gazetteer.toIndexedSeq; w <- seq) yield (w, kind)
      justWords.groupBy(_._1).mapValues(_.map(_._2)).toMap
    }

    val inner = new WordShapeFeaturizer(wordCounts)

    def localize(words: IndexedSeq[String])= new Localization(words)

    val interner = new Interner[Feature]

    class Localization(words: IndexedSeq[String]) {
      val classes = words.map(EnglishWordClassGenerator)
      val shapes = words.map(WordShapeGenerator)

      val featuresForWord: immutable.IndexedSeq[Array[Feature]] = 0 until words.length map { pos =>
        val feats = new ArrayBuffer[Feature]()
        feats ++= inner.featuresFor(words, pos)
        feats ++= Array(
          SFeature(if(pos == 0) "##" else shapes(pos-1), 'PrevWordShape),
          SFeature(if(pos == words.length - 1) "##" else shapes(pos+1), 'NextWordShape),
          SFeature(if(pos == 0) "##" else classes(pos-1), 'PrevWordClass),
          SFeature(if(pos == words.length - 1) "##" else classes(pos+1), 'NextWordClass))
        feats ++= flattenedGazetteer.getOrElse(words(pos), IndexedSeq.empty).map(SFeature(_, 'WordSeenInSegment))
        feats.map(interner.intern _).toArray
      }

      def featuresForSpan(start: Int, end: Int):Array[Feature] = {
        val arr = Array[Feature](DistanceFeature(binDistance(end - start)))

        gazetteer.get(Span(start, end).map(words).toIndexedSeq).map(arr :+ SFeature(_, 'WordSeenInSegment)).getOrElse(arr)
//        IndexedSeq.empty
      }

      private def binDistance(len: Int) = {
        if(len <= 1) 1
        else if(len <= 4) 2
        else if(len <= 8) 3
        else if(len <= 16) 4
        else 5
      }
    }

  }

  class IndexedStandardFeaturizer(f: StandardFeaturizer,
                                  val labelIndex: Index[NERType.Value],
                                  val basicFeatureIndex: Index[Feature]) extends SemiCRFModel.BIEOFeaturizer[NERType.Value,String] {

    def startSymbol: NERType.Value = NERType.OutsideSentence
    private val label2Index = new PairIndex(labelIndex, labelIndex)
    private val labeledFeatureIndex = new PairIndex(labelIndex, basicFeatureIndex)
    private val labeled2FeatureIndex = new PairIndex(label2Index, basicFeatureIndex)
    // feature mappings... sigh
    private implicit val beginIso = Isomorphism[(NERType.Value, Feature), BeginFeature[NERType.Value]](
      tu={pair => BeginFeature(pair._2, pair._1)},
      ut={f => (f.cur, f.w) }
    )
    private implicit val endIso = Isomorphism[(NERType.Value, Feature), EndFeature[NERType.Value]](
      tu={pair => EndFeature(pair._2, pair._1)},
      ut={f => (f.cur, f.w) }
    )

    private implicit val uniIso = Isomorphism[(NERType.Value, Feature), UnigramFeature[NERType.Value]](
      tu={pair => UnigramFeature(pair._2, pair._1)},
      ut={f => (f.cur, f.w) }
    )

    private implicit val spanIso = Isomorphism[((NERType.Value, NERType.Value), Feature), SpanFeature[(NERType.Value, NERType.Value)]](
      tu={pair => SpanFeature(pair._2, pair._1)},
      ut={f => (f.cur, f.distance) }
    )

    private val distanceFeatureIndex = Index[Feature](Iterator(DistanceFeature(1), DistanceFeature(2), DistanceFeature(3), DistanceFeature(4), DistanceFeature(5)))
    private val spanFeatureIndex = new PairIndex(label2Index, distanceFeatureIndex)

    val compositeIndex = new CompositeIndex[Feature](new IsomorphismIndex(labeledFeatureIndex)(beginIso),
      new IsomorphismIndex(labeledFeatureIndex)(endIso),
      new IsomorphismIndex(labeledFeatureIndex)(uniIso),
      new IsomorphismIndex(spanFeatureIndex)(spanIso)
    )

    val BEGIN_COMP = 0
    val END_COMP = 1
    val UNI_COMP = 2
    val SPAN_COMP = 3

    private implicit val featureIso = Isomorphism[(Int,Feature), Feature](
      tu={pair => CFeature(pair._1, pair._2)},
      ut={f => f.asInstanceOf[CFeature].component -> f.asInstanceOf[CFeature].f}
    )

    val featureIndex: IsomorphismIndex[(Int, Feature), Feature] = new IsomorphismIndex(compositeIndex)(featureIso)
    println(featureIndex.size)

    def anchor(w: IndexedSeq[String]): SemiCRFModel.BIEOAnchoredFeaturizer[NERType.Value, String] = new SemiCRFModel.BIEOAnchoredFeaturizer[NERType.Value, String] {
      val loc = f.localize(w)

      val basicFeatureCache = Array.tabulate(w.length){ pos =>
        val feats =  loc.featuresForWord(pos)
        feats.map(basicFeatureIndex).filter(_ >= 0)
      }

      val beginCache = Array.tabulate(labelIndex.size, labelIndex.size, w.length){ (p,c,w) =>
        val builder = new SparseVector.Builder[Double](featureIndex.size)
        val feats = basicFeatureCache(w)
        builder.sizeHint(feats.length)
        var i = 0
        while(i < feats.length) {
          val index = compositeIndex.mapIndex(BEGIN_COMP, labeledFeatureIndex.mapIndex(c, feats(i)))
          if(index != -1) {
            builder.add(index, 1.0)
          }

          i += 1
        }
        builder.result()
      }
      val endCache = Array.tabulate(labelIndex.size, w.length){ (l, w) =>
        val builder = new SparseVector.Builder[Double](featureIndex.size)
        val feats = basicFeatureCache(w)
        builder.sizeHint(feats.length)
        var i = 0
        while(i < feats.length) {
          val index = compositeIndex.mapIndex(END_COMP, labeledFeatureIndex.mapIndex(l, feats(i)))
          if(index != -1) {
            builder.add(index, 1.0)
          }

          i += 1
        }
        builder.result()
      }
      val wordCache = Array.tabulate(labelIndex.size, w.length){ (l, w) =>
        val builder = new SparseVector.Builder[Double](featureIndex.size)
        val feats = basicFeatureCache(w)
        builder.sizeHint(feats.length)
        var i = 0
        while(i < feats.length) {
          val index = compositeIndex.mapIndex(UNI_COMP, labeledFeatureIndex.mapIndex(l, feats(i)))
          if(index != -1) {
            builder.add(index, 1.0)
          }

          i += 1
        }
        builder.result()
      }

      def featureIndex: Index[Feature] = IndexedStandardFeaturizer.this.featureIndex

      def featuresForBegin(prev: Int, cur: Int, pos: Int): SparseVector[Double] = {
        beginCache(prev)(cur)(pos)
      }

      def featuresForEnd(cur: Int, pos: Int): SparseVector[Double] = {
        endCache(cur)(pos-1)
      }

      def featuresForInterior(cur: Int, pos: Int): SparseVector[Double] = {
        wordCache(cur)(pos)
      }

      private val spanCache = TriangularArray.tabulate(w.length+1){ (beg,end) =>
        loc.featuresForSpan(beg, end).map(distanceFeatureIndex).filter(_ >= 0)
      }
      private val spanFeatures = Array.tabulate(labelIndex.size, labelIndex.size){ (prev, cur) =>
        TriangularArray.tabulate(w.length+1) { (beg, end) =>
          val feats = spanCache(beg, end)
          val builder = new SparseVector.Builder[Double](featureIndex.size)
          builder.sizeHint(feats.length)
          var i = 0
          while(i < feats.length) {
            val index = compositeIndex.mapIndex(SPAN_COMP, spanFeatureIndex.mapIndex(label2Index.mapIndex(prev, cur), feats(i)))
            if(index != -1) {
              builder.add(index, 1.0)
            }
            i += 1
          }
          builder.result()
        }
      }


      def featuresForSpan(prev: Int, cur: Int, beg: Int, end: Int): SparseVector[Double] = {
        spanFeatures(prev)(cur)(beg,end)
      }



    }
  }

  object IndexedStandardFeaturizer {
    def apply(f: StandardFeaturizer, data: IndexedSeq[Segmentation[NERType.Value, String]], maxLength: Int=>Int) = {
      val labelIndex: Index[NERType.Value] = EnumerationIndex(NERType)
      val featureIndex = Index[Feature]()

      val maxMaxLength = (0 until labelIndex.size).map(maxLength).max
      var i = 0
      for(s <- data) {
        if(i % 200 == 0)
        println(i + "/" + data.length)
        val loc = f.localize(s.words)

        for(b <- 0 until s.length) {
          loc.featuresForWord(b) foreach {featureIndex.index _}
          for(e <- (b+1) until math.min(s.length,b+maxMaxLength)) {
            loc.featuresForSpan(b, e) foreach {featureIndex.index _}
          }

        }
        i += 1
      }
      new IndexedStandardFeaturizer(f, labelIndex, featureIndex)
    }

  }
  case class Params(path: File, name: String = "eval/ner", nfiles: Int = 100000,
                    iterPerEval: Int = 20,
                    opt: OptParams)

  def main(args: Array[String]) {
    val (baseConfig, files) = CommandLineParser.parseArguments(args)
    val config = baseConfig backoff Configuration.fromPropertiesFiles(files.map(new File(_)))
    val params = config.readIn[Params]("")
    val instances: Array[Segmentation[NERType.Value, String]] = for {
      file <- params.path.listFiles take params.nfiles
      doc <- ConllOntoReader.readDocuments(file)
      s <- doc.sentences
    } yield makeSegmentation(s.ner, s.words, s.id)

    val train = instances.take(instances.length * 9 / 10)
    val test = instances.drop(instances.length * 9 / 10)

    val maxLengthMap = train.flatMap(_.segments.iterator).groupBy(_._1).mapValues(arr => arr.map(_._2.length).max)
    val labelIndex: Index[NERType.Value] = EnumerationIndex(NERType)
    val maxLengthArray = Encoder.fromIndex(labelIndex).tabulateArray(maxLengthMap.getOrElse(_, 0))
    println("Max Length: " + maxLengthMap)

    val wordCounts:Counter[String, Double] = Counter.count(instances.flatMap(_.words):_*).mapValues(_.toDouble)
    val gazeteer = NERGazetteer.load("en")

    // build feature Index
    val feat = new SemiNERPipeline.StandardFeaturizer(wordCounts, gazeteer)
    val indexed = IndexedStandardFeaturizer(feat, train, maxLengthArray)

    val model = new SemiCRFModel(indexed.featureIndex, indexed, maxLengthArray)
    val obj = new ModelObjective(model, train)
    val cached = new CachedBatchDiffFunction(obj)

    val checking = new RandomizedGradientCheckingFunction[Int, DenseVector[Double]](cached, toString={ (i: Int) => indexed.featureIndex.get(i).toString})
//
    def eval(state: FirstOrderMinimizer[DenseVector[Double], BatchDiffFunction[DenseVector[Double]]]#State) {
      val crf = model.extractCRF(state.x)
      println("Eval + " + (state.iter+1) + " " + SegmentationEval.eval(crf, test, NERType.NotEntity))
    }

    val weights = params.opt.iterations(cached, obj.initialWeightVector(randomize=true)).tee(state => if((state.iter +1) % params.iterPerEval == 0) eval(state)).take(params.opt.maxIterations).last
//    eval(weights)
    val weights2 = new LBFGS[DenseVector[Double]].minimize(checking, weights.x)



  }

}

