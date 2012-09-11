package epic.sequences

import java.io.File
import breeze.config.{Configuration, CommandLineParser}
import epic.ontonotes.ConllOntoReader
import epic.everything.{NERType, DSpan}
import collection.mutable.ArrayBuffer
import epic.trees.Span
import breeze.util.{Encoder, Interner, Index, EnumerationIndex}
import breeze.text.analyze.{EnglishWordClassGenerator, WordShapeGenerator}
import epic.sequences.SemiCRF.AnchoredFeaturizer
import breeze.linalg.{DenseVector, SparseVector}
import collection.immutable
import epic.framework.{ModelObjective, Feature}
import breeze.optimize.{RandomizedGradientCheckingFunction, LBFGS, GradientCheckingDiffFunction, CachedBatchDiffFunction}
import breeze.optimize.FirstOrderMinimizer.OptParams

/**
 *
 * @author dlwh
 */
object SemiNERPipeline {

  def makeSegmentation(ner: Map[DSpan, NERType.Value], words: IndexedSeq[String], id: String): Segmentation[NERType.Value, IndexedSeq[String]]  = {
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

    Segmentation(out, words, words.length, id)
  }

  case class SFeature(w: String, kind: Symbol) extends Feature
  case class BeginFeature(w: Feature, prev: Int, cur: Int) extends Feature
  case class EndFeature(w: Feature, prev: Int, cur: Int) extends Feature
  case class LengthFeature(distance: Int, prev: Int, cur: Int) extends Feature
  case class UnigramFeature(w: Feature, prev: Int, cur: Int) extends Feature

  // features that can be quickly computed
  class StandardFeaturizer {
    def localize(words: IndexedSeq[String])= new Localization(words)

    val interner = new Interner[Feature]

    class Localization(words: IndexedSeq[String]) {
      val classes = words.map(EnglishWordClassGenerator)
      val shapes = words.map(WordShapeGenerator)
      val featuresForWord: immutable.IndexedSeq[IndexedSeq[Feature]] = 0 until words.length map { pos =>
        IndexedSeq(SFeature(classes(pos), 'Class),
          SFeature(shapes(pos), 'Shape),
          SFeature(words(pos), 'Word),
          SFeature(if(pos == 0) "##" else words(pos-1), 'PrevWord),
          SFeature(if(pos == words.length - 1) "##" else words(pos+1), 'NextWord),
          SFeature(if(pos == 0) "##" else shapes(pos-1), 'PrevWordShape),
          SFeature(if(pos == words.length - 1) "##" else shapes(pos+1), 'NextWordShape),
          SFeature(if(pos == 0) "##" else classes(pos-1), 'PrevWordClass),
          SFeature(if(pos == words.length - 1) "##" else classes(pos+1), 'NextWordClass)
        ).map(interner.intern _)
      }

      def featuresForUnigram(prev: Int, cur:Int, pos: Int):IndexedSeq[Feature] = {
        featuresForWord(pos).map(UnigramFeature(_, prev, cur))
      }

      def featuresForBegin(prev: Int, cur: Int, start: Int):IndexedSeq[Feature] = {
        featuresForWord(start).map(BeginFeature(_, prev, cur))
      }

      def featuresForEnd(prev: Int, cur: Int, end: Int): IndexedSeq[Feature] = {
        featuresForWord(end).map(EndFeature(_, prev, cur))
      }

      def featuresForSpan(prev: Int, cur: Int, start: Int, end: Int):IndexedSeq[Feature] = {
        IndexedSeq(LengthFeature(binDistance(end - start), prev, cur), LengthFeature(binDistance(end - start), -1, cur))
      }

      private def binDistance(len: Int) = {
        if(len <= 1) 1
        else if(len <= 4) 2
        else if(len <= 8) 3
        else if(len <= 16) 4
        else 6
      }
    }

  }

  class IndexedStandardFeaturizer(f: StandardFeaturizer,
                                  val labelIndex: Index[NERType.Value],
                                  val featureIndex: Index[Feature]) extends SemiCRF.IndexedFeaturizer[NERType.Value,IndexedSeq[String]] {
    def startSymbol: NERType.Value = NERType.OutsideSentence

    def anchor(w: IndexedSeq[String]): AnchoredFeaturizer[NERType.Value, IndexedSeq[String]] = new AnchoredFeaturizer[NERType.Value, IndexedSeq[String]] {
      val loc = f.localize(w)

      val beginCache = Array.tabulate(labelIndex.size, labelIndex.size, w.length){
        loc.featuresForBegin(_,_,_).map(featureIndex).toArray
      }
      val endCache = Array.tabulate(labelIndex.size, labelIndex.size, w.length){
        loc.featuresForEnd(_,_,_).map(featureIndex).toArray
      }
      val wordCache = Array.tabulate(labelIndex.size, labelIndex.size, w.length){
        loc.featuresForUnigram(_,_,_).map(featureIndex).toArray
      }


      def featuresForTransition(prev: Int, cur: Int, start: Int, end: Int): SparseVector[Double] = {
        val builder = new SparseVector.Builder[Double](featureIndex.size)
        val _begin = beginCache(prev)(cur)(start)
        val _end = endCache(prev)(cur)(end-1)
        var i = 0
        while( i < _begin.length) {
          val index = _begin(i)
          if(index >= 0)
            builder.add(index, 1.0)
          i += 1
        }
        i = 0
        while( i < _end.length) {
          val index = _end(i)
          if(index >= 0)
            builder.add(index, 1.0)
          i += 1
        }
        var p = start
        while(p < end) {
          val w = wordCache(prev)(cur)(p)
          i = 0
          while( i < w.length) {
            val index = w(i)
            if(index >= 0)
            builder.add(index, 1.0)
            i += 1
          }
          p += 1
        }

        i = 0
        val forSpan = loc.featuresForSpan(prev, cur, start, end)
        while( i < forSpan.length) {
          val ind = featureIndex(forSpan(i))
          if(ind >= 0)
            builder.add(ind, 1)
          i += 1
        }

        builder.result()
      }
    }
  }

  object IndexedStandardFeaturizer {
    def apply(f: StandardFeaturizer, data: IndexedSeq[Segmentation[NERType.Value, IndexedSeq[String]]], maxLength: Int=>Int) = {
      val labelIndex: Index[NERType.Value] = EnumerationIndex(NERType)
      val featureIndex = Index[Feature]()
      var i = 0
      for(s <- data) {
        println(i + "/" + data.length)
        val loc = f.localize(s.words)

        var prev = labelIndex(NERType.OutsideSentence)
        for( (label, dspan) <- s.segments ) {
          val cur = labelIndex(label)
          loc.featuresForBegin(prev, cur, dspan.start) foreach {featureIndex.index _}
          for(b <- dspan.start until dspan.end) {
            loc.featuresForUnigram(prev, cur, b) foreach {featureIndex.index _}
          }
          loc.featuresForEnd(prev, cur, dspan.end-1) foreach {featureIndex.index _}
          loc.featuresForSpan(prev, cur, dspan.start, dspan.end) foreach {featureIndex.index _}

          prev = cur
        }

        /*
        for(prev <- 0 until labelIndex.size; cur <- 0 until labelIndex.size) {
          for(b <- 0 until s.length) {
            loc.featuresForUnigram(prev, cur, b) foreach {featureIndex.index _}
            loc.featuresForBegin(prev, cur, b) foreach {featureIndex.index _}
            loc.featuresForEnd(prev, cur, b) foreach {featureIndex.index _}
            for(e <- (b+1) until math.min(s.length,b+maxLength(cur))) {
              loc.featuresForSpan(prev, cur, b, e) foreach {featureIndex.index _}
            }

          }
        }
        */
        i += 1
      }
      new IndexedStandardFeaturizer(f, labelIndex, featureIndex)
    }

  }
  case class Params(path: File, name: String = "eval/ner", nfiles: Int = 100000)

  def main(args: Array[String]) {
    val (baseConfig, files) = CommandLineParser.parseArguments(args)
    val config = baseConfig backoff Configuration.fromPropertiesFiles(files.map(new File(_)))
    val params = config.readIn[Params]("")
    val instances = for {
      file <- params.path.listFiles take params.nfiles
      doc <- ConllOntoReader.readDocuments(file)
      s <- doc.sentences
    } yield makeSegmentation(s.ner, s.words, s.id)

    val maxLengthMap = instances.flatMap(_.segments.iterator).groupBy(_._1).mapValues(arr => arr.map(_._2.length).max)
    val labelIndex: Index[NERType.Value] = EnumerationIndex(NERType)
    val maxLengthArray = Encoder.fromIndex(labelIndex).tabulateArray(maxLengthMap.getOrElse(_, 0))
    println("Max Length: " + maxLengthMap)

    // build feature Index
    val feat = new SemiNERPipeline.StandardFeaturizer
    val indexed = IndexedStandardFeaturizer(feat, instances, maxLengthArray)

    val model = new SemiCRFModel(indexed.featureIndex, indexed, maxLengthArray)
    val obj = new ModelObjective(model, instances)
    val cached = new CachedBatchDiffFunction(obj)
    val whatevs = new RandomizedGradientCheckingFunction(cached, toString = {(i:Int) =>indexed.featureIndex.get(i).toString })

//    val weights = OptParams(maxIterations=100).minimize(whatevs, obj.initialWeightVector(randomize=true))
val weights = new LBFGS[DenseVector[Double]](100).minimize(whatevs, obj.initialWeightVector(randomize=false))



  }

}
