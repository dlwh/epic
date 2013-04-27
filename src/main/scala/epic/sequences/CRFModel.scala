package epic.sequences

import epic.framework._
import breeze.util._
import breeze.linalg._
import scala.collection.mutable.ArrayBuffer
import epic.sequences.CRF.{AnchoredFeaturizer, TransitionVisitor}
import epic.parser.features.{LabelFeature, WordShapeFeaturizer, PairFeature}
import breeze.text.analyze.{WordShapeGenerator, EnglishWordClassGenerator}
import scala.collection

/**
 *
 * @author dlwh
 */
@SerialVersionUID(1L)
class CRFModel[L, W](val featureIndex: Index[Feature],
                         val featurizer: CRF.IndexedFeaturizer[L, W],
                         initialWeights: Feature=>Double = {(_: Feature) => 0.0}) extends Model[TaggedSequence[L, W]] with StandardExpectedCounts.Model with Serializable {
  def labelIndex: Index[L] = featurizer.labelIndex

  def extractCRF(weights: DenseVector[Double]) = {
    val grammar = inferenceFromWeights(weights)
    new CRF(grammar)
  }

  type Inference = CRFInference[L, W]
  type Marginal = CRF.Marginal[L, W]

  def initialValueForFeature(f: Feature): Double = initialWeights(f)

  def inferenceFromWeights(weights: DenseVector[Double]): Inference =
    new CRFInference(weights, featureIndex, featurizer)

}


@SerialVersionUID(1)
class CRFInference[L, W](weights: DenseVector[Double],
                         featureIndex: Index[Feature],
                         featurizer: CRF.IndexedFeaturizer[L, W]) extends AugmentableInference[TaggedSequence[L, W], CRF.Anchoring[L, W]] with CRF.Grammar[L, W] with Serializable {
  def viterbi(sentence: IndexedSeq[W], anchoring: CRF.Anchoring[L, W]): TaggedSequence[L, W] = {
    CRF.viterbi(new Anchoring(sentence, anchoring))
  }


  type Marginal = CRF.Marginal[L, W]
  type ExpectedCounts = StandardExpectedCounts[Feature]

  def emptyCounts = StandardExpectedCounts.zero(this.featureIndex)

  def anchor(w: IndexedSeq[W]) = new Anchoring(w, new IdentityAnchoring(w))


  def labelIndex = featurizer.labelIndex
  def startSymbol = featurizer.startSymbol

  def marginal(v: TaggedSequence[L, W], aug: CRF.Anchoring[L, W]): Marginal = {
    CRF.Marginal(new Anchoring(v.words, aug))
  }

  def goldMarginal(v: TaggedSequence[L, W], augment: CRF.Anchoring[L, W]): CRF.Marginal[L, W] = {
    CRF.Marginal.goldMarginal[L, W](new Anchoring(v.words, augment), v.label)
  }

  def countsFromMarginal(v: TaggedSequence[L, W], marg: Marginal, counts: ExpectedCounts, scale: Double): ExpectedCounts = {
    counts.loss += marg.logPartition * scale
    val localization = marg.anchoring.asInstanceOf[Anchoring].localization
    val visitor = new TransitionVisitor[L, W] {

      def apply(pos: Int, prev: Int, cur: Int, count: Double) {
        axpy(scale * count, localization.featuresForTransition(pos, prev, cur), counts.counts)
      }
    }
    marg.visit(visitor)
    counts
  }


  def baseAugment(v: TaggedSequence[L, W]): CRF.Anchoring[L, W] = new IdentityAnchoring(v.words)

  class IdentityAnchoring(val words: IndexedSeq[W]) extends CRF.Anchoring[L, W] {

    def labelIndex: Index[L] = featurizer.labelIndex

    def startSymbol: L = featurizer.startSymbol

    def validSymbols(pos: Int): Set[Int] = (0 until labelIndex.size).toSet

    def scoreTransition(pos: Int, prev: Int, cur: Int): Double = 0.0
  }

  class Anchoring(val words: IndexedSeq[W], augment: CRF.Anchoring[L, W]) extends CRF.Anchoring[L, W] {
    val localization = featurizer.anchor(words)

    val transCache = Array.tabulate(labelIndex.size, labelIndex.size, length){ (p,c,w) =>
      val f = localization.featuresForTransition(w, p, c)
      if (f eq null) Double.NegativeInfinity
      else weights dot f
    }


    def validSymbols(pos: Int): Set[Int] = localization.validSymbols(pos)

    def scoreTransition(pos: Int, prev: Int, cur: Int): Double = {
      augment.scoreTransition(pos, prev, cur) + transCache(prev)(cur)(pos)
    }

    def labelIndex: Index[L] = featurizer.labelIndex

    def startSymbol = featurizer.startSymbol
  }


  def posteriorDecode(m: Marginal):TaggedSequence[L, W] = {
    CRF.posteriorDecode(m)
  }
}

class TaggedSequenceModelFactory[L](val startSymbol: L,
                                    gazetteer: Gazetteer[Any, String] = Gazetteer.empty[String, String],
                                    weights: Feature=>Double = { (f:Feature) => 0.0}) {

  import TaggedSequenceModelFactory._

  def makeModel(train: IndexedSeq[TaggedSequence[L, String]]): CRFModel[L, String] = {
    val labelIndex: Index[L] = Index[L](Iterator(startSymbol) ++ train.iterator.flatMap(_.label))

    val counts: Counter2[String, L, Int] = Counter2.count(train.flatMap(p => p.words zip p.label))

    val f = new StandardFeaturizer[L](gazetteer, labelIndex, counts.mapValues(_.toDouble))
    val featureIndex = Index[Feature]()

    var i = 0
    for(s <- train) {
      val loc = f.localize(s.words)

      for {
        b <- 0 until s.length
        prevTag <- if(b == 0) Set(labelIndex(startSymbol)) else loc.allowedTags(b-1)
        l <- loc.allowedTags(b)
      } {
        loc.featuresForWord(b) foreach {f =>
          featureIndex.index(PairFeature(LabelFeature(labelIndex.get(l)), f) )
          featureIndex.index(PairFeature(LabelFeature(labelIndex.get(prevTag) -> labelIndex.get(l)), f) )
        }
      }
      i += 1
    }

    val indexed = new IndexedStandardFeaturizer[L](f, startSymbol,  labelIndex, featureIndex)
    val model = new CRFModel(indexed.featureIndex, indexed, weights(_))

    model
  }

}

object TaggedSequenceModelFactory {
  case class SFeature(w: Any, kind: Symbol) extends Feature
  case class BeginFeature[L](w: Feature, cur: L) extends Feature
  case class EndFeature[L](w: Feature, cur: L) extends Feature
  case class TrigramFeature(a: Any, b: Any, c: Any) extends Feature
  case class SpanFeature[L](distance: Feature, cur: L) extends Feature
  case class UnigramFeature[L](w: Feature, cur: L) extends Feature
  case class CFeature(component: Int, f: Feature) extends Feature
  case class DistanceFeature(distanceBin: Int) extends Feature
  case object TransitionFeature extends Feature
  case object SpanStartsSentence extends Feature
  case object SpansWholeSentence extends Feature

  /**
   * Computes basic features from word counts
   * @param wordTagCounts
   */
  @SerialVersionUID(1L)
  class StandardFeaturizer[L](gazetteer: Gazetteer[Any, String],
                              labelIndex: Index[L],
                              wordTagCounts: Counter2[String, L, Double]) extends Serializable {
    val wordCounts : Counter[String, Double] = sum(wordTagCounts, Axis._1)
    val inner = new WordShapeFeaturizer(wordCounts)

    val closedWords: Set[String] =  wordCounts.findAll(_ > 10).toSet

    def localize(words: IndexedSeq[String])= new Localization(words)

    val interner = new Interner[Feature]

    val labelSet = collection.immutable.BitSet.empty ++ (0 until labelIndex.size)

    val noShapeThreshold = 100
    class Localization(words: IndexedSeq[String]) {
      val allowedTags: IndexedSeq[Set[Int]] = words.map(w => if(closedWords(w)) Set.empty ++ wordTagCounts(w, ::).keySet.map(labelIndex(_)) else labelSet)

      private val classes = words.map(w => if (wordCounts(w) > noShapeThreshold) w else EnglishWordClassGenerator(w))
      private val shapes = words.map(w => if (wordCounts(w) > noShapeThreshold) w else WordShapeGenerator(w))

      val basicFeatures = (0 until words.length) map { i =>
        val w = words(i)
        if (wordCounts(w) > 10) IndexedSeq(w)
        else if (wordCounts(w) > 5) IndexedSeq(w, classes(i), shapes(i))
        else IndexedSeq(classes(i), shapes(i))
      } map {_.map(_.intern)}

      def basicFeature(pos: Int) = {
        if (pos < 0 || pos >= words.length) IndexedSeq("#")
        else basicFeatures(pos)
      }


      val featuresForWord: IndexedSeq[Array[Feature]] = 0 until words.length map { pos =>
        val feats = new ArrayBuffer[Feature]()
        val basic = basicFeature(pos).map(SFeature(_, 'Cur))
        val basicLeft = basicFeature(pos - 1).map(SFeature(_, 'Prev))
        val basicRight = basicFeature(pos + 1).map(SFeature(_, 'Next))
        feats ++= basicLeft
        feats ++= basicRight
        feats ++= inner.featuresFor(words, pos)
        for (a <- basicLeft; b <- basic) feats += PairFeature(a,b)
        for (a <- basic; b <- basicRight) feats += PairFeature(a,b)
        //        for (a <- basicLeft; b <- basicRight) feats += PairFeature(a,b)
        feats += TrigramFeature(basicLeft(0), basic(0), basicRight(0))
        if (pos > 0 && pos < words.length - 1) {
          feats += TrigramFeature(shapes(pos-1), shapes(pos), shapes(pos+1))
          feats += TrigramFeature(classes(pos-1), classes(pos), classes(pos+1))
        }
        feats ++= gazetteer.lookupWord(words(pos)).map(SFeature(_, 'WordSeenInSegment))
        feats.map(interner.intern _).toArray
      }

    }

  }

  @SerialVersionUID(1L)
  class IndexedStandardFeaturizer[L](
                                  f: StandardFeaturizer[L],
                                  val startSymbol: L,
                                  val labelIndex: Index[L],
                                  val featureIndex: Index[Feature]) extends CRF.IndexedFeaturizer[L,String] with Serializable { outer =>



    def anchor(w: IndexedSeq[String]): AnchoredFeaturizer[L, String] = new AnchoredFeaturizer[L, String] {
      val loc = f.localize(w)
      def featureIndex: Index[Feature] =  outer.featureIndex

      val allowedTags: IndexedSeq[Set[Int]] = Set(labelIndex(startSymbol)) +: loc.allowedTags

      val featureArray = Array.tabulate(w.length, labelIndex.size, labelIndex.size) { (b, prevTag, l) =>
        if(allowedTags(b+1)(l) && allowedTags(b)(prevTag)) {
          val vb = new VectorBuilder[Double](featureIndex.size)
          loc.featuresForWord(b) foreach {f =>
            val fi1 = featureIndex(PairFeature(LabelFeature(labelIndex.get(l)), f) )
            if(fi1 >= 0) {
              vb.add(fi1, 1.0)
              val fi2 = featureIndex(PairFeature(LabelFeature(labelIndex.get(prevTag) -> labelIndex.get(l)), f) )
              if(fi2 >= 0)
                vb.add(fi2, 1.0)
            }
          }
          vb.toSparseVector
        } else {
          null
        }
      }

      def featuresForTransition(pos: Int, prev: Int, cur: Int): SparseVector[Double] = {
        featureArray(pos)(prev)(cur)
      }

      def validSymbols(pos: Int): Set[Int] = allowedTags(pos+1)
    }
  }


}