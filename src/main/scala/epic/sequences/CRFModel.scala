package epic.sequences

import epic.framework._
import breeze.util._
import breeze.linalg._
import epic.sequences.CRF.{ AnchoredFeaturizer, TransitionVisitor }
import breeze.features.FeatureVector
import epic.features._
import epic.lexicon.{ SignatureLexicon, SimpleLexicon }
import java.util

import epic.util.{ Optional, ProgressLog }
import epic.constraints.TagConstraints

/**
 *
 * @author dlwh
 */
@SerialVersionUID(1L)
class CRFModel[L, W](val featureIndex: Index[Feature],
                     val lexicon: TagConstraints.Factory[L, W],
                     val featurizer: CRF.IndexedFeaturizer[L, W],
                     initialWeights: Feature=>Double = {(_: Feature) => 0.0}) extends Model[TaggedSequence[L, W]] with StandardExpectedCounts.Model[TaggedSequence[L, W]] with Serializable {
  def labelIndex: Index[L] = featurizer.labelIndex

  def extractCRF(weights: DenseVector[Double]) = {
     inferenceFromWeights(weights)
  }

  type Inference = CRFInference[L, W]
  type Marginal = CRF.Marginal[L, W]
  type Scorer = CRF.Anchoring[L, W]

  def initialValueForFeature(f: Feature): Double = initialWeights(f)

  def inferenceFromWeights(weights: DenseVector[Double]): Inference =
    new CRFInference(weights, featureIndex, lexicon, featurizer)


  def accumulateCounts(inf: Inference, s: Scorer, d: TaggedSequence[L, W], marg: Marginal, counts: ExpectedCounts, scale: Double): Unit = {
    counts.loss += marg.logPartition * scale
    val localization = s.asInstanceOf[inf.Anchoring].localization
    val visitor = new TransitionVisitor[L, W] {

      def apply(pos: Int, prev: Int, cur: Int, count: Double) {
        val feats = localization.featuresForTransition(pos, prev, cur)
        if (count != 0) assert(feats ne null, (pos, prev, cur, marg.length, marg.anchoring.validSymbols(pos), marg.anchoring.validSymbols(pos-1)))
        axpy(scale * count, feats, counts)
      }
    }
    marg.visit(visitor)
  }

}


@SerialVersionUID(1)
class CRFInference[L, W](val weights: DenseVector[Double],
                         val featureIndex: Index[Feature],
                         val lexicon: TagConstraints.Factory[L, W],
                         featurizer: CRF.IndexedFeaturizer[L, W]) extends AugmentableInference[TaggedSequence[L, W], CRF.Anchoring[L, W]] with CRF[L, W] with AnnotatingInference[TaggedSequence[L, W]] with Serializable {

  def scorer(v: TaggedSequence[L, W]): Scorer = new Anchoring(v.words)

  def viterbi(sentence: IndexedSeq[W], anchoring: CRF.Anchoring[L, W]): TaggedSequence[L, W] = {
    CRF.viterbi(new Anchoring(sentence) * anchoring)
  }

  def annotate(datum: TaggedSequence[L, W], m: Marginal): TaggedSequence[L, W] = {
    CRF.posteriorDecode(m)
  }

  type Marginal = CRF.Marginal[L, W]
  type ExpectedCounts = StandardExpectedCounts[Feature]
  type Scorer = CRF.Anchoring[L, W]

  def emptyCounts = StandardExpectedCounts.zero(this.featureIndex)

  def anchor(w: IndexedSeq[W]) = new Anchoring(w)

  def labelIndex = featurizer.labelIndex
  def startSymbol = featurizer.startSymbol

  def marginal(scorer: Scorer, v: TaggedSequence[L, W], aug: CRF.Anchoring[L, W]): CRFInference[L, W]#Marginal = {
    CRF.Marginal(scorer * aug)
  }

  def goldMarginal(scorer: Scorer, v: TaggedSequence[L, W], aug: CRF.Anchoring[L, W]): Marginal = {
    CRF.Marginal.goldMarginal[L, W](new Anchoring(v.words) * aug, v.label)
  }

  private val allLabels =  (0 until labelIndex.size).toSet

  def baseAugment(v: TaggedSequence[L, W]): CRF.Anchoring[L, W] = {
    new CRF.IdentityAnchoring(v.words, IndexedSeq.fill(v.words.length)(allLabels),labelIndex, featurizer.startSymbol)
  }

  class Anchoring(val words: IndexedSeq[W]) extends CRF.Anchoring[L, W] {
    val localization = featurizer.anchor(words)

    val transCache = Array.ofDim[Double](labelIndex.size, labelIndex.size, length)
    for(a <- transCache; b <- a) util.Arrays.fill(b, Double.NegativeInfinity)
    for(i <- 0 until length; c <- validSymbols(i); p <- validSymbols(i-1)) {
      val feats = localization.featuresForTransition(i, p, c)
      if (feats ne null)
        transCache(p)(c)(i) = weights dot feats
      else transCache(p)(c)(i) = Double.NegativeInfinity
    }

    def validSymbols(pos: Int): Set[Int] = localization.validSymbols(pos)

    def scoreTransition(pos: Int, prev: Int, cur: Int): Double = {
       transCache(prev)(cur)(pos)
    }

    def labelIndex: Index[L] = featurizer.labelIndex

    def startSymbol = featurizer.startSymbol
  }

  def posteriorDecode(m: Marginal):TaggedSequence[L, W] = {
    CRF.posteriorDecode(m)
  }
}

class TaggedSequenceModelFactory[L](val startSymbol: L,
                                    gazetteer: Optional[Gazetteer[Any, String]] = None,
                                    wordFeaturizer: Optional[WordFeaturizer[String]] = None,
                                    transitionFeaturizer: Optional[WordFeaturizer[String]] = None,
                                    weights: Feature=>Double = { (f:Feature) => 0.0},
                                    hashFeatureScale: Double = 0.0) extends SerializableLogging {

  import TaggedSequenceModelFactory._

  def makeModel(train: IndexedSeq[TaggedSequence[L, String]]): CRFModel[L, String] = {
    val labelIndex: Index[L] = Index[L](Iterator(startSymbol) ++ train.iterator.flatMap(_.label))
    val counts: Counter2[L, String, Double] = Counter2.count(train.flatMap(p => p.label zip p.words)).mapValues(_.toDouble)

    val lexicon: TagConstraints.Factory[L, String] = SignatureLexicon.fromCounts[L, String](labelIndex, counts)

    var featurizer: WordFeaturizer[String] = wordFeaturizer.getOrElse(WordFeaturizer.goodPOSTagFeaturizer(counts))
    featurizer = gazetteer.foldLeft(featurizer)(_ + _)
    val l2featurizer: WordFeaturizer[String] = transitionFeaturizer.getOrElse(WordFeaturizer.goodPOSTagTransitionFeaturizer(counts))

    val indexedFeaturizer = IndexedWordFeaturizer.fromData(featurizer, train.map{_.words})
    val indexedL2featurizer = IndexedWordFeaturizer.fromData(l2featurizer, train.map{_.words})

    val lfBuilder = new CrossProductIndex.Builder(labelIndex, indexedFeaturizer.featureIndex, includeLabelOnlyFeatures = false, hashFeatures = HashFeature.Relative(hashFeatureScale))
    val label2Index = Index[(L, L)]()
    val label2Features = Array.tabulate(labelIndex.size, labelIndex.size) { (l1, l2) =>
      label2Index.index(labelIndex.get(l1) -> labelIndex.get(l2))
    }
    val l2Builder = new CrossProductIndex.Builder(label2Index, indexedL2featurizer.featureIndex, includeLabelOnlyFeatures = true, hashFeatures = HashFeature.Relative(hashFeatureScale))

    val progress = new ProgressLog(logger, train.length, frequency=1000, name = "NumFeatures")
    for(s <- train) {
      val loc = indexedFeaturizer.anchor(s.words)
      val l2loc = indexedL2featurizer.anchor(s.words)
      val lexLoc = lexicon.anchor(s.words)

      for {
        b <- 0 until s.length
        l <- lexLoc.allowedTags(b)
      } {
        if (lexLoc.allowedTags(b).size > 1) {
          lfBuilder.add(l, loc.featuresForWord(b))
          for(prevTag <- if (b == 0) Set(labelIndex(startSymbol)) else lexLoc.allowedTags(b-1)) {
            l2Builder.add(label2Features(prevTag)(l), l2loc.featuresForWord(b))
          }
        }
      }
      progress.info(s"${lfBuilder.size + l2Builder.size}")
    }

    val indexed = new IndexedStandardFeaturizer[L, String](indexedFeaturizer,
      indexedL2featurizer, lexicon, startSymbol, labelIndex, label2Features, lfBuilder.result(), l2Builder.result())

    logger.info(s"There are ${indexed.label2FeatureIndex.size} and ${indexed.labelFeatureIndex.size} features total.")
    val model = new CRFModel(indexed.featureIndex, lexicon, indexed, weights)

    model
  }

}

object TaggedSequenceModelFactory {

  @SerialVersionUID(1L)
  class IndexedStandardFeaturizer[L, String](wordFeaturizer: IndexedWordFeaturizer[String],
                                             l2WordFeaturizer: IndexedWordFeaturizer[String],
                                             val lexicon: TagConstraints.Factory[L, String],
                                             val startSymbol: L,
                                             val labelIndex: Index[L],
                                             label2Features: Array[Array[Int]],
                                             val labelFeatureIndex: CrossProductIndex[L, Feature],
                                             val label2FeatureIndex: CrossProductIndex[(L, L), Feature]
                                             ) extends CRF.IndexedFeaturizer[L,String] with Serializable { outer =>

    val featureIndex = SegmentedIndex(labelFeatureIndex, label2FeatureIndex)
    private val loff = featureIndex.componentOffset(0)
    private val l2off = featureIndex.componentOffset(1)

    private val startSymbolSet = Set(labelIndex(startSymbol))

    def anchor(w: IndexedSeq[String]): AnchoredFeaturizer[L, String] = new AnchoredFeaturizer[L, String] {
      val loc = wordFeaturizer.anchor(w)
      val l2loc = l2WordFeaturizer.anchor(w)
      val lexLoc = lexicon.anchor(w)
      def featureIndex: Index[Feature] =  outer.featureIndex

      def validSymbols(pos: Int): Set[Int] = if (pos < 0 || pos >= w.length) startSymbolSet else  lexLoc.allowedTags(pos)

      def length = w.length

      val featureArray = Array.ofDim[FeatureVector](length, labelIndex.size, labelIndex.size)
      private val posNeedsAmbiguity = Array.tabulate(length)(i => validSymbols(i).size > 1)
      for {
        pos <- 0 until length
        curTag <- validSymbols(pos)
        features = loc.featuresForWord(pos)
        justLabel = labelFeatureIndex.crossProduct(Array(curTag), features, offset = loff, usePlainLabelFeatures = false)
        prevTag <- validSymbols(pos-1)
      } {
        val l2feats = l2loc.featuresForWord(pos)
        val feats = if (posNeedsAmbiguity(pos)) {
          justLabel++ label2FeatureIndex.crossProduct(Array(label2Features(prevTag)(curTag)), l2feats, offset = l2off, usePlainLabelFeatures = true)
        } else {
          justLabel
        }

        featureArray(pos)(prevTag)(curTag) = new FeatureVector(feats)
      }

      def featuresForTransition(pos: Int, prev: Int, cur: Int): FeatureVector = {
        featureArray(pos)(prev)(cur)
      }

    }
  }

}
