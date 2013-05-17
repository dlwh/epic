package epic.features

import epic.framework.Feature
import breeze.linalg.Counter
import breeze.util.{Encoder, Interner, Index}
import scala.Array
import epic.parser.features.{PairFeature, IndicatorFeature}
import breeze.text.analyze.{WordShapeGenerator, EnglishWordClassGenerator}
import scala.collection.mutable.ArrayBuffer
import epic.parser.features.StandardSpanFeatures._
import epic.features.FeaturizationLevel.{FullFeatures, BasicFeatures, MinimalFeatures}

/**
 *
 * @author dlwh
 */
@SerialVersionUID(1L)
class StandardSurfaceFeaturizer(wordCounts: Counter[String, Double],
                                functionWordThreshold: Int = 100,
                                commonWordThreshold: Int = 20,
                                unknownWordThreshold: Int = 2) extends SurfaceFeaturizer[String] with Serializable {

  def anchor(words: IndexedSeq[String]):SurfaceFeatureAnchoring[String] = {
    val w = words
    new SurfaceFeatureAnchoring[String] {
      val indices = words.map(wordIndex)
      val wholeSentenceIsUpperCase = words.forall(_.forall(c => !c.isLetter || c.isUpper))
      def words = w
      val sentenceLengthFeature = SentenceLengthFeature(distanceBinner.binnedDistance(0,words.length))

      def featuresForWord(pos: Int, level: FeaturizationLevel): Array[Feature] = {
        if(pos < 0 || pos >= words.length) {
          boundaryFeatures
        } else {
          level match {
            case MinimalFeatures => _minimalFeatures(pos)
            case BasicFeatures   => _minimalFeatures(pos)
            case FullFeatures    =>  _fullFeatures(pos)
          }
        }
      }


      def featuresForSpan(begin: Int, end: Int, level: FeaturizationLevel): Array[Feature] = {
        val feats = ArrayBuffer[Feature]()
        if (begin < end - 1 && level.level >= BasicFeatures.level) {
          for(f1 <- featuresForWord(begin, level.less); f2 <- featuresForWord(end-1, level.less)) {
            feats += WordEdges('Inside, f1, f2)
            feats += WordEdges('Outside, f1, f2)
          }
        }

        if(level.level >= FeaturizationLevel.BasicFeatures.level) {
          feats += sentenceLengthFeature
          if(wholeSentenceIsUpperCase)
            feats += WholeSentenceIsUpperCaseFeature
        }


        if (begin == 0)
          feats += BeginSentFeature
        if(end == words.length)
          feats += EndSentFeature
        if (begin == 0 && end == words.length)
          feats += WholeSentFeature

        if(level.level >= BasicFeatures.level) {
          feats += SpanShapeFeature(SpanShapeGenerator.apply(words, begin,end))
          feats += SpanShapeFeature(SpanShapeGenerator.signatureFor(words, begin, end, false))
        }

        if(level == FullFeatures) {
          val spanLength = SpanLengthFeature(distanceBinner.binnedDistance(begin, end))
          for(f <- feats.toArray) {
            feats += PairFeature(f, spanLength)
          }
          feats += spanLength
        } else if(level == BasicFeatures) {
          feats += SpanLengthFeature(distanceBinner.binnedDistance(begin, end))
        }


        feats.toArray
      }

      private val _minimalFeatures = (0 until words.length) map { i =>
        val index = indices(i)
        if(index >= 0) {
          StandardSurfaceFeaturizer.this.minimalFeatures(index)
        } else {
          val ww = words(i)
          val classe = interner(WordFeature(EnglishWordClassGenerator(ww), 'Class))
          val shape = interner(WordFeature(WordShapeGenerator(ww), 'Shape))
          Array(Unk, classe, shape)
        }
      }

      def wordIsReasonablyRare(i: Int): Boolean = _minimalFeatures.length > 1

      private val _fullFeatures = (0 until words.length) map {  i =>
        val index = indices(i)
        val base = if(index >= 0) {
          StandardSurfaceFeaturizer.this.basicFeatures(index).toArray
        } else {
          wordShapeFeaturizer.apply(words(i)).map(interner(_)).toArray
        } ++ _minimalFeatures(i)

        // initial words nee special treatment
        if( (words(i).charAt(0).isUpper || words(i).charAt(0).isTitleCase) && wordIsReasonablyRare(i)) {
          val isInitialWord = (i == 0 || words(i-1) == "``")
          if(isInitialWord) {
            base ++ base.map(FirstWordCapsAnd)
          } else {
            base ++ base.map(NthWordCapsAnd)
          }
        } else {
          base
        }
      }

    }

  }


  private val wordShapeFeaturizer = new WordShapeFeaturizer(wordCounts, commonWordThreshold, unknownWordThreshold)
  // more positional shapes to add

  private val wordIndex = Index(wordCounts.keySet)
  private val interner = new Interner[Feature]

  private val Unk = WordFeature("#UNK#", 'LowCount)
  private val boundaryFeatures = Array[Feature](BoundaryFeature)

  private val wordFeatures = Encoder.fromIndex(wordIndex).tabulateArray(s => if(wordCounts(s) > unknownWordThreshold) interner(IndicatorFeature(s)) else Unk)
  private val classes = Encoder.fromIndex(wordIndex).tabulateArray(w => if(wordCounts(w) > functionWordThreshold) wordFeatures(wordIndex(w)) else interner(WordFeature(EnglishWordClassGenerator(w), 'Class)))
  private val shapes =  Encoder.fromIndex(wordIndex).tabulateArray(w => if(wordCounts(w) > functionWordThreshold) wordFeatures(wordIndex(w)) else interner(WordFeature(WordShapeGenerator(w), 'Shape)))


  // caches
  private val minimalFeatures = Array.tabulate(wordIndex.size){ i =>
    val wc = wordCounts(wordIndex.get(i))
    val w = wordFeatures(i)
    val shape =  shapes(i)
    val classe =  classes(i)
    if(wc > functionWordThreshold) Array(w)
    else if (wc > unknownWordThreshold) Array(w, shape, classe)
    else Array(shape, classe, Unk)
  }

  private val basicFeatures = Array.tabulate(wordIndex.size) {i =>
    wordShapeFeaturizer.apply(wordIndex.get(i)).map(interner.intern _)
  }

  val distanceBinner = DistanceBinner()

}

case class FirstWordCapsAnd(f: Feature) extends Feature
case class NthWordCapsAnd(f: Feature) extends Feature
case class SentenceLengthFeature(length: Int) extends Feature
case object WholeSentenceIsUpperCaseFeature extends Feature
case class WordFeature(word: Any, kind: Symbol) extends Feature

case object BoundaryFeature extends Feature
