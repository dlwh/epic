package epic.features

import breeze.linalg.Counter
import epic.framework.Feature
import epic.features.FeaturizationLevel.{FullFeatures, BasicFeatures, MinimalFeatures}
import breeze.util.{Encoder, Interner, Index}
import epic.parser.features.IndicatorFeature
import breeze.text.analyze.{WordShapeGenerator, EnglishWordClassGenerator}

/**
 * TODO
 *
 * @author dlwh
 **/
class StandardWordFeaturizer(wordCounts: Counter[String, Double],
                             functionWordThreshold: Int = 100,
                             commonWordThreshold: Int = 20,
                             unknownWordThreshold: Int = 2) extends WordFeaturizer[String] with Serializable {

  def anchor(words: IndexedSeq[String]):WordFeatureAnchoring[String] = {
    val w = words
    new WordFeatureAnchoring[String] {
      val indices = words.map(wordIndex)
      val wholeSentenceIsUpperCase = words.forall(_.forall(c => !c.isLetter || c.isUpper))
      def words = w

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

      private val _minimalFeatures = (0 until words.length) map { i =>
        val index = indices(i)
        if(index >= 0) {
          StandardWordFeaturizer.this.minimalFeatures(index)
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
        val base: Array[Feature] = Array.concat(_minimalFeatures(i), {
          if(index >= 0) {
            StandardWordFeaturizer.this.basicFeatures(index).toArray[Feature]
          } else {
            wordShapeFeaturizer.apply(words(i)).map(interner(_)).toArray[Feature]
          }
        })

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

}
