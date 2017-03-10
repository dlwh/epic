package epic.features

import epic.framework.Feature
import breeze.linalg.Counter
import breeze.util.Index
import epic.trees.TreeInstance
import scala.collection.mutable.ArrayBuffer
import breeze.util.SerializableLogging

/**
 *
 *
 * @author dlwh
 */
@SerialVersionUID(1L)
class NGramSpanFeaturizer(wordCounts: Counter[String,Double],
                          bigramCounts: Counter[(String,String),Double],
                          allSents: Seq[Seq[String]],
                          ngramCountThreshold: Int,
                          maxOrder: Int,
                          useNot: Boolean) extends SurfaceFeaturizer[String] with Serializable {
  private val higherOrderCounts = (3 to maxOrder).map(n => NGramSpanFeaturizer.countNgrams(allSents, n))
  
  private val wordIndex = Index(wordCounts.keysIterator)
  private val bigramIndex = Index(bigramCounts.keysIterator)
  private val higherOrderIndices = (3 to maxOrder).map(n => Index(higherOrderCounts(n-3).keysIterator))
  println(wordIndex.size + " unigrams, " + bigramIndex.size + " bigrams, " + higherOrderIndices.map(_.size) + " higher-order n-grams")
  
  def anchor(words: IndexedSeq[String]): SurfaceFeatureAnchoring[String] = {
    new SurfaceFeatureAnchoring[String] {
      def featuresForSpan(begin: Int, end: Int): Array[Feature] = {
      // println("Span: " + words.slice(begin, end))
        val unigramFeats = for (i <- begin until end) yield {
        // println(words(i) + ": " + wordCounts(words(i)))
          NGramUnigramFeature(if (wordCounts(words(i)) < ngramCountThreshold) -1 else wordIndex(words(i)))
        }
        val bigramFeats = for (i <- begin until end - 1) yield {
          val pair = (words(i), words(i+1))
          // println(pair + ": " + bigramCounts(pair))
          NGramBigramFeature(if (bigramCounts(pair) < ngramCountThreshold) -1 else bigramIndex(pair))
        }
        val notFeats = if (useNot) {
          val notFeats = new ArrayBuffer[Feature]
          var inNotSpan = false
          for (i <- begin until end) {
            if (NGramSpanFeaturizer.NotWords.contains(words(i))) {
              inNotSpan = true
            } else if (NGramSpanFeaturizer.NotEndingPunc.contains(words(i))) {
              inNotSpan = false
            } else if (inNotSpan) {
            // println(words.slice(begin, end) + " (not span): " + words(i))
              notFeats += NotFeature(if (wordCounts(words(i)) < ngramCountThreshold) -1 else wordIndex(words(i)))
            }
          }
          notFeats.toArray
        } else {
          Array[Feature]()
        }
        if (maxOrder >= 3) {
          val ngramFeats = (3 to maxOrder).flatMap(n => {
            for (i <- begin until end - n + 1) yield {
              val slice = words.slice(i, i+n)
              // println(slice + ": " + higherOrderCounts(n-3)(slice))
              NGramFeature(n, if (higherOrderCounts(n-3)(slice) < ngramCountThreshold) -1 else higherOrderIndices(n-3)(slice))
            }
          })
          (unigramFeats ++ bigramFeats ++ notFeats ++ ngramFeats).toArray
        } else {
          (unigramFeats ++ bigramFeats ++ notFeats).toArray
        }
        
      }
    }
  }
}

object NGramSpanFeaturizer {
  
  val NotWords = Set("not", "n't")
  val NotEndingPunc = Set(".", ",", ";", "--", ":")

  def countBigrams[L, W](data: TraversableOnce[TreeInstance[L, W]]) = {
    val bigrams = Counter[(W,W), Double]()
    for( ti <- data) {
      val TreeInstance(_, tree, words) = ti
      for (i <- 0 until words.size - 1) {
        bigrams((words(i), words(i+1))) += 1.0
      }
    }
    bigrams
  }

  def countNgrams(allSents: Seq[Seq[String]], n: Int) = {
    val ngrams = Counter[Seq[String], Double]()
    for(sent <- allSents) {
      for (i <- 0 until sent.size - n) {
        ngrams(sent.slice(i, i+n)) += 1.0
      }
    }
    ngrams
  }
}

case class NotFeature(idx: Int) extends Feature

case class NGramUnigramFeature(idx: Int) extends Feature

case class NGramBigramFeature(idx: Int) extends Feature

case class NGramFeature(n: Int, idx: Int) extends Feature
