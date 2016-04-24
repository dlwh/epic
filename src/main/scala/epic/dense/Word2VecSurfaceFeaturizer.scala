package epic.dense

import breeze.linalg.DenseVector
import scala.collection.mutable.HashMap
import breeze.util.Index
import breeze.linalg.DenseMatrix
import breeze.linalg.Counter2
import breeze.linalg.Counter
import epic.features.HackyLexicalProductionFeaturizer
import breeze.linalg.sum
import epic.features.RuleBasedHackyHeadFinder
import epic.features.HackyHeadFinder
import epic.parser.RuleTopology
import epic.trees.AnnotatedLabel

/**
 * converter is used to map words into the word2vec vocabulary, which might include things
 * like lowercasing, replacing numbers, changing -LRB-, etc. See Word2Vec.convertWord
 */
class Word2VecIndexed[W](private val wordIndex: Index[W],
                         private val word2vec: Array[Array[Double]],
                         private val converter: W => W) extends Serializable {
  
  def wordRepSize = word2vec.head.length
  def vocSize = wordIndex.size
    
  val zeroVector = Array.tabulate(wordRepSize)(i => 0.0)
  
  def containsWord(rawStr: W) = wordIndex.contains(converter(rawStr))
  
  def indexWord(rawStr: W) = wordIndex(converter(rawStr))
  
  def convertIndexToVector(idx: Int) = word2vec(idx)
   
  private def assemble(vectors: Seq[Array[Double]]) = vectors.reduce(_ ++ _)
  
  def convertToVector(indexedWords: Array[Int]): Array[Double] = {
    assemble(indexedWords.map(wordIdx => if (wordIdx == -1) zeroVector else word2vec(wordIdx)))
  }
  
  def augment(numSparseFeats: Int, featurizer: W => Array[Int]): Word2VecIndexed[W] = {
    val newWord2Vec = Array.tabulate(word2vec.length)(i => {
      val word = wordIndex.get(i)
      val feats = featurizer(word)
      word2vec(i) ++ Array.tabulate(numSparseFeats)(j => if (feats.contains(j)) 1.0 else 0.0)
    })
    new Word2VecIndexed(wordIndex, newWord2Vec, converter)
  }
}

object Word2VecIndexed {
  
  def apply[W](word2vec: HashMap[W,Array[Double]],
               converter: W => W) = {
    val index = Index[W]
    val arr = new Array[Array[Double]](word2vec.size)
    for (word <- word2vec.keySet) {
      arr(index.index(word)) = word2vec(word)
    }
    new Word2VecIndexed(index, arr, converter)
  }
}

trait WordVectorAnchoringIndexed[String] {
  def reducedFeaturesForSpan(start: Int, end: Int): Array[Int]
  def featuresForSpan(start: Int, end: Int): Array[Int]
  def featuresForSplit(start: Int, split: Int, end: Int): Array[Int]
}

class Word2VecSurfaceFeaturizerIndexed[W](val word2vecIndexed: Word2VecIndexed[W],
                                          val featureSpec: String) extends Serializable {
  
  def reducedInputSize = {
    anchor(IndexedSeq[W]()).reducedFeaturesForSpan(0, 0).length * word2vecIndexed.wordRepSize
  }
  
  def splitInputSize = {
    anchor(IndexedSeq[W]()).featuresForSplit(0, 0, 0).length * word2vecIndexed.wordRepSize
  }
  
  def anchor(words: IndexedSeq[W]): WordVectorAnchoringIndexed[W] = {
    val indexedWords = words.map(word2vecIndexed.indexWord(_))
    new WordVectorAnchoringIndexed[W] {
      
      def reducedFeaturesForSpan(start: Int, end: Int) = {
        if (featureSpec == "" || featureSpec == "moresplit" || featureSpec == "basic") {
          Array(fetchWord(start - 1), fetchWord(start), fetchWord(end - 1), fetchWord(end))
        } else if (featureSpec == "morecontext") {
          Array(fetchWord(start - 2), fetchWord(start - 1), fetchWord(start), fetchWord(end - 1), fetchWord(end), fetchWord(end + 1))
        } else if (featureSpec == "morefirstlast") {
          Array(fetchWord(start - 1), fetchWord(start), fetchWord(start + 1), fetchWord(end - 2), fetchWord(end - 1), fetchWord(end))
        } else if (featureSpec == "mcmfl") {
          Array(fetchWord(start - 2), fetchWord(start - 1), fetchWord(start), fetchWord(start + 1), fetchWord(end - 2), fetchWord(end - 1), fetchWord(end), fetchWord(end + 1))
        } else if (featureSpec == "most") {
          Array(fetchWord(start - 2), fetchWord(start - 1), fetchWord(start), fetchWord(start + 1), fetchWord(end - 2), fetchWord(end - 1), fetchWord(end), fetchWord(end + 1))
        } else {
          throw new RuntimeException("Unknown featureSpec: " + featureSpec)
        }
      }
      
      def featuresForSpan(start: Int, end: Int) = {
        if (featureSpec == "" || featureSpec == "basic") {
          Array(fetchWord(start - 1), fetchWord(start), -1, -1, fetchWord(end - 1), fetchWord(end))
        } else if (featureSpec == "morecontext") {
          Array(fetchWord(start - 2), fetchWord(start - 1), fetchWord(start), -1, -1, fetchWord(end - 1), fetchWord(end), fetchWord(end + 1))
        } else if (featureSpec == "morefirstlast") {
          Array(fetchWord(start - 1), fetchWord(start), fetchWord(start + 1), -1, -1, fetchWord(end - 2), fetchWord(end - 1), fetchWord(end))
        } else if (featureSpec == "moresplit") {
          Array(fetchWord(start - 1), fetchWord(start), -1, -1, -1, -1, fetchWord(end - 1), fetchWord(end))
        } else if (featureSpec == "mcmfl") {
          Array(fetchWord(start - 2), fetchWord(start - 1), fetchWord(start), fetchWord(start + 1), -1, -1, fetchWord(end - 2), fetchWord(end - 1), fetchWord(end), fetchWord(end + 1))
        } else if (featureSpec == "most") {
          Array(fetchWord(start - 2), fetchWord(start - 1), fetchWord(start), fetchWord(start + 1), -1, -1, -1, -1, fetchWord(end - 2), fetchWord(end - 1), fetchWord(end), fetchWord(end + 1))
        } else {
          throw new RuntimeException("Unknown featureSpec: " + featureSpec)
        }
      }
        
      def featuresForSplit(start: Int, split: Int, end: Int) = {
        if (featureSpec == "" || featureSpec == "basic") {
          Array(fetchWord(start - 1), fetchWord(start), fetchWord(split - 1), fetchWord(split), fetchWord(end - 1), fetchWord(end))
        } else if (featureSpec == "morecontext") {
          Array(fetchWord(start - 2), fetchWord(start - 1), fetchWord(start), fetchWord(split - 1), fetchWord(split), fetchWord(end - 1), fetchWord(end), fetchWord(end + 1))
        } else if (featureSpec == "morefirstlast") {
          Array(fetchWord(start - 1), fetchWord(start), fetchWord(start + 1), fetchWord(split - 1), fetchWord(split), fetchWord(end - 2), fetchWord(end - 1), fetchWord(end))
        } else if (featureSpec == "moresplit") {
          Array(fetchWord(start - 1), fetchWord(start), fetchWord(split - 2), fetchWord(split - 1), fetchWord(split), fetchWord(split + 1), fetchWord(end - 1), fetchWord(end))
        } else if (featureSpec == "mcmfl") {
          Array(fetchWord(start - 2), fetchWord(start - 1), fetchWord(start), fetchWord(start + 1), fetchWord(split - 1), fetchWord(split), fetchWord(end - 2), fetchWord(end - 1), fetchWord(end), fetchWord(end + 1))
        } else if (featureSpec == "most") {
          Array(fetchWord(start - 2), fetchWord(start - 1), fetchWord(start), fetchWord(start + 1), fetchWord(split - 2), fetchWord(split - 1), fetchWord(split), fetchWord(split + 1), fetchWord(end - 2), fetchWord(end - 1), fetchWord(end), fetchWord(end + 1))
        } else {
          throw new RuntimeException("Unknown featureSpec: " + featureSpec)
        }
      }
      
      private def fetchWord(idx: Int): Int = {
        if (idx < 0 || idx >= words.size) -1 else indexedWords(idx)
      }
    } 
  }
}

object Word2VecSurfaceFeaturizerIndexed {
  
  def makeVectFromParams(wordIndices: Array[Int], params: DenseMatrix[Double]): DenseVector[Double] = {
    var currVect = DenseVector[Double](Array[Double]())
    for (wordIndex <- wordIndices) {
      currVect = DenseVector.vertcat(currVect, params(wordIndex, ::).t)
    }
    currVect
  }
}


trait WordVectorDepAnchoringIndexed[String] {
  def getHeadDepPair(begin: Int, split: Int, end: Int, rule: Int): (Int, Int)
  def featuresForHeadPair(head: Int, dep: Int): Array[Int]
}

class Word2VecDepFeaturizerIndexed[W](val word2VecIndexed: Word2VecIndexed[W],
                                      val tagger: Tagger[W],
                                      val topology: RuleTopology[AnnotatedLabel]) extends Serializable  {
  
  val hackyHeadFinder: HackyHeadFinder[String,String] = new RuleBasedHackyHeadFinder
    
  def anchor(words: IndexedSeq[W]): WordVectorDepAnchoringIndexed[W] = {
    val indexedWords = words.map(word2VecIndexed.indexWord)
    new WordVectorDepAnchoringIndexed[W] {
      
      val preterminals = new Array[String](words.size)
      words.indices.foreach { i =>
        preterminals(i) = tagger.tag(words(i))
      }
      
      def getHeadDepPair(begin: Int, split: Int, end: Int, rule: Int): (Int, Int) = {
        val lc = topology.labelIndex.get(topology.leftChild(rule)).baseLabel
        val rc = topology.labelIndex.get(topology.rightChild(rule)).baseLabel
        val parent = topology.labelIndex.get(topology.parent(rule)).baseLabel
      
        val lcHeadIdx = begin + hackyHeadFinder.findHead(lc, preterminals.slice(begin, split))
        val rcHeadIdx = split + hackyHeadFinder.findHead(rc, preterminals.slice(split, end))
        val overallHeadIdx = begin + hackyHeadFinder.findHead(parent, preterminals.slice(begin, end))
        if (overallHeadIdx == rcHeadIdx) {
          (rcHeadIdx, lcHeadIdx)
        } else {
          (lcHeadIdx, rcHeadIdx)
        }
      } 
      
      def featuresForHeadPair(head: Int, dep: Int) = {
        Array(fetchWord(head - 1), fetchWord(head), fetchWord(head+1), fetchWord(dep-1), fetchWord(dep), fetchWord(dep+1))
      }
        
      private def fetchWord(idx: Int): Int = {
        if (idx < 0 || idx >= words.size) -1 else indexedWords(idx)
      }
    } 
  }
}

trait Tagger[W] {
  def tag(word: W): String
}

class FrequencyTagger[W](wordTagCounts: Counter2[String, W, Double]) extends Tagger[W] with Serializable {
  
  private val wordCounts = Counter[W,Double]
  private val wordToTagMap = new HashMap[W,String]
  for (word <- wordTagCounts.keysIterator.map(_._2).toSeq.distinct) {
    wordCounts(word) = sum(wordTagCounts(::, word))
    if (!wordToTagMap.contains(word)) {
      val tagCounts = wordTagCounts(::, word).iterator
      var bestTag = HackyLexicalProductionFeaturizer.UnkTag
      var bestTagCount = 0.0
      for ((tag, count) <- tagCounts) {
        if (count > bestTagCount) {
          bestTag = tag
          bestTagCount = count
        }
      }
      wordToTagMap.put(word, bestTag)
    }
  }
  val tagTypesIdx = Index[String]
  wordToTagMap.values.toSet[String].foreach(tagType => tagTypesIdx.index(tagType))
  tagTypesIdx.index(HackyLexicalProductionFeaturizer.UnkTag)
  
  def tag(word: W) = if (wordToTagMap.contains(word)) wordToTagMap(word) else HackyLexicalProductionFeaturizer.UnkTag
  
  def convertToFeaturizer: W => Array[Int] = (word: W) => Array(tagTypesIdx.index(tag(word)))
}

