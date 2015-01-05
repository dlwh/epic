package epic.dense

import breeze.linalg.DenseVector
import scala.collection.mutable.HashMap
import breeze.util.Index
import breeze.linalg.DenseMatrix


trait WordVectorAnchoring[String] {
  def featuresForSpan(start: Int, end: Int): DenseVector[Double];
  def featuresForSplit(start: Int, split: Int, end: Int): DenseVector[Double];
}
  
class Word2VecSurfaceFeaturizer[W](val word2vec: HashMap[W,Array[Double]],
                                   val converter: W => W) {
    
  def wordRepSize = word2vec.head._2.size
  def vectorSize: Int = 6 * wordRepSize
    
  val zeroVector = Array.tabulate(wordRepSize)(i => 0.0)
    
  def assemble(vectors: Seq[Array[Double]]) = vectors.reduce(_ ++ _)
    
  def anchor(words: IndexedSeq[W]): WordVectorAnchoring[W] = {
    val convertedWords = words.map(converter(_))
    new WordVectorAnchoring[W] {
        
      private def fetchVector(idx: Int): Array[Double] = {
        if (idx < 0 || idx >= words.size || !word2vec.contains(convertedWords(idx))) zeroVector else word2vec(convertedWords(idx))
      }
        
      def featuresForSpan(start: Int, end: Int) = {
        val vect = new DenseVector[Double](assemble(Seq(fetchVector(start - 1), fetchVector(start), zeroVector, zeroVector, fetchVector(end - 1), fetchVector(end))))
        vect
      }
        
      def featuresForSplit(start: Int, split: Int, end: Int) = {
        val vect = new DenseVector[Double](assemble(Seq(fetchVector(start - 1), fetchVector(start), fetchVector(split - 1), fetchVector(split), fetchVector(end - 1), fetchVector(end))))
        vect
      }
    }
      
  }
}

trait WordVectorAnchoringIndexed[String] {
  def featuresForSpan(start: Int, end: Int): Array[Int];
  def featuresForSplit(start: Int, split: Int, end: Int): Array[Int];
}


class Word2VecSurfaceFeaturizerIndexed[W](val wordIndex: Index[W],
                                          val word2vec: Array[Array[Double]],
                                          val converter: W => W) {
    
  def wordRepSize = word2vec.head.size
  def vectorSize: Int = 6 * wordRepSize
  
  def vocSize = wordIndex.size
    
  val zeroVector = Array.tabulate(wordRepSize)(i => 0.0)
    
  def assemble(vectors: Seq[Array[Double]]) = vectors.reduce(_ ++ _)
  
  def convertToVector(indexedWords: Array[Int]) = assemble(indexedWords.map(wordIdx => if (wordIdx == -1) zeroVector else word2vec(wordIdx)))
    
  def anchor(words: IndexedSeq[W]): WordVectorAnchoringIndexed[W] = {
    val convertedWords = words.map(converter(_))
    val indexedWords = convertedWords.map(wordIndex(_))
    new WordVectorAnchoringIndexed[W] {
      
      def featuresForSpan(start: Int, end: Int) = {
//        val vect = new DenseVector[Double](assemble(Seq(fetchVector(start - 1), fetchVector(start), zeroVector, zeroVector, fetchVector(end - 1), fetchVector(end))))
        Array(fetchWord(start - 1), fetchWord(start), -1, -1, fetchWord(end - 1), fetchWord(end))
      }
        
      def featuresForSplit(start: Int, split: Int, end: Int) = {
//        val vect = new DenseVector[Double](assemble(Seq(fetchVector(start - 1), fetchVector(start), fetchVector(split - 1), fetchVector(split), fetchVector(end - 1), fetchVector(end))))
        Array(fetchWord(start - 1), fetchWord(start), fetchWord(split - 1), fetchWord(split), fetchWord(end - 1), fetchWord(end))
      }
        
//      private def fetchWord(idx: Int): Int = {
//        if (idx < 0 || idx >= words.size || !word2vec.contains(convertedWords(idx))) -1 else wordIndex(convertedWords(idx))
//      }
      
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
  
  def apply[W](word2vec: HashMap[W,Array[Double]], converter: W => W) = {
    val index = Index[W]
    val arr = new Array[Array[Double]](word2vec.size)
    for (word <- word2vec.keySet) {
      arr(index.index(word)) = word2vec(word)
    }
    new Word2VecSurfaceFeaturizerIndexed(index, arr, converter)
  }
}

