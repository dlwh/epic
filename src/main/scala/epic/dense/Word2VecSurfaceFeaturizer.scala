package epic.dense

import breeze.linalg.DenseVector
import scala.collection.mutable.HashMap

  
trait WordVectorAnchoring {
  def featuresForSpan(start: Int, end: Int): DenseVector[Double];
  def featuresForSplit(start: Int, split: Int, end: Int): DenseVector[Double];
}
  
class Word2VecSurfaceFeaturizer[W](val word2vec: HashMap[W,Array[Double]],
                                   val converter: W => W) {
    
  def wordRepSize = word2vec.head._2.size
  def vectorSize: Int = 6 * wordRepSize
    
  val zeroVector = Array.tabulate(wordRepSize)(i => 0.0)
    
  def assemble(vectors: Seq[Array[Double]]) = vectors.reduce(_ ++ _)
    
  def anchor(words: IndexedSeq[W]): WordVectorAnchoring = {
    val convertedWords = words.map(converter(_))
    new WordVectorAnchoring {
        
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