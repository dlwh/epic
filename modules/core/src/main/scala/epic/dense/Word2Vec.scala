package epic.dense

import java.io.BufferedInputStream
import java.io.DataInputStream
import java.io.FileInputStream
import java.util.regex.Pattern
import scala.collection.mutable.HashMap
import scala.util.Random
import breeze.linalg.Counter
import java.io.File

object Word2Vec {
  /**
   * Loads vectors from one or more sources in word2vecPaths; these might be in
   * word2vec format (should end in .bin) or C+W/Bansal format (should end
   * in .txt).
   * 
   * For each word, vectors are appended from each source. If at least one source
   * is present, others are zeroes. Otherwise, it gets a random vector.
   */
  def smartLoadVectorsForVocabulary(word2vecPaths: Seq[String], voc: Set[String], vocCounts: Counter[String,Double] = Counter[String,Double], maxVectorLen: Int = Int.MaxValue, inputVectorBias: Boolean, randomizeUnks: Boolean = true) = {
    val vectorsEachSource = for (word2vecPath <- word2vecPaths) yield {
      if (word2vecPath.endsWith("bin")) {
        readWord2Vec(word2vecPath, voc, false)
      } else if (word2vecPath.endsWith(".txt")) {
        readBansalEmbeddings(word2vecPath, voc, false)
      } else {
        throw new RuntimeException("Unrecognized vectors: " + word2vecPath)
      }
    }
    val dimsEachSource = vectorsEachSource.map(_.values.head.length)
    val finalVectorDim = Math.min(maxVectorLen, dimsEachSource.sum + (if (inputVectorBias) 1 else 0))
    val finalVectors = new HashMap[String,Array[Float]]
    val rng = new Random(0)
    val mostCommonMisses = Counter[String,Double]
    var numRand = 0
    for (word <- voc) {
      val containedInSome = vectorsEachSource.map(_.keySet.contains(word)).reduce(_ || _)
      val vector = if (containedInSome) {
        var finalVector = vectorsEachSource.indices.map(i => vectorsEachSource(i).getOrElse(word, { Array.tabulate(dimsEachSource(i))(j => 0.0F) })).reduce(_ ++ _)
        if (inputVectorBias) {
          finalVector = finalVector ++ Array(1.0F) 
        }
        finalVector
      } else {
        mostCommonMisses(word) = vocCounts(word)
        numRand += 1
        if (randomizeUnks) {
          Array.tabulate(finalVectorDim)(i => if (i == finalVectorDim - 1 && inputVectorBias) 1.0F else ((rng.nextDouble - 0.5) * 0.5).toFloat)
        } else {
          Array.tabulate(finalVectorDim)(i => if (i == finalVectorDim - 1 && inputVectorBias) 1.0F else 0.0F)
        }
      }
      val vectorTrimmed = if (vector.length > finalVectorDim) vector.slice(0, finalVectorDim) else vector
      require(vectorTrimmed.length == finalVectorDim, "Mismatched sizes, expected dimension " + finalVectorDim + " but got " + vector.length + " clipped to " + vectorTrimmed.length)
      finalVectors.put(word, vectorTrimmed)
    }
    println("Read embeddings for " + voc.size + " words from " + word2vecPaths.size + " sources, " +
            "total embedding size = " + finalVectorDim + ", " + numRand + " present in no source")
    println("Fifty most common misses: " + mostCommonMisses.argtopk(50).map(word => word + ": " + mostCommonMisses(word)))
    finalVectors
  }
  
  def makeRandomVectorsForVocabulary(voc: Set[String], dim: Int, inputVectorBias: Boolean) = {
    val finalVectors = new HashMap[String,Array[Float]]
    val finalVectorDim = dim + (if (inputVectorBias) 1 else 0)
    val rng = new Random(0)
    var numRand = 0
    for (word <- voc) {
      val vec = Array.tabulate(finalVectorDim)(i => if (i == finalVectorDim - 1 && inputVectorBias) 1.0F else ((rng.nextDouble - 0.5) * 0.5).toFloat)
      finalVectors.put(word, vec)
    }
    finalVectors
  }
  
  /**
   * Loads vectors for a vocabulary from word2vec, with OOV words having random vectors
   * generated for them.
   */
  def loadVectorsForVocabulary(word2vecPath: String, voc: Set[String], inputVectorBias: Boolean) = {
    val word2vecMap = readWord2Vec(word2vecPath, voc, inputVectorBias)
    if (word2vecMap.isEmpty) {
      throw new RuntimeException("No word2vec vectors loaded")
    }
    augmentVectorsToCompleteVocabulary(word2vecMap, voc, inputVectorBias)
  }
  
  def loadBansalVectorsForVocabulary(word2vecPath: String, voc: Set[String], inputVectorBias: Boolean) = {
    val word2vecMap = readBansalEmbeddings(word2vecPath, voc, inputVectorBias)
    if (word2vecMap.isEmpty) {
      throw new RuntimeException("No Bansal vectors loaded")
    }
    augmentVectorsToCompleteVocabulary(word2vecMap, voc, inputVectorBias)
  }
  
  private def augmentVectorsToCompleteVocabulary(word2vecMap: HashMap[String,Array[Float]], voc: Set[String], inputVectorBias: Boolean) = {
    val word2vecDim = word2vecMap.values.head.length
    val rng = new Random(0)
    for (unkWord <- voc -- word2vecMap.keySet) {
      // Set to random noise except for the bias feature, if it's there
      word2vecMap.put(unkWord, Array.tabulate(word2vecDim)(i => if (i == word2vecDim - 1 && inputVectorBias) 1.0F else ((rng.nextDouble - 0.5) * 0.5).toFloat))
    }
    word2vecMap
  }

  /**
   * Reads the vectors in words from the given word2vec path and augments with a bias feature
   * if necessary. The returned map does not include entries for words that are not in the w2v
   * file.
   */
  def readWord2Vec(word2VecPath: String, words: Set[String], inputVectorBias: Boolean) = {
    val bis = new BufferedInputStream(new FileInputStream(word2VecPath))
    val dis = new DataInputStream(bis)
    val word2Vec = new HashMap[String,Array[Float]]
    // First two entries are vocabulary size and dimension of vectors
    val vocSize = Word2VecUtils.readString(dis).toInt
    val dim = Word2VecUtils.readString(dis).toInt
    // Now read vectors, augmented with 1s for bias
    for (i <- 0 until vocSize) {
      if (i % 1000000 == 0) {
        println("On line " + i)
      }
      val word = Word2VecUtils.readString(dis)
      val vector = new Array[Float](if (inputVectorBias) dim + 1 else dim)
      val len = 0
      var j = 0
      while (j < dim) {
        vector(j) = Word2VecUtils.readFloat(dis)
        j += 1
      }
      if (inputVectorBias) {
        vector(j) = 1.0F
      }
      if (words.isEmpty || words.contains(word)) {
        word2Vec.put(word, vector)
      }
    }
    println("Loaded " + word2Vec.size + " word2vec representations out of " + words.size + " attempted words")
    word2Vec
  }
  
  val hyphenPattern = Pattern.compile("(\\w+-)+(\\w+)")
  
  def convertWord(str: String, lowercase: Boolean = false) = {
    var strRep = str
    strRep = strRep.replace("-LRB-", "(")
    strRep = strRep.replace("-RRB-", ")")
    strRep = strRep.replace("-LSB-", "[")
    strRep = strRep.replace("-RSB-", "]")
    strRep = strRep.replace("-LCB-", "{")
    strRep = strRep.replace("-RCB-", "}")
    // Replace all numbers with 15
    strRep = strRep.replaceAll("^-?[0-9,.]{2,15}$", "fifteen")
    // Replace hyphenated words with the last part
    val m = hyphenPattern .matcher(str)
    strRep = if (m.find()) {
      m.group(2)
    } else {
      strRep
    }
    if (lowercase) {
      strRep = strRep.toLowerCase()
    }
    strRep
  }
  
  def readBansalEmbeddings(embeddingsPath: String, words: Set[String], inputVectorBias: Boolean) = {
    val inFile = scala.io.Source.fromFile(new File(embeddingsPath)).getLines()
    val word2Vec = new HashMap[String,Array[Float]]
    var firstLine = true
    while (inFile.hasNext) {
      val line = inFile.next
      if (firstLine) {
        if (line.split("\\s+").size == 2) {
          println("Skipping first line: " + line)
          // Just an indicator of how many words there are and the vector dim, so
          // skip over it by leaving firstLine set to true
        } else {
          println("Not skipping first line: " + line)
          firstLine = false
        }
      }
      if (!firstLine) {
        // If the line contains a tab, then that's the delimiter between the word and
        // the vectors
        if (line.contains("\t")) {
          val word = line.substring(0, line.indexOf("\t"))
          if (words.isEmpty || words.contains(word)) {
            val entries = line.substring(line.indexOf("\t") + 1).split(" ")
            val arr = Array.tabulate(if (inputVectorBias) entries.size + 1 else entries.size)(i => {
              if (inputVectorBias && i == entries.size) {
                1.0F
              } else {
                entries(i).toFloat
              }
            })
            word2Vec.put(word, arr)
          }
        } else {
          // Otherwise, a space is the first delimiter
          val word = line.substring(0, line.indexOf(" "))
          if (words.isEmpty || words.contains(word)) {
            val entries = line.substring(line.indexOf(" ") + 1).split(" ")
            val arr = Array.tabulate(if (inputVectorBias) entries.size + 1 else entries.size)(i => {
              if (inputVectorBias && i == entries.size) {
                1.0F
              } else {
                entries(i).toFloat
              }
            })
            word2Vec.put(word, arr)
          }
        }
      }
      firstLine = false
    }
    println("Loaded " + word2Vec.size + " Bansal representations out of " + words.size + " attempted words")
    word2Vec
  }
}