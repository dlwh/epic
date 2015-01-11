package epic.corefdense

import java.io.FileInputStream
import java.io.BufferedInputStream
import scala.collection.mutable.HashMap
import java.io.DataInputStream
import java.util.regex.Pattern
import edu.berkeley.nlp.futile.fig.basic.IOUtils
import java.io.File
import scala.util.Random
import breeze.linalg.Counter

object Word2Vec {
  
  /**
   * Loads vectors for a vocabulary unless a certain file exists, in which case things are just loaded from there.
   * Does not check that the vocabularies are the same.
   */
  def loadVectorsForVocabularyUseCache(word2vecPath: String, voc: Set[String], inputVectorBias: Boolean, word2vecCache: String) = {
    if (!new File(word2vecCache).exists) {
      val map = loadVectorsForVocabulary(word2vecPath, voc, inputVectorBias);
      println("Writing word2vec cache to " + word2vecCache)
      IOUtils.writeObjFile(word2vecCache, map);
      map;
    } else {
      println("Reading word2vec cache from " + word2vecCache)
      IOUtils.readObjFile(word2vecCache).asInstanceOf[HashMap[String,Array[Float]]]
    }
  }
  
  /**
   * Loads vectors from one or more sources in word2vecPaths; these might be in
   * word2vec format (should end in .bin) or C+W/Bansal format (should end
   * in .txt).
   * 
   * For each word, vectors are appended from each source. If at least one source
   * is present, others are zeroes. Otherwise, it gets a random vector.
   */
  def smartLoadVectorsForVocabulary(word2vecPaths: Seq[String], voc: Set[String], inputVectorBias: Boolean) = {
    val vectorsEachSource = for (word2vecPath <- word2vecPaths) yield {
      if (word2vecPath.endsWith("bin")) {
        readWord2Vec(word2vecPath, voc, false)
      } else if (word2vecPath.endsWith(".txt")) {
        readBansalEmbeddings(word2vecPath, voc, false)
      } else {
        throw new RuntimeException("Unrecognized vectors: " + word2vecPath)
      }
    }
    val dimsEachSource = vectorsEachSource.map(_.values.head.size)
    val finalVectorDim = dimsEachSource.reduce(_ + _) + (if (inputVectorBias) 1 else 0)
    val finalVectors = new HashMap[String,Array[Float]]
    val rng = new Random(0)
    var numRand = 0
    for (word <- voc) {
      val containedInSome = vectorsEachSource.map(_.keySet.contains(word)).reduce(_ || _)
      val vector = if (containedInSome) {
//        Logger.logss("Vectors for " + word)
//        (0 until vectorsEachSource.size).foreach(i => Logger.logss(vectorsEachSource(i).getOrElse(word, { Array[Float]() }).toSeq))
        var finalVector = (0 until vectorsEachSource.size).map(i => vectorsEachSource(i).getOrElse(word, { Array.tabulate(dimsEachSource(i))(j => 0.0F) })).reduce(_ ++ _)
        if (inputVectorBias) {
          finalVector = finalVector ++ Array(1.0F)
        }
//        Logger.logss(finalVector.toSeq)
        finalVector
      } else {
//        Logger.logss("No vectors at all for " + word)
        numRand += 1
        Array.tabulate(finalVectorDim)(i => if (i == finalVectorDim - 1 && inputVectorBias) 1.0F else ((rng.nextDouble - 0.5) * 0.5).toFloat)
      }
      require(vector.size == finalVectorDim, "Mismatched sizes, expected dimension " + finalVectorDim + " but got " + vector.size)
      finalVectors.put(word, vector)
    }
    println("Read embeddings for " + voc.size + " words from " + word2vecPaths.size + " sources, " +
            "total embedding size = " + finalVectorDim + ", " + numRand + " present in no source")
    finalVectors
  }
  
  /**
   * Loads vectors for a vocabulary from word2vec, with OOV words having random vectors
   * generated for them.
   */
  def loadVectorsForVocabulary(word2vecPath: String, voc: Set[String], inputVectorBias: Boolean) = {
    val word2vecMap = readWord2Vec(word2vecPath, voc, inputVectorBias);
    if (word2vecMap.isEmpty) {
      throw new RuntimeException("No word2vec vectors loaded")
    }
    augmentVectorsToCompleteVocabulary(word2vecMap, voc, inputVectorBias)
  }
  
  def loadBansalVectorsForVocabulary(word2vecPath: String, voc: Set[String], inputVectorBias: Boolean) = {
    val word2vecMap = readBansalEmbeddings(word2vecPath, voc, inputVectorBias);
    if (word2vecMap.isEmpty) {
      throw new RuntimeException("No Bansal vectors loaded")
    }
    augmentVectorsToCompleteVocabulary(word2vecMap, voc, inputVectorBias)
  }
  
  private def augmentVectorsToCompleteVocabulary(word2vecMap: HashMap[String,Array[Float]], voc: Set[String], inputVectorBias: Boolean) = {
    val word2vecDim = word2vecMap.values.head.size
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
    val bis = new BufferedInputStream(new FileInputStream(word2VecPath));
    val dis = new DataInputStream(bis);
    val word2Vec = new HashMap[String,Array[Float]];
    // First two entries are vocabulary size and dimension of vectors
    val vocSize = Word2VecUtils.readString(dis).toInt;
    val dim = Word2VecUtils.readString(dis).toInt;
    // Now read vectors, augmented with 1s for bias
    for (i <- 0 until vocSize) {
      if (i % 1000000 == 0) {
        println("On line " + i)
      }
      val word = Word2VecUtils.readString(dis);
      val vector = new Array[Float](if (inputVectorBias) dim + 1 else dim);
      val len = 0;
      var j = 0;
      while (j < dim) {
        vector(j) = Word2VecUtils.readFloat(dis);
        j += 1;
      }
      if (inputVectorBias) {
        vector(j) = 1.0F
      }
      if (words.isEmpty || words.contains(word)) {
        word2Vec.put(word, vector);
      }
    }
    println("Loaded " + word2Vec.size + " word2vec representations out of " + words.size + " attempted words");
    word2Vec;
  }
  
//  def printTopMissedWords(word2VecPath: String, words: Counter[String,Double]) {
//    val w2v = readWord2Vec(word2VecPath, words.keySet.toSet[String], false)
//    for (word <- words.keysIterator) {
//      println(word + ": " + words(word))
//    }
//  }
  
  val hyphenPattern = Pattern.compile("(\\w+-)+(\\w+)");
  
  def convertWord(str: String) = {
    var strRep = str;
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
    strRep
    
//    string = re.sub(r"-LRB-", "(", string)
//    string = re.sub(r"-RRB-", ")", string)
//    string = re.sub(r"-LSB-", "[", string)
//    string = re.sub(r"-RSB-", "]", string)
//    string = re.sub(r"-LCB-", "{", string)
//    string = re.sub(r"-RCB-", "}", string)
//    # Replace hyphenated words with the final part
//    string = re.sub(r"(\w+-)+(\w+)", "\g<2>", string)
//    # Replace all numbers with 15 to fight sparsity. For some reason
//    # this doesn't replace periods (even not sentence-final ones) though
//    # it looks like it might...
//    string = re.sub(r"\s-?[0-9,.]{2,15}\s", " fifteen ", string)
//    string = re.sub(r"\s{2,}", " ", string)
//    return string.strip()   
    
    
  }
  
  def readBansalEmbeddings(embeddingsPath: String, words: Set[String], inputVectorBias: Boolean) = {
    val inFile = IOUtils.lineIterator(embeddingsPath)
    val word2Vec = new HashMap[String,Array[Float]];
    var firstLine = true
    while (inFile.hasNext()) {
      val line = inFile.next;
      if (firstLine) {
        if (line.split("\\s+").size == 2) {
          println("Skipping first line: " + line)
          // Just an indicator of how many words there are and the vector dim, so
          // skip over it by leaving firstLine set to true
        } else {
          println("Not skipping first line: " + line)
          firstLine = false;
        }
      }
      if (!firstLine) {
        // If the line contains a tab, then that's the delimiter between the word and
        // the vectors
        if (line.contains("\t")) {
          val word = line.substring(0, line.indexOf("\t"));
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
          val word = line.substring(0, line.indexOf(" "));
          if (words.isEmpty || words.contains(word)) {
            val entries = line.substring(line.indexOf(" ") + 1).split(" ");
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
      firstLine = false;
    }
    println("Loaded " + word2Vec.size + " Bansal representations out of " + words.size + " attempted words");
    word2Vec;
  }
  
  def main(args: Array[String]) {
//    smartLoadVectorsForVocabulary(Seq("data/syntacticEmbeddings/skipdep_embeddings.txt",
//                                      "../cnnkim/data/GoogleNews-vectors-negative300.bin"), Set("the", ",", "crazyoovword"), true)
//    readBansalEmbeddings("data/kushw2v/swedishwordvector.txt", Set("Den", ","), false)
//    smartLoadVectorsForVocabulary(Seq("data/syntacticEmbeddings/skipdep_embeddings.txt"), Set("the", ","), true)
    
//    readBansalEmbeddings("data/syntacticEmbeddings/skipdep_embeddings.txt", Set("the", ","), false)
    
//    val counts = Counter[String,Double]
//    counts("thing") = 5
//    counts("stuff") = 3
//    counts("a") = 1
//    counts("b") = 10
//    for (word <- counts.keySet.toSeq.sortBy(word => -counts(word))) {
//      println(word + ": " + counts(word))
//    }
//    System.exit(0)
//    
//    val w2v = readWord2Vec("data/w2v/sv2.bin", Set(), true)
//    for (word <- w2v.keySet) {
//      println(word + ": " + w2v(word).toSeq)
//    }
    
    
//    println(convertWord("a15"))
//    println(convertWord("1526"))
//    val str = "triple-hyphenated-word"
//    val m = hyphenPattern.matcher(str);
//    if (m.find()) {
//      println(m.group(2))
//    }
  }
}