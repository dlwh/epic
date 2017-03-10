package epic.features

import breeze.linalg.Counter
import epic.framework.Feature
import breeze.util.SerializableLogging
import breeze.io.FileStreams
import java.io.File
import java.io.BufferedReader
import scala.io.Source
import epic.parser.morph.MorphFeat
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import java.io.InputStreamReader

class MorphFeaturizer private (morphLookupTable: MorphFeaturizer.MorphLookupTable) extends WordFeaturizer[String] with Serializable {
  def anchor(w: IndexedSeq[String]): WordFeatureAnchoring[String] = new WordFeatureAnchoring[String] {
    val morphFeats = if (!morphLookupTable.contains(w)) {
      println("Sentence wasn't found in lookup table: " + w)
      w.indices.map(i => Array[MorphFeat]())
    } else {
      morphLookupTable(w)
    }
    val feats = w.indices.map(i => morphFeats(i).filter(feat => feat.label == "lem").map(feat => IndicatorFeature(feat): Feature))
    // logger.info("Feats for sentence: " + w)
    // (0 until w.size).foreach(i => logger.info(w(i) + ": " + feats(i).toSeq))

    def featuresForWord(pos: Int): Array[Feature] = if (pos < 0 || pos >= w.length) Array(BeginSentFeature) else feats(pos)

    def words: IndexedSeq[String] = w
  }
}

object MorphFeaturizer {
  
  // Stores each sentence's associated vector of 
  type MorphLookupTable = HashMap[IndexedSeq[String],IndexedSeq[Array[MorphFeat]]]
  
  def makeLookupTable(pathToTaggedSentences: String): MorphLookupTable = {
    val in = breeze.io.FileStreams.input(new File(pathToTaggedSentences))
    val br = new BufferedReader(new InputStreamReader(in, "UTF-8"))
    val lookupTable = new HashMap[IndexedSeq[String],IndexedSeq[Array[MorphFeat]]]
    val morphFeatArr = new ArrayBuffer[IndexedSeq[Array[MorphFeat]]]
    var thisSent = new ArrayBuffer[String]
    var thisSentFeats = new ArrayBuffer[Array[MorphFeat]]
    while (br.ready()) {
      val line = br.readLine()
      if (line.trim.isEmpty) {
        lookupTable.put(thisSent, thisSentFeats)
        morphFeatArr += thisSentFeats
        thisSent = new ArrayBuffer[String]
        thisSentFeats = new ArrayBuffer[Array[MorphFeat]]
      } else {
        val splitLine = line.split("\\s+")
        if (splitLine.size != 3) {
          println("WARNING: Bad line, split into more than three parts on whitespace: " + splitLine)
        }
        thisSent += splitLine(0)
        thisSentFeats += MorphFeat.readMorphFeatsFromBit(splitLine(2)).toArray
      }
    }
    if (thisSent.nonEmpty) {
      lookupTable.put(thisSent, thisSentFeats)
    }
    println("Loaded " + lookupTable.size + " entries from " + pathToTaggedSentences)
    lookupTable
  }
  
  def apply(pathsToTaggedSentences: Seq[String]) = {
    val lookupTable = pathsToTaggedSentences.map(makeLookupTable(_)).reduce(_ ++ _)
    new MorphFeaturizer(lookupTable)
  }
}
