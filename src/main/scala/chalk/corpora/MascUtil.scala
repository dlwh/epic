package chalk.corpora

import scala.xml._
import java.io._
import io.Codec

case class MNode(id: String, targets: Seq[String])
case class MAnnotation(id: String, label: String, ref: String, features: Map[String,String])
case class MEdge(id: String, from: String, to: String)
case class MRegion(id: String, start: Int, end: Int) extends Ordered[MRegion] {
  def compare(that: MRegion) = this.start - that.start
}


/**
* Convert native MASC xml into CONLL format for named entity recognition.
*
* @author jasonbaldridge
*/
object MascTransform {

  import io.Source
  import MascUtil._

  def main(args: Array[String]) {
    val mascDir = args(0)
    val outputDir = new File(if (args.length > 1) args(1) else "/tmp")
    outputDir.mkdirs

    val targets = collectTargets(new File(mascDir))

    // Get 3/5 for train, 1/5 for dev, and 1/5 for test
    val targetsAndIndices = targets.zipWithIndex
    val trainSet = targetsAndIndices.filter(_._2 % 5 < 3).unzip._1
    val devSet = targetsAndIndices.filter(_._2 % 5 == 3).unzip._1
    val testSet = targetsAndIndices.filter(_._2 % 5 == 4).unzip._1
    processSet(outputDir, "train", trainSet)
    processSet(outputDir, "dev", devSet)
    processSet(outputDir, "test", testSet)
  }

  def collectTargets(dir: File): Seq[(File,String)] = {
    val files = dir.listFiles.toSeq
    val filesInDir = files
      .filter(_.getName.endsWith(".txt"))
      .map(file => (dir, file.getName.dropRight(4)))
    filesInDir ++ files.filter(_.isDirectory).flatMap(collectTargets)
  }

  def processSet(parentDir: File, outputName: String, targets: Seq[(File, String)]) {
    System.err.println("Creating " + outputName)
    val outputDir = new File(parentDir, outputName)
    outputDir.mkdirs

    val outputSentences = new FileWriter(new File(outputDir,outputName+"-sent.txt"))
    val outputTokens = new FileWriter(new File(outputDir,outputName+"-tok.txt"))
    val outputNer = new FileWriter(new File(outputDir,outputName+"-ner.txt"))

    for (mfile <- MascFile(targets)) {
      for (sentence <- mfile.sentences) {
        val tokenizedSentence = new StringBuffer
        val MascSentence(tokens,postags,nerLabels,regions) = sentence
        (0 until sentence.numTokens).foreach { i => {
          val (tok, pos, ner, region) = (tokens(i), postags(i), nerLabels(i), regions(i))
            if (tok.exists(_.isSpaceChar)) {
              println("Weird token! '" + tok +"' " + mfile.dir + "/" + mfile.prefix +".txt:" + + region.start + "-" + region.end)
            }
          val split =
            if (i<sentence.numTokens-1 && region.end == regions(i+1).start) "<SPLIT>" else " "
          tokenizedSentence.append(tok).append(split)
          outputNer.write(tok + " " + pos + " " + pos + " " + ner + "\n")
        }}
        outputNer.write("\n")
        val sentStart = sentence.orderedRegions.head.start
        val sentEnd = sentence.orderedRegions.last.end
        val sentenceText = mfile.rawtext.slice(sentStart,sentEnd).replaceAll("\n"," ")
        outputSentences.write(sentenceText+"\n")
        outputTokens.write(tokenizedSentence.toString.trim + "\n")
      }
    }
    outputNer.flush
    outputNer.close
    outputSentences.flush
    outputSentences.close
    outputTokens.flush
    outputTokens.close
    System.err.println
  }

}


case class MascSentence (
  orderedTokens: Seq[String],
  orderedPos: Seq[String],
  bioLabels: Seq[String],
  orderedRegions: Seq[MRegion]
) {

  lazy val numTokens = orderedTokens.length
}

class MascFile (
  val dir: File,
  val prefix: String,
  val rawtext: String,
  val sentences: Seq[MascSentence]
) {

  lazy val numSentences = sentences.length

}

object MascFile {

  import MascUtil._
  import io.Source

  lazy val outsideNe = MAnnotation("outside", "outside", "none", Map[String,String]())

  def apply(targets: Seq[(File, String)]): Iterator[MascFile] = {
    targets.toIterator.flatMap { case(file, prefix) => {
      try {
        val mfile = MascFile(file,prefix)
        System.err.println("Success: " + file + "," + prefix)
        Some(mfile)
      }
      catch { case e: Throwable =>
        System.err.println("Failure: " + file + "," + prefix)
        None
      }
    }}
  }

  def apply(dir: File, prefix: String): MascFile = {

    def dirFile(prefix: String) = new File(dir, prefix)
    def loadXML(file: File) = XML.load(new InputStreamReader( new FileInputStream(file), "UTF-8"))

    implicit val codec = Codec.UTF8

    // Raw text
    val rawtext = Source.fromFile(dirFile(prefix+".txt"))(codec).mkString

    // Sentence information
    val sentenceXml = loadXML(dirFile(prefix+"-s.xml"))
    val sentenceRegions = getRegions(sentenceXml).sorted

    
    // Basic segment information
    val segmentXml = loadXML(dirFile(prefix+"-seg.xml"))
    val segmentRegions = getRegions(segmentXml).map(r => (r.id -> r)).toMap

    // POS information
    val pennXml = loadXML(dirFile(prefix+"-penn.xml"))

    val tokenRegions = getNodes(pennXml).map { n =>
      val regions = n.targets.map(segmentRegions).sorted
      (n.id -> MRegion(n.id, regions.head.start, regions.last.end))
    }.toMap

    val tokens = tokenRegions.mapValues(region => rawtext.slice(region.start, region.end))
    val posAnnotations = getAnnotations(pennXml).map(anno => (anno.ref -> anno)).toMap

    // NER information
    val neXml = loadXML(dirFile(prefix+"-ne.xml"))
    val neAnnotations =
      getAnnotations(neXml).map(anno => (anno.ref -> anno)).toMap.withDefault(x=>outsideNe)

    val neEdges =
      getEdges(neXml).map(edge => (edge.to -> edge.from)).toMap.withDefault(x=>"outside")

    // A helper function for pulling out the information associated with a
    // subsequence of the tokens in the document.
    def orderedTokPosNer(orderedRegions: Seq[MRegion]) = {
      if (orderedRegions.length == 0) None
      else {
        val orderedTokens = orderedRegions.map(reg=>tokens(reg.id))
        
        val (orderedPos, orderedNe) = orderedRegions.map { region => {
          val posAnno = posAnnotations(region.id)
          val neAnno = neAnnotations(neEdges(posAnno.ref))
          (getPos(posAnno), neAnno)
        }}.unzip
        
        val bioLabels = (outsideNe +: orderedNe).sliding(2).toSeq.map {
          case Seq(prev, curr) =>
            if (curr.label == "outside")
              nerLabelStandardizer(curr.label)
            else {
              val prefix = if (prev.id != curr.id) "B-" else "I-"
              prefix+nerLabelStandardizer(curr.label)
            }
        }
        Some(MascSentence(orderedTokens, orderedPos, bioLabels, orderedRegions))
      }
    }


    // Insert the "missing" sentences. (Content not marked as a sentence,
    // but containing tokens.)
    val paddedSentenceRegionBuffer =
      collection.mutable.ListBuffer[MRegion](sentenceRegions.head)

    sentenceRegions.sliding(2).foreach {
      case Seq(prev, curr) => {
        if (prev.end + 1 < curr.start)
          paddedSentenceRegionBuffer.append(MRegion("", prev.end + 1, curr.start - 1))
        paddedSentenceRegionBuffer.append(curr)
      }
    }
    
    val paddedSentenceRegions = paddedSentenceRegionBuffer.toSeq

    // Pull out the sequence of token, pos, and NE for each sentence.
    val allOrderedTokRegions = tokenRegions.values.toIndexedSeq.sorted
    var index = 0
    val allDataBySentence = paddedSentenceRegions.flatMap { region => {
      val endIndex = allOrderedTokRegions.indexWhere(t=>t.end>region.end,index)
      if (index == endIndex) None
      else {
        val sentence = allOrderedTokRegions.slice(index,endIndex)
        index = endIndex
        orderedTokPosNer(sentence)
      }
    }}

    new MascFile(dir, prefix, rawtext, allDataBySentence)
  }

}

/**
* Simple objects and functions for working with MASC data.
*
* @author jasonbaldridge
*/
object MascUtil {

  def xmlId(node: Node) = (node \ "@{http://www.w3.org/XML/1998/namespace}id").toString

  lazy val nerLabelStandardizer = Map(
    "location" -> "LOC",
    "person" -> "PER",
    "org" -> "ORG",
    //"date" -> "DAT"
    "date" -> "MISC"
  ).withDefault(x=>"O")


  def getRegions(doc: Elem) = (doc \\ "region").toSeq.map { rxml =>
    val Array(start, end) = (rxml \ "@anchors").toString.split(" ")
    MRegion(xmlId(rxml), start.toInt, end.toInt)
  }
    
  def getNodes(doc: Elem) = (doc \\ "node").toSeq.flatMap { nxml =>
    val link = (nxml \ "link")
    if (!link.isEmpty) {
      val targets = (link.head \ "@targets").toString.split(" ").toSeq
      Some(MNode(xmlId(nxml), targets))
    } else throw new Exception("Missing link element.") //None OK?
  }
  
  def getEdges(doc: Elem) = (doc \\ "edge").toSeq
    .map(exml => MEdge(xmlId(exml), (exml \ "@from").toString, (exml \ "@to").toString))
  
  def getAnnotations(doc: Elem) = (doc \\ "a").toSeq.map { axml =>
    val features = (axml \\ "f").toSeq
      .map(fnode => ((fnode \ "@name").toString -> fnode.child.toString)).toMap
    MAnnotation(xmlId(axml), (axml \ "@label").toString, (axml \ "@ref").toString, features)
  }

  // Have to go through some pains to make sure we get a POS for every token.
  def getPos(anno: MAnnotation) = {
    if (anno.features.isDefinedAt("msd")) anno.features("msd")
    else if (anno.features.get("kind").getOrElse("") == "urlAddress") "URL"
    else if (anno.features.isDefinedAt("categor")) anno.features("categor")
    else "UNK"
  }

}
