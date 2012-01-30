package scalanlp.ontonotes

import scalanlp.trees.Treebank
import java.io.{FilenameFilter, File}
import java.lang.String

/**
 * 
 * @author dlwh
 */
class Corpus(path: File, split: Corpus.Split) extends Treebank[OntoLabel] { outer =>
  val train = Portion("train",split.trainSections)
  val test = Portion("test",split.testSections)
  val dev = Portion("dev",split.devSections)

  def sections = split.allSections

  def documentFromSection(sec: String):Option[Document] = {
    val p = new File(new File(path,sec.take(2)),path.getName + "_" + sec + ".xml")
    if(p.exists) Some(Document.fromXML(scala.xml.XML.loadFile(p)))
    else None
  }

  def treesFromSection(sec: String) = {
    for(d <- documentFromSection(sec).iterator; t <- d.sentences.iterator; t2 = t.stripTraces()) yield {
      (t2.tree,t2.words)
    }
  }


}

object Corpus {
  // TODO: make a principled decision for dev
  case class Split(train: Int, dev: Int, test: Int, start: Int = 1) {
    def trainSections = (start to train).map("%04d" format _)
    def devSections = ((train + 1) to dev).map("%04d" format _)
    def testSections = ((dev + 1) to test).map("%04d" format _)
    def allSections = (start to test).map("%04d" format _)
  }

  val splits = Map(
    "ABC" -> Split(51,55,69),
    "CNN" -> Split(360,375,437),
    "MNB" -> Split(15,17,25),
    "NBC" -> Split(26,29,39),
    "PRI" -> Split(83,89,112),
    "VOA" -> Split(185,198,264),
    "WSJ" -> Split(2199,2299,2399,start=201)
  )

  def fromXMLDirectory(path: File) = {
    val spl = splits(path.getName().toUpperCase)
    new Corpus(path,spl)
  }
}