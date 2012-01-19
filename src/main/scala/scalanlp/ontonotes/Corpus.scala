package scalanlp.ontonotes

import scalanlp.trees.Treebank
import java.io.{FilenameFilter, File}
import java.lang.String

/**
 * 
 * @author dlwh
 */
class Corpus(path: File, split: Corpus.Split) extends Treebank[OntoLabel] {
  val train = Portion("train",split.trainSections)
  val test = Portion("test",split.testSections)
  val dev = Portion("dev",split.devSections)

  def sections = split.allSections

  def documentsFromSection(sec: String) = {
    val files = new File(path,sec).listFiles(new FilenameFilter {
      def accept(p1: File, p2: String) = p2.endsWith(".xml")
    })

    files.map(scala.xml.XML.loadFile(_)).map(Document.fromXML _)
  }

  def treesFromSection(sec: String) = {
    for(d <- documentsFromSection(sec).iterator; t <- d.sentences.iterator) yield {
      (t.tree,t.words)
    }
  }
}

object Corpus {
  // TODO: make a principled decision for dev
  case class Split(train: Int, dev: Int, test: Int, start: Int = 0) {
    def trainSections = (start to train).map("%02d" format _)
    def devSections = ((train + 1) to dev).map("%02d" format _)
    def testSections = ((dev + 1) to test).map("%02d" format _)
    def allSections = (start to test).map("%02d" format _)
  }

  val splits = Map(
    "ABC" -> Split(51,55,69),
    "CNN" -> Split(360,375,437),
    "MNB" -> Split(15,17,25),
    "NBC" -> Split(26,29,39),
    "PRI" -> Split(83,89,112),
    "VOA" -> Split(185,198,264),
    "WSJ" -> Split(21,22,23,start=2)
  )

  def fromXMLDirectory(path: File) = {
    val spl = splits(path.getName().toUpperCase)
    new Corpus(path,spl)
  }
}