package epic.trees

/*
 Copyright 2012 David Hall

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/


import java.io.{FileReader, File, FileInputStream, InputStreamReader}
import java.io.File

/**
 * A Treebank contains a train set, a test set, and a dev set, which are "Portions". Portions
 * are made up of sections, which have the trees.
 *
 * @author dlwh
 */
trait Treebank[L] { outer =>

  /**
   * Class for a split of a training set.
   */
  case class Portion(name: String, sections: IndexedSeq[String]) {
    def trees = treesFromSections(sections)
  }

  /**
   * Training set
   */
  val train: Portion
  /**
   * Test set
   */
  val test: Portion
  /**
   * Dev set
   */
  val dev: Portion
  /**
   * All sentences
   */
  val all:Portion = Portion("all",sections)

  /**
   * Every section in the treebank
   */
  def sections: IndexedSeq[String]

  /**
   * Read the trees from a section
   */
  def treesFromSection(sec: String): Iterator[(Tree[L],IndexedSeq[String])]

  def treesFromSections(secs: IndexedSeq[String]) = {
    for(sec <- secs.iterator; tree <- treesFromSection(sec))
      yield tree
  }

  /**
   * All trees
   */
  def trees: Iterator[(Tree[L],IndexedSeq[String])] = treesFromSections(sections)
}

object Treebank {

  import scala.io.Source
  /**
  * Reads a treebank from the "mrg/wsj" section
  * of the parsed Treebank.
  */
  def fromPennTreebankDir(dir: File):Treebank[String] = new Treebank[String] {
    def sections = dir.listFiles.filter(_.isDirectory).map(_.getName)
    val train = Portion("train", IndexedSeq.range(2,10).map("0" + _) ++ IndexedSeq.range(10,22).map(""+_))

    val test = Portion("test",IndexedSeq("23"))

    val dev = Portion("dev",IndexedSeq("22"))

    def treesFromSection(sec: String) = {
      for(file <- new File(dir,sec).listFiles.iterator;
        pennReader = new PennTreeReader(new FileReader(file));
        tree <- pennReader)
      yield tree
    }
  }

  def fromGermanTreebank(dir: File):Treebank[String] = {
    import scala.io.Codec.ISO8859
    implicit val cod = ISO8859
    new SimpleTreebank(new File(dir + "/negra_1.mrg"),new File(dir + "/negra_2.mrg"),new File(dir + "/negra_3.mrg"))
  }

  def fromChineseTreebankDir(dir: File):Treebank[String] = new Treebank[String] {
    def sections = dir.listFiles.map(_.getName)
    private def id_to_name(id: Int) = s"chtb_${if(id < 100)  "0" + id else id}.mrg"

    val train = Portion("train",{(1 to 270) ++ (400 to 1151)} map(id_to_name))
    val test = Portion("test", 271 to 300 map id_to_name)
    val dev = Portion("dev",301 to 325 map id_to_name)

    def treesFromSection(sec: String) = {
      val file = new File(dir,sec)
      println(file)
      val pennReader = new PennTreeReader(new InputStreamReader(new FileInputStream(file),"UTF-8"))
      for(tree <- pennReader)
        yield tree
    }
  }
}
