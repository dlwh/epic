package scalanlp.trees;
/*
 Copyright 2010 David Hall

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/


import java.io.File

trait Treebank { outer =>

  case class Portion(name: String, sections: Seq[String]) {
    def trees = treesFromSections(sections);
  }

  val train: Portion
  val test: Portion
  val dev: Portion
  val all:Portion = Portion("all",sections)

  def sections: Seq[String];

  def treesFromSection(sec: String): Iterator[(Tree[String],Seq[String])];
  def treesFromSections(secs: Seq[String]) = {
    for(sec <- secs.iterator;
        tree <- treesFromSection(sec))
      yield tree;
  }

  def trees: Iterator[(Tree[String],Seq[String])] = treesFromSections(sections);
          
}

object Treebank {

  import scala.io.Source;
  /**
  * Reads a treebank from the "mrg/wsj" section
  * of the parsed Treebank.
  */
  def fromPennTreebankDir(dir: File) = new Treebank {
    def sections = dir.listFiles.filter(_.isDirectory).map(_.getName);
    val train = Portion("train", Seq.range(2,10).map("0" + _) ++ Seq.range(10,22).map(""+_));

    val test = Portion("test",Seq("23"));

    val dev = Portion("dev",Seq("22"));

    def treesFromSection(sec: String) = {
      val pennReader = new PennTreeReader();
      for(file <- new File(dir,sec).listFiles.iterator;
        tree <- pennReader.readTrees(Source.fromFile(file).mkString("")).fold( x=>x, x => error("error in " + file + " " + x.toString)).iterator)
      yield tree;
    }
  }

  def fromGermanTreebank(dir: File):Treebank = {
    import scala.io.Codec.ISO8859
    implicit val cod = ISO8859;
    new SimpleTreebank(new File(dir + "/negra_1.mrg"),new File(dir + "/negra_2.mrg"),new File(dir + "/negra_3.mrg"))
  }
}
