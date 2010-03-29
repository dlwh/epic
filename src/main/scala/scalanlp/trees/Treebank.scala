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


import java.io.File;

trait Treebank {
  def sections: Seq[String];

  def trainSections: Seq[String];
  def trainTrees = treesFromSections(trainSections);

  def devSections: Seq[String];
  def devTrees = treesFromSections(devSections);

  def testSections: Seq[String];
  def testTrees = treesFromSections(testSections);

  def treesFromSection(sec: String): Iterator[(Tree[String],Seq[String])];
  def treesFromSections(secs: Seq[String]) = {
    for(sec <- secs.elements;
        tree <- treesFromSection(sec))
      yield tree;
  }

  def trees: Iterator[(Tree[String],Seq[String])] = {
    for( sec <- sections.elements;
      tree <- treesFromSection(sec))
    yield tree;
  }
          
}


object Treebank {
  import scala.io.Source;
  /**
  * Reads a treebank from the "mrg/wsj" section
  * of the parsed Treebank.
  */
  def fromPennTreebankDir(dir: File) = new Treebank {
    def sections = dir.listFiles.filter(_.isDirectory).map(_.getName);
    val trainSections = List.range(2,10).map("0" + _) ++ List.range(10,22).map(""+_)
    val devSections = List("24");
    val testSections = List("22");
    def treesFromSection(sec: String) = {
      val pennReader = new PennTreeReader();
      for(file <- new File(dir,sec).listFiles.elements;
        tree <- pennReader.readTrees(Source.fromFile(file).mkString("")).fold( x=>x, x => error("error in " + file + " " + x.toString)) elements)
      yield tree;
    }
  }
}
