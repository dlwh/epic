package epic.trees
/*
 Copyright 2012 David Hall

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
import breeze.data.Example

/**
 * Represents a treebank with attendant spans, binarization, etc. Used in all the parser trainers.
 *
 * @author dlwh
 */
case class ProcessedTreebank(path: File,
                             maxLength: Int = 40,
                             binarization: String = "head") {

  lazy val treebank = {
    if (path.isDirectory) Treebank.fromPennTreebankDir(path)
    else DenseTreebank.fromZipFile(path);
  }

  lazy val trainTreesWithUnaries = transformTrees(treebank.train, maxLength);
  lazy val trainTrees = trainTreesWithUnaries.map(ti => ti.copy(tree = UnaryChainRemover.removeUnaryChains(ti.tree)))
  lazy val devTrees = transformTrees(treebank.dev, 100000);
  lazy val testTrees = transformTrees(treebank.test, 1000000);


  def transformTrees(portion: treebank.Portion, maxL: Int): IndexedSeq[TreeInstance[AnnotatedLabel, String]] = {
    val binarizedAndTransformed = for (
      ((tree, words), index) <- portion.trees.zipWithIndex if words.length <= maxL
    ) yield {
      val transformed = process(tree)
      val name = portion.name + "-" + index
      TreeInstance(name, transformed, words)
    }

    binarizedAndTransformed.toIndexedSeq
  }

  def headRules = {
    binarization match {
      case "xbar" | "right" => HeadFinder.right[String]
      case "leftXbar" | "left" => HeadFinder.left[String]
      case "head" => HeadFinder.collins
      case _ => HeadFinder.collins
    }
  }

  val process = new StandardTreeProcessor(headRules)
}



