package epic.parser.repl

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

import epic.trees._

/**
 * This class just provides some objects I commonly want on the REPL.
 * @author dlwh
 */
class ReplGrammar(treebankPath: String, binarizationKind: String = "xbar") {
  lazy val treebank = Treebank.fromPennTreebankDir(new java.io.File(treebankPath))

  val binarize = {
    val headRules = binarizationKind match {
      case "xbar" | "right" => HeadFinder.right[String]
      case "leftXbar" | "left" => HeadFinder.left[String]
      case "head" => HeadFinder.collins
      case _ => HeadFinder.collins
    }
    Trees.binarize(_:Tree[String], headRules)
  }

  val maxLength = 15

  val xform = Trees.Transforms.StandardStringTransform

  lazy val trainTrees = IndexedSeq.empty ++ (for( (tree,words) <- treebank.train.trees.filter(_._2.length <= maxLength))
    yield TreeInstance(words.toString(),binarize(xform(tree)),words)).toIndexedSeq

  lazy val devTrees = IndexedSeq.empty ++ (for( (tree,words) <- treebank.dev.trees.filter(_._2.length <= maxLength))
    yield TreeInstance(words.toString(),binarize(xform(tree)),words)).toIndexedSeq
}