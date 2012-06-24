package epic.parser
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
import collection.mutable.ArrayBuffer
import epic.trees._

/**
 *
 * @author dlwh
 */
trait ParserTestHarness {
  def getTrainTrees(maxLength:Int= 15) = {
    massageTrees(TstTreebank.treebank.train.trees,  maxLength).map(ti => ti.copy(tree=UnaryChainRemover.removeUnaryChains(ti.tree)))
  }

  def getTestTrees(maxLength:Int= 15) = {
    massageTrees(TstTreebank.treebank.test.trees, maxLength)
  }

  def massageTrees(trees: Iterator[(Tree[String], Seq[String])], maxLength:Int=15) = {
    val trainTrees = ArrayBuffer() ++= (for( (tree, words) <- trees.filter(_._2.length <= maxLength))
    yield TreeInstance("", transform(tree), words))

    trainTrees
  }


  def evalParser(testTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]], parser: Parser[AnnotatedLabel, String]) = {
    ParseEval.evaluate(testTrees, parser, AnnotatedLabelChainReplacer, asString = {(_:AnnotatedLabel).baseLabel})
  }

  val transform = new StandardTreeProcessor(HeadFinder.left)
}

object ParserTestHarness extends ParserTestHarness {
  val (simpleLexicon, simpleGrammar) = {
    try {
    val trees = getTrainTrees()
    GenerativeParser.extractLexiconAndGrammar(trees.map(_.mapLabels(_.baseAnnotatedLabel)))
    } catch {
      case e => e.printStackTrace(); throw e
    }
  }
  val simpleParser: SimpleChartParser[AnnotatedLabel, String] = {
    val trees = getTrainTrees()
    val grammar = GenerativeParser.extractGrammar[AnnotatedLabel, String](trees.head.label.label, trees.map(_.mapLabels(_.baseAnnotatedLabel)))
    SimpleChartParser(AugmentedGrammar.fromRefined(grammar))
  }
}