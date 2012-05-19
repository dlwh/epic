package scalanlp.parser
/*
 Copyright 2010 David Hall

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

import scalanlp.trees._

import annotations.{FilterAnnotations, TreeAnnotator}
import scalala.tensor.Counter2
import scalanlp.parser.ParserParams.{BaseParser, NoParams}

object GenerativeParser {
  def extractLexiconAndGrammar[L, W](data: Traversable[TreeInstance[AnnotatedLabel, String]]) = {
    val root = data.head.tree.label
    val (words, binary, unary) = extractCounts(data)
    val grammar = Grammar(root,
      binary.keysIterator.map(_._2) ++ unary.keysIterator.map(_._2)
    )

    val lexicon = new SimpleLexicon(words)
    (lexicon, grammar)
  }

  def fromTrees[L, W](data: Traversable[TreeInstance[L, W]]):SimpleChartParser[L, W] = {
    val grammar = extractGrammar(data.head.tree.label, data)
    SimpleChartParser(grammar)
  }

  def extractGrammar[L, W](root: L, data: TraversableOnce[TreeInstance[L, W]]): DerivationScorer.Factory[L, W] = {
    val (wordCounts, binaryProductions, unaryProductions) = extractCounts(data)
    DerivationScorerFactory.generative(root, binaryProductions, unaryProductions, wordCounts)
  }

  def extractCounts[L, W](data: TraversableOnce[TreeInstance[L, W]]) = {
    val lexicon = Counter2[L, W, Double]()
    val binaryProductions = Counter2[L, BinaryRule[L], Double]()
    val unaryProductions = Counter2[L, UnaryRule[L], Double]()

    for( ti <- data) {
      val TreeInstance(_, tree, words) = ti
      val leaves = tree.leaves map (l => (l, words(l.span.start)))
      tree.allChildren foreach { 
        case t @ BinaryTree(a, bc, cc) =>
          binaryProductions(a, BinaryRule(a, bc.label, cc.label)) += 1.0
        case t@UnaryTree(a, bc) =>
          unaryProductions(a, UnaryRule(a, bc.label)) += 1.0
        case t => 
      }
      for( (l, w) <- leaves) {
        lexicon(l.label, w) += 1
      }
      
    }
    (lexicon, binaryProductions, unaryProductions)
  }
}

object GenerativePipeline extends ParserPipeline {
  case class Params(baseParser: BaseParser,
                    annotator: TreeAnnotator[AnnotatedLabel, String, AnnotatedLabel] = new FilterAnnotations(Set.empty[Annotation]))
  protected val paramManifest = manifest[Params]

  def trainParser(trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]],
                  validate: Parser[AnnotatedLabel, String]=>ParseEval.Statistics,
                  params: Params) = {
    val (xbar,xbarLexicon) = params.baseParser.xbarGrammar(trainTrees)
    val trees = trainTrees.map(params.annotator(_))
    val (words, binary, unary) = GenerativeParser.extractCounts(trees)
    val grammar = DerivationScorerFactory.generative[AnnotatedLabel, String](xbar, xbarLexicon, binary, unary, words)
    val parser = SimpleChartParser(grammar)
    Iterator.single(("Gen", parser))
  }
}
