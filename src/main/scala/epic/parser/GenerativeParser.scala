package epic.parser
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

import java.io.{BufferedWriter, File, FileWriter}

import breeze.config.Help
import breeze.linalg.Counter2
import epic.features.WordShapeGenerator
import epic.lexicon.{Lexicon, SignatureLexicon, SimpleTagScorer}
import epic.parser.ParserParams.XbarGrammar
import epic.parser.projections.GrammarRefinements
import epic.trees._
import epic.trees.annotations._

/**
 * Contains codes to read off parsers and grammars from
 * a treebank.
 */
object GenerativeParser {
  /**
   * Extracts a [[epic.parser.RuleTopology]] and [[epic.lexicon.Lexicon]]
   * from a treebank
   * @param data the treebank
   * @return
   */
  def extractLexiconAndGrammar(data: Traversable[TreeInstance[AnnotatedLabel, String]]): (Lexicon[AnnotatedLabel, String], RuleTopology[AnnotatedLabel]) = {
    val root = data.head.tree.label
    val (words, binary, unary) = extractCounts(data)
    val grammar = RuleTopology(root,
      binary.keysIterator.map(_._2) ++ unary.keysIterator.map(_._2)
    )

    val sigs = words.activeIterator.collect { case ((label, word), count) if count < 2 => (label, "SHAPE-" + WordShapeGenerator(word))}
    val multimap = (words.activeKeysIterator ++ sigs).toSet[(AnnotatedLabel, String)].groupBy(_._2).mapValues(_.map(pair => grammar.labelIndex(pair._1))).toMap

    val lexicon = new SignatureLexicon(grammar.labelIndex, multimap, (w: String) => "SHAPE-" + WordShapeGenerator(w))
    //val lexicon = new SimpleLexicon(grammar.labelIndex, words)
    (lexicon, grammar)
  }

  /**
   * Makes a basic
   * @param data
   * @tparam L
   * @tparam W
   * @return
   */
  def fromTrees[L, W](data: Traversable[TreeInstance[L, W]])(implicit deb: Debinarizer[L]):Parser[L, W] = {
    val grammar = extractGrammar(data.head.tree.label, data)
    Parser(grammar)
  }

  /**
   * Extracts a [[epic.parser.SimpleGrammar]] from a raw treebank.
   * @param root
   * @param data
   * @tparam L
   * @tparam W
   * @return
   */
  def extractGrammar[L, W](root: L, data: TraversableOnce[TreeInstance[L, W]]): SimpleGrammar[L, L, W] = {
    val (wordCounts, binaryProductions, unaryProductions) = extractCounts(data)
    Grammar.generative(root, binaryProductions, unaryProductions, wordCounts)
  }

  def extractCounts[L, W](data: TraversableOnce[TreeInstance[L, W]]) = {
    val lexicon = Counter2[L, W, Double]()
    val binaryProductions = Counter2[L, BinaryRule[L], Double]()
    val unaryProductions = Counter2[L, UnaryRule[L], Double]()

    for( ti <- data) {
      val TreeInstance(_, tree, words) = ti
      val leaves = tree.leaves map (l => (l, words(l.span.begin)))
      tree.allChildren foreach { 
        case BinaryTree(a, bc, cc, span) =>
          binaryProductions(a, BinaryRule(a, bc.label, cc.label)) += 1.0
        case UnaryTree(a, bc, chain, span) =>
          unaryProductions(a, UnaryRule(a, bc.label, chain)) += 1.0
        case t => 
      }
      for( (l, w) <- leaves) {
        lexicon(l.label, w) += 1
      }
      
    }
    (lexicon, binaryProductions, unaryProductions)
  }

  def annotatedParser(ruleTopology: RuleTopology[AnnotatedLabel], baseLexicon: Lexicon[AnnotatedLabel, String],
                      annotator: TreeAnnotator[AnnotatedLabel, String, AnnotatedLabel],
                      trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]]): Parser[AnnotatedLabel, String] = {

    val refinedGrammar = annotated(ruleTopology, baseLexicon, annotator, trainTrees)
    Parser(refinedGrammar)
  }

  def annotated(annotator: TreeAnnotator[AnnotatedLabel, String, AnnotatedLabel], trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]]): SimpleGrammar[AnnotatedLabel, AnnotatedLabel, String] = {
    val (baseLexicon, ruleTopology) = extractLexiconAndGrammar(trainTrees.map(Xbarize()))
    annotated(ruleTopology, baseLexicon, annotator, trainTrees)
  }

  def annotated(ruleTopology: RuleTopology[AnnotatedLabel], baseLexicon: Lexicon[AnnotatedLabel, String], annotator: TreeAnnotator[AnnotatedLabel, String, AnnotatedLabel], trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]]): SimpleGrammar[AnnotatedLabel, AnnotatedLabel, String] = {
    val transformed = trainTrees.par.map {
      ti => annotator(ti)
    }.seq.toIndexedSeq

    val (words, binary, unary) = GenerativeParser.extractCounts(transformed)

    val refGrammar = RuleTopology(AnnotatedLabel.TOP, binary, unary)
    val indexedRefinements = GrammarRefinements(ruleTopology, refGrammar, {
      (_: AnnotatedLabel).baseAnnotatedLabel
    })

    Grammar.generative(ruleTopology, baseLexicon, indexedRefinements, binary, unary, words)
  }

  def defaultAnnotator(vertical: Int = 1, horizontal: Int = 0): PipelineAnnotator[AnnotatedLabel, String] =  PipelineAnnotator(Seq(FilterAnnotations(), ForgetHeadTag(), Markovize(horizontal = horizontal,vertical = vertical), SplitPunct()))
}

object GenerativeTrainer extends ParserPipeline {
  @Help(text="Training parameters")
  case class Params(@Help(text="Location to read/write the baseParser") baseParser: XbarGrammar,
                    @Help(text=
                      "The kind of annotation to do on the refined grammar. Default uses v2h1 markovization and nothing else.")
                    annotator: TreeAnnotator[AnnotatedLabel, String, AnnotatedLabel] = GenerativeParser.defaultAnnotator(),
                    threads: Int = -1,
                    @Help(text="Use max rule decoding instead of max constituent")
                    maxRule: Boolean = false,
                    @Help(text="dump the grammar to a text file")
                    grammarDumpPath: File = null,
                    pruneUnlikelyLongSpans: Boolean  = true
                     )
  protected val paramManifest = manifest[Params]

  def trainParser(trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]],
                  validate: Parser[AnnotatedLabel, String]=>ParseEval.Statistics,
                  params: Params) = {
    val annotator: TreeAnnotator[AnnotatedLabel, String, AnnotatedLabel] = params.annotator
    val baseParser: XbarGrammar = params.baseParser
    val (xbar,xbarLexicon) = baseParser.xbarGrammar(trainTrees)

    val transformed = trainTrees.par.map { ti => annotator(ti) }.seq.toIndexedSeq

    val (wordCounts, binaryCounts, initUnaries) = GenerativeParser.extractCounts(transformed)

    val refGrammar = RuleTopology(AnnotatedLabel.TOP, binaryCounts, initUnaries)
    val indexedRefinements = GrammarRefinements(xbar, refGrammar, { (_: AnnotatedLabel).baseAnnotatedLabel })

    logger.info("Num coarse rules:" + xbar.index.size)
    logger.info("Num coarse symbols:" + xbar.labelIndex.size)

    logger.info("Num refined rules:" + refGrammar.index.size)
    logger.info("Num refined symbols:" + refGrammar.labelIndex.size)


    val scorer = {
      new SimpleTagScorer(wordCounts)
    }

    val refinedGrammar = Grammar.generative(xbar, xbarLexicon, indexedRefinements, binaryCounts, initUnaries, scorer)

    if (params.grammarDumpPath != null) {
      val out = new BufferedWriter(new FileWriter(params.grammarDumpPath))
      refinedGrammar.prettyPrint(out)
      out.close()
    }


    val decoder = if (params.maxRule) new MaxRuleProductDecoder[AnnotatedLabel, String]() else new ViterbiDecoder[AnnotatedLabel, String]
    val parser = Parser(refinedGrammar, decoder)
    Iterator.single(("Gen", parser))
  }
}
