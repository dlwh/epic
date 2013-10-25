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

import epic.parser.projections.{ConstraintCoreGrammarAdaptor, GrammarRefinements}
import epic.trees._

import epic.trees.annotations._
import breeze.linalg.Counter2
import breeze.config.Help
import epic.parser.ParserParams.XbarGrammar
import epic.lexicon.{SimpleTagScorer, MaxEntTagScorer, Lexicon, SimpleLexicon}
import epic.features._
import java.io.{FileWriter, BufferedWriter, File}
import epic.trees._
import epic.constraints.LongSpanConstraints

/**
 * Contains codes to read off parsers and grammars from
 * a treebank.
 */
object GenerativeParser {
  /**
   * Extracts a [[epic.parser.BaseGrammar]] and [[epic.lexicon.Lexicon]]
   * from a treebank
   * @param data the treebank
   * @return
   */
  def extractLexiconAndGrammar(data: Traversable[TreeInstance[AnnotatedLabel, String]]): (Lexicon[AnnotatedLabel, String], BaseGrammar[AnnotatedLabel]) = {
    val root = data.head.tree.label
    val (words, binary, unary) = extractCounts(data)
    val grammar = BaseGrammar(root,
      binary.keysIterator.map(_._2) ++ unary.keysIterator.map(_._2)
    )

    val lexicon = new SimpleLexicon(grammar.labelIndex, words)
    (lexicon, grammar)
  }

  /**
   * Makes a basic
   * @param data
   * @tparam L
   * @tparam W
   * @return
   */
  def fromTrees[L, W](data: Traversable[TreeInstance[L, W]]):SimpleChartParser[L, W] = {
    val grammar = extractGrammar(data.head.tree.label, data)
    SimpleChartParser(AugmentedGrammar.fromRefined(grammar))
  }

  /**
   * Extracts a RefinedGrammar from a raw treebank. The refined grammar could be a core grammar,
   * maybe I should do that.
   *
   * @param root
   * @param data
   * @tparam L
   * @tparam W
   * @return
   */
  def extractGrammar[L, W](root: L, data: TraversableOnce[TreeInstance[L, W]]): SimpleRefinedGrammar[L, L, W] = {
    val (wordCounts, binaryProductions, unaryProductions) = extractCounts(data)
    RefinedGrammar.generative(root, binaryProductions, unaryProductions, wordCounts)
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

  def annotatedParser(baseGrammar: BaseGrammar[AnnotatedLabel], baseLexicon: Lexicon[AnnotatedLabel, String],
                      annotator: TreeAnnotator[AnnotatedLabel, String, AnnotatedLabel],
                      trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]]): SimpleChartParser[AnnotatedLabel, String] = {

    val refinedGrammar = annotated(baseGrammar, baseLexicon, annotator, trainTrees)
    val parser = SimpleChartParser(AugmentedGrammar.fromRefined(refinedGrammar))

    parser
  }

  def annotated(baseGrammar: BaseGrammar[AnnotatedLabel], baseLexicon: Lexicon[AnnotatedLabel, String], annotator: TreeAnnotator[AnnotatedLabel, String, AnnotatedLabel], trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]]): SimpleRefinedGrammar[AnnotatedLabel, AnnotatedLabel, String] = {
    val transformed = trainTrees.par.map {
      ti => annotator(ti)
    }.seq.toIndexedSeq

    val (words, binary, unary) = GenerativeParser.extractCounts(transformed)

    val refGrammar = BaseGrammar(AnnotatedLabel.TOP, binary, unary)
    val indexedRefinements = GrammarRefinements(baseGrammar, refGrammar, {
      (_: AnnotatedLabel).baseAnnotatedLabel
    })

    val refinedGrammar = RefinedGrammar.generative(baseGrammar, baseLexicon, indexedRefinements, binary, unary, words)
    refinedGrammar
  }

  def defaultAnnotator(): PipelineAnnotator[AnnotatedLabel, String] =  PipelineAnnotator(Seq(FilterAnnotations(), ForgetHeadTag(), Markovize(0,2)))
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
                    @Help(text="Use the awesome cheating lexicon (slower to train)")
                    awesomeLexicon: Boolean = false,
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

    val refGrammar = BaseGrammar(AnnotatedLabel.TOP, binaryCounts, initUnaries)
    val indexedRefinements = GrammarRefinements(xbar, refGrammar, { (_: AnnotatedLabel).baseAnnotatedLabel })

    logger.info("Num coarse rules:" + xbar.index.size)
    logger.info("Num coarse symbols:" + xbar.labelIndex.size)

    logger.info("Num refined rules:" + refGrammar.index.size)
    logger.info("Num refined symbols:" + refGrammar.labelIndex.size)


    val scorer = if(params.awesomeLexicon) {
      val refLexicon = new SimpleLexicon(refGrammar.labelIndex, wordCounts)
      MaxEntTagScorer.make(WordFeaturizer.goodPOSTagFeaturizer(wordCounts), refLexicon, transformed)
    } else {
      new SimpleTagScorer(wordCounts)
    }

    val refinedGrammar = RefinedGrammar.generative(xbar, xbarLexicon, indexedRefinements, binaryCounts, initUnaries, scorer)

    if(params.grammarDumpPath != null) {
      val out = new BufferedWriter(new FileWriter(params.grammarDumpPath))
      refinedGrammar.prettyPrint(out)
      out.close()
    }

    val finalGrammar = if(params.pruneUnlikelyLongSpans) {
      val ccs = trainTrees.flatMap(_.asTaggedSequence.pairs.collect{ case (tag, word) if tag.label == "CC" || tag.label == "C" || tag.label == "KON" => word}).toSet
      val constraints = new ConstraintCoreGrammarAdaptor(refinedGrammar.grammar, refinedGrammar.lexicon, new LongSpanConstraints.Factory[AnnotatedLabel](30, ccs))
      AugmentedGrammar(refinedGrammar, constraints)
    } else {
      AugmentedGrammar.fromRefined(refinedGrammar)
    }

    val decoder = if (params.maxRule) new MaxRuleProductDecoder[AnnotatedLabel, String]() else new ViterbiDecoder[AnnotatedLabel, String]
    val parser = new SimpleChartParser(finalGrammar, decoder)
    Iterator.single(("Gen", parser))
  }
}
