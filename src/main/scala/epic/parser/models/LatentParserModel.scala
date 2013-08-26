package epic.parser
package models

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

import features._
import epic.parser.projections.GrammarRefinements
import breeze.linalg._
import java.io.File
import io.Source
import epic.framework.Feature
import epic.trees.annotations.{FilterAnnotations, TreeAnnotator}
import epic.trees._
import breeze.config.Help
import epic.features.{WordPropertyFeaturizer, MinimalWordFeaturizer, IndexedWordFeaturizer}
import epic.lexicon.Lexicon
import epic.util.{SafeLogging, CacheBroker}

class LatentParserModel[L, L3, W](indexedFeatures: IndexedFeaturizer[L, L3, W],
                                  reannotate: (BinarizedTree[L], Seq[W]) => BinarizedTree[L],
                                  val projections: GrammarRefinements[L, L3],
                                  baseFactory: CoreGrammar[L, W],
                                  val baseGrammar: BaseGrammar[L],
                                  val lexicon: Lexicon[L, W],
                                  initialFeatureVal: (Feature => Option[Double]) = { _ => None }) extends ParserModel[L, W] {
  type L2 = L3
  type Inference = LatentParserInference[L, L2, W]

  def featureIndex = indexedFeatures.index

  override def initialValueForFeature(f: Feature) = {
    initialFeatureVal(f) getOrElse (math.random * 1E-5)
  }


  def accumulateCounts(d: TreeInstance[L, W], m: Marginal, accum: ExpectedCounts, scale: Double) {
    m.expectedCounts(indexedFeatures, accum, scale)
  }

  def inferenceFromWeights(weights: DenseVector[Double]) = {
    val lexicon = new FeaturizedLexicon(weights, indexedFeatures)
    val grammar = FeaturizedGrammar(this.baseGrammar, this.lexicon, projections, weights, indexedFeatures, lexicon)

    new LatentParserInference(indexedFeatures, reannotate, grammar, baseFactory, projections)
  }


}

case class LatentParserInference[L, L2, W](featurizer: RefinedFeaturizer[L, W, Feature],
                                           reannotate: (BinarizedTree[L], Seq[W]) => BinarizedTree[L],
                                           grammar: RefinedGrammar[L, W],
                                           baseMeasure: CoreGrammar[L, W],
                                           projections: GrammarRefinements[L, L2]) extends ParserInference[L, W] {


  def goldMarginal(ti: TreeInstance[L, W], aug: CoreAnchoring[L, W]) = {
    val reannotated = reannotate(ti.tree, ti.words)
    val product = AugmentedAnchoring.fromRefined(grammar.anchor(ti.words))
    LatentTreeMarginal(product, projections.labels, reannotated)
  }
}

/**
 * Model for latent annotated grammars (Petrov and Klein, 2008).
 * @param annotator
 * @param substates
 * @param numStates
 * @param oldWeights
 */
case class LatentModelFactory(
                              @Help(text=
                                """The kind of annotation to do on the refined grammar. Default uses no annotations.
You can also epic.trees.annotations.KMAnnotator to get more or less Klein and Manning 2003.
                                """)
                              annotator: TreeAnnotator[AnnotatedLabel, String, AnnotatedLabel] = FilterAnnotations(),
                              @Help(text="Path to substates to use for each symbol. Uses numStates for missing states.")
                              substates: File = null,
                              @Help(text="Split states that the Berkeley Parser doesn't want to split.")
                              splitUselessStates: Boolean = false,
                              @Help(text="Number of states to use. Overridden by substates file")
                              numStates: Int = 2,
                              @Help(text="Old weights to initialize with. Optional.")
                              oldWeights: File = null) extends ParserModelFactory[AnnotatedLabel, String] with SafeLogging {
  type MyModel = LatentParserModel[AnnotatedLabel, (AnnotatedLabel, Int), String]

  val statesToNotSplit = Set("""#
                               |$
                               |''
                               |,
                               |-LRB-
                               |-RRB-
                               |@CONJP
                               |@INTJ
                               |@LST
                               |@RRC
                               |@SBARQ
                               |@WHADJP
                               |@WHADVP
                               |@WHNP
                               |@X
                               |CONJP
                               |EX
                               |FRAG
                               |FW
                               |INTJ
                               |LS
                               |LST
                               |NAC
                               |PRT
                               |RBS
                               |ROOT
                               |RP
                               |RRC
                               |SBARQ
                               |SINV
                               |SYM
                               |TO
                               |UCP
                               |UH
                               |WDT
                               |WHADJP
                               |WHADVP
                               |WHPP
                               |WP
                               |WP$
                               |WRB
                               |X""".stripMargin.split("\\s+"):_*)

  def make(trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]],
           constrainer: CoreGrammar[AnnotatedLabel, String])(implicit cache: CacheBroker):MyModel = {
    val annTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]] = trainTrees.map(annotator(_))

    val (annWords, annBinaries, annUnaries) = this.extractBasicCounts(annTrees)

    val (xbarGrammar, xbarLexicon) = (constrainer.grammar, constrainer.lexicon)

    val substateMap = if (substates != null && substates.exists) {
      val in = Source.fromFile(substates).getLines()
      val pairs = for (line <- in) yield {
        val split = line.split("\\s+")
        AnnotatedLabel(split(0)) -> split(1).toInt
      }
      pairs.toMap + (xbarGrammar.root -> 1)
    } else if(splitUselessStates) {
      Map(xbarGrammar.root -> 1)
    } else {
      statesToNotSplit.iterator.map(s => AnnotatedLabel(s) -> 1).toMap  + (xbarGrammar.root -> 1)
    }

    def split(x: AnnotatedLabel): Seq[(AnnotatedLabel, Int)] = {
      for (i <- 0 until substateMap.getOrElse(x, numStates)) yield (x, i)
    }

    val presplit = xbarGrammar.labelIndex.map(l => l -> split(l)).toMap

    def unsplit(x: (AnnotatedLabel, Int)): AnnotatedLabel = x._1

    def splitRule[L, L2](r: Rule[L], split: L=>Seq[L2]):Seq[Rule[L2]] = r match {
      case BinaryRule(a, b, c) => for(aa <- split(a); bb <- split(b); cc <- split(c)) yield BinaryRule(aa, bb, cc)
        // don't allow non-identity rule refinements for identity rewrites
      case UnaryRule(a, b, chain) if a == b => for(aa <- split(a)) yield UnaryRule(aa, aa, chain)
      case UnaryRule(a, b, chain) => for(aa <- split(a); bb <- split(b)) yield UnaryRule(aa, bb, chain)
    }

    val wordCounts: Counter[String, Double] = sum(annWords, Axis._0)
    val surfaceFeaturizer = new MinimalWordFeaturizer(wordCounts) + new WordPropertyFeaturizer(wordCounts)
    val wordFeaturizer = IndexedWordFeaturizer.fromData(surfaceFeaturizer, annTrees.map{_.words})

    val annGrammar: BaseGrammar[AnnotatedLabel] = BaseGrammar(annTrees.head.tree.label, annBinaries, annUnaries)
    val firstLevelRefinements = GrammarRefinements(xbarGrammar, annGrammar, {(_: AnnotatedLabel).baseAnnotatedLabel})
    val secondLevel = GrammarRefinements(annGrammar, split _, {splitRule(_ :Rule[AnnotatedLabel], presplit)}, unsplit)
    val finalRefinements = firstLevelRefinements compose secondLevel
    logger.info("Label refinements:" + finalRefinements.labels)

    val featureCounter = readWeights(oldWeights)

    val feat = new ProductionFeaturizer[AnnotatedLabel, (AnnotatedLabel, Int), String](xbarGrammar, finalRefinements)
    val indexedFeaturizer = IndexedFeaturizer(feat, wordFeaturizer, trainTrees, annotator andThen (_.tree.map(finalRefinements.labels.refinementsOf)), finalRefinements)

    new LatentParserModel[AnnotatedLabel, (AnnotatedLabel, Int), String](indexedFeaturizer,
    annotator,
    finalRefinements,
    constrainer,
    xbarGrammar,
    xbarLexicon,
    featureCounter.get)
  }
}

