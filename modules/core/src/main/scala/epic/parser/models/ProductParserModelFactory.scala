package epic.parser.models

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

import java.io.File

import epic.trees.annotations.{ FilterAnnotations, TreeAnnotator, Xbarize }

import io.Source
import breeze.linalg._
import breeze.util.SerializableLogging
import epic.parser.projections.GrammarRefinements
import epic.framework.Feature
import epic.parser._
import epic.trees._
import epic.features.{ IndexedWordFeaturizer, IndicatorFeature, MinimalWordFeaturizer, WordPropertyFeaturizer }
import epic.framework.ComponentFeature
import epic.trees.BinaryRule
import epic.trees.UnaryRule
import epic.trees.TreeInstance
import epic.lexicon.Lexicon
import epic.constraints.ChartConstraints.Factory

/**
 *
 * @author dlwh
 */

case class ProductParserModelFactory(annotator: TreeAnnotator[AnnotatedLabel, String, AnnotatedLabel] = Xbarize(),
                                     substates: File = null,
                                     numStates: Int = 2,
                                     numModels: Int = 2,
                                     oldWeights: File = null,
                                     splitFactor: Int = 1)  extends ParserModelFactory[AnnotatedLabel, String] with SerializableLogging {

  type MyModel = LatentParserModel[AnnotatedLabel, (AnnotatedLabel, Seq[Int]), String]

  def genSplits(numModels: Int, numStates: Int):Seq[IndexedSeq[Int]] = {
    if (numModels == 0)  Seq(IndexedSeq.empty)
    else for(r <- genSplits(numModels -1, numStates); i <- 0 until numStates) yield i +: r
  }

  def split(x: AnnotatedLabel, counts: Map[AnnotatedLabel, Int]):Seq[(AnnotatedLabel, Seq[Int])] = {
    for (split <- genSplits(numModels, counts.getOrElse(x, numStates))) yield x -> split
  }

  def unsplit(x: (AnnotatedLabel, Seq[Int])) = x._1

  def splitRule[L, L2](r: Rule[L], split: L=>Seq[L2]):Seq[Rule[L2]] = r match {
      case BinaryRule(a, b, c) => for(aa <- split(a); bb <- split(b); cc <- split(c)) yield BinaryRule(aa, bb, cc)
        // don't allow non-identity rule refinements for identity rewrites
      case UnaryRule(a, b, chain) if a == b => for(aa <- split(a)) yield UnaryRule(aa, aa, chain)
      case UnaryRule(a, b, chain) => for(aa <- split(a); bb <- split(b)) yield UnaryRule(aa, bb, chain)
    }

  override def make(trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]], topology: RuleTopology[AnnotatedLabel], lexicon: Lexicon[AnnotatedLabel, String], constrainer: Factory[AnnotatedLabel, String]): MyModel = {
    val annTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]] = trainTrees.map(annotator(_))
    val (annWords, annBinaries, annUnaries) = this.extractBasicCounts(annTrees)

    val (xbarGrammar, xbarLexicon) = topology -> lexicon

    val cFactory = constrainer

    val substateMap = if (substates != null && substates.exists) {
      val in = Source.fromFile(substates).getLines()
      val pairs = for (line <- in) yield {
        val split = line.split("\\s+")
        AnnotatedLabel(split(0)) -> split(1).toInt
      }
      pairs.toMap + (xbarGrammar.root -> 1)
    } else {
      Map(xbarGrammar.root -> 1)
    }

    val wordCounts: Counter[String, Double] = sum(annWords, Axis._0)
    val surfaceFeaturizer = new MinimalWordFeaturizer(wordCounts) + new WordPropertyFeaturizer(wordCounts)
    val wordFeaturizer = IndexedWordFeaturizer.fromData(surfaceFeaturizer, annTrees.map{_.words})
    def labelFlattener(l: (AnnotatedLabel, Seq[Int])) = {
      for( (ref,m) <- l._2.zipWithIndex) yield ComponentFeature(m, IndicatorFeature(l._1, ref))
    }
    def ruleFlattener(r: Rule[(AnnotatedLabel, Seq[Int])]) = {
      for( (ref,m) <- r.parent._2.zipWithIndex) yield ComponentFeature(m, r.map( pair => pair._1  -> pair._2(m)))
    }

    val annGrammar = RuleTopology(annTrees.head.tree.label, annBinaries, annUnaries)
    val firstLevelRefinements = GrammarRefinements(xbarGrammar, annGrammar, {(_: AnnotatedLabel).baseAnnotatedLabel})
    val secondLevel = GrammarRefinements(annGrammar, {split(_:AnnotatedLabel,substateMap)}, {splitRule(_ :Rule[AnnotatedLabel], {split(_:AnnotatedLabel,substateMap)})}, unsplit)
    val finalRefinements = firstLevelRefinements compose secondLevel
    logger.info("Will learn with these label refinements: " + finalRefinements.labels)
    val feat = new ProductionFeaturizer[AnnotatedLabel, (AnnotatedLabel, Seq[Int]), String](xbarGrammar, finalRefinements, labelFlattener, ruleFlattener)

    val featureCounter = if (oldWeights ne null) {
      val baseCounter = breeze.util.readObject[Counter[Feature, Double]](oldWeights)
      baseCounter
    } else {
      Counter[Feature, Double]()
    }

    def latentAnnotator(t: BinarizedTree[AnnotatedLabel], w: IndexedSeq[String]) = {
      annotator(t, w).map(finalRefinements.labels.refinementsOf)
    }

    val indexedFeaturizer = IndexedFeaturizer[AnnotatedLabel, (AnnotatedLabel, Seq[Int]), String](feat, wordFeaturizer, trainTrees, annotator andThen (_.tree.map(finalRefinements.labels.refinementsOf)), finalRefinements)

    new LatentParserModel[AnnotatedLabel, (AnnotatedLabel, Seq[Int]), String](indexedFeaturizer,
      latentAnnotator,
      finalRefinements,
      cFactory,
      xbarGrammar,
      xbarLexicon,
      featureCounter.get)
  }

}
