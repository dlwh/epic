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
import epic.trees.annotations.FilterAnnotations
import epic.trees.annotations.TreeAnnotator
import io.Source
import breeze.linalg._
import epic.parser.features.{GenFeaturizer, IndicatorFeature, WordShapeFeaturizer}
import epic.parser.projections.GrammarRefinements
import epic.framework.{ComponentFeature, Feature}
import epic.parser.{AugmentedGrammar, BaseGrammar, RefinedGrammar, ParserParams}
import epic.trees._

/**
 *
 * @author dlwh
 */

case class ProductParserModelFactory(baseParser: ParserParams.BaseParser,
                                     constraints: ParserParams.Constraints[AnnotatedLabel, String],
                                     annotator: TreeAnnotator[AnnotatedLabel, String, AnnotatedLabel] = FilterAnnotations(),
                                     substates: File = null,
                                     numStates: Int = 2,
                                     numModels: Int = 2,
                                     oldWeights: File = null,
                                     splitFactor: Int = 1)  extends ParserModelFactory[AnnotatedLabel, String]  {


  type MyModel = LatentParserModel[AnnotatedLabel, (AnnotatedLabel, Seq[Int]), String]

  def genSplits(numModels: Int, numStates: Int):Seq[IndexedSeq[Int]] = {
    if(numModels == 0)  Seq(IndexedSeq.empty)
    else for(r <- genSplits(numModels -1, numStates); i <- 0 until numStates) yield i +: r
  }

  def split(x: AnnotatedLabel, counts: Map[AnnotatedLabel, Int]):Seq[(AnnotatedLabel, Seq[Int])] = {
    for (split <- genSplits(numModels, counts.getOrElse(x, numStates))) yield (x -> split)
  }

  def unsplit(x: (AnnotatedLabel, Seq[Int])) = x._1


  def splitRule[L, L2](r: Rule[L], split: L=>Seq[L2]):Seq[Rule[L2]] = r match {
    case BinaryRule(a, b, c) => for(aa <- split(a); bb <- split(b); cc <- split(c)) yield BinaryRule(aa, bb, cc)
      // don't allow ref
    case UnaryRule(a, b, chain) if a == b => for(aa <- split(a)) yield UnaryRule(aa, aa, chain)
    case UnaryRule(a, b, chain) => for(aa <- split(a); bb <- split(b)) yield UnaryRule(aa, bb, chain)
  }

  def make(trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]]) = {
    val annTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]] = trainTrees.map(annotator(_))
    val (annWords, annBinaries, annUnaries) = this.extractBasicCounts(annTrees)


    val (xbarParser, xbarLexicon) = baseParser.xbarGrammar(trainTrees)

    val baseFactory = RefinedGrammar.generative(xbarParser, xbarLexicon, annBinaries, annUnaries, annWords)
    val cFactory = constraints.cachedFactory(AugmentedGrammar.fromRefined(baseFactory))

    val substateMap = if (substates != null && substates.exists) {
      val in = Source.fromFile(substates).getLines()
      val pairs = for (line <- in) yield {
        val split = line.split("\\s+")
        AnnotatedLabel(split(0)) -> split(1).toInt
      }
      pairs.toMap + (xbarParser.root -> 1)
    } else {
      Map(xbarParser.root -> 1)
    }

    val gen = new WordShapeFeaturizer(sum(annWords, Axis._0))
    def labelFlattener(l: (AnnotatedLabel, Seq[Int])) = {
      val basic = for( (ref,m) <- l._2.zipWithIndex) yield ComponentFeature(m, IndicatorFeature(l._1, ref))
      basic
    }
    val feat = new GenFeaturizer[(AnnotatedLabel, Seq[Int]), String](gen, labelFlattener _)

    val annGrammar = BaseGrammar(annTrees.head.tree.label, annBinaries, annUnaries)
    val firstLevelRefinements = GrammarRefinements(xbarParser, annGrammar, {(_: AnnotatedLabel).baseAnnotatedLabel})
    val secondLevel = GrammarRefinements(annGrammar, {split(_:AnnotatedLabel,substateMap)}, {splitRule(_ :Rule[AnnotatedLabel], {split(_:AnnotatedLabel,substateMap)})}, unsplit)
    val finalRefinements = firstLevelRefinements compose secondLevel
    println(finalRefinements.labels)

    val featureCounter = if (oldWeights ne null) {
      val baseCounter = breeze.util.readObject[Counter[Feature, Double]](oldWeights)
      baseCounter
    } else {
      Counter[Feature, Double]()
    }

    new LatentParserModel[AnnotatedLabel, (AnnotatedLabel, Seq[Int]), String](feat,
    annotator,
    finalRefinements,
    cFactory,
    xbarParser,
    xbarLexicon, {
      featureCounter.get(_)
    })
  }

}
