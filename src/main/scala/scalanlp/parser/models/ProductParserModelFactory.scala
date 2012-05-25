package scalanlp.parser.models


import java.io.File
import scalanlp.trees.annotations.FilterAnnotations
import scalanlp.trees.annotations.TreeAnnotator
import scalanlp.trees.{TreeInstance, AnnotatedLabel}
import io.Source
import scalala.library.Library
import scalanlp.parser.features.{GenFeaturizer, IndicatorFeature, WordShapeFeaturizer}
import scalanlp.parser.projections.GrammarRefinements
import scalala.tensor.Counter
import scalanlp.epic.{ComponentFeature, Feature}
import scalanlp.parser.{AugmentedGrammar, BaseGrammar, RefinedGrammar, ParserParams}

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

  def genSplits(numModels: Int, numStates: Int):Seq[Vector[Int]] = {
    if(numModels == 0)  Seq(Vector.empty)
    else for(r <- genSplits(numModels -1, numStates); i <- 0 until numStates) yield i +: r
  }

  def split(x: AnnotatedLabel, counts: Map[AnnotatedLabel, Int]):Seq[(AnnotatedLabel, Seq[Int])] = {
    for (split <- genSplits(numModels, counts.getOrElse(x, numStates))) yield (x -> split)
  }

  def unsplit(x: (AnnotatedLabel, Seq[Int])) = x._1

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

    val gen = new WordShapeFeaturizer(Library.sum(annWords))
    def labelFlattener(l: (AnnotatedLabel, Seq[Int])) = {
      val basic = for( (ref,m) <- l._2.zipWithIndex) yield ComponentFeature(m, IndicatorFeature(l._1, ref))
      basic
    }
    val feat = new GenFeaturizer[(AnnotatedLabel, Seq[Int]), String](gen, labelFlattener _)

    val annGrammar = BaseGrammar(annTrees.head.tree.label, annBinaries, annUnaries)
    val firstLevelRefinements = GrammarRefinements(xbarParser, annGrammar, {(_: AnnotatedLabel).baseAnnotatedLabel})
    val secondLevel = GrammarRefinements(annGrammar, split(_: AnnotatedLabel, substateMap), unsplit)
    val finalRefinements = firstLevelRefinements compose secondLevel
    println(finalRefinements.labels)

    val featureCounter = if (oldWeights ne null) {
      val baseCounter = scalanlp.util.readObject[Counter[Feature, Double]](oldWeights)
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
