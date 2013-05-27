package epic.parser.kbest

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
import epic.framework._
import epic.parser._
import breeze.linalg._
import epic.trees.AnnotatedLabel
import epic.parser.models._
import breeze.numerics._
import breeze.util._
import java.io.File
import epic.util.CacheBroker
import epic.trees.annotations._
import epic.parser.projections.ConstraintCoreGrammarAdaptor
import epic.lexicon.Lexicon
import epic.constraints.ChartConstraints.Factory
import epic.trees.TreeInstance
import epic.trees.annotations.FilterAnnotations
import epic.trees.annotations.PipelineAnnotator

case class LocalRerankingParserModelFactory(modelFactory: ParserModelFactory[AnnotatedLabel, String],
                                            k: Int = 200,
                                            baseAnnotator: TreeAnnotator[AnnotatedLabel, String, AnnotatedLabel] = PipelineAnnotator(Seq(FilterAnnotations(),AddMarkovization())),
                                            parser: File = null) extends ParserModelFactory[AnnotatedLabel, String] {
  type MyModel = LocalRerankingModel[AnnotatedLabel, String]
  def make(trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]], constrainer: Factory[AnnotatedLabel, String])(implicit broker: CacheBroker):MyModel = {
    val initialParser = parser match {
      case null =>
        GenerativeParser.fromTrees(trainTrees)
      case f =>
        readObject[SimpleChartParser[AnnotatedLabel, String]](f)
    }

    val kbest = {
      val constraints = new ConstraintCoreGrammarAdaptor(initialParser.grammar, initialParser.lexicon,  constrainer)
      KBestParser.cached(KBestParser(AugmentedGrammar(initialParser.augmentedGrammar.refined, constraints)))
    }

    new LocalRerankingModel(modelFactory.make(trainTrees, constrainer), kbest, k)
  }
}

class LocalRerankingModel[L, W](val model: ParserModel[L, W],
                                kbest: KBestParser[L, W], k: Int = 200) extends ParserModel[L, W] {
  type Inference = LocalRerankingInference[L, W]


  def baseGrammar: BaseGrammar[L] = model.baseGrammar

  def lexicon: Lexicon[L, W] = model.lexicon



  /**
   * Models have features, and this defines the mapping from indices in the weight vector to features.
   * @return
   */
  def featureIndex: Index[Feature] = model.featureIndex

  def initialValueForFeature(f: Feature): Double = model.initialValueForFeature(f)

  def inferenceFromWeights(weights: DenseVector[Double]): Inference =  {
    val inf = model.inferenceFromWeights(weights)
    new LocalRerankingInference[L, W](inf, kbest, k)
  }

  def expectedCountsToObjective(ecounts: ExpectedCounts): (Double, DenseVector[Double]) = {
    model.expectedCountsToObjective(ecounts)
  }


}

class LocalRerankingInference[L, W](val inf: ParserInference[L, W], val kbest: KBestParser[L, W], val k: Int) extends ParserInference[L, W] {


  def baseMeasure: CoreGrammar[L, W] = inf.baseMeasure

  def grammar: RefinedGrammar[L, W] = inf.grammar
  def featurizer: RefinedFeaturizer[L, W, Feature] = inf.featurizer


  def goldMarginal(v: TreeInstance[L, W], aug: CoreAnchoring[L, W]) = {
    inf.goldMarginal(v, aug)
  }


  override def marginal(v: TreeInstance[L, W], aug: CoreAnchoring[L, W]): ParseMarginal[L, W] = {
    val marg = kbest.bestKParses(v.words, k).map{ case (t,_ ) => inf.goldMarginal(TreeInstance("",t, v.words))}
    KBestListMarginal(marg.head.anchoring, marg)
  }

}

case class KBestListMarginal[L, W](anchoring: AugmentedAnchoring[L, W],
                                   marginals: IndexedSeq[ParseMarginal[L, W]]) extends ParseMarginal[L, W] {

  def this(marg: ParseMarginal[L, W]) = this(marg.anchoring, IndexedSeq(marg))

  val probsPerTree = DenseVector(marginals.map(_.logPartition):_*)
  val logPartition: Double = softmax(probsPerTree)
  probsPerTree -= logPartition
  exp.inPlace(probsPerTree)

  /**
   * Forest traversal that visits spans in a "bottom up" order.
   * @param spanVisitor
   */
  def visitPostorder(spanVisitor: AnchoredVisitor[L], spanThreshold: Double) {
    for ((m, i) <- marginals.zipWithIndex if probsPerTree(i) >= math.exp(spanThreshold)) {
      m.visitPostorder(new AnchoredVisitor[L] {
        def visitUnaryRule(begin: Int, end: Int, rule: Int, ref: Int, score: Double) {
          spanVisitor.visitUnaryRule(begin, end, rule, ref, score * probsPerTree(i))

        }

        def visitSpan(begin: Int, end: Int, tag: Int, ref: Int, score: Double) {
          spanVisitor.visitSpan(begin, end, tag, ref, score * probsPerTree(i))

        }

        def visitBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int, score: Double) {
          spanVisitor.visitBinaryRule(begin, split, end, rule, ref, score * probsPerTree(i))
        }
      }, spanThreshold - math.log(probsPerTree(i)))
    }
  }
}
