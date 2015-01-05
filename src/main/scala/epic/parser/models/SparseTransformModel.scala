package epic.parser
package models

import java.io.File
import breeze.config.Help
import breeze.features.FeatureVector
import breeze.linalg._
import breeze.util.Index
import epic.constraints.ChartConstraints
import epic.dense.{IdentityTransform, AffineTransform, Transform}
import epic.features.SurfaceFeaturizer.SingleWordSpanFeaturizer
import epic.features._
import epic.framework.Feature
import epic.lexicon.Lexicon
import epic.parser.projections.GrammarRefinements
import epic.trees._
import epic.trees.annotations.TreeAnnotator
import epic.util.{LRUCache, NotProvided, Optional}
import epic.dense.TanhTransform
import epic.corefdense.Word2Vec
import scala.collection.mutable.HashMap

/**
 * TODO
 *
 * @author dlwh
 **/
case class SparseTransformModelFactory(@Help(text=
                              """The kind of annotation to do on the refined grammar. Default uses just parent annotation.
You can also epic.trees.annotations.KMAnnotator to get more or less Klein and Manning 2003.
                              """)
                            annotator: TreeAnnotator[AnnotatedLabel, String, AnnotatedLabel] = GenerativeParser.defaultAnnotator(),
                            @Help(text="Old weights to initialize with. Optional")
                            oldWeights: File = null,
                            @Help(text="For features not seen in gold trees, we bin them into dummyFeats * numGoldFeatures bins using hashing. If negative, use absolute value as number of hash features.")
                            dummyFeats: Double = 0.5,
                            commonWordThreshold: Int = 100,
                            ngramCountThreshold: Int = 5,
                            useGrammar: Boolean = false,
                            numHidden: Int = 100,
                            posFeaturizer: Optional[WordFeaturizer[String]] = NotProvided,
                            spanFeaturizer: Optional[SplitSpanFeaturizer[String]] = NotProvided) extends ParserModelFactory[AnnotatedLabel, String] {

  type MyModel = TransformModel[AnnotatedLabel, AnnotatedLabel, String]



  override def make(trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]],
                    topology: RuleTopology[AnnotatedLabel],
                    lexicon: Lexicon[AnnotatedLabel, String],
                    constrainer: ChartConstraints.Factory[AnnotatedLabel, String]): MyModel = {
    val annTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]] = trainTrees.map(annotator(_))
    println("Here's what the annotation looks like on the first few trees")
    annTrees.slice(0, Math.min(3, annTrees.size)).foreach(tree => println(tree.render(false)))

    val (annWords, annBinaries, annUnaries) = this.extractBasicCounts(annTrees)
    val refGrammar = RuleTopology(AnnotatedLabel.TOP, annBinaries, annUnaries)

    val xbarGrammar = topology
    val xbarLexicon = lexicon

    val indexedRefinements = GrammarRefinements(xbarGrammar, refGrammar, (_: AnnotatedLabel).baseAnnotatedLabel)

    val summedWordCounts: Counter[String, Double] = sum(annWords, Axis._0)
    var wf = posFeaturizer.getOrElse( SpanModelFactory.defaultPOSFeaturizer(annWords))
    var span: SplitSpanFeaturizer[String] = spanFeaturizer.getOrElse(SpanModelFactory.goodFeaturizer(annWords, commonWordThreshold, useShape = false))
    span += new SingleWordSpanFeaturizer[String](wf)
    val indexedSurface = IndexedSplitSpanFeaturizer.fromData(span, annTrees, bloomFilter = false)

    def labelFeaturizer(l: AnnotatedLabel) = Set(l, l.baseAnnotatedLabel).toSeq
//    def ruleFeaturizer(r: Rule[AnnotatedLabel]) = if(useGrammar) Set(r, r.map(_.baseAnnotatedLabel)).toSeq else if(r.isInstanceOf[UnaryRule[AnnotatedLabel]]) Set(r.parent, r.parent.baseAnnotatedLabel).toSeq else Seq.empty
//    def ruleFeaturizer(r: Rule[AnnotatedLabel]) = if(r.isInstanceOf[UnaryRule[AnnotatedLabel]]) Set(r.parent, r.parent.baseAnnotatedLabel).toSeq else Seq.empty
    def ruleFeaturizer(r: Rule[AnnotatedLabel]) = if(useGrammar) Set(r, r.map(_.baseAnnotatedLabel)).toSeq else if(r.isInstanceOf[UnaryRule[AnnotatedLabel]]) Set(r.parent, r.parent.baseAnnotatedLabel).toSeq else Seq.empty

    val featurizer = new ProductionFeaturizer[AnnotatedLabel, AnnotatedLabel, String](xbarGrammar, indexedRefinements,
      lGen=labelFeaturizer,
      rGen=ruleFeaturizer)
      
    // Affine transform of word embeddings, tanh, affine transform to output layer
    val affineTransform = new AffineTransform(featurizer.index.size, indexedSurface.featureIndex.size, new IdentityTransform[FeatureVector]())
    println("Instantiated affine transform with " + indexedSurface.featureIndex.size + " inputs and " + featurizer.index.size + " outputs") 
    
    new TransformModel(annotator.latent,
      constrainer,
      topology, lexicon,
      refGrammar,
      indexedRefinements,
      featurizer,
      indexedSurface,
      affineTransform
      )
  }



}