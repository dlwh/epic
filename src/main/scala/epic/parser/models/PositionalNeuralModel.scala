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
import edu.berkeley.nlp.nn.Word2Vec
import scala.collection.mutable.HashMap

/**
 * TODO
 *
 * @author dlwh
 **/
case class PositionalNeuralModelFactory(@Help(text=
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
                            spanFeaturizer: Optional[SplitSpanFeaturizer[String]] = NotProvided,
                            word2vecPath: String = "../cnnkim/data/GoogleNews-vectors-negative300.bin",
                            useNonlinearity: Boolean = true) extends ParserModelFactory[AnnotatedLabel, String] {

  type MyModel = PositionalTransformModel[AnnotatedLabel, AnnotatedLabel, String]



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
//    var wf = posFeaturizer.getOrElse( SpanModelFactory.defaultPOSFeaturizer(annWords))
//    var span: SplitSpanFeaturizer[String] = spanFeaturizer.getOrElse(SpanModelFactory.goodFeaturizer(annWords, commonWordThreshold, useShape = false))
//    span += new SingleWordSpanFeaturizer[String](wf)
//    val indexedSurface = IndexedSplitSpanFeaturizer.fromData(span, annTrees, bloomFilter = false)

    def labelFeaturizer(l: AnnotatedLabel) = Set(l, l.baseAnnotatedLabel).toSeq
    def ruleFeaturizer(r: Rule[AnnotatedLabel]) = if(useGrammar) Set(r, r.map(_.baseAnnotatedLabel)).toSeq else if(r.isInstanceOf[UnaryRule[AnnotatedLabel]]) Set(r.parent, r.parent.baseAnnotatedLabel).toSeq else Seq.empty
//    def ruleFeaturizer(r: Rule[AnnotatedLabel]) = if(r.isInstanceOf[UnaryRule[AnnotatedLabel]]) Set(r.parent, r.parent.baseAnnotatedLabel).toSeq else Seq.empty

    val featurizer = new ProductionFeaturizer[AnnotatedLabel, AnnotatedLabel, String](xbarGrammar, indexedRefinements,
      lGen=labelFeaturizer,
      rGen=ruleFeaturizer)
      
    val word2vec = Word2Vec.loadVectorsForVocabulary(word2vecPath, summedWordCounts.keySet.toSet[String].map(str => Word2Vec.convertWord(str)), true)
    // Convert Array[Float] values to DenseVector[Double] values
    val word2vecDenseVect = word2vec.map(keyValue => (keyValue._1 -> keyValue._2.map(_.toDouble)))
//    val word2vecDenseVect = word2vec.map(keyValue => (keyValue._1 -> new DenseVector[Double](keyValue._2.map(_.toDouble))))
    val surfaceFeaturizer = new PositionalTransformModel.Word2VecSurfaceFeaturizer(word2vecDenseVect, (str: String) => Word2Vec.convertWord(str))
    

//    val baselineAffineTransform = new AffineTransform(featurizer.index.size, indexedSurface.featureIndex.size, new IdentityTransform[FeatureVector]())
//    val lowRankAffineTransform = new AffineTransform(featurizer.index.size, rank, new AffineTransform(rank, indexedSurface.featureIndex.size, new IdentityTransform[FeatureVector]()))
    
    val transform = if (useNonlinearity) {
      // Affine transform of word embeddings, tanh, affine transform to output layer
      new AffineTransform(featurizer.index.size, numHidden, new TanhTransform(new AffineTransform(numHidden, surfaceFeaturizer.vectorSize, new IdentityTransform[DenseVector[Double]]())))
    } else {
      new AffineTransform(featurizer.index.size, surfaceFeaturizer.vectorSize, new IdentityTransform[DenseVector[Double]]())
    }
    println(surfaceFeaturizer.vectorSize + " x " + numHidden + " x " + featurizer.index.size + " neural net")
    
    new PositionalTransformModel(annotator.latent,
      constrainer,
      topology, lexicon,
      refGrammar,
      indexedRefinements,
      featurizer,
      surfaceFeaturizer,
      transform
      )
  }



}