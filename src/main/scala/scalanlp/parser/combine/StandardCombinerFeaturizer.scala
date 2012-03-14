package scalanlp.parser.combine

import scalanlp.parser.features._
import scalala.tensor.mutable.Counter
import scalanlp.util.Index
import collection.immutable.Set
import scalala.tensor.sparse.SparseVector
import scalanlp.parser.projections.AnchoredRuleProjector.AnchoredData._
import scalanlp.tensor.sparse.OldSparseVector
import scalanlp.collection.mutable.{OpenAddressHashArray, TriangularArray}
import scalanlp.parser.{SpanScorer, ParseChart, CKYChartBuilder, Grammar}

/**
 * Not thread-safe... maybe
 * @author dlwh
 */
class StandardCombinerFeaturizer(grammar: Grammar[String],
                                 tb: TreeBundle[String, String],
                                 useRuleFeatures: Boolean,
                                 featureIndex: Index[Feature],
                                 systemIndex: Index[String],
                                 systemFeatures: Array[Int],
                                // SystemIndex -> Rule Index -> FeatureIndex
                                 ruleIndex: Array[Array[Int]],
                                 // SystemIndex -> Label Index -> FeatureIndex
                                 labelIndex: Array[Array[Int]]) extends CombinerFeaturizer[String,String] {

  val outputRules = Array.tabulate(systemIndex.size){ i =>
    val system = systemIndex.get(i)
    val data = for(tree <- tb.outputs.get(system)) yield LabeledSpanExtractor.extractAnchoredRules(grammar.labelIndex, grammar.index, Seq(tree))
    data.getOrElse(null)
  }

  // (begin,end)->split->rule->features
  val binaryCache = new TriangularArray(tb.words.length+1,null:Array[OpenAddressHashArray[OldSparseVector]])
  val dummySV = new OldSparseVector(featureIndex.size,0,0)

  def featuresForBinary(begin: Int, split: Int, end: Int, rule: Int):OldSparseVector = if(!useRuleFeatures) dummySV else  {
    var forSpan = binaryCache(begin,end)
    if(forSpan eq null) {
      binaryCache(begin,end) = new Array[OpenAddressHashArray[OldSparseVector]](end-begin)
      forSpan = binaryCache(begin,end)
    }
    var forSplit = forSpan(split-begin)
    if(forSplit eq null) {
      forSpan(split-begin) = new OpenAddressHashArray(grammar.index.size,null:OldSparseVector)
      forSplit = forSpan(split-begin)
    }

    var sv = forSplit(rule)
    if(sv eq null) {
      sv = new OldSparseVector(featureIndex.size)
      var system = 0
      while(system < outputRules.length) {
        val data = outputRules(system)
        import data._
        if(data ne null) {
          val ruleValue = {
            val forSpan = binaryScores(TriangularArray.index(begin, end))
            if(forSpan eq null) 0.0
            else {
              val forSplit = forSpan(split - begin)
              if(forSplit eq null) 0.0
              else forSplit(rule)
            }
          }
          if(ruleValue != 0) {
            val ruleFeature = ruleIndex(system)(rule)
            val genRuleFeature = ruleIndex(0)(rule)
            val systemFeature = systemFeatures(system)
            sv(ruleFeature) = ruleValue
            sv(genRuleFeature) += ruleValue
            sv(systemFeature) = ruleValue
          }

        }
        system += 1
      }
      forSplit(rule) = sv
    }
    sv
  }

  def featuresForUnary(begin: Int, end: Int, rule: Int) =  if(!useRuleFeatures) dummySV else {
    val sv = new OldSparseVector(featureIndex.size)
    for( (data, system) <- outputRules.zipWithIndex if data ne null) {
      import data._
      val ruleValue = {
        val forSpan = unaryScores(TriangularArray.index(begin, end))
        if(forSpan eq null) 0.0
        else forSpan(rule)
      }
      if(ruleValue != 0) {
        val ruleFeature = ruleIndex(system)(rule)
        val genRuleFeature = ruleIndex(0)(rule)
        val systemFeature = systemFeatures(system)
        sv(ruleFeature) = ruleValue
        sv(genRuleFeature) += ruleValue
        sv(systemFeature) = ruleValue
      }
    }
    sv
  }

  def featuresForSpan(begin: Int, end: Int, tag: Int) = {
    val sv = new OldSparseVector(featureIndex.size)
    for( (data, system) <- outputRules.zipWithIndex if data ne null) {
      import data._
      val ruleValue = {
        val scores = spanScores(TriangularArray.index(begin, end))
        if(scores ne null) scores(tag)
        else 0.0
      }
      if(ruleValue != 0) {
        val ruleFeature = labelIndex(system)(tag)
        val genRuleFeature = labelIndex(0)(tag)
        val systemFeature = systemFeatures(system)
        sv(ruleFeature) = ruleValue
        sv(genRuleFeature) += ruleValue
        sv(systemFeature) = ruleValue
      }
    }
    sv
  }

}

case class LabelFeature[L](l: L) extends Feature

class StandardCombinerFeaturizerFactory(systems: Set[String],
                                        grammar: Grammar[String],
                                        useRuleFeatures: Boolean) extends CombinerFeaturizerFactory[String, String] {
  val systemIndex = Index[String]()
  systemIndex.index("ALL")
  systems foreach {systemIndex.index _}

  val featureIndex = Index[Feature]()
  val systemFeatures = Array.tabulate(systemIndex.size)(i => featureIndex.index(SystemFeature(systemIndex.get(i))))
  val ruleFeatureIndex = Array.tabulate(systemIndex.size,grammar.index.size) { (i,j) =>
    val system = featureIndex.get(systemFeatures(i))
    val rule = grammar.index.get(j)
    val f = PairFeature(system,RuleFeature(rule))
    featureIndex.index(f)
  }

  val labelFeatureIndex = Array.tabulate(systemIndex.size,grammar.labelIndex.size) { (i,j) =>
    val system = featureIndex.get(systemFeatures(i))
    val rule = grammar.labelIndex.get(j)
    val f = PairFeature(system,LabelFeature(rule))
    featureIndex.index(f)
  }

  def featurizerFor(tb: TreeBundle[String, String]):CombinerFeaturizer[String,String] = {
    // make a coarse filter:

    new StandardCombinerFeaturizer(grammar, tb, useRuleFeatures:Boolean, featureIndex, systemIndex, systemFeatures, ruleFeatureIndex, labelFeatureIndex)
  }


}

// Features. Lot's more in scalanlp.parser.features._
case class SystemFeature(system: String) extends Feature
