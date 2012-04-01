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
import collection.mutable.BitSet
import scalanlp.trees.AnnotatedLabel

/**
 * Not thread-safe... maybe
 * @author dlwh
 */
class StandardCombinerFeaturizer[L,W](grammar: Grammar[L],
                                      tb: TreeBundle[L, W],
                                      useRuleFeatures: Boolean,
                                      useLabelFeatures: Boolean,
                                      featureIndex: Index[Feature],
                                      systemIndex: Index[String],
                                      systemFeatures: Array[Int],
                                      // SystemIndex -> Rule Index -> FeatureIndex
                                      ruleIndex: Array[Array[Int]],
                                      // SystemIndex -> Label Index -> FeatureIndex
                                      labelIndex: Array[Array[Int]],
                                      topLabelIndex: Array[Array[Int]]) extends CombinerFeaturizer[L,W] {

  // yuck
  val SOME = 0 // some system has it (max over values if they're not all 1)
  val COUNT = 1 // sum of all systems scores for this pos

  val outputRules = Array.tabulate(systemIndex.size){ i =>
    val system = systemIndex.get(i)
    val data = for(tree <- tb.outputs.get(system)) yield LabeledSpanExtractor.extractAnchoredRules(grammar.labelIndex, grammar.index, tree)
    data.getOrElse(null)
  }

  // (begin,end)->split->rule->features
  val binaryCache = new TriangularArray(tb.words.length+1,null:Array[OpenAddressHashArray[OldSparseVector]])
  val dummySV = new OldSparseVector(featureIndex.size,0,0)


  def featuresForBinary(begin: Int, split: Int, end: Int, rule: Int):OldSparseVector = {
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
      val tag = grammar.parent(rule)
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
          updateFeatureCounts(sv, system, rule, tag, ruleValue)

        }
        system += 1
      }

      forSplit(rule) = sv
    }
    sv
  }


  def featuresForUnary(begin: Int, end: Int, rule: Int) =  {
    val sv = new OldSparseVector(featureIndex.size)
    val tag = grammar.parent(rule)
    for( (data, system) <- outputRules.zipWithIndex if data ne null) {
      import data._
      val ruleValue = {
        val forSpan = unaryScores(TriangularArray.index(begin, end))
        if(forSpan eq null) 0.0
        else forSpan(rule)
      }
      updateFeatureCounts(sv, system, rule, tag, ruleValue)
      updateLabelFeatureCounts(topLabelIndex, tag, sv, system, ruleValue)
    }

    sv
  }

  private def updateLabelFeatureCounts(labelIndex: Array[Array[Int]], tag: Int, sv: OldSparseVector, system: Int, ruleValue: Double) {
    if (useLabelFeatures) {
      if(ruleValue >= .5) {
        sv(labelIndex(system)(tag)) = 1.0
        sv(labelIndex(COUNT)(tag)) += ruleValue / tb.outputs.size
        sv(labelIndex(SOME)(tag)) = 1.0
      }
    }
  }

  private def updateFeatureCounts(sv: OldSparseVector, system: Int, rule: Int, tag: Int, ruleValue: Double) {
    if (ruleValue != 0) {
      if (useRuleFeatures) {
        sv(ruleIndex(system)(rule)) = ruleValue
        sv(ruleIndex(COUNT)(rule)) += ruleValue / tb.outputs.size
        sv(ruleIndex(SOME)(rule)) = math.max(ruleValue, sv(ruleIndex(SOME)(rule)))
      }

      sv(systemFeatures(system)) = ruleValue
      sv(systemFeatures(COUNT)) += ruleValue / tb.outputs.size
      sv(systemFeatures(SOME)) = math.max(ruleValue, sv(systemFeatures(SOME)))
    }
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
        updateLabelFeatureCounts(labelIndex, tag, sv, system, ruleValue)
      }
    }
    sv
  }

}

case class LabelFeature[L](l: L) extends Feature

class StandardCombinerFeaturizerFactory[L,W](systems: Set[String],
                                             grammar: Grammar[L],
                                             useRuleFeatures: Boolean,
                                             useLabelFeatures: Boolean) extends CombinerFeaturizerFactory[L, W] {
  val systemIndex = Index[String]()
  systemIndex.index("SOME")
  systemIndex.index("COUNT")
  systems foreach {systemIndex.index _}

  val featureIndex = Index[Feature]()
  val systemFeatures = Array.tabulate(systemIndex.size)(i => featureIndex.index(SystemFeature(systemIndex.get(i))))
  val ruleFeatureIndex = Array.tabulate(systemIndex.size,grammar.index.size) { (i,j) =>
    val system = featureIndex.get(systemFeatures(i))
    val rule = grammar.index.get(j)
    if(useRuleFeatures) {
      val f = PairFeature(system,RuleFeature(rule))
      featureIndex.index(f)
    } else {
      -1
    }
  }

  val labelFeatureIndex = Array.tabulate(systemIndex.size,grammar.labelIndex.size) { (i,j) =>
    if(useLabelFeatures) {
      val system = featureIndex.get(systemFeatures(i))
      val rule = grammar.labelIndex.get(j)
      val f = PairFeature(system,LabelFeature(rule))
      featureIndex.index(f)
    } else {
      -1
    }
  }

  val topLabelIndex = Array.tabulate(systemIndex.size,grammar.labelIndex.size) { (i,j) =>
    val system = featureIndex.get(systemFeatures(i))
    val rule = grammar.labelIndex.get(j)
    if(useLabelFeatures)  {
      val f = PairFeature(system,LabelFeature("BOT-" +rule))
      featureIndex.index(f)
    } else {
      -1
    }
  }

  def featurizerFor(tb: TreeBundle[L, W]):CombinerFeaturizer[L,W] = {
    // make a coarse filter:

    new StandardCombinerFeaturizer(grammar,
      tb,
      useRuleFeatures:Boolean,
      useLabelFeatures,
      featureIndex,
      systemIndex,
      systemFeatures,
      ruleFeatureIndex,
      labelFeatureIndex, topLabelIndex)
  }


}

// Features. Lot's more in scalanlp.parser.features._
case class SystemFeature(system: String) extends Feature
