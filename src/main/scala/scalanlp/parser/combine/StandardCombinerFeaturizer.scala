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

/**
 * Not thread-safe... maybe
 * @author dlwh
 */
class StandardCombinerFeaturizer(grammar: Grammar[String],
                                 tb: TreeBundle[String, String],
                                 useRuleFeatures: Boolean,
                                 useLabelFeatures: Boolean,
                                 featureIndex: Index[Feature],
                                 systemIndex: Index[String],
                                 systemFeatures: Array[Int],
                                // SystemIndex -> Rule Index -> FeatureIndex
                                 ruleIndex: Array[Array[Int]],
                                 // SystemIndex -> Label Index -> FeatureIndex
                                 labelIndex: Array[Array[Int]],
                                 topLabelIndex: Array[Array[Int]]) extends CombinerFeaturizer[String,String] {

  // yuck
  val ALL = 0 // 1 if all systems ahve it
  val SOME = 1 // some system has it (max over values if they're not all 1)
  val COUNT = 2 // sum of all systems scores for this system
  val REJECT = 3 // 1 if some system doesn't have it, but some other system does
  val MOST = 4 // 1 if count feature >= .5

  val outputRules = Array.tabulate(systemIndex.size){ i =>
    val system = systemIndex.get(i)
    val data = for(tree <- tb.outputs.get(system)) yield LabeledSpanExtractor.extractAnchoredRules(grammar.labelIndex, grammar.index, Seq(tree))
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
      // features everyone has agreed to touch
      val allFeatures = new collection.mutable.BitSet()
      val rejectedAllFeatures = new collection.mutable.BitSet()
      // features someone has said no to, but someone else yes
      val rejectFeatures = new collection.mutable.BitSet()
      val someFeatures = new collection.mutable.BitSet()
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
          updateFeatureCounts(sv, system, rule, tag, ruleValue, allFeatures, someFeatures, rejectedAllFeatures, rejectFeatures)

        }
        system += 1
      }

      handleCompositeFeatures(sv, allFeatures, rejectedAllFeatures, rejectFeatures, someFeatures, tag, rule)
      forSplit(rule) = sv
    }
    sv
  }


  def featuresForUnary(begin: Int, end: Int, rule: Int) =  {
    val sv = new OldSparseVector(featureIndex.size)
    val allFeatures = new collection.mutable.BitSet()
    val rejectedAllFeatures = new collection.mutable.BitSet()
    // features someone has said no to, but someone else yes
    val rejectFeatures = new collection.mutable.BitSet()
    val someFeatures = new collection.mutable.BitSet()
    val tag = grammar.parent(rule)
    for( (data, system) <- outputRules.zipWithIndex if data ne null) {
      import data._
      val ruleValue = {
        val forSpan = unaryScores(TriangularArray.index(begin, end))
        if(forSpan eq null) 0.0
        else forSpan(rule)
      }
      updateFeatureCounts(sv, system, rule, tag, ruleValue, allFeatures, someFeatures, rejectedAllFeatures, rejectFeatures)
      updateLabelFeatureCounts(topLabelIndex, allFeatures, tag, someFeatures, sv, system, ruleValue, rejectedAllFeatures, rejectFeatures)
    }

    handleCompositeFeatures(sv, allFeatures, rejectedAllFeatures, rejectFeatures, someFeatures, tag, rule)
    sv
  }


  def updateLabelFeatureCounts(labelIndex: Array[Array[Int]], allFeatures: BitSet, tag: Int, someFeatures: BitSet, sv: OldSparseVector, system: Int, ruleValue: Double, rejectedAllFeatures: BitSet, rejectFeatures: BitSet) {
    if (useLabelFeatures) {
      if(ruleValue >= 0) {

      allFeatures += labelIndex(ALL)(tag)
      someFeatures += labelIndex(REJECT)(tag)

      sv(labelIndex(system)(tag)) += ruleValue
      sv(labelIndex(COUNT)(tag)) += ruleValue / tb.outputs.size
      sv(labelIndex(SOME)(tag)) = math.max(ruleValue, sv(labelIndex(SOME)(tag)))
      } else {
        rejectedAllFeatures += labelIndex(ALL)(tag)
        rejectFeatures += labelIndex(REJECT)(tag)
      }
    }
  }

  private def updateFeatureCounts(sv: OldSparseVector, system: Int, rule: Int, tag: Int, ruleValue: Double, allFeatures: BitSet, someFeatures: BitSet, rejectedAllFeatures: BitSet, rejectFeatures: BitSet) {
    if (ruleValue != 0) {
      if (useRuleFeatures) {
        allFeatures += ruleIndex(ALL)(rule)
        someFeatures += ruleIndex(REJECT)(rule)

        sv(ruleIndex(system)(rule)) = ruleValue
        sv(ruleIndex(COUNT)(rule)) += ruleValue / tb.outputs.size
        sv(ruleIndex(SOME)(rule)) = math.max(ruleValue, sv(ruleIndex(SOME)(rule)))
      }

      sv(systemFeatures(system)) = ruleValue
      sv(systemFeatures(COUNT)) += ruleValue / tb.outputs.size
      sv(systemFeatures(SOME)) = math.max(ruleValue, sv(systemFeatures(SOME)))
      someFeatures += systemFeatures(REJECT)
    } else {
      if (useRuleFeatures) {
        rejectedAllFeatures += ruleIndex(ALL)(rule)
        rejectFeatures += ruleIndex(REJECT)(rule)
      }
      rejectedAllFeatures += systemFeatures(ALL)
      rejectFeatures += systemFeatures(REJECT)
    }
  }


  private def handleCompositeFeatures(sv: OldSparseVector, allFeatures: BitSet, rejectedAllFeatures: BitSet, rejectFeatures: BitSet, someFeatures: BitSet, tag: Int, rule: Int) {
    // process all features and reject features
    // these are features that every ststem has seen
    for (f <- allFeatures -- rejectedAllFeatures) {
      sv(f) = 1
    }
    // these are features that some system has seen, and some system has said isn't ok.
    for (f <- rejectFeatures & someFeatures) {
      sv(f) = 1
    }

    // handle most features, by looking for count features with score >= .48
    val MOST_THRESHOLD = 0.48
    if (sv(systemFeatures(COUNT)) >= MOST_THRESHOLD) {
      sv(systemFeatures(MOST)) = 1
    }
    if (useLabelFeatures && sv(labelIndex(COUNT)(tag)) >= MOST_THRESHOLD) {
      sv(labelIndex(MOST)(tag)) = 1
    }
    if (rule >= 0 && useRuleFeatures && sv(ruleIndex(COUNT)(rule)) >= MOST_THRESHOLD) {
      sv(ruleIndex(MOST)(rule)) = 1
    }
  }

  def featuresForSpan(begin: Int, end: Int, tag: Int) = {
    // features everyone has agreed to touch
    val allFeatures = new collection.mutable.BitSet()
    val rejectedAllFeatures = new collection.mutable.BitSet()
    // features someone has said no to, but someone else yes
    val rejectFeatures = new collection.mutable.BitSet()
    val someFeatures = new collection.mutable.BitSet()
    val sv = new OldSparseVector(featureIndex.size)
    for( (data, system) <- outputRules.zipWithIndex if data ne null) {
      import data._
      val ruleValue = {
        val scores = spanScores(TriangularArray.index(begin, end))
        if(scores ne null) scores(tag)
        else 0.0
      }
      if(ruleValue != 0) {
        updateLabelFeatureCounts(labelIndex, allFeatures, tag, someFeatures, sv, system, ruleValue, rejectedAllFeatures, rejectFeatures)
      }
    }
    handleCompositeFeatures(sv, allFeatures, rejectedAllFeatures, rejectFeatures, someFeatures, tag, -1)
    sv
  }

}

case class LabelFeature[L](l: L) extends Feature

class StandardCombinerFeaturizerFactory(systems: Set[String],
                                        grammar: Grammar[String],
                                        useRuleFeatures: Boolean,
                                        useLabelFeatures: Boolean) extends CombinerFeaturizerFactory[String, String] {
  val systemIndex = Index[String]()
  systemIndex.index("ALL")
  systemIndex.index("SOME")
  systemIndex.index("COUNT")
  systemIndex.index("REJECT")
  systemIndex.index("MOST")
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
    val system = featureIndex.get(systemFeatures(i))
    val rule = grammar.labelIndex.get(j)
    val f = PairFeature(system,LabelFeature(rule))
    if(useLabelFeatures) {
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

  def featurizerFor(tb: TreeBundle[String, String]):CombinerFeaturizer[String,String] = {
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
