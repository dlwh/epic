package scalanlp.parser
package discrim

import scalala.tensor.Counter
import scalala.tensor.dense.DenseVector
import java.util.concurrent.ConcurrentHashMap
import scala.collection.JavaConversions._
import scala.collection.mutable.ConcurrentMap
import scalanlp.tensor.sparse.OldSparseVector
import scalanlp.collection.mutable.{OpenAddressHashArray, TriangularArray}
import scalanlp.util.{MutableIndex, Index}
import discrim.SpanFeaturizer.Bundle

trait SuffStat extends Serializable

/**
 * 
 * @author dlwh
 */
trait SpanTemplate[W] {
  def featuresForBinary(words: Seq[W], begin: Int, split: Int, end: Int):Counter[SuffStat,Double]
  def featuresForUnary(words: Seq[W], begin: Int, end: Int):Counter[SuffStat,Double]
  def featuresForSpan(words: Seq[W], begin: Int, end: Int):Counter[SuffStat,Double]
}


object SpanFeaturizer {
  private case class IntermediateBundle(spanScores: Array[OpenAddressHashArray[Double]], // triangular index -> suff stat index -> score
                                 // (begin,end) -> suff stat index -> score
                                 unaryScores: Array[OpenAddressHashArray[Double]],
                                 // (begin,end) -> (split-begin) -> suff stat index -> score
                                 binaryScores: Array[Array[OpenAddressHashArray[Double]]])

  case class Bundle(spanScores: Array[OldSparseVector], // triangular index -> suff stat index -> score
                            // (begin,end) -> suff stat index -> score
                            unaryScores: Array[OldSparseVector],
                            // (begin,end) -> (split-begin) -> suff stat index -> score
                            binaryScores: Array[Array[OldSparseVector]])

  private def mkDummyScorer[L, W](bundle: IntermediateBundle, template: SpanTemplate[W],
                                  words: Seq[W],
                                  indexForSpans: MutableIndex[SuffStat],
                                  indexForRules: MutableIndex[SuffStat]) = {
    new SpanScorer[L] {
      import bundle._

      def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int) = {
        val i = TriangularArray.index(begin, end)
        if (binaryScores(i) eq null) {
          binaryScores(i) = new Array(end - split)
        }

        if (binaryScores(i)(split - begin) eq null) {
          val ctr = template.featuresForBinary(words, begin, split, end)
          val hash = new OpenAddressHashArray[Double](Int.MaxValue, ctr.size * 2)
          binaryScores(i)(split - begin) = hash
          for ((k, v) <- ctr.pairsIterator) {
            hash(indexForRules.index(k)) = v
          }
        }
        0.0
      }

      def scoreUnaryRule(begin: Int, end: Int, rule: Int) = {
        val i = TriangularArray.index(begin, end)
        if (unaryScores(i) eq null) {
          val ctr = template.featuresForUnary(words, begin, end)
          val hash = new OpenAddressHashArray[Double](Int.MaxValue, ctr.size * 2)
          unaryScores(i) = hash
          for ((k, v) <- ctr.pairsIterator) {
            hash(indexForRules.index(k)) = v
          }
        }
        0.0
      }

      def scoreSpan(begin: Int, end: Int, tag: Int) = {
        val i = TriangularArray.index(begin, end)
        if (spanScores(i) eq null) {
          val ctr = template.featuresForSpan(words, begin, end)
          val hash = new OpenAddressHashArray[Double](Int.MaxValue, ctr.size * 2)
          spanScores(i) = hash
          for ((k, v) <- ctr.pairsIterator) {
            hash(indexForSpans.index(k)) = v
          }
        }
        0.0
      }
    }
  }

  private def finishBundle(bundle: SpanFeaturizer.IntermediateBundle, ruleIndex: MutableIndex[SuffStat], spanIndex: MutableIndex[SuffStat]): SpanFeaturizer.Bundle = {
    val newUnary = bundle.unaryScores.map {
      (arr: OpenAddressHashArray[Double]) =>
        if (arr eq null) null
        else {
          val vec = new OldSparseVector(ruleIndex.size, initialNonzeros = arr.activeSize)
          for ((k, v) <- arr.pairsIterator) {
            vec(k) = v
          }
          vec
        }
    }

    val newBinary = bundle.binaryScores.map {
      (arr: Array[OpenAddressHashArray[Double]]) =>
        if (arr eq null) null
        else {
          arr.map {
            (arr: OpenAddressHashArray[Double]) =>
              if (arr eq null) null
              else {
                val vec = new OldSparseVector(ruleIndex.size, initialNonzeros = arr.activeSize)
                for ((k, v) <- arr.pairsIterator) {
                  vec(k) = v
                }
                vec
              }
          }
        }
    }

    val newSpans = bundle.spanScores.map {
      (arr: OpenAddressHashArray[Double]) =>
        if (arr eq null) null
        else {
          val vec = new OldSparseVector(spanIndex.size, initialNonzeros = arr.activeSize)
          for ((k, v) <- arr.pairsIterator) {
            vec(k) = v
          }
          vec
        }
    }

    new Bundle(newSpans, newUnary, newBinary)
  }

  def mkDummyTestScorer[L,W](bundle: Bundle, template: SpanTemplate[W],
                                      words: Seq[W],
                                      indexForSpans: Index[SuffStat],
                                      indexForRules: Index[SuffStat]) = {
    new SpanScorer[L] {
      import bundle._

      def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int) = {
        val i = TriangularArray.index(begin, end)
        if (binaryScores(i) eq null) {
          binaryScores(i) = new Array(end - split)
        }

        if (binaryScores(i)(split - begin) eq null) {
          val ctr = template.featuresForBinary(words, begin, split, end)
          val hash = new OldSparseVector(indexForRules.size,initialNonzeros = ctr.size)
          binaryScores(i)(split - begin) = hash
          for ((k, v) <- ctr.pairsIterator) {
            val i = indexForRules(k)
            if(i != -1)
              hash(i) = v
          }
        }
        0.0
      }

      def scoreUnaryRule(begin: Int, end: Int, rule: Int) = {
        val i = TriangularArray.index(begin, end)
        if (unaryScores(i) eq null) {
          val ctr = template.featuresForUnary(words, begin, end)
          val hash = new OldSparseVector(indexForRules.size,initialNonzeros = ctr.size)
          unaryScores(i) = hash
          for ((k, v) <- ctr.pairsIterator) {
            val i = indexForRules(k)
            if(i != -1)
              hash(i) = v
          }
        }
        0.0
      }

      def scoreSpan(begin: Int, end: Int, tag: Int) = {
        val i = TriangularArray.index(begin, end)
        if (spanScores(i) eq null) {
          val ctr = template.featuresForSpan(words, begin, end)
          val hash = new OldSparseVector(indexForSpans.size,initialNonzeros = ctr.size)
          spanScores(i) = hash
          for ((k, v) <- ctr.pairsIterator) {
            val i = indexForSpans(k)
            if(i != -1)
              hash(i) = v
          }
        }
        0.0
      }
    }
  }

  def index[L,L2,W](cb: ChartBuilder[ParseChart,L,W], template: SpanTemplate[W], sentences: Seq[TreeInstance[L,W]]) = {
    val indexForRules = Index[SuffStat]()
    val indexForSpans = Index[SuffStat]()
    // string == id
    val partiallyCached = new collection.mutable.HashMap[String,IntermediateBundle]
    for (ti <- sentences) {
      val length = ti.words.length
      val bundle = new IntermediateBundle(TriangularArray.raw(length,null), TriangularArray.raw(length,null), TriangularArray.raw(length,null))
      // use the scorer infrastructure to expand all feasible spans
      val dummyScorer = mkDummyScorer[L,W](bundle, template, ti.words, indexForSpans, indexForRules)
      val chart = cb.buildInsideChart(ti.words,SpanScorer.sum(ti.spanScorer,dummyScorer))
      cb.buildOutsideChart(chart,SpanScorer.sum(ti.spanScorer,dummyScorer))
      partiallyCached(ti.id) = bundle
    }


    val cached = Map.empty[String,Bundle] ++ partiallyCached.mapValues { bundle =>
      finishBundle(bundle, indexForRules, indexForSpans)
    }

    WeightedSpanBrokerFactory.normal(cb,template,cached,indexForSpans,indexForRules)
  }

}

trait WeightedSpanBrokerFactory[L,W] extends Serializable {
  def numWeights[L2](labelIndex:Index[L2], ruleIndex:Index[Rule[L2]]): Int
  def broker[L2](labelIndex: Index[L2],
                ruleIndex: Index[Rule[L2]],
                weights: DenseVector[Double]): BaseWeightedSpanBroker[L,L2,W]
}

object WeightedSpanBrokerFactory {
  def normal[L,W](cb: ChartBuilder[ParseChart,L,W],
             template: SpanTemplate[W],
             cache: Map[String,SpanFeaturizer.Bundle],
             indexForLabels: Index[SuffStat],
             indexForRules: Index[SuffStat]):WeightedSpanBrokerFactory[L,W] = {
    new WeightedSpanBrokerFactory[L,W] {
      def numWeights[L2](labelIndex:Index[L2], ruleIndex:Index[Rule[L2]]) =  indexForLabels.size * labelIndex.size + indexForRules.size * ruleIndex.size

      def broker[L2](labelIndex: Index[L2], ruleIndex: Index[Rule[L2]], weights: DenseVector[Double]) = {
        new WeightedSpanBroker(cb, template, cache, indexForLabels, indexForRules, labelIndex, ruleIndex, weights)
      }
    }
  }

  def identity[L,W]:WeightedSpanBrokerFactory[L,W] = new WeightedSpanBrokerFactory[L,W] {
    def numWeights[L](labelIndex:Index[L], ruleIndex:Index[Rule[L]]) =  0

    def broker[L2](labelIndex: Index[L2], ruleIndex: Index[Rule[L2]], weights: DenseVector[Double]) = {
      new NullWeightedSpanBroker[L,L2,W]
    }
  }
}


trait BaseWeightedSpanBroker[L,L2,W] extends SpanBroker[L2] with Serializable {
  def spanScorerFor(w: Seq[W], scorer: SpanScorer[L]):SpanScorer[L2]
  def spanForId(id: String):SpanScorer[L2]
  def ecountsVisitor(id: String, counts: Array[Double]):AnchoredSpanVisitor
  def numWeights:Int
}

class NullWeightedSpanBroker[L,L2,W] extends BaseWeightedSpanBroker[L,L2,W] {
  def spanScorerFor(w: Seq[W], scorer: SpanScorer[L]) = SpanScorer.identity[L2]

  def spanForId(id: String) = SpanScorer.identity

  def numWeights = 0
  def ecountsVisitor(id: String, counts: Array[Double]) = AnchoredSpanVisitor.noOp
}

class WeightedSpanBroker[L,L2,W](cb: ChartBuilder[ParseChart,L,W],
                             template: SpanTemplate[W],
                             cache: Map[String,SpanFeaturizer.Bundle],
                             indexForLabels: Index[SuffStat],
                             indexForRules: Index[SuffStat],
                             val labelIndex: Index[L2],
                             val ruleIndex: Index[Rule[L2]],
                             weights: DenseVector[Double]) extends BaseWeightedSpanBroker[L,L2,W] {

  def spanScorerFor(w: Seq[W], scorer: SpanScorer[L]):SpanScorer[L2] = {
    val length = w.length
    val bundle = new Bundle(TriangularArray.raw(length,null), TriangularArray.raw(length,null), TriangularArray.raw(length,null))
    val dummyScorer = SpanFeaturizer.mkDummyTestScorer[L,W](bundle, template, w, indexForLabels, indexForRules)
    val chart = cb.buildInsideChart(w,SpanScorer.sum(scorer, dummyScorer))
    cb.buildOutsideChart(chart,SpanScorer.sum(scorer,dummyScorer))
    new SpanFeatureScorer(bundle, weights.data, indexForLabels.size, labelIndex.size, ruleIndex.size)
  }

  def spanForId(id: String) = {
    new SpanFeatureScorer(cache(id),weights.data,indexForLabels.size,labelIndex.size,ruleIndex.size)
  }

  def ecountsVisitor(id: String, counts: Array[Double]) = {
    new SpanFeatureECounts(cache(id),counts,indexForLabels.size,labelIndex.size,ruleIndex.size)
  }

  def numWeights = indexForLabels.size * labelIndex.size + indexForRules.size * ruleIndex.size
}


class SpanFeatureScorer[L](bundle: SpanFeaturizer.Bundle,
                           weights: Array[Double], numSpanFeatures: Int, labelSize: Int, ruleSize: Int) extends SpanScorer[L] {
  def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int) = {
    val ind = TriangularArray.index(begin,end)
    if(bundle.binaryScores(ind) eq null) 0.0
    else {
      val features = bundle.binaryScores(ind)(split - begin)
      if(features eq null) 0.0
      else {
        var sum = 0.0
        var i = 0
        while(i < features.activeSize) {
          val f = features.indexAt(i)
          val actualF = numSpanFeatures * labelSize + f * ruleSize + rule
          sum += features.valueAt(i) * weights(actualF)
          i += 1
        }
        sum
      }
    }
  }

  def scoreUnaryRule(begin: Int, end: Int, rule: Int) = {
    val ind = TriangularArray.index(begin,end)
    if(bundle.unaryScores(ind) eq null) 0.0
    else {
      val features = bundle.unaryScores(ind)
      var sum = 0.0
      var i = 0
      while(i < features.activeSize) {
        val f = features.indexAt(i)
        val actualF = numSpanFeatures * labelSize + f * ruleSize + rule
        sum += features.valueAt(i) * weights(actualF)
        i += 1
      }
      sum
    }
  }

  def scoreSpan(begin: Int, end: Int, tag: Int) = {
    val ind = TriangularArray.index(begin,end)
    if(bundle.spanScores(ind) eq null) 0.0
    else {
      val features = bundle.spanScores(ind)
      var sum = 0.0
      var i = 0
      while(i < features.activeSize) {
        val f = features.indexAt(i)
        val actualF = f * labelSize + tag
        sum += features.valueAt(i) * weights(actualF)
        i += 1
      }
      sum
    }
  }
}

class SpanFeatureECounts(bundle: SpanFeaturizer.Bundle,
                         counts: Array[Double], numSpanFeatures: Int, labelSize: Int, ruleSize: Int) extends AnchoredSpanVisitor {
  def visitBinaryRule(begin: Int, split: Int, end: Int, rule: Int, score: Double) {
    val ind = TriangularArray.index(begin,end)
    if(bundle.binaryScores(ind) eq null) 0.0
    else {
      val features = bundle.binaryScores(ind)(split - begin)
      if(features eq null) 0.0
      else {
        var i = 0
        while(i < features.activeSize) {
          val f = features.indexAt(i)
          val actualF = numSpanFeatures * labelSize + f * ruleSize + rule
          counts(actualF) += features.valueAt(i) * score
          i += 1
        }
      }
    }
  }

  def visitUnaryRule(begin: Int, end: Int, rule: Int, score: Double) {
    val ind = TriangularArray.index(begin,end)
    if(bundle.unaryScores(ind) eq null) 0.0
    else {
      val features = bundle.unaryScores(ind)
      var i = 0
      while(i < features.activeSize) {
        val f = features.indexAt(i)
        val actualF = numSpanFeatures * labelSize + f * ruleSize + rule
        counts(actualF) += features.valueAt(i) * score
        i += 1
      }
    }
  }

  def visitSpan(begin: Int, end: Int, tag: Int, score: Double) {
    val ind = TriangularArray.index(begin,end)
    if(bundle.spanScores(ind) eq null) 0.0
    else {
      val features = bundle.spanScores(ind)
      var i = 0
      while(i < features.activeSize) {
        val f = features.indexAt(i)
        val actualF = f * labelSize + tag
        counts(actualF) += features.valueAt(i) * score
        i += 1
      }
    }
  }
}

object Petrov08SpanTemplate extends SpanTemplate[String] {
  case class WordPreceding(w: String) extends SuffStat
  case class WordFollowing(w: String) extends SuffStat
  case class FirstWord(w: String) extends SuffStat
  case class LastWord(w: String) extends SuffStat
  case class BegBorder(l: String, r: String) extends SuffStat
  case class EndBorder(l: String, r: String) extends SuffStat
  case class PunctShape(shape: String) extends SuffStat
  case class CapShape(shape: String) extends SuffStat

  val emptyCounter = Counter[SuffStat,Double]()
  def featuresForBinary(words: Seq[String], begin: Int, split: Int, end: Int) = {
    emptyCounter
  }

  def featuresForUnary(words: Seq[String], begin: Int, end: Int) = {
    emptyCounter
  }

  def featuresForSpan(words: Seq[String], begin: Int, end: Int) = {
    if(begin +1 < end) {
      val ctr = Counter[SuffStat, Double]()
      val precWord = if(begin > 0)  words(begin - 1) else "#"
      val postWord = if(end < words.length)  words(end) else "#"
      ctr(WordPreceding(precWord)) = 1.0
      ctr(WordPreceding(postWord)) = 1.0
      ctr(FirstWord(words(begin))) = 1.0
      ctr(LastWord(words(end-1))) = 1.0
      ctr(BegBorder(precWord,words(begin))) = 1.0
      ctr(EndBorder(words(end-1),postWord)) = 1.0

      ctr(PunctShape(punctShape(words, begin, end))) = 1.0
      ctr(CapShape(capShape(words, begin, end))) = 1.0
      ctr
    } else {
      emptyCounter
    }
  }

  private def punctShape(words: Seq[String], begin: Int, end: Int) = {
    def oneShape(w: String) = {
      if(!w(0).isLetterOrDigit)  w
      else "x"
    }
    val buf = new StringBuilder()

    if (begin == 0) buf += '#'
    else buf ++= oneShape(words(begin-1))

    buf += '['
    var i = begin
    while(i < end) {
      val nextShape = oneShape(words(i))
      if(nextShape(0) == 'x')  buf.last match {
          case 'x' =>
            buf += '+'
          case '+' =>
            // nothing
          case _ =>
            buf ++= nextShape
      } else {
        buf ++= nextShape
      }

      i += 1
    }

    buf += ']'

    if (end == words.length) buf += '#'
    else buf ++= oneShape(words(end))

    buf.toString
  }

  private def capShape(words: Seq[String], begin: Int, end: Int) = {
    def oneShape(w: String) = {
      if(!w(0).isLetterOrDigit) '.'
      else if(w(0).isUpper) 'X'
      else 'x'
    }
    val buf = new StringBuilder()

    if (begin == 0) buf += '#'
    else buf += oneShape(words(begin-1))

    buf += '['
    var i = begin
    while(i < end) {
      val nextShape = oneShape(words(i))
      if(nextShape == 'x')  buf.last match {
          case 'x' =>
            buf += '+'
          case '+' =>
            // nothing
          case _ =>
            buf += nextShape
      } else {
        buf += nextShape
      }

      i += 1
    }

    buf += ']'

    if (end == words.length) buf += '#'
    else buf += oneShape(words(end))

    buf.toString
  }
}