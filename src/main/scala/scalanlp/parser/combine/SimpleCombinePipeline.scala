package scalanlp.parser
package combine

import collection.immutable.IndexedSeq
import scalanlp.collection.mutable.TriangularArray
import scalanlp.util.Index
import scalanlp.tensor.sparse.OldSparseVector
import projections.AnchoredRuleProjector.AnchoredData
import scalanlp.trees._
import projections.{AnchoredRuleScorer, LabeledSpanScorer}

/**
 * 
 * @author dlwh
 */

object SimpleCombinePipeline extends CombinePipeline {
  def trainParser(trainTrees: IndexedSeq[TreeBundle[String, String]], goldTrees: IndexedSeq[TreeBundle[String, String]], params: Params) = {
    // get a basic grammar over the input tree space. we won't use it for
    // for anything other than just what rules are allowed.
    val basicParser = {
      val allTrees = goldTrees.flatMap(_.treeInstances(withGold=false)).toArray ++ trainTrees.flatMap(_.treeInstances(withGold=true))
      val (lexicon,grammar) = GenerativeParser.extractLexiconAndGrammar(allTrees)
      // erase rule counts
//      val zeroParser = new CKYChartBuilder("ROOT", lexicon, grammar, ParseChart.viterbi)
      val zeroParser = new CKYChartBuilder("ROOT", new ZeroLexicon(lexicon), Grammar.zero(grammar), ParseChart.viterbi)
      zeroParser
    }

    val sentToDataMap = goldTrees.iterator.map(tb => tb.words -> (Map.empty ++ tb.outputs)).toMap

    def sentToScorer(s: Seq[String]) = {
      val outputs = sentToDataMap(s)
      val data = LabeledSpanExtractor.extractAnchoredRules(basicParser.grammar.labelIndex, basicParser.grammar.index, outputs.values.flatten)
      new AnchoredRuleScorer[String](data.spanScores, data.unaryScores, data.binaryScores)
    }

    val parser = new Parser[String,String] with Serializable {
      def bestParse(s: Seq[String], spanScorer: SpanScorer[String]) = {
        val scorer = SpanScorer.sum(sentToScorer(s),spanScorer)
        val inside = basicParser.buildInsideChart(s, scorer)
        val outside = basicParser.buildOutsideChart(inside, scorer)
        decoder.extractBestParse(basicParser.root, basicParser.grammar, inside, outside, s, scorer)
      }
//      val decoder = new SimpleViterbiDecoder[String,String](basicParser.grammar)
      val decoder = MaxConstituentDecoder.simple[String,String](basicParser.grammar)
    }
    Iterator(("MaxRecall" -> parser ))
  }

}


object LabeledSpanExtractor {
  /** Returns a triangular array of counts of particular labels showing up on certain spans. 0 counts are reprseented by -Infinity */
  def extractSpans[L](labelIndex: Index[L], trees: Iterable[BinarizedTree[L]]):Array[OldSparseVector] = {
    val arr = TriangularArray.raw(trees.head.span.end+1, null:OldSparseVector)
    // skip unaries.
    for(t <- trees; c <- t.allChildren if c.children.size != 1) {
      val l = labelIndex(c.label)
      val ind = TriangularArray.index(c.span.start,c.span.end)
      var vec = arr(ind)
      if(vec eq null) {
        vec = new OldSparseVector(labelIndex.size, Double.NegativeInfinity, 1)
        arr(ind) = vec
      }
      if(vec(l) < 0.0) {
        vec(l)  = 0.0
      }
      vec(l) += 1
    }
    arr
  }

  /** Returns a AnchoredData with counts of particular *anchored* rules showing up. 0 counts are -Inf.
   * Nulls are used liberally  for spans without counts*/
  def extractAnchoredRules[L](labelIndex: Index[L],
                              ruleIndex: Index[Rule[L]],
                              trees: Iterable[BinarizedTree[L]]):AnchoredData = {
    val length = trees.head.span.end
    // preliminaries: we're not going to fill in everything: some things will be null.
    // all of this is how to deal with it.
    val numProjectedLabels = labelIndex.size
    val numProjectedRules = ruleIndex.size
    def projFill[T>:Null<:AnyRef:ClassManifest]() = Array.fill[T](numProjectedLabels)(null)
    def projVector() = {
      new OldSparseVector(numProjectedLabels, 0.0)
    }

    def projRuleVector() = {
      new OldSparseVector(numProjectedRules, 0.0)
    }

    def getOrElseUpdate[T<:AnyRef](arr: Array[T], i: Int, t : =>T) = {
      if(arr(i) == null) {
        arr(i) = t
      }
      arr(i)
    }

    // The data, and initialization. most things init'd to null
    val lexicalScores = TriangularArray.raw(length+1,null:OldSparseVector)
    val unaryScores = TriangularArray.raw(length+1,null:OldSparseVector)

    val totals = TriangularArray.raw(length+1,null:OldSparseVector)
    val totalsUnaries = TriangularArray.raw(length+1,null:OldSparseVector)

    val binaryScores = TriangularArray.raw[Array[OldSparseVector]](length+1,null);

    val constant = 1.0/trees.size
    for(t <- trees; child <- t.allChildren) {
      child match {
        case NullaryTree(a) =>
          val ind = TriangularArray.index(child.span.start, child.span.end)
          val pL = labelIndex(a)
          getOrElseUpdate(lexicalScores, ind, projVector())(pL) += 1
        case BinaryTree(a, bc@Tree(b, _), Tree(c, _)) =>
          val rule = BinaryRule(a, b, c)
          val pR = ruleIndex(rule)

          val l = labelIndex(a)
          val index = TriangularArray.index(child.span.start, child.span.end)
          val innerIndex = bc.span.end - bc.span.start
          val parentArray = if (binaryScores(index) eq null) {
            val numSplits = child.span.end - child.span.start
            binaryScores(index) = Array.fill(numSplits)(null: OldSparseVector)
            binaryScores(index)(innerIndex) = projRuleVector()
            binaryScores(index)(innerIndex)
          } else if (binaryScores(index)(innerIndex) eq null) {
            binaryScores(index)(innerIndex) = projRuleVector()
            binaryScores(index)(innerIndex)
          } else {
            binaryScores(index)(innerIndex)
          }
          parentArray(pR) += constant
          if (totals(index) eq null) {
            totals(index) = projVector;
          }
          totals(index)(l) += constant
          getOrElseUpdate(lexicalScores, index, projVector())(l) += constant
        case UnaryTree(a, bc@Tree(b, _)) =>
          val rule = UnaryRule(a, b)
          val pR = ruleIndex(rule)

          val l = labelIndex(a)
          val index = TriangularArray.index(child.span.start, child.span.end)
          val parentArray = if (unaryScores(index) eq null) {
            unaryScores(index) = projRuleVector()
            unaryScores(index)
          } else {
            unaryScores(index)
          }
          parentArray(pR) += constant
          if (totalsUnaries(index) eq null) {
            totalsUnaries(index) = projVector;
          }
          totalsUnaries(index)(l) += constant

      }
    }
    new AnchoredData(lexicalScores, unaryScores, totalsUnaries, binaryScores, totals);
  }
}

