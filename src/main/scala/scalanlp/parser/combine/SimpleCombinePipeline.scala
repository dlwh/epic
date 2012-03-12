package scalanlp.parser
package combine

import collection.immutable.IndexedSeq
import scalanlp.trees.BinarizedTree
import scalanlp.collection.mutable.TriangularArray
import scalanlp.util.Index
import scalanlp.tensor.sparse.OldSparseVector
import projections.LabeledSpanScorer

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
      val zeroParser = new CKYChartBuilder("ROOT", lexicon, grammar, ParseChart.viterbi)
      zeroParser
    }

    val sentToScorerMap = { for {
      tb <- goldTrees.par
    } yield {
      val spans = LabeledSpanExtractor.extractSpans(basicParser.grammar.labelIndex, tb.outputs.values)
      tb.words -> new LabeledSpanScorer[String](spans)
    }}.seq.toMap

    val parser = new Parser[String,String] with Serializable {
      def bestParse(s: Seq[String], spanScorer: SpanScorer[String]) = {
        val scorer = SpanScorer.sum(sentToScorerMap(s),spanScorer)
        val inside = basicParser.buildInsideChart(s, scorer)
        decoder.extractBestParse(basicParser.root, basicParser.grammar, inside, null, s, scorer)
      }
      val decoder = new SimpleViterbiDecoder[String,String](basicParser.grammar)
    }
    Iterator(("MaxRecall" -> parser ))
  }

}


object LabeledSpanExtractor {
  /** Returns a triangular array of counts of particular labels showing up on certain spans */
  def extractSpans[L](labelIndex: Index[L], trees: Iterable[BinarizedTree[L]]):Array[OldSparseVector] = {
    val arr = TriangularArray.raw(trees.head.span.end+1, null:OldSparseVector)
    // skip unaries.
    for(t <- trees; c <- t.allChildren if c.children.size != 1) {
      val l = labelIndex(c.label)
      val ind = TriangularArray.index(c.span.start,c.span.end)
      var vec = arr(ind)
      if(vec eq null) {
        vec = new OldSparseVector(labelIndex.size, 0.0, 1)
        arr(ind) = vec
      }
      vec(l) += 1
    }
    arr
  }
}

