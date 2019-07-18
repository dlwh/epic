package epic.features

import scala.collection.mutable.HashMap

/**
 * HackyHeadFinders find "heads" in a span using only preterminal labels.
 * It doesn't use the syntactic structure of the sentence.
 *
 * @author gdurrett
 * @tparam L
 * @tparam T
 */
trait HackyHeadFinder[L,T] extends Serializable {
  def findHead(label: L, preterminals: Seq[T]): Int
}

case class RuleBasedHackyHeadFinder() extends HackyHeadFinder[String,String] {
  
  def findHead(label: String, preterminals: Seq[String]): Int = {
    if (!RuleBasedHackyHeadFinder.headRules.contains(label)) {
      0
    } else {
      val result = RuleBasedHackyHeadFinder.headRules(label)(preterminals)
      if (result == -1) {
        println("-1 for " + label + ": " + preterminals)
      }
      result
    }
  }
}

object RuleBasedHackyHeadFinder {
  
  val L2R = true
  val R2L = false
  
  val headRules = new HashMap[String,(Seq[String] => Int)]
  
  // Ss: lots of problems are due to fronted PPs, NPs with sentential complements, etc.
  // NPs: lots of stuff due to CD / $, weird NPs
  // SBAR: I can't figure out why 0 tends to work the best but it does
  
  headRules.put("ADJP", (preterminals) => searchFindFirst(preterminals, L2R, Set("NNS", "NN", "$", "JJ", "VBN", "VBG", "JJR", "JJS")))
  headRules.put("ADVP", (preterminals) => searchFindFirst(preterminals, R2L, Set("RB", "RBR", "RBS", "FW")))
  headRules.put("NP", (preterminals) => searchFindLastBefore(preterminals, L2R,
                     Set("NN", "NNP", "NNPS", "NNS", "NX", "POS", "JJR", "$", "PRN"),
                     Set(",", "WDT", "TO", "IN", "-LRB-", ":", "CC", "("
                         ))); // block appositives, complementizers, prepositions, parentheticals, conjunctions
  headRules.put("QP", (preterminals) => searchFindFirst(preterminals, L2R, Set("$", "IN", "CD")))
  headRules.put("PP", (preterminals) => searchFindFirst(preterminals, L2R, Set("IN", "TO", "VBG", "VBN", "RP", "FW")))
  headRules.put("PRN", (preterminals) => if (preterminals.size > 1) 1 else 0)
  headRules.put("S", (preterminals) => searchFindFirst(preterminals, L2R, Set("TO", "VBD", "VBN", "MD", "VBZ", "VB", "VBG", "VBP")))
  headRules.put("VP", (preterminals) => searchFindFirst(preterminals, L2R, Set("TO", "VBD", "VBN", "MD", "VBZ", "VB", "VBG", "VBP")))
  // headRules.put("SBAR", (preterminals) => searchFindFirst(preterminals, L2R, Set("WP", "WP$", "WDT", "WRB", "IN", "PRP", "PRP$")));
  
  def searchFindFirst(preterminals: Seq[String], leftToRight: Boolean, goodOnes: Set[String]): Int = {
    val start = if (leftToRight) 0 else preterminals.size - 1
    val end = if (leftToRight) preterminals.size else -1
    var headIdx = -1
    var i = start
    while (i != end && headIdx == -1) {
      if (goodOnes.contains(preterminals(i))) {
        headIdx = i
      }
      i += (if (leftToRight) 1 else -1)
    }
    if (headIdx < 0 || headIdx >= preterminals.size) {
      headIdx = start
    }
    headIdx
  }
  
  def searchFindLastBefore(preterminals: Seq[String], leftToRight: Boolean, goodOnes: Set[String], blockers: Set[String]) = {
    val start = if (leftToRight) 0 else preterminals.size - 1
    val end = if (leftToRight) preterminals.size else -1
    var headIdx = -1
    var i = start
    var blocked = false
    while (i != end && !blocked) {
      if (goodOnes.contains(preterminals(i))) {
        headIdx = i
      }
      if (blockers.contains(preterminals(i))) {
        blocked = true
      } else {
        i += (if (leftToRight) 1 else -1)
      }
    }
    if (headIdx == -1) {
    // headIdx = if (leftToRight) preterminals.size - 1 else 0;
      headIdx = if (leftToRight) Math.max(0, i - 1) else Math.min(i+1, preterminals.size)
    }
    if (headIdx < 0 || headIdx >= preterminals.size) {
      headIdx = 0
    }
    headIdx
  }
  
}
