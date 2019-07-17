package epic.lexicon

import breeze.util.Index

/**
 * A simple lexicon that does no smoothing at all. Only
 * tag/word pairs we've seen are allowed.
 */
class UnsmoothedLexicon[L, W](val labelIndex: Index[L], knownProductions: Set[(L, W)]) extends Lexicon[L, W] {
  private val byWord = Map.empty[W, Set[Int]] ++ knownProductions.groupBy(_._2).mapValues(_.map(pair => labelIndex(pair._1)))

  def anchor(w: IndexedSeq[W]) = new Anchoring {
    def length = w.length
    def allowedTags(pos: Int): Set[Int] = byWord.getOrElse(w(pos), Set.empty)
  }

  override def morePermissive: Lexicon[L, W] = ???
}
