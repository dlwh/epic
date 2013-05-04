package epic.lexicon

import breeze.util.Index
import epic.trees.LexicalProduction

class UnsmoothedLexicon[L, W](val labelIndex: Index[L], knownProductions: Set[(L, W)]) extends Lexicon[L, W] {
  private val byWord = Map.empty[W, Set[Int]] ++ knownProductions.groupBy(_._2).mapValues(_.map(pair => labelIndex(pair._1)))

  def knownLexicalProductions = for( (w,set) <- byWord.iterator; l <- set.iterator) yield LexicalProduction(labelIndex.get(l), w)

  def anchor(w: IndexedSeq[W]) = new Localization {
    def tagsForWord(pos: Int): Set[Int] = byWord.getOrElse(w(pos), Set.empty)
  }
}
