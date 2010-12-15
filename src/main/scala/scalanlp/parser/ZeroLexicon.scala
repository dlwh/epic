package scalanlp.parser

/**
 * 
 * @author dlwh
 */
class ZeroLexicon[L,W](l: Lexicon[L,W]) extends Lexicon[L,W] {
  def knownTagWords = l.knownTagWords

  def tags = l.tags

  def wordScore(label: L, w: W) = if(l.wordScore(label,w) != Double.NegativeInfinity) 0.0 else Double.NegativeInfinity
}

