package scalanlp.parser

/**
 * A CoreGrammar is a weighted grammar over x-bar symbols
 * (that is, symbols that are not refined or annotated) that
 * can be "anchored" to a sentence, giving a
 * [[scalanlp.parser.CoreAnchoring]]. This anchoring
 * can be used to parse.
 *
 * @author dlwh
 * @tparam L label type
 * @tparam W word type
 */
trait CoreGrammar[L, W] extends Serializable {
  def grammar: BaseGrammar[L]
  def lexicon: Lexicon[L, W]

  def root = grammar.root
  def index = grammar.index
  def labelIndex = grammar.labelIndex
  def labelEncoder = grammar.labelEncoder

  /**
   * Returns a [[scalanlp.parser.CoreAnchoring]] for this particular sentence.
   * @param words
   * @return
   */
  def anchor(words: Seq[W]):CoreAnchoring[L, W]
}

object CoreGrammar {
  def identity[L, W](grammar: BaseGrammar[L], lexicon: Lexicon[L, W]):CoreGrammar[L, W] = {
    val g = grammar
    val l = lexicon
    new CoreGrammar[L, W] {
      def grammar =  g
      def lexicon = l

      def anchor(words: Seq[W]) = CoreAnchoring.identity(grammar, lexicon, words)
    }
  }
}