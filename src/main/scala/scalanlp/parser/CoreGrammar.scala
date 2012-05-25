package scalanlp.parser

/**
 * TODO
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

  def specialize(words: Seq[W]):CoreAnchoring[L, W]


}

object CoreGrammar {
  def identity[L, W](grammar: BaseGrammar[L], lexicon: Lexicon[L, W]):CoreGrammar[L, W] = {
    val g = grammar
    val l = lexicon
    new CoreGrammar[L, W] {
      def grammar =  g
      def lexicon = l

      def specialize(words: Seq[W]) = CoreAnchoring.identity(grammar, lexicon, words)
    }
  }
}