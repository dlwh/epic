package chalk.util

/**
 * A very simple tokenizer that pulls most puncuation off the characters.
 * Given a raw string, tokenize it with a simple regular expression, returning
 * an IndexedSeq[String] with one token per element.
 */
object SimpleTokenizer {
  def apply(text: String): IndexedSeq[String] = text
    .replaceAll("""([\?!()\";\|\[\].,'])""", " $1 ")
    .trim
    .split("\\s+")
    .toIndexedSeq
}
