package epic.newfeatures

/**
 *
 * @author dlwh
 */
object SpanShapeGenerator extends Serializable {


  def apply(v1: IndexedSeq[String], begin: Int, end: Int): String = signatureFor(v1,begin, end)

  def signatureFor(words: IndexedSeq[String], begin: Int, end: Int, includeContext: Boolean = true) = {
    val result = new StringBuilder(end-begin)
    if(includeContext) {
      if(begin <= 0) {
        result += '#'
      } else {
        result += binCharacter(words(begin-1).head)
        result += '['
      }
    }
    var i = begin
    while (i < end) {
      val w = if(i < 0 || i >= words.length) "#" else words(i)
      if(w.isEmpty) {
        // probably won't happen.
        result += 'ε'
      } else {
        val c = w(0)
        val x = binCharacter(c)
        if (result.length > 1 && (result.last == x)) {
          result += 'e'
        } else if (result.length > 2 && result.last == 'e' && result(result.length - 2) == x) {
          () // nothing, already have our e
        } else  if(x.isLetterOrDigit) {
          result += x
        } else {
          // keep all punctuation
          result ++= w
        }
      }
      i += 1
    }
    if(includeContext) {
      if(end >= words.length) {
        result += '#'
      } else {
        result += ']'
        result += binCharacter(words(end).head)
      }
    }
    result.toString
  }

  def binCharacter(c: Char): Char = {
    if (c.isLetter && c.isUpper) 'X' else if (c.isLetter) 'x' else if (c.isDigit) 'd' else c
  }
}