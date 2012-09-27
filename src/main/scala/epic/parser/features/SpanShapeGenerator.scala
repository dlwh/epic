package epic.parser.features

import epic.trees.Span

/**
 *
 * @author dlwh
 */
object SpanShapeGenerator extends ((IndexedSeq[String],Span)=>String) with Serializable {


  def apply(v1: IndexedSeq[String], v2: Span): String = signatureFor(v1,v2)

  def signatureFor(words: IndexedSeq[String], span: Span) = {
    val result = new StringBuilder(span.length)
    var i = 0
    if(span.head == 0) {
      result += '#'
    } else {
      result += binCharacter(words(span.head-1).head)
      result += '['
    }
    while (i < span.length) {
      val w = words(span(i))
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
    if(span.last == words.length - 1) {
      result += '#'
    } else {
      result += ']'
      result += binCharacter(words(span.last+1).head)
    }
    result.toString
  }

  def binCharacter(c: Char): Char = {
    if (c.isLetter && c.isUpper) 'X' else if (c.isLetter) 'x' else if (c.isDigit) 'd' else c
  }
}