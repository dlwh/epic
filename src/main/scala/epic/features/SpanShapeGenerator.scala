package epic.features

import epic.framework.Feature

@SerialVersionUID(1L)
class SpanShapeFeaturizer extends SurfaceFeaturizer[String] with Serializable {
  def anchor(words: IndexedSeq[String]): SurfaceFeatureAnchoring[String] = {
    new SurfaceFeatureAnchoring[String] {
      def featuresForSpan(begin: Int, end: Int): Array[Feature] = {
        val sig = SpanShapeGenerator.signatureFor(words, begin, end, includeContext = true)
//        println("Features for span " + words.slice(begin, end) + ": " + sig);
//        val sig2 = SpanShapeGenerator.signatureFor(words, begin, end, includeContext = false)
//        Array(SpanShapeFeature(sig), SpanShapeFeature(sig2))
        Array(SpanShapeFeature(sig))
      }
    }
  }
}

class SpanShapeFeaturizerBetter(numContextWords: Int, useRichContext: Boolean) extends SurfaceFeaturizer[String] with Serializable {
  def anchor(words: IndexedSeq[String]): SurfaceFeatureAnchoring[String] = {
    new SurfaceFeatureAnchoring[String] {
      def featuresForSpan(begin: Int, end: Int): Array[Feature] = {
        val sig = SpanShapeGenerator.signatureAndContextFor(words, begin, end, numContextWords, useRichContext)
//        println("Features for span " + words.slice(begin, end) + ": " + sig);
//        val sig2 = SpanShapeGenerator.signatureFor(words, begin, end, includeContext = false)
//        Array(SpanShapeFeature(sig), SpanShapeFeature(sig2))
        Array(SpanShapeFeature(sig))
      }
    }
  }
}

/**
 *
 * @author dlwh
 */
object SpanShapeGenerator extends Serializable {

  val MAX_LEN = 6


  def apply(v1: IndexedSeq[String], begin: Int, end: Int): String = signatureFor(v1,begin, end)

  def signatureFor(words: IndexedSeq[String], begin: Int, end: Int, includeContext: Boolean = true) = {
    val result = new StringBuilder(end-begin)
    if (includeContext) {
      if (begin - 1 < 0) {
        result += '#'
      } else {
        result += binCharacter(words(begin - 1).head)
      }
      result += '['
    }
    var i = begin;
    while (i < math.min(begin + MAX_LEN/2 + 1, end)) {
      appendWordShape(i, words, result)
      i += 1
    }
    if(i < end) {
      //val remainingLength = distanceBinner.binnedDistance(begin, end - MAX_LEN)
      //result ++= "~"  * remainingLength
      result += '~'
      i = end - MAX_LEN/2
    }
    while (i < end) {
      appendWordShape(i, words, result)
      i += 1
    }
    if (includeContext) {
      result += ']';
      if (end >= words.length) {
        result += '#';
      } else {
        result += binCharacter(words(end).head)
      }
    }
    result.toString
  }
  
  // Similar, but has the capability to use more and richer context
  def signatureAndContextFor(words: IndexedSeq[String], begin: Int, end: Int, numContextWords: Int = 1, richContext: Boolean = false) = {
    val result = new StringBuilder(end-begin)
    var i = begin - numContextWords;
    while (i < begin) {
      if (i < 0) {
        result += '#'
      } else {
        if (richContext) {
          appendWordShape(i, words, result)
        } else {
          result += binCharacter(words(i).head)
        }
      }
      i += 1;
    }
    result += '['
    while (i < math.min(begin + MAX_LEN/2 + 1, end)) {
      appendWordShape(i, words, result)
      i += 1
    }
    if(i < end) {
      //val remainingLength = distanceBinner.binnedDistance(begin, end - MAX_LEN)
      //result ++= "~"  * remainingLength
      result += '~'
      i = end - MAX_LEN/2
    }
    while (i < end) {
      appendWordShape(i, words, result)
      i += 1
    }
    result += ']';
    while (i < end + numContextWords) {
      if (i >= words.length) {
        result += '#';
      } else {
        if (richContext) {
          appendWordShape(i, words, result)
        } else {
          result += binCharacter(words(i).head)
        }
      }
      i += 1;
    }
    result.toString
  }


  def appendWordShape(i: Int, words: IndexedSeq[String], result: StringBuilder) {
    val w = if (i < 0 || i >= words.length) "#" else words(i)
    if (w.isEmpty) {
      // probably won't happen.
      result += 'ε'
    } else {
      var c = w(0)
      if(c == '-') {
        c = w match {
          case "-LRB-" => '('
          case "-RRB-" => ')'
          case "-LSB-" => '['
          case "-RSB-" => ']'
          case "-LCB-" => '{'
          case "-RCB-" => '}'
          case _ => c
        }
      }
      val x = binCharacter(c)
      if (result.length > 1 && (result.last == x)) {
        result += 'e'
        ()
      } else if (result.length > 2 && result.last == 'e' && result(result.length - 2) == x) {
        () // nothing, already have our e
      } else {
        result += x
      }
    }
  }

  def binCharacter(c: Char): Char = {
    if (c.isLetter && c.isUpper) 'X' else if (c.isLetter) 'x' else if (c.isDigit) 'd' else c
  }

  val distanceBinner = DistanceBinner()
}

case class SpanShapeFeature(shape: String) extends SpanFeature
