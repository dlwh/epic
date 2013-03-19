package chalk.lang.eng

/**
 *
 * Porter stemmer in Scala. The original paper is in
 *
 * Porter, 1980, An algorithm for suffix stripping, Program, Vol. 14,
 * no. 3, pp 130-137,
 *
 * See also http://www.tartarus.org/~martin/PorterStemmer
 *
 * A few methods were borrowed from the existing Java port from the above page.
 *
 * This version is adapted from the original by Ken Faulkner. 
 */
class PorterStemmer {

  // word to be stemmed.
  var b = ""

  // Character sets to test membership for
  val vowels = Set('a', 'e', 'i', 'o', 'u')
  val wxy = Set('w', 'x', 'y')

  // Just recode the existing stuff, then go through and refactor with some intelligence.
  def cons(i: Int): Boolean = {
    val ch = b(i)

    if (vowels(ch))
      false
    else {
      if (ch == 'y')
        if (i == 0) true else !cons(i - 1)
      else
        true
    }
  }

  // Add via letter or entire word
  def add(ch: Char) = b += ch
  def add(word: String) = b = word

  /**
   *  m() measures the number of consonant sequences between 0 and j. if c is
   * a consonant sequence and v a vowel sequence, and <..> indicates arbitrary
   * presence,
   *
   * <c><v>       gives 0
   * <c>vc<v>     gives 1
   * <c>vcvc<v>   gives 2
   * <c>vcvcvc<v> gives 3
   * ....
   *
   * I think this can be recoded far more neatly.
   */

  def calcM(s: String): Int = {
    val l = s.length
    var count = 0
    var currentConst = false

    for (c <- 0 to l - 1) {
      if (cons(c)) {
        if (!currentConst && c != 0) {
          count += 1
        }
        currentConst = true
      } else {
        currentConst = false
      }
    }

    count
  }

  // removing the suffix 's', does a vowel exist?'
  def vowelInStem(s: String): Boolean = {
    for (i <- 0 to b.length - 1 - s.length) {
      if (!cons(i)) {
        return true
      }
    }
    return false
  }

  /* doublec(j) is true <=> j,(j-1) contain a double consonant. */
  def doublec(): Boolean = {
    val l = b.length - 1

    if (l < 1)
      false
    else {
      if (b(l) != b(l - 1)) false else cons(l)
    }
  }

  /**
   * cvc(i) is true <=> i-2,i-1,i has the form consonant - vowel - consonant
   * and also if the second c is not w,x or y. this is used when trying to
   * restore an e at the end of a short word. e.g.
   *
   * cav(e), lov(e), hop(e), crim(e), but
   * snow, box, tray.
   *
   */
  def cvc(s: String): Boolean = {
    val i = b.length - 1 - s.length
    if (i < 2 || !cons(i) || cons(i - 1) || !cons(i - 2))
      false
    else
      !wxy(b(i))
  }

  // returns true if it did the change.
  def replacer(orig: String, replace: String, checker: Int => Boolean): Boolean = {
    val l = b.length
    val origLength = orig.length

    if (b.endsWith(orig)) {
      val n = b.substring(0, l - origLength)
      val m = calcM(n)

      if (checker(m))
        b = n + replace

      true
    } else {
      false
    }
  }

  // process the list of tuples to find which prefix matches the case.
  // checker is the conditional checker for m.
  def processSubList(l: List[(String, String)], checker: Int => Boolean): Boolean = {
    val iter = l.iterator
    var done = false

    while (!done && iter.hasNext) {
      val v = iter.next
      done = replacer(v._1, v._2, checker)
    }

    done
  }

  def step1() {

    val l = b.length

    var m = calcM(b)

    // step 1a
    val esses = List(("sses", "ss"), ("ies", "i"), ("ss", "ss"), ("s", ""))
    processSubList(esses, _ >= 0)

    // step 1b
    if (!(replacer("eed", "ee", _ > 0))) {

      if ((vowelInStem("ed") && replacer("ed", "", _ >= 0)) || (vowelInStem("ing") && replacer("ing", "", _ >= 0))) {

        val atebleize = List(("at", "ate"), ("bl", "ble"), ("iz", "ize"))

        if (!processSubList(atebleize, _ >= 0)) {
          // if this isn't done, then it gets more confusing.

          m = calcM(b)
          val last = b(b.length - 1)
          if (doublec() && !"lsz".contains(last)) {
            b = b.substring(0, b.length - 1)
          } else if (m == 1 && cvc("")) {
            b = b + "e"
          }
        }
      }
    }

    // step 1c

    (vowelInStem("y") && replacer("y", "i", _ >= 0))

  }

  def step2 = {

    val suffixes = List(("ational", "ate"), ("tional", "tion"), ("enci", "ence"), ("anci", "ance"), ("izer", "ize"), ("bli", "ble"), ("alli", "al"),
      ("entli", "ent"), ("eli", "e"), ("ousli", "ous"), ("ization", "ize"), ("ation", "ate"), ("ator", "ate"), ("alism", "al"),
      ("iveness", "ive"), ("fulness", "ful"), ("ousness", "ous"), ("aliti", "al"), ("iviti", "ive"), ("biliti", "ble"), ("logi", "log"))

    processSubList(suffixes, _ > 0)
  }

  def step3 = {
    val suffixes = List(("icate", "ic"), ("ative", ""), ("alize", "al"), ("iciti", "ic"), ("ical", "ic"), ("ful", ""), ("ness", ""))
    processSubList(suffixes, _ > 0)
  }

  def step4 = {

    // first part.
    val suffixes = List(("al", ""), ("ance", ""), ("ence", ""), ("er", ""), ("ic", ""), ("able", ""), ("ible", ""), ("ant", ""), ("ement", ""),
      ("ment", ""), ("ent", ""))

    var res = processSubList(suffixes, _ > 1)

    // special part.
    if (!res) {
      if (b.length > 4) {
        if (b(b.length - 4) == 's' || b(b.length - 4) == 't') {
          res = replacer("ion", "", _ > 1)
        }
      }
    }

    // third part.
    if (!res) {
      val suffixes = List(("ou", ""), ("ism", ""), ("ate", ""), ("iti", ""), ("ous", ""), ("ive", ""), ("ize", ""))
      res = processSubList(suffixes, _ > 1)
    }

  }

  def step5a = {
    replacer("e", "", _ > 1)
    if (!cvc("e"))
      replacer("e", "", _ == 1)
  }

  def step5b = {
    val m = calcM(b)
    if (m > 1 && doublec() && b.endsWith("l"))
      b = b.substring(0, b.length - 1)
  }

  def apply(token: String) = {
    add(token)
    if (b.length > 2) {
      step1
      step2
      step3
      step4
      step5a
      step5b
    }
    b
  }

}

object PorterStemmerTest {

  def main(args: Array[String]) {
    val stemmer = new PorterStemmer
    args.foreach { line =>
      println(stemmer(line.trim))
    }
  }
}
