package epic.coref

/**
 * Created with IntelliJ IDEA.
 * User: dlwh
 * Date: 8/16/12
 * Time: 2:19 PM
 * To change this template use File | Settings | File Templates.
 */

object Phrases {
  /**
   * Attempts to infer the headword for a given (noun) phrase. Doesn't use tags or anything
   */
  def headFor(mention: IndexedSeq[String]):String = {
    if(mention.length == 1) mention.head
    else {
      var i = mention.indexWhere(s => prepositions.contains(s) || !(s(0).isLetterOrDigit && !isQuote(s)))
      if(i <= 0)
        i = mention.length
      val withoutPrep = mention.view(0, i)
      withoutPrep.reverseIterator.dropWhile(s => !s(0).isLetterOrDigit).next()
    }
  }

  def isPronoun(word: String) = pronouns.contains(word)
  def isQuote(s: String) = s != "``" && s != "''"

  def isPlural(word: String) = {
    (word.endsWith("s") && !word.endsWith("ss")) || pluralPronouns.contains(word)
  }


   val prepositions = Set("aboard",
  "about",
  "above",
  "across",
  "after",
  "against",
  "along",
  "amid",
  "among",
  "anti",
  "around",
  "as",
  "at",
  "before",
  "behind",
  "below",
  "beneath",
  "beside",
  "besides",
  "between",
  "beyond",
  "but",
  "by",
  "concerning",
  "considering",
  "despite",
  "down",
  "during",
  "except",
  "excepting",
  "excluding",
  "following",
  "for",
  "from",
  "in",
  "inside",
  "into",
  "like",
  "minus",
  "near",
  "of",
  "off",
  "on",
  "onto",
  "opposite",
  "outside",
  "over",
  "past",
  "per",
  "plus",
  "regarding",
  "round",
  "save",
  "since",
  "than",
  "through",
  "to",
  "toward",
  "towards",
  "under",
  "underneath",
  "unlike",
  "until",
  "up",
  "upon",
  "versus",
  "via",
  "with",
  "within",
  "without")

  val pronouns = Set(
    "He", "She", "They", "It", "You", "I", "We", "Me", "Us", "Him", "Her", "Them",
    "he", "she", "they", "it", "you", "i", "we", "me", "us", "him", "her", "them",
    "His", "Her", "Their", "Its", "Your", "My", "Our",
    "his", "her", "their", "its", "your", "my", "our",
  "Himself", "Herself", "Themselves", "Itself", "Yourself", "Yourselves", "Myself", "Ourselves",
  "himself", "herself", "themselves", "itself", "yourselves", "myself", "ourselves",
  "This","this","These","Those","that","these","those","that","both"
  )

  val singularPronouns = Set(
    "He", "She", "It", "I", "Me", "Him", "Her",
    "he", "she", "it", "i", "me", "him", "her",
    "His", "Her", "Its", "My",
    "his", "her", "its", "my",
    "Himself", "Herself", "Itself", "Yourself", "Myself",
    "himself", "herself", "itself", "myself",
    "This", "this"
  )

  val pluralPronouns = pronouns -- singularPronouns -- Set("your", "you", "that")
}
