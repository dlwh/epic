package epic.coref

import breeze.linalg.Counter
import epic.framework.Feature

/**
 *
 * @author dlwh
 */
trait PairwiseFeaturizer {
  def featuresFor(a: MentionCandidate, b: MentionCandidate, context: IndexedSeq[IndexedSeq[String]]):Counter[Feature, Double]
  def featuresForRoot(a: MentionCandidate, context: IndexedSeq[IndexedSeq[String]]):Counter[Feature, Double]
}

class SimplePairwiseFeaturizer extends PairwiseFeaturizer {
  def featuresFor(a: MentionCandidate,
                  b: MentionCandidate,
                  context: IndexedSeq[IndexedSeq[String]]): Counter[Feature, Double] = {
    val res = Counter[Feature, Double]()

    val tpeA = mentionType(a)
    val tpeB = mentionType(b)
    val typeFeature = PairFeature(tpeA, tpeB)
    res(typeFeature) = 1.0

    // exact match
    if(a.words == b.words) {
      res(PairFeature(typeFeature, ExactStringMatch)) = 1.0
    }

    val aLower = a.words.map(_.toLowerCase)
    val bLower = b.words.map(_.toLowerCase)
    if(aLower == bLower) {
      res(PairFeature(typeFeature, LowerStringMatch)) = 1.0
    }

    // head match
    val headA = semHead(a.words)
    val headB = semHead(b.words)
    if(headA == headB) {
      res(HeadMatch) = 1.0
      res(PairFeature(HeadMatch, typeFeature)) = 1.0
    } else {
      res(Not(HeadMatch)) = 1.0
      res(Not(PairFeature(HeadMatch, typeFeature))) = 1.0
    }

    // string match
    if(tpeA == tpeB && tpeA != 'Pronoun) {
      val aSet = a.words.toSet
      val bSet = b.words.toSet
      val inter = (aSet & bSet)
      res(PartialStringMatch('Jacard)) = inter.size.toDouble / (aSet | bSet).size

      val aLSet = a.words.toSet
      val bLSet = b.words.toSet
      val interL = (aSet & bSet)
      res(PartialStringMatch('JacardLower)) = interL.size.toDouble / (aLSet | bLSet).size

      if(inter.isEmpty)
        res(NoOverlap) = 1.0
      else if(inter == aSet || inter == bSet) {
        res(ContainsString) = 1.0
      }
    }

    // discourse-y stuff
    val distance = binnedSentenceDistance(a.sentence, b.sentence)
    res(SentenceDistFeature(distance)) = 1.0
    res(PairFeature(SentenceDistFeature(distance), typeFeature)) = 1.0


    res
  }

  def featuresForRoot(a: MentionCandidate, context: IndexedSeq[IndexedSeq[String]]): Counter[Feature, Double] = {
    val res = Counter[Feature, Double]()
    res(PairFeature('Root, mentionType(a))) = 1.0
    res(PairFeature(PairFeature('Root, mentionType(a)),SentenceDistFeature(binnedSentenceDistance(0, a.sentence)))) = 1.0


    res
  }

  def mentionType(a: MentionCandidate) = {
    if (a.words.length == 1 && isPronoun(a.words(0))) 'Pronoun
    else if (a.words(0)(0).isUpper && a.span.head != 0) 'Proper
    else 'Nominal
  }

  val pronouns = Set(
    "He", "She", "They", "It", "You", "I", "We", "Me", "Us", "Him", "Her", "Them",
    "he", "she", "they", "it", "you", "i", "we", "me", "us", "him", "her", "them",
    "His", "Her", "Their", "Its", "Your", "My", "Our",
    "his", "her", "their", "its", "your", "my", "our"
  )

  def isPronoun(word: String) = pronouns.contains(word)

  private def binnedSentenceDistance(a: Int, b: Int) = {
    val dist = b - a
    if(dist == 0) 0
    else if(dist == 4) 1
    else if(dist < 5) 2
    else if(dist < 10) 3
    else 4
  }

  private def semHead(mention: IndexedSeq[String]) = {
    if(mention.length == 1) mention.head.toLowerCase
    else {
      var i = mention.indexWhere(prepositions)
      i = mention.length
      val withoutPrep = mention.take(i)
      withoutPrep.reverseIterator.dropWhile(_ == "'s").next().toLowerCase
    }

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


}
