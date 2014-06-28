package epic.corpora

import org.scalatest._
import org.scalatest.junit._
import org.junit.runner.RunWith
import epic.slab.Source
import epic.slab.Sentence
import epic.slab.Segment
import epic.slab.PartOfSpeech
import epic.slab.EntityMention
import epic.corpora
import epic.trees.Span

@RunWith(classOf[JUnitRunner])
class MascTest extends FunSuite {

  test("MASC to Slab") {
    val url = this.getClass.getResource("/masc/data/written/newspaper/nyt/20000424_nyt-NEW.txt")
    val text = io.Source.fromURL(url)(io.Codec.UTF8).mkString
    val slab = corpora.MascSlab(url)
    assert(slab.content === text)
    assert(slab.iterator[Source].toList === List(Span(0, text.length) -> Source(url)))

    val sentSlab = corpora.MascSlab.s(slab)
    assert(sentSlab.iterator[Sentence].map(pair => sentSlab.spanned(pair._1)).toList === List(
      "IRAQ-POVERTY (Washington)",
      "Rep. Tony Hall, D-Ohio, urges the United Nations to allow a freer flow\n\t\t\t\tof food and medicine into Iraq.",
      "Hall, who recently returned from a trip\n\t\t\t\tto Iraq, said U.N. economic sanctions have hurt millions of civilians\n\t\t\t\tthere.",
      "By AUSTIN ZALKIN."))

    val segSlab = corpora.MascSlab.seg(sentSlab)
    assert(segSlab.iterator[Segment].map(pair => segSlab.spanned(pair._1)).toList === List(
      "IRAQ", "-", "POVERTY", "(", "Washington", ")",
      "Rep", ".", "Tony", "Hall", ",", "D", "-", "Ohio", ",", "urges", "the", "United", "Nations",
      "to", "allow", "a", "freer", "flow", "of", "food", "and", "medicine", "into", "Iraq", ".",
      "Hall", ",", "who", "recently", "returned", "from", "a", "trip", "to", "Iraq", ",", "said",
      "U.", "N.", "economic", "sanctions", "have", "hurt", "millions", "of", "civilians",
      "there", ".",
      "By", "AUSTIN", "ZALKIN", "."))

    val posSlab = corpora.MascSlab.penn(segSlab)
    assert(posSlab.iterator[PartOfSpeech].map(pair => posSlab.spanned(pair._1)).toList === List(
      "IRAQ-POVERTY", "(", "Washington", ")",
      "Rep.", "Tony", "Hall", ",", "D-Ohio", ",", "urges", "the", "United", "Nations",
      "to", "allow", "a", "freer", "flow", "of", "food", "and", "medicine", "into", "Iraq", ".",
      "Hall", ",", "who", "recently", "returned", "from", "a", "trip", "to", "Iraq", ",", "said",
      "U.N.", "economic", "sanctions", "have", "hurt", "millions", "of", "civilians",
      "there", ".",
      "By", "AUSTIN", "ZALKIN", "."))

    assert(posSlab.iterator[PartOfSpeech].map(_._2.tag).toList === List(
      "NNP", "(", "NNP", ")",
      "NN", "NNP", "NNP", ",", "NNP", ",", "VBZ", "DT", "NNP", "NNPS",
      "TO", "VB", "DT", "JJR", "NN", "IN", "NN", "CC", "NN", "IN", "NNP", ".",
      "NNP", ",", "WP", "RB", "VBD", "IN", "DT", "NN", "TO", "NNP", ",", "VBD",
      "NN", "JJ", "NNS", "VBP", "VBN", "NNS", "IN", "NNS", "RB", ".",
      "IN", "NNP", "NNP", "."))

    val neSlab = corpora.MascSlab.namedEntities(posSlab)
    // error in MASC data: "," as "location" instead of "D-Ohio", and "Hall" missed
    assert(neSlab.iterator[EntityMention].map(pair => neSlab.spanned(pair._1)).toList === List(
      "IRAQ-POVERTY", "Washington", "Tony Hall", "," /*"D-Ohio"*/, "United Nations", "Iraq",
      /*"Hall", */"Iraq", "U.N.",
      "AUSTIN ZALKIN"))
    assert(neSlab.iterator[EntityMention].map(_._2.entityType).toList === List(
      "location", "location", "person", "location", "org", "location",
      /*"person", */"location", "org",
      "person"))
  }
}
