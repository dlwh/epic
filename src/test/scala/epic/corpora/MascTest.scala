package epic.corpora

import org.scalatest._
import org.scalatest.junit._
import org.junit.runner.RunWith
import epic.slab._
import epic.corpora
import epic.trees.Span

@RunWith(classOf[JUnitRunner])
class MascTest extends FunSuite {

  test("MASC to Slab") {
    val url = this.getClass.getResource("/masc/data/written/newspaper/nyt/20000424_nyt-NEW.txt")
    val text = scala.io.Source.fromURL(url)(scala.io.Codec.UTF8).mkString
    val slab = corpora.MascSlab(url)
    assert(slab.content === text)
    assert(slab.select[Source].toList === Vector(Source(url)))

    val sentSlab = corpora.MascSlab.s(slab)
    assert(sentSlab.select[Sentence].map(sentSlab.substring).toList === List(
      "IRAQ-POVERTY (Washington)",
      "Rep. Tony Hall, D-Ohio, urges the United Nations to allow a freer flow\n\t\t\t\tof food and medicine into Iraq.",
      "Hall, who recently returned from a trip\n\t\t\t\tto Iraq, said U.N. economic sanctions have hurt millions of civilians\n\t\t\t\tthere.",
      "By AUSTIN ZALKIN."))

    val segSlab = corpora.MascSlab.seg(sentSlab)
    assert(segSlab.select[Segment].map(segSlab.substring).toList === List(
      "IRAQ", "-", "POVERTY", "(", "Washington", ")",
      "Rep", ".", "Tony", "Hall", ",", "D", "-", "Ohio", ",", "urges", "the", "United", "Nations",
      "to", "allow", "a", "freer", "flow", "of", "food", "and", "medicine", "into", "Iraq", ".",
      "Hall", ",", "who", "recently", "returned", "from", "a", "trip", "to", "Iraq", ",", "said",
      "U.", "N.", "economic", "sanctions", "have", "hurt", "millions", "of", "civilians",
      "there", ".",
      "By", "AUSTIN", "ZALKIN", "."))

    val posSlab = corpora.MascSlab.penn(segSlab)
    assert(posSlab.spanIndex[Tagged[PartOfSpeech]].all.map(posSlab.substring).toList === List(
      "IRAQ-POVERTY", "(", "Washington", ")",
      "Rep.", "Tony", "Hall", ",", "D-Ohio", ",", "urges", "the", "United", "Nations",
      "to", "allow", "a", "freer", "flow", "of", "food", "and", "medicine", "into", "Iraq", ".",
      "Hall", ",", "who", "recently", "returned", "from", "a", "trip", "to", "Iraq", ",", "said",
      "U.N.", "economic", "sanctions", "have", "hurt", "millions", "of", "civilians",
      "there", ".",
      "By", "AUSTIN", "ZALKIN", "."))

    assert(posSlab.spanIndex[Tagged[PartOfSpeech]].all.map(_.tag.tag).toList === List(
      "NNP", "(", "NNP", ")",
      "NN", "NNP", "NNP", ",", "NNP", ",", "VBZ", "DT", "NNP", "NNPS",
      "TO", "VB", "DT", "JJR", "NN", "IN", "NN", "CC", "NN", "IN", "NNP", ".",
      "NNP", ",", "WP", "RB", "VBD", "IN", "DT", "NN", "TO", "NNP", ",", "VBD",
      "NN", "JJ", "NNS", "VBP", "VBN", "NNS", "IN", "NNS", "RB", ".",
      "IN", "NNP", "NNP", "."))

    val neSlab = corpora.MascSlab.namedEntities(posSlab)
    // error in MASC data: "," as "location" instead of "D-Ohio", and "Hall" missed
    assert(neSlab.select[Tagged[EntityMention]].map(neSlab.substring).toList === List(
      "IRAQ-POVERTY", "Washington", "Tony Hall", "," /*"D-Ohio"*/, "United Nations", "Iraq",
      /*"Hall", */"Iraq", "U.N.",
      "AUSTIN ZALKIN"))
    assert(neSlab.select[Tagged[EntityMention]].map(_.tag.entityType).toList === List(
      "location", "location", "person", "location", "org", "location",
      /*"person", */"location", "org",
      "person"))
  }
}
