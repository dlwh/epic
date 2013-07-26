package chalk.corpora

import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import chalk.slab.Source
import chalk.slab.Sentence
import chalk.slab.Segment
import chalk.slab.PartOfSpeech

@RunWith(classOf[JUnitRunner])
class MascTest extends FunSuite {

  test("MASC to Slab") {
    val url = this.getClass.getResource("/masc/data/written/newspaper/nyt/20000424_nyt-NEW.txt")
    val text = io.Source.fromURL(url)(io.Codec.UTF8).mkString
    val slab = MascSlab(url)
    assert(slab.content === text)
    assert(slab.iterator[Source].toList === List(Source(0, text.length, url)))

    val sentSlab = MascSlab.s(slab)
    assert(sentSlab.iterator[Sentence].map(_.in(sentSlab).content).toList === List(
      "IRAQ-POVERTY (Washington)",
      "Rep. Tony Hall, D-Ohio, urges the United Nations to allow a freer flow\n\t\t\t\tof food and medicine into Iraq.",
      "Hall, who recently returned from a trip\n\t\t\t\tto Iraq, said U.N. economic sanctions have hurt millions of civilians\n\t\t\t\tthere.",
      "By AUSTIN ZALKIN."))

    val segSlab = MascSlab.seg(sentSlab)
    assert(segSlab.iterator[Segment].map(_.in(segSlab).content).toList === List(
      "IRAQ", "-", "POVERTY", "(", "Washington", ")",
      "Rep", ".", "Tony", "Hall", ",", "D", "-", "Ohio", ",", "urges", "the", "United", "Nations",
      "to", "allow", "a", "freer", "flow", "of", "food", "and", "medicine", "into", "Iraq", ".",
      "Hall", ",", "who", "recently", "returned", "from", "a", "trip", "to", "Iraq", ",", "said",
      "U.", "N.", "economic", "sanctions", "have", "hurt", "millions", "of", "civilians",
      "there", ".",
      "By", "AUSTIN", "ZALKIN", "."))

    val posSlab = MascSlab.penn(segSlab)
    assert(posSlab.iterator[PartOfSpeech].map(_.in(posSlab).content).toList === List(
      "IRAQ-POVERTY", "(", "Washington", ")",
      "Rep.", "Tony", "Hall", ",", "D-Ohio", ",", "urges", "the", "United", "Nations",
      "to", "allow", "a", "freer", "flow", "of", "food", "and", "medicine", "into", "Iraq", ".",
      "Hall", ",", "who", "recently", "returned", "from", "a", "trip", "to", "Iraq", ",", "said",
      "U.N.", "economic", "sanctions", "have", "hurt", "millions", "of", "civilians",
      "there", ".",
      "By", "AUSTIN", "ZALKIN", "."))

    assert(posSlab.iterator[PartOfSpeech].map(_.tag).toList === List(
      "NNP", "(", "NNP", ")",
      "NN", "NNP", "NNP", ",", "NNP", ",", "VBZ", "DT", "NNP", "NNPS",
      "TO", "VB", "DT", "JJR", "NN", "IN", "NN", "CC", "NN", "IN", "NNP", ".",
      "NNP", ",", "WP", "RB", "VBD", "IN", "DT", "NN", "TO", "NNP", ",", "VBD",
      "NN", "JJ", "NNS", "VBP", "VBN", "NNS", "IN", "NNS", "RB", ".",
      "IN", "NNP", "NNP", "."))
  }
}
