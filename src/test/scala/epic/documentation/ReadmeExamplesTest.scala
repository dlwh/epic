package epic.readme

import org.scalatest._
import org.scalatest.junit._
import epic.slab._
import scalaz.std.list._
import epic.util.slabutils._

// This file contains all the examples from the readme, making sure
// they work.

class ReadmeExamplesTest extends FunSpec {
  val text = List("This is an example sentence. And this one is another.")
  val output = List(List(List("This", "is", "an", "example", "sentence", "."), List("And", "this", "one", "is", "another", ".")))
  it("should run the tokenizer example") {
    val sentenceSplitter = epic.preprocess.MLSentenceSegmenter.bundled().get
    val tokenizer = epic.preprocess.TreebankTokenizer
    val slabs = text.map(sentenceSplitter.slabFrom(_)).map(tokenizer(_))
    val tokens = slabs.map(_.tokens)
    assert(tokens == output)
  }
  it("should run the tokenizer example") {
    val parser = epic.models.ParserSelector.loadParser("en").get
  }
}
