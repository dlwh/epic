package epic.readme

import org.scalatest._
import epic.slab._
import scalaz.std.list._
import epic.util.slabutils._
import epic.trees.AnnotatedLabel

// This file contains all the examples from the readme, making sure
// they work.

class ReadmeExamplesTest extends FunSpec {
  val documents = List("This is an example sentence. And this one is another.")
  val output = List(List(List("This", "is", "an", "example", "sentence", "."), List("And", "this", "one", "is", "another", ".")))
  it("should run the tokenizer example") {
    val sentenceSegmenter = epic.preprocess.MLSentenceSegmenter.bundled().get
    val tokenizer = epic.preprocess.TreebankTokenizer
    val slabs = documents.map(Slab(_)).map(sentenceSegmenter(_)).map(tokenizer(_))
    val tokens = slabs.map(_.tokens)
    assert(tokens == output)
  }
  it("should run the parser example") {
    val sentenceSegmenter = epic.preprocess.MLSentenceSegmenter.bundled().get
    val tokenizer = epic.preprocess.TreebankTokenizer
    val parser = epic.models.ParserSelector.loadParser("en").get
    val slabs = documents.map(Slab(_))
      .map(sentenceSegmenter(_))
      .map(tokenizer(_))
      .map(parser(_))
    val slab = slabs.head
    assert(slab.select[Tree[AnnotatedLabel]].map(_.leaves.map(_.substring(slab.content))) == output.head)
  }
  it("should run the PoS tagger example") {
    val sentenceSegmenter = epic.preprocess.MLSentenceSegmenter.bundled().get
    val tokenizer = epic.preprocess.TreebankTokenizer
    val tagger = epic.models.PosTagSelector.loadTagger("en").get
    val slabs = documents.map(Slab(_))
      .map(sentenceSegmenter(_))
      .map(tokenizer(_))
      .map(tagger(_))
    val slab = slabs.head
    assert(slab.perSentence[Tagged[AnnotatedLabel]].map(_.map(_.substring(slab.content))) == output.head)
  }
}
