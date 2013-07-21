package chalk.slab

import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class SLABTest extends FunSuite {

  // =========================
  // Annotation infrastructure
  // =========================
  trait StringAnnotation {
    val begin: Int
    val end: Int
    def in[AnnotationTypes <: StringAnnotation](slab: SLAB[String, StringAnnotation, AnnotationTypes]) =
      new SLABAnnotationOps(this, slab) {
        def content = this.slab.content.substring(this.annotation.begin, this.annotation.end)
      }
  }
  implicit object StringAnnotationHasBounds extends SLAB.HasBounds[StringAnnotation] {
    def covers(annotation1: StringAnnotation, annotation2: StringAnnotation): Boolean =
      annotation1.begin <= annotation2.begin && annotation2.end <= annotation1.end
    def follows(annotation1: StringAnnotation, annotation2: StringAnnotation): Boolean =
      annotation2.end <= annotation1.begin
    def precedes(annotation1: StringAnnotation, annotation2: StringAnnotation): Boolean =
      annotation1.end <= annotation2.begin
  }

  // ===========
  // Annotations
  // ===========
  case class Sentence(val begin: Int, val end: Int) extends StringAnnotation
  case class Token(val begin: Int, val end: Int) extends StringAnnotation

  // =========
  // Analyzers
  // =========
  val stringBegin = (slab: SLAB[String, StringAnnotation, StringAnnotation]) => slab

  def sentenceSegmenter[AnnotationTypes <: StringAnnotation](slab: SLAB[String, StringAnnotation, AnnotationTypes]) =
    slab ++ "[^\\s.!?]+[^.!?]+[.!?]".r.findAllMatchIn(slab.content).map(m => Sentence(m.start, m.end))

  def tokenizer[AnnotationTypes <: Sentence](slab: SLAB[String, StringAnnotation, AnnotationTypes]) =
    slab ++ slab.iterator[Sentence].flatMap(sentence =>
      "\\p{L}+|\\p{P}+|\\p{N}+".r.findAllMatchIn(sentence.in(slab).content).map(m =>
        Token(sentence.begin + m.start, sentence.begin + m.end)))

  // =========
  // Tests
  // =========
  test("text SLAB") {
    // should not compile because tokenizer must be after sentence segmenter
    //val pipeline = begin andThen tokenizer andThen sentenceSegmenter

    // should cause following code (e.g. slab.iterator[Token]) to not compile
    //val pipeline = begin andThen sentenceSegmenter

    val pipeline = stringBegin andThen sentenceSegmenter andThen tokenizer
    val slab = pipeline(SLAB("""
        The skunk thought the stump
        stunk. The stump thought the
        skunk stunk.
        """))

    val sentences = slab.iterator[Sentence].toList
    assert(sentences.map(_.in(slab).content) === List(
      """The skunk thought the stump
        stunk.""",
      """The stump thought the
        skunk stunk."""))

    val tokens = slab.iterator[Token].toList
    assert(tokens.map(_.in(slab).content) === List(
      "The", "skunk", "thought", "the", "stump", "stunk", ".",
      "The", "stump", "thought", "the", "skunk", "stunk", "."))

    val sentenceTokens = sentences.map(_.in(slab).covered[Token].toList)
    assert(sentenceTokens.map(_.map(_.in(slab).content)) === List(
      List("The", "skunk", "thought", "the", "stump", "stunk", "."),
      List("The", "stump", "thought", "the", "skunk", "stunk", ".")))

    val tokensBeforeSentence1 = sentences(1).in(slab).preceding[Token].toList
    assert(tokensBeforeSentence1.map(_.in(slab).content) === List(
      "The", "skunk", "thought", "the", "stump", "stunk", ".").reverse)

    val sentencesAfterToken5 = tokens(5).in(slab).following[Sentence].toList
    assert(sentencesAfterToken5.map(_.in(slab).content) === List(
      """The stump thought the
        skunk stunk."""))
  }

  // =====
  // Image
  // =====
  case class Image(val pixelValues: Map[(Int, Int), Int]) {
    def subimage(pixels: Set[(Int, Int)]): Image = {
      new Image(this.pixelValues.filter(p => pixels.contains(p._1)))
    }
  }

  // =========================
  // Annotation infrastructure
  // =========================
  trait ImageAnnotation {
    val pixels: Set[(Int, Int)]
    def in[AnnotationTypes <: ImageAnnotation](slab: SLAB[Image, ImageAnnotation, AnnotationTypes]) =
      new SLABAnnotationOps(this, slab) {
        def content = this.slab.content.subimage(this.annotation.pixels)
      }
  }
  implicit object ImageAnnotationHasBounds extends SLAB.HasBounds[ImageAnnotation] {
    def covers(annotation1: ImageAnnotation, annotation2: ImageAnnotation): Boolean =
      annotation2.pixels.subsetOf(annotation1.pixels)
    def follows(annotation1: ImageAnnotation, annotation2: ImageAnnotation): Boolean = ???
    def precedes(annotation1: ImageAnnotation, annotation2: ImageAnnotation): Boolean = ???
  }

  // ===========
  // Annotations
  // ===========
  case class Face(val pixels: Set[(Int, Int)]) extends ImageAnnotation

  // =========
  // Analyzers
  // =========
  val imageBegin = (slab: SLAB[Image, ImageAnnotation, ImageAnnotation]) => slab

  def faceRecognizer[AnnotationTypes <: ImageAnnotation](slab: SLAB[Image, ImageAnnotation, AnnotationTypes]) =
    slab ++ Iterator(Face(Set((0, 0), (0, 1))))

  // =========
  // Tests
  // =========
  test("image SLAB") {
    val pipeline = imageBegin andThen faceRecognizer

    val image = new Image(Map.empty ++ (for (x <- 0 until 5; y <- 0 until 5) yield ((x, y), 255)))
    val expectedFaceImage = new Image(Map((0, 0) -> 255, (0, 1) -> 255))
    val slab = pipeline(SLAB(image))

    val expectedFaceAnnotation = Face(Set((0, 0), (0, 1)))
    val actualFaces = slab.iterator[Face].toList
    assert(actualFaces === List(expectedFaceAnnotation))
    assert(actualFaces.map(_.in(slab).content) === List(expectedFaceImage))
    assert(actualFaces(0).in(slab).covered[Face].toList === List(expectedFaceAnnotation))
  }
}
