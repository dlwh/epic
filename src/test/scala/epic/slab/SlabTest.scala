package epic.slab

import org.scalatest._
import org.scalatest.junit._
import org.junit.runner.RunWith
import epic.trees.Span
import epic.preprocess.{JavaWordTokenizer, JavaSentenceSegmenter}
import epic.slab._

@RunWith(classOf[JUnitRunner])
class SlabTest extends FunSuite {



// =========
// Tests
// =========
test("text Slab") {
  // should not compile because tokenizer must be after sentence segmenter
  //val pipeline = begin andThen tokenizer andThen sentenceSegmenter

  // should cause following code (e.g. slab.iterator[Token]) to not compile
  //val pipeline = begin andThen sentenceSegmenter

  val pipeline = new JavaSentenceSegmenter().andThen(new JavaWordTokenizer())
  val slab = pipeline(Slab("""
      The skunk thought the stump
      stunk. The stump thought the
      skunk stunk.
      """))

  val sentences = slab.iterator[Sentence].toList
  assert(sentences.map(pair => slab.spanned(pair._1).trim) === List(
    """The skunk thought the stump
      stunk.""",
      """The stump thought the
        skunk stunk."""))

    val tokens = slab.iterator[Token].toList
    assert(tokens.map(pair => slab.spanned(pair._1)) === List(
      "The", "skunk", "thought", "the", "stump", "stunk", ".",
      "The", "stump", "thought", "the", "skunk", "stunk", "."))

    val sentenceTokens = sentences.map(pair => slab.covered[Token](pair._1).toList)
    assert(sentenceTokens.map(_.map(pair => slab.spanned(pair._1))) === List(
      List("The", "skunk", "thought", "the", "stump", "stunk", "."),
      List("The", "stump", "thought", "the", "skunk", "stunk", ".")))

    val tokensBeforeSentence1 = slab.preceding[Token](sentences(1)._1).toList
    assert(tokensBeforeSentence1.map(_._2.token) === List(
      "The", "skunk", "thought", "the", "stump", "stunk", ".").reverse)

    val sentencesAfterToken5 = slab.following[Sentence](tokens(5)._1)
    assert(sentencesAfterToken5.map(pair => slab.spanned(pair._1).trim).toList === List(
      """The stump thought the
        skunk stunk."""))
  }

  /*

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
  }
  implicit object ImageAnnotationHasBounds extends Slab.HasBounds[ImageAnnotation] {
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
  val imageBegin = (slab: Slab[Image, ImageAnnotation, ImageAnnotation]) => slab

  def faceRecognizer[AnnotationTypes <: ImageAnnotation](slab: Slab[Image, ImageAnnotation, AnnotationTypes]) =
    slab ++ Iterator(Face(Set((0, 0), (0, 1))))

  // =========
  // Tests
  // =========
  test("image Slab") {
    val pipeline = imageBegin andThen faceRecognizer

    val image = new Image(Map.empty ++ (for (x <- 0 until 5; y <- 0 until 5) yield ((x, y), 255)))
    val expectedFaceImage = new Image(Map((0, 0) -> 255, (0, 1) -> 255))
    val slab = pipeline(Slab(image))

    val expectedFaceAnnotation = Face(Set((0, 0), (0, 1)))
    val actualFaces = slab.iterator[Face].toList
    assert(actualFaces === List(expectedFaceAnnotation))
    assert(actualFaces.map(_.in(slab).content) === List(expectedFaceImage))
    assert(actualFaces(0).in(slab).covered[Face].toList === List(expectedFaceAnnotation))
  }
  */

  test("sorted slab issues with overlapping annotations") {
    case class Annotation()
    val slab = Slab("""This is a test.""") ++ Iterator(Span(0, 1) -> Annotation(), Span(0, 2) -> Annotation(), Span(1, 3) -> Annotation(), Span(1, 2) -> Annotation(), Span(2, 3) -> Annotation())
    assert(slab.preceding[Annotation](Span(0, 1)).isEmpty)
    assert(slab.preceding[Annotation](Span(0, 2)).isEmpty)
    assert(slab.preceding[Annotation](Span(1, 3)).toList.map(_._1) === List(Span(0, 2), Span(0, 1)))
    assert(slab.covered[Annotation](Span(1, 3)).toList.map(_._1) === List(Span(1, 2), Span(1, 3), Span(2, 3)))
    assert(slab.covered[Annotation](Span(0, 2)).toList.map(_._1) === List(Span(0, 1), Span(0, 2), Span(1, 2)))
  }
}

import org.scalatest.FunSpec

class HListUtilsTest extends FunSpec {
  import shapeless._
  implicit def hlistOps[L <: HList](l : L) : Utils.HListOps[L] = new Utils.HListOps(l)

  describe("covariant filter") {
    it("should not filter subtypes") {
      class Foo(val foo: Int)
      class Bar(val bar: Int) extends Foo(bar)
      val l = new Foo(1) :: new Bar(2) :: new Foo(3) :: new Bar(4) :: HNil
      assert(l.covariantFilter[Foo] == l)
    }
    it("should filter supertypes") {
      class Foo(val foo: Int)
      class Bar(val bar: Int) extends Foo(bar)
      val l = new Foo(1) :: new Bar(2) :: new Foo(3) :: new Bar(4) :: HNil
      assert(l.covariantFilter[Bar] != l)
    }
  }
}

class ShapelessSlabTest extends FunSpec {
  import shapeless.test.illTyped
  import shapeless._
  import LUBConstraint._

  describe("basic slab") {
    val slab = InefficientShapelessSlab("Foobar")
    val annotation = new Annotation {}
    it("should accept an annotation") {
      val slab2 = annotation +: slab
      illTyped("""
        "foo" +: slab
      """)
    }
    it("should add the annotation type") {
      case class Foo(foo: Int) extends Annotation
      val foo = Foo(42)
      def testForFoo[T](slab: InefficientShapelessSlab[_, _, T with Foo, _]) = true
      testForFoo(foo +: slab)
      illTyped("""
        testForFoo(slab)
      """)
    }
  }
}
