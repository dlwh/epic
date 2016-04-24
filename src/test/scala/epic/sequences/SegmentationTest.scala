package epic.sequences

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.scalacheck.{Prop, Arbitrary}
import Arbitrary._
import epic.trees.Span

/**
 *
 * @author dlwh
 */
class SegmentationTest extends FunSuite with Checkers {
  test("to/from BIOSequence doesn't change sequence") {
    implicit val arbSeg = Arbitrary(for {
      segs <- arbitrary[Array[(Int, Int)]].map{_.map(pair => pair._1.abs % 10 -> (pair._2.abs % 10 + 1).abs)}
    } yield {
      val segments = segs.foldLeft((Vector((0,Span(0,0))))) { (cur, sl) =>
        val (segId, len) = sl
        if (segId == 0) cur :+ (segId -> Span(cur.last._2.end, cur.last._2.end + 1))
        else cur :+ (segId -> Span(cur.last._2.end, cur.last._2.end + len))
      }
      Segmentation(segments.drop(1), 0 until segments.last._2.end)
    })


    check(Prop.forAll { (seg: Segmentation[Int, Int]) =>
      (seg.words.length == 0 ) || {
        val toBIO = seg.asBIOSequence(0)
        val fromBIO = Segmentation.fromBIOSequence(toBIO, 0)
        fromBIO.segments == seg.segments
      }
    })
  }

  test("toTree doesn't crash") {
    val segments = Vector( (0, Span(2, 4)))
    val words = IndexedSeq.range(0, 8)
    Segmentation(segments, words).toTree(-1)
  }

  test("strip punctuation works") {
    val segments = Vector( 0 -> Span(1, 4), 1 -> Span(4, 5), 2 -> Span(6, 7))
    val words = Vector(
      Vector('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i'),
      Vector('a', 'b', 'c', 'd', 'e', 'f', 'g')
    )

    val segmentations = words.map(Segmentation(segments, _))

    assert(segmentations(0).filterWords(_ != 'b').segments == Vector( 0 -> Span(1, 3), 1 -> Span(3, 4), 2 -> Span(5, 6)))
    assert(segmentations(0).filterWords(_ != 'a').segments == Vector( 0 -> Span(0, 3), 1 -> Span(3, 4), 2 -> Span(5, 6)))
    println(segmentations(0).filterWords(_ < 'g').render)
    assert(segmentations(0).filterWords(_ < 'g').segments == Vector( 0 -> Span(1, 4), 1 -> Span(4, 5)))

    assert(segmentations(1).filterWords(_ < 'g').segments == Vector( 0 -> Span(1, 4), 1 -> Span(4, 5)))


  }
}
