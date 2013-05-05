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
        if(segId == 0) cur :+ (segId -> Span(cur.last._2.end, cur.last._2.end + 1))
        else cur :+ (segId -> Span(cur.last._2.end, cur.last._2.end + len))
      }
      Segmentation(segments.drop(1), 0 until segments.last._2.end)
    })


    check(Prop.forAll { (seg: Segmentation[Int, Int]) =>
      (seg.words.length == 0 ) || {

        val toBIO = seg.asBIOSequence(0)
        val fromBIO = Segmentation.fromBIOSequence(toBIO, 0)
        if(fromBIO.segments != seg.segments) {
          println(seg)
          println(fromBIO)
          println(toBIO)
          false
        } else {
          true
        }
      }
    })
  }
}
