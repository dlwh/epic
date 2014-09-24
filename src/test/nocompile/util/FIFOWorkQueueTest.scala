package epic.util

import org.scalatest.FunSuite

/**
 * TODO
 *
 * @author dlwh
 **/
class FIFOWorkQueueTest extends FunSuite {


  test("Simple test") {
    implicit val executionContext = scala.concurrent.ExecutionContext.global
    val queue = FIFOWorkQueue(0 until 10)((_: Int) * 10)

    assert(queue.toIndexedSeq == IndexedSeq.tabulate(10)(_ * 10))
  }

}
