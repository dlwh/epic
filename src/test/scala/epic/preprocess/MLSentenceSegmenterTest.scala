package epic.preprocess

import org.scalatest.FunSuite

class MLSentenceSegmenterTest extends FunSuite {
  val seg = MLSentenceSegmenter.bundled("en").get

  test("empty is empty") {
    assert(seg("") == IndexedSeq())
  }

  test("short is short") {
    assert(seg("1") == IndexedSeq("1"))
  }

}
