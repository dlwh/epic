package epic.preprocess

import org.scalatest._
import org.scalatest.junit._
import epic.slab._
import scalaz.std.list._

import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class JavaSentenceSegmenterTest extends FunSuite {
  val text = """
     But, in a larger sense, we can not dedicate -- we can not consecrate -- we can not hallow -- this ground. The brave men, living and dead, who struggled here, have consecrated it, far above our poor power to add or detract. The world will little note, nor long remember what we say here, but it can never forget what they did here. It is for us the living, rather, to be dedicated here to the unfinished work which they who fought here have thus far so nobly advanced. It is rather for us to be here dedicated to the great task remaining before us -- that from these honored dead we take increased devotion to that cause for which they gave the last full measure of devotion -- that we here highly resolve that these dead shall not have died in vain -- that this nation, under God, shall have a new birth of freedom -- and that government of the people, by the people, for the people, shall not perish from the earth.
             """

  test("Gettysburg address") {
    val slab = JavaSentenceSegmenter.slabFrom(text)
    assert(slab.select[Sentence].length === 5)
    assert(slab.substring(slab.select[Sentence](0)).trim === "But, in a larger sense, we can not dedicate -- we can not consecrate -- we can not hallow -- this ground.")
  }
}
