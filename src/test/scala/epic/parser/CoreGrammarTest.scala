package epic.parser

import org.scalatest.FunSuite
import java.io._

/**
 * TODO
 *
 * @author dlwh
 **/
class CoreGrammarTest extends FunSuite {
  test("Serializaton") {
    val g = ParserTestHarness.simpleParser.constraintsFactory
    breeze.linalg.Counter[Int, Int]()
    val bos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(bos)
    oos.writeObject(g)
    oos.close()
    val oin = breeze.util.nonstupidObjectInputStream(new ByteArrayInputStream(bos.toByteArray))
    val in = oin.readObject()
  }

}

