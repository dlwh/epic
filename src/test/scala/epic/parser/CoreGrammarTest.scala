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
    val g = ParserTestHarness.simpleParser.coreGrammar
    breeze.linalg.Counter[Int, Int]()
    val bos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(bos)
    oos.writeObject(g)
    oos.close()
    val oin = breeze.util.nonstupidObjectInputStream(new ByteArrayInputStream(bos.toByteArray))
    val in = oin.readObject()
  }

  test("???")  {
    val g = new X.Foo
    val bos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(bos)
    oos.writeObject(g)
    oos.close()
    val in = new ObjectInputStream(new ByteArrayInputStream(bos.toByteArray)).readObject()
    assert(in == g)

  }


}

object X {
  @SerialVersionUID(1L)
  case class Foo() extends Serializable {
    @throws(classOf[ObjectStreamException])
    private def writeReplace():Object = {
      new Bar()
    }
  }

  @SerialVersionUID(1L)
  class Bar() extends Serializable {
    @throws(classOf[ObjectStreamException])
    private def readResolve():Object = {
      new Foo()
    }
  }
}