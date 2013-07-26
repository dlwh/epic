package chalk.corpora

import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class MascTest extends FunSuite {

  test("MASC to Slab") {
    val url = this.getClass.getResource("/masc/data/written/newspaper/nyt/20000424_nyt-NEW.txt")
    val slab = MascSlab(url)
    assert(slab.content === io.Source.fromURL(url)(io.Codec.UTF8).mkString)
  }
}
