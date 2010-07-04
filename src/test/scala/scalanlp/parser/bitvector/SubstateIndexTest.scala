package scalanlp.parser.bitvector

import org.junit.runner.RunWith;
import org.scalatest._;
import org.scalatest.junit._;
import org.scalatest.prop._;

import scalanlp.util.Index;

@RunWith(classOf[JUnitRunner])
class SubstateIndexTest extends FunSuite with Checkers {
  test("Substate indices are reasonable") {
    val base = Index(Seq("A","B","C","D"));
    def substateForStr(a: String): Int = 3;
    val ind = new SubstateIndex(base, substateForStr _ );
    for(x <- Seq("A","B","C","D"); s <- 0 until 3) {
      val pair = (x,s);
      val i = ind(pair);
      assert(pair === ind.get(i));
    }
  }
}