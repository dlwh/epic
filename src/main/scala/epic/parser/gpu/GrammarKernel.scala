package epic.parser.gpu

import epic.parser.SimpleRefinedGrammar
import collection.immutable.IndexedSeq

/**
 * Created with IntelliJ IDEA.
 * User: dlwh
 * Date: 11/21/12
 * Time: 4:55 PM
 * To change this template use File | Settings | File Templates.
 */
class GrammarKernel {

}

object GrammarKernel {
  def fromSimpleGrammar[L, L2, W](grammar: SimpleRefinedGrammar[L, L2, W]) = {
    import grammar.refinedGrammar._
    val (binaryRules, unaryRules) = (0 until index.size).partition(isBinary(_))
    val sortedBinary: IndexedSeq[Int] = binaryRules.sortBy{r1 => (leftChild(r1), rightChild(r1), parent(r1))}(Ordering.Tuple3)
    val sortedUnary = unaryRules.sortBy(r => child(r) -> parent(r))(Ordering.Tuple2)


  }

  val insideTemplate =
    """
      | __kernel void inside_inner(__global float * charts, __global const int spanLength, __global const int* nwords) {
      |   const int begin = get_global_id(0);
      |   const float * out = CHART(charts, begin, begin + spanLength)
      |   const int myLength =
      |   if (begin > )
      |
      |
      |
      |
      |}
    """.stripMargin
}
