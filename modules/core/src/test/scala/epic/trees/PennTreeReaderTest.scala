package epic.trees

import org.scalatest.FunSuite

/**
 * TODO
 *
 * @author dlwh
 **/
class PennTreeReaderTest extends FunSuite {

  test("Undoes -LRB- etc") {
    val (tree,words) = Tree.fromString("""
    ( (S
        (NP-SBJ (DT The) (JJ top) (NN money) (-LRB- -LRB-) )
        (VP (VBP are)
          (ADVP-TMP (RB currently) )
          (VP (VBG yielding)
            (NP
              (QP (RB well) (IN over) (CD 9) )
              (NN %) )))
        (. .) ('' '')))
                                       """)


    assert(words(3) == "(")
    assert(words.last == "''")

    val (tree2, words2) =  Tree.fromString(tree.render(words))

    assert(tree2 === tree)
    assert(words2 === words)
  }

}
