package epic.features

import org.scalatest.FunSuite

/**
 * TODO
 *
 * @author dlwh
 **/
class packageTest extends FunSuite {
  test("build feature index with no overlap") {
    val feats = Map('A -> Seq('fA, 'fB, 'fC, 'fD),
                    'B -> Seq('fA, 'fB, 'fC, 'fD),
                    'C -> Seq('fA, 'fB, 'fD),
                    'D -> Seq('fD)
      )

    val ind = buildNonRedundantFeatureIndex(feats.keys, feats)
    assert(ind('fB) == -1 || ind('fA) == -1, ind)
    assert(ind('fB) != -1 || ind('fA) != -1, ind)
    assert(ind('fC) !== -1)
    assert(ind('fD) !== -1)
  }
}
