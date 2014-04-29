package epic.parser

import org.scalatest.FunSuite
import epic.trees.AnnotatedLabel

/**
 * TODO
 *
 * @author dlwh
 **/
class HammingLossAugmentationTest  extends ParserTestHarness with FunSuite {

  test("training set trees, gold has 0 loss, max tree has loss at least as big.") {
    val hla = new HammingLossAugmentation(ParserTestHarness.simpleGrammar, ParserTestHarness.simpleLexicon, (_:AnnotatedLabel).baseAnnotatedLabel, (_:AnnotatedLabel).isIntermediate).asCoreGrammar(ParserTestHarness.getTrainTrees())
    val parser = ParserTestHarness.simpleParser
    for(ti <- ParserTestHarness.getTrainTrees()) {
      val aug = hla.anchor(ti.words)
      val marg = new TreeMarginal(AugmentedAnchoring.fromCore(aug), ti.tree.map(x => x -> 0))
      assert(marg.logPartition === 0, marg.logPartition + " " + ti)
      val t2 = parser.parse(ti.words)
      val marg2 = new TreeMarginal(AugmentedAnchoring.fromCore(aug), t2.map(x => x -> 0))
      assert(marg.logPartition <= marg2.logPartition, marg.logPartition + " " + ti + " " + t2)
    }
  }
}

