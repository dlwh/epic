package scalanlp.parser;

import scalanlp.trees._;


/**
* Hack approximation to true parse eval. Gives Labeled Precision
* and Labeled Recall.
*
* @author dlwh
*/
class ParseEval[L](ignoredLabels: Set[L]) {
  /**
  * Computes precision, recall, and exact match for the each
  * guess/gold pair of trees.
  */
  def apply(guessgold: Iterator[(Tree[L],Tree[L])]) = {
    var totalGuess = 0;
    var totalGold = 0;
    var totalRight = 0;
    var exact = 0;
    var numParses = 0;
    for( (guess,gold) <- guessgold) {
      val guessSet = labeledConstituents(guess);
      val goldSet = labeledConstituents(gold);
      val inter = (guessSet intersect goldSet)
      numParses += 1;
      if(goldSet.size == inter.size && guessSet.size == inter.size) {
        exact += 1;
      }
      totalGuess += guessSet.size;
      totalGold += goldSet.size;
      totalRight += inter.size;
    }

    (totalRight * 1.0 /totalGuess, totalRight * 1.0 /totalGold, exact*1.0/numParses)
    
  }

  private def labeledConstituents(tree: Tree[L]) = Set() ++ {
    for(child <- tree.preorder;
        if !ignoredLabels.contains(child.label))
        yield (child.label,child.span);
  }
}
