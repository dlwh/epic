package scalanlp.parser;
/*
 Copyright 2010 David Hall

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/


import java.io.File
import java.io.FileInputStream
import java.util.Properties
import scalanlp.config.Configuration
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

object ParseEval {
  def evaluate(trees: Iterator[(Tree[String],Seq[String])],
           parser: Parser[String,String],
           xform: Tree[String]=>Tree[String]=Trees.Transforms.StandardStringTransform) = {
    val peval = new ParseEval(Set(""));
    val goldGuess = {
      for {
        (goldTree, words) <- trees;
        () = println(words);
        () = println(xform(goldTree) render words);
        startTime = System.currentTimeMillis;
        guessTree = Trees.debinarize(parser(words))
      } yield {
        val endTime = System.currentTimeMillis;
        println(guessTree render words);
        val ret = (xform(goldTree),guessTree)
        println("Local Accuracy:" + peval(Iterator.single(ret)));
        println( (endTime - startTime) / 1000.0 + " Seconds");
        println("===========");
        ret;
      }
    }
    val (prec,recall,exact) = peval(goldGuess);
    (prec,recall,exact);
  }
}

