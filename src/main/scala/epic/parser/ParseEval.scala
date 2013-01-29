package epic.parser
/*
 Copyright 2012 David Hall

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/


import epic.trees._
import java.io.BufferedOutputStream
import java.io.File
import java.io.FileOutputStream
import java.io.PrintStream
import java.util.concurrent.atomic.AtomicInteger
import epic.framework.EvaluationResult


/**
* Hack approximation to true parse eval. Gives Labeled Precision
* and Labeled Recall.
*
* @author dlwh
*/
class ParseEval[L](ignoredLabels: Set[L]) {
  import ParseEval.Statistics
  /**
  * Computes precision, recall, and exact match for the each
  * guess/gold pair of trees.
  */
  def apply(guessgold: Iterator[(Tree[L],Tree[L])]):Statistics = {
    val allStats = for( (guess,gold) <- guessgold) yield { apply(guess,gold) }

    val stats = allStats.reduceLeft(_ + _)

    stats
  }

  def apply(guess: Tree[L], gold: Tree[L]): Statistics = {
    val guessSet = labeledConstituents(guess)
    val goldSet = labeledConstituents(gold)
    val inter = (guessSet intersect goldSet)
    val exact = if(goldSet.size == inter.size && guessSet.size == inter.size) 1 else 0
    val guessLeaves = guess.leaves
    val goldLeaves = gold.leaves
    val numRight = goldLeaves.zip(guessLeaves).foldLeft(0) { (acc,gg) => if(gg._1.label == gg._2.label) acc + 1 else acc}
    Statistics(guessSet.size, goldSet.size, inter.size, exact, numRight, guess.span.end, 1)
  }


  private def labeledConstituents(tree: Tree[L]) = Set() ++ {
    for(child <- tree.preorder
        if !ignoredLabels.contains(child.label) && !child.isLeaf)
        yield (child.label,child.span)
  }
}

object ParseEval {

  case class Statistics(guess: Int, gold: Int, right: Int, numExact: Int,
                        tagsRight: Int, numWords: Int,
                        numParses: Int) extends EvaluationResult[Statistics] {
    def +(stats: Statistics) = {
      Statistics(guess + stats.guess,
        gold + stats.gold,
        right + stats.right,
        numExact + stats.numExact,
        tagsRight + stats.tagsRight,
        numWords + stats.numWords,
        numParses + stats.numParses)
    }

    def precision = if(guess == 0) 1.0 else (right * 1.0 / guess)
    def recall = if(guess == 0) 1.0 else (right * 1.0 / gold)
    def exact = (numExact * 1.0 / numParses)
    def tagAccuracy = tagsRight * 1.0 / numWords
    def f1 = (2 * precision * recall)/(precision + recall)

    override def toString() = {
      s"Statistics(precision=$precision, recall=$recall, f1=$f1, exact=$exact, tagAccuracy=$tagAccuracy)"
    }
  }

  case class ParseResult[L](sent: TreeInstance[L, String], gold: Tree[String], guess: Tree[String], time: Double) {
    val stats =  new ParseEval(Set("","''", "``", ".", ":", ",", "TOP")) apply (guess, gold)
  }

  def parseAll[L](trees: IndexedSeq[TreeInstance[L,String]],
                  parser: Parser[L,String],
                  chainReplacer: UnaryChainReplacer[L],
                  asString: L=>String,
                  logProgress: Boolean = true): Seq[ParseResult[L]] = {
    val acc = new AtomicInteger(0)
    trees.par.flatMap { sent =>
      try {
        val TreeInstance(id,goldTree,words) = sent
        val startTime = System.currentTimeMillis
        val tree: Tree[String] = chainReplacer.replaceUnaries(parser.bestParse(words)).map(asString)
        val guessTree = Trees.debinarize(Trees.deannotate(tree))
        val deBgold = Trees.debinarize(Trees.deannotate(goldTree.map(asString)))
        val endTime = System.currentTimeMillis
        if(logProgress) {
          val i = acc.incrementAndGet()
          if(i % 1000 == 0) {
            println("Parsed %d/%d sentences.".format(i, trees.length))
          }
        }
        Some(ParseResult(sent, deBgold, guessTree, (endTime-startTime) / 1000.0))
      } catch {
        case e: Exception =>
          new RuntimeException("Error while parsing " + sent.words, e).printStackTrace()
          None

      }
    }.seq
  }

  type PostParseFn = (Tree[String],Tree[String],Seq[String],Statistics,Int)=>Unit
  val noPostParseFn = (_:Tree[String],_:Tree[String],_:Seq[String],_:Statistics,_:Int)=>()

  def evaluate[L](trees: IndexedSeq[TreeInstance[L,String]],
                  parser: Parser[L,String],
                  chainReplacer: UnaryChainReplacer[L],
                  asString: L=>String, logProgress: Boolean = true): Statistics = {
    val results = parseAll(trees, parser, chainReplacer, asString, logProgress)
    results.map(_.stats).reduceLeft(_ + _)
  }

  def evaluateAndLog[L](trees: IndexedSeq[TreeInstance[L,String]],
                     parser: Parser[L,String],
                     evalDir: String,
                     chainReplacer: UnaryChainReplacer[L],
                     asString: L=>String = (_:L).toString) = {

    val parsedir = new File(evalDir)
    parsedir.exists() || parsedir.mkdirs() || sys.error("Couldn't make directory: " + parsedir)
    val goldOut = new PrintStream(new BufferedOutputStream(new FileOutputStream(new File(parsedir,"gold"))))
    val guessOut = new PrintStream(new BufferedOutputStream(new FileOutputStream(new File(parsedir,"guess"))))
    val results = parseAll(trees, parser, chainReplacer, asString)
    results foreach { res =>
      import res._
      val buf = new StringBuilder()
      buf ++= "======\n"
      buf ++= sent.words.mkString(",")
      buf ++= "\nGold:\n"
      buf ++= gold.render(sent.words)
      buf ++= "\nGuess:\n"
      buf ++= guess.render(sent.words)
      buf ++= ("\nLocal Accuracy:" + (stats.precision,stats.recall,stats.f1,stats.exact,stats.tagAccuracy) + "\n")
      buf ++= (time +" Seconds")
      buf ++= "\n======"
      goldOut.println(gold.render(sent.words, newline=false)); guessOut.println(guess.render(sent.words, newline=false))
      println(buf.toString)
    }

    guessOut.close()
    goldOut.close()

    val allStats = results.map(_.stats).reduceLeft(_ + _)

    allStats
  }
}
