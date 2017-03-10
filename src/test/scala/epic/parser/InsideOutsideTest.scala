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

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit._
import repl.DSLGrammar

/**
 * 
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class InsideOutsideTest extends FunSuite {

  implicit class Near(val x: Double) {
    def near(y: Double) = (x-y).abs < 1E-4 * math.max(x+y,1E-4)/2
  }

  test("Simple test from iobasics") {
    val grammar = DSLGrammar.simpleGrammar
    val sent = "She eats pizza without anchovies" split " "
    val io = SimpleChartMarginal(grammar, sent)
    val counts = io.expectedCounts(new RuleFeaturizer(grammar.topology))
    assert(counts(BinaryRule("Sb","NPu","VPu")) near 1.0)
    assert(counts(BinaryRule("NPb","Nu","PPu")) near 1.0)
    assert(counts(BinaryRule("PPb","Pu","Nu")) near 1.0)
    assert(counts(BinaryRule("VPb","Vu","NPu")) near 1.0)
    //assert(counts(LexicalProduction("P","without")) near 1.0)
    //assert(counts(LexicalProduction("N","pizza")) near 1.0)
    //assert(counts(LexicalProduction("N","She")) near 1.0)
    //assert(counts(LexicalProduction("N","anchovies")) near 1.0)
    //assert(counts(LexicalProduction("V","eats")) near 1.0)
  }

  test("complex example") {
    val grammar = grammarForComplexExample
    val lexicon = lexiconForComplexExample
    val sent = "He has good control" split " "
    val io = RefinedChartMarginal(grammar, sent)
    val counts = io.expectedCounts(new RuleFeaturizer(grammar.topology))
    assert(counts(BinaryRule("Sb","NPu","VPu")) near  1.0)
    assert(counts(BinaryRule("VPb","VBZu","NPu")) near  0.5)
    assert(counts(BinaryRule("VPb","VBZu","ADJPu"))near 0.5)
    assert(counts(BinaryRule("NPb","JJu","NNu"))near 0.5)
    assert(counts(UnaryRule("NPu","NN", IndexedSeq.empty))near 0.5)
    assert(counts(UnaryRule("NPu","PRP", IndexedSeq.empty))near 1.0)
    assert(counts(BinaryRule("ADJPb","JJu","NPu")) near  0.5)
    //assert(counts(LexicalProduction("JJ","good")) near  1.0)
    //assert(counts(LexicalProduction("NN","control")) near  1.0)
    //assert(counts(LexicalProduction("VBZ","has")) near  1.0)
    //assert(counts(LexicalProduction("PRP","He")) near  1.0)
  }



  test("complex example charts, SimpleChartMarginal") {
    val grammar = grammarForComplexExample
    val sent = "He has good control" split " "
    val io = SimpleChartMarginal.apply(grammar, sent)
    import io._
    assert(inside.bot.labelScore(0,1,"PRP") near 0.0)
    assert(inside.top.labelScore(0,1,"NPu") near math.log(1.0/3.0))
//    assert(inside.top.enteredLabelScores(0,1).length near 1,"t01")
//    assert(inside.bot.enteredLabelScores(0,1).length near 1,"b01")
    assert(inside.bot.labelScore(1,2, "VBZ") near 0.0)
    assert(inside.top.labelScore(1,2, "VBZu") near 0.0)
    assert(inside.bot.labelScore(2,3, "JJ") near 0.0)
    assert(inside.top.labelScore(2,3, "JJu") near 0.0)
    assert(inside.top.labelScore(3,4, "NPu") near math.log(1.0/3.0))
    assert(inside.bot.labelScore(3,4, "NN") near 0.0)
    assert(inside.top.labelScore(3,4, "NNu") near 0.0)

    assert(inside.bot.labelScore(2,4, "NPb") near 0.0,"24npb")
    assert(inside.bot.labelScore(2,4, "ADJPb") near math.log(1.0/3.0),"24adjpb")
    assert(inside.top.labelScore(2,4, "ADJPu") near math.log(1.0/3.0),"24adjpu")
    assert(inside.top.labelScore(2,4, "NPu") near math.log(1.0/3.0),"24npu")

    assert((inside.top.labelScore(1,4, "VPu") near math.log(1.0/3.0)), "1,4 vpu")

    assert((inside.top.labelScore(0,4, "S") - math.log(1.0/9.0)).abs < 1E-4)

    assert((outside.top.labelScore(0,4, "S") near 0.0))


    assert((outside.top.labelScore(2,4, "NPu") - math.log(1.0/6.0)).abs < 1E-6, outside.top.labelScore(2,4,"NPu") -> "NP" -> outside.bot.labelScore(2,4,"NPb"))
    assert((outside.top.labelScore(2,4, "ADJPu") - math.log(1.0/6.0)).abs < 1E-6, outside.bot.labelScore(2,4,"ADJPb") -> "ADJPb")

    assert((outside.bot.labelScore(3,4,"NN") - math.log(1.0/9.0)).abs < 1E-5, outside.bot.labelScore(3,4,"NN") + "NN")

  }

  test("complex example charts") {
    val grammar = grammarForComplexExample
    val lexicon = lexiconForComplexExample
    val sent = "He has good control" split " "
    val io = RefinedChartMarginal.apply(grammar, sent)
    import io._
    assert(inside.bot.labelScore(0,1,"PRP", 0) near 0.0)
    assert(inside.top.labelScore(0,1,"NPu", 0) near math.log(1.0/3.0))
    assert(inside.top.enteredLabelScores(0,1).length near 1,"t01")
    assert(inside.bot.enteredLabelScores(0,1).length near 1,"b01")
    assert(inside.bot.labelScore(1,2, "VBZ", 0) near 0.0)
    assert(inside.top.labelScore(1,2, "VBZu", 0) near 0.0)
    assert(inside.bot.enteredLabelScores(1,2).length near 1,"t12")
    assert(inside.top.enteredLabelScores(1,2).length near 1,"b12")
    assert(inside.bot.labelScore(2,3, "JJ", 0) near 0.0)
    assert(inside.top.labelScore(2,3, "JJu", 0) near 0.0)
    assert(inside.bot.enteredLabelScores(2,3).toSeq.length near 1,"b23")
    assert(inside.top.enteredLabelScores(2,3).toSeq.length near 1,"t23")
    assert(inside.top.labelScore(3,4, "NPu", 0) near math.log(1.0/3.0))
    assert(inside.bot.labelScore(3,4, "NN", 0) near 0.0)
    assert(inside.top.labelScore(3,4, "NNu", 0) near 0.0)
    assert(inside.bot.enteredLabelScores(3,4).toSeq.length near 1, "b34")
    assert(inside.top.enteredLabelScores(3,4).toSeq.length near 2, "t34")

    assert(inside.bot.enteredLabelScores(0,2).toSeq.length near 0)
    assert(inside.bot.enteredLabelScores(1,3).toSeq.length near 0)
    assert(inside.bot.labelScore(2,4, "NPb", 0) near 0.0,"24npb")
    assert(inside.bot.labelScore(2,4, "ADJPb", 0) near math.log(1.0/3.0),"24adjpb")
    assert(inside.top.labelScore(2,4, "ADJPu", 0) near math.log(1.0/3.0),"24adjpu")
    assert(inside.top.labelScore(2,4, "NPu", 0) near math.log(1.0/3.0),"24npu")
    assert(inside.top.enteredLabelScores(2,4).toSeq.length near 2)
    assert(inside.bot.enteredLabelScores(2,4).toSeq.length near 2)

    assert(inside.bot.enteredLabelScores(0,3).toSeq.length near 0)
    assert((inside.top.labelScore(1,4, "VPu", 0) - math.log(1.0/3.0)).abs < 1E-4)
    assert(inside.bot.enteredLabelScores(1,4).toSeq.length near 1,"b14")

    assert((inside.top.labelScore(0,4, "S", 0) - math.log(1.0/9.0)).abs < 1E-4)
    assert(inside.top.enteredLabelScores(0,4).toSeq.length near 1)

    assert((outside.top.labelScore(0,4, "S", 0) near 0.0))
    assert(outside.top.enteredLabelScores(0,4).toSeq.length === 1)

    assert(outside.top.enteredLabelScores(0,3).toSeq.length === 0)
    assert((outside.top.labelScore(1,4, "VPu", 0) - math.log(1.0/3.0)).abs < 1E-4, outside.top.enteredLabelScores(1,4).toIndexedSeq)

    assert((outside.bot.labelScore(0,1,"PRP", 0) - math.log(1.0/9.0)).abs < 1E-5, outside.bot.enteredLabelScores(1,4) + " " + outside.bot.labelScore(0,1,"PRP", 0) + " " + math.log(1.0/9.0))

    assert((outside.top.labelScore(2,4, "NPu", 0) - math.log(1.0/6.0)).abs < 1E-6, outside.top.labelScore(2,4,"NPu", 0) -> "NP" -> outside.bot.labelScore(2,4,"NPb", 0))
    assert((outside.top.labelScore(2,4, "ADJPu", 0) - math.log(1.0/6.0)).abs < 1E-6, outside.bot.labelScore(2,4,"ADJPb", 0) -> "ADJPb")

    assert((outside.top.labelScore(0,1,"NPu", 0) - math.log(1.0/3.0)).abs < 1E-5, outside.top.enteredLabelScores(0,1).toIndexedSeq + "NPu")
    assert((outside.bot.labelScore(1,2,"VBZ", 0) - math.log(1.0/9.0)).abs < 1E-5, outside.bot.enteredLabelScores(1,2).toIndexedSeq + "VBZb")
    assert((outside.bot.labelScore(2,3,"JJ", 0) - math.log(1.0/9.0)).abs < 1E-5, outside.bot.enteredLabelScores(2,3).toIndexedSeq + "JJ")
    assert((outside.top.labelScore(3,4, "NPu", 0) - math.log(1.0/6.0)).abs < 1E-5, outside.top.enteredLabelScores(3,4).toIndexedSeq + "NP")
    assert((outside.bot.labelScore(3,4,"NN", 0) - math.log(1.0/9.0)).abs < 1E-5, outside.bot.labelScore(3,4,"NN", 0) + "NN")
  }

  import DSLGrammar._
  def grammarForComplexExample = grammar(
    'S -> 'Sb -> 1.0,
    'Sb -> ('NPu,'VPu) -> (1.0),

    'VPu -> 'VPb -> 1.0,
    'VPb -> ('VBZu,'NPu) -> (1.0),
    'VPb -> ('VBZu,'ADJPu) -> (1.0),

    'ADJPu-> 'ADJPb -> 1.0,

    'ADJPb-> ('JJu,'NPu) -> (1.0),

    'NPu-> 'NPb -> 1.0,
    'NPu-> ('PRP) -> (1.0),
    'NPu-> ('NN) -> (1.0),

    'NPb-> ('JJu,'NNu) -> (1.0),

    'JJu -> 'JJ -> 1.0,
    'NNu -> 'NN -> 1.0,
    'VBZu -> 'VBZ -> 1.0,

    'XXX -> 'PUNCT -> 1.0,

    'JJ -> "good" -> (1.0),
    'NN -> "control" -> (1.0),
    'VBZ -> "has" -> (1.0),
    'PRP -> "He" -> (1.0),
    'PUNCT -> "." -> (1.0)
   )

  def lexiconForComplexExample = IndexedSeq(
    LexicalProduction("JJ","good"),
    LexicalProduction("NN","control"),
    LexicalProduction("VBZ","has"),
    LexicalProduction("PRP","He"),
    LexicalProduction("PUNCT",".")
  )


}

