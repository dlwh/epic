package scalanlp.parser

import scalanlp.trees._

import org.junit.runner.RunWith;
import org.scalatest._
import org.scalatest.junit._
import scalala.tensor.::

/**
 * 
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class InsideOutsideTest extends FunSuite {

  implicit def near(x: Double) = new {
    def near(y: Double) = if( (x-y).abs < 1E-4 * math.max(x+y,1E-4)/2) None else Some(x + " not near " + y)
  }

  test("Simple test from iobasics") {
    val grammar = DSLGrammar.simpleGrammar
    val sent = "She eats pizza without anchovies" split " "
    val io = ChartMarginal.fromSentence(grammar, sent)
    val counts = io.expectedCounts(new ProductionFeaturizer(grammar.grammar, DSLGrammar.lexicon))
    assert(counts(BinaryRule("Sb","NPu","VPu")) near 1.0)
    assert(counts(BinaryRule("NPb","Nu","PPu")) near 1.0)
    assert(counts(BinaryRule("PPb","Pu","Nu")) near 1.0)
    assert(counts(BinaryRule("VPb","Vu","NPu")) near 1.0)
    assert(counts(LexicalProduction("P","without")) near 1.0)
    assert(counts(LexicalProduction("N","pizza")) near 1.0)
    assert(counts(LexicalProduction("N","She")) near 1.0)
    assert(counts(LexicalProduction("N","anchovies")) near 1.0)
    assert(counts(LexicalProduction("V","eats")) near 1.0)
  }

  test("complex example") {
    val grammar = grammarForComplexExample
    val lexicon = lexiconForComplexExample
    val sent = "He has good control" split " "
    val io = ChartMarginal.fromSentence(grammar, sent)
    val counts = io.expectedCounts(new ProductionFeaturizer(grammar.grammar, lexicon))
    assert(counts(BinaryRule("Sb","NPu","VPu")) near  1.0)
    assert(counts(BinaryRule("VPb","VBZu","NPu")) near  0.4999999999999997)
    assert(counts(BinaryRule("VPb","VBZu","ADJPu"))near 0.4999999999999997)
    assert(counts(BinaryRule("NPb","JJu","NNu"))near 0.49999999999999994)
    assert(counts(UnaryRule("NPu","NN"))near 0.49999999999999994)
    assert(counts(UnaryRule("NPu","PRP"))near 1.0)
    assert(counts(BinaryRule("ADJPb","JJu","NPu")) near  0.49999999999999994)
    assert(counts(LexicalProduction("JJ","good")) near  1.0)
    assert(counts(LexicalProduction("NN","control")) near  1.0)
    assert(counts(LexicalProduction("VBZ","has")) near  1.0)
    assert(counts(LexicalProduction("PRP","He")) near  1.0)
  }

  test("complex example charts") {
    val grammar = grammarForComplexExample
    val lexicon = lexiconForComplexExample
    val sent = "He has good control" split " "
    val io = ChartMarginal.fromSentence(grammar, sent)
    import io._
    assert(inside.bot.labelScore(0,1,"PRP", 0) === 0.0)
    assert(inside.top.labelScore(0,1,"NPu", 0) === math.log(1.0/3.0))
    assert(inside.top.enteredLabelScores(0,1).length === 1,"t01")
    assert(inside.bot.enteredLabelScores(0,1).length === 1,"b01")
    assert(inside.bot.labelScore(1,2, "VBZ", 0) === 0.0)
    assert(inside.top.labelScore(1,2, "VBZu", 0) === 0.0)
    assert(inside.bot.enteredLabelScores(1,2).length === 1,"t12")
    assert(inside.top.enteredLabelScores(1,2).length === 1,"b12")
    assert(inside.bot.labelScore(2,3, "JJ", 0) === 0.0)
    assert(inside.top.labelScore(2,3, "JJu", 0) === 0.0)
    assert(inside.bot.enteredLabelScores(2,3).toSeq.length === 1,"b23")
    assert(inside.top.enteredLabelScores(2,3).toSeq.length === 1,"t23")
    assert(inside.top.labelScore(3,4, "NPu", 0) === math.log(1.0/3.0))
    assert(inside.bot.labelScore(3,4, "NN", 0) === 0.0)
    assert(inside.top.labelScore(3,4, "NNu", 0) === 0.0)
    assert(inside.bot.enteredLabelScores(3,4).toSeq.length === 1, "b34")
    assert(inside.top.enteredLabelScores(3,4).toSeq.length === 2, "t34")

    assert(inside.bot.enteredLabelScores(0,2).toSeq.length === 0)
    assert(inside.bot.enteredLabelScores(1,3).toSeq.length === 0)
    assert(inside.bot.labelScore(2,4, "NPb", 0) === 0.0,"24npb")
    assert(inside.bot.labelScore(2,4, "ADJPb", 0) === math.log(1.0/3.0),"24adjpb")
    assert(inside.top.labelScore(2,4, "ADJPu", 0) === math.log(1.0/3.0),"24adjpu")
    assert(inside.top.labelScore(2,4, "NPu", 0) === math.log(1.0/3.0),"24npu")
    assert(inside.top.enteredLabelScores(2,4).toSeq.length === 2)
    assert(inside.bot.enteredLabelScores(2,4).toSeq.length === 2)

    assert(inside.bot.enteredLabelScores(0,3).toSeq.length === 0)
    assert((inside.top.labelScore(1,4, "VPu", 0) - math.log(1.0/3.0)).abs < 1E-4)
    assert(inside.bot.enteredLabelScores(1,4).toSeq.length === 1,"b14")

    assert((inside.top.labelScore(0,4, "S", 0) - math.log(1.0/9.0)).abs < 1E-4)
    assert(inside.top.enteredLabelScores(0,4).toSeq.length === 1)

    assert((outside.top.labelScore(0,4, "S", 0) === 0.0))
    assert(outside.top.enteredLabelScores(0,4).toSeq.length === 1)

    assert(outside.top.enteredLabelScores(0,3).toSeq.length === 0)
    assert((outside.top.labelScore(1,4, "VPu", 0) - math.log(1.0/3.0)).abs < 1E-4, outside.top.enteredLabelScores(1,4).toIndexedSeq)
    assert(outside.top.enteredLabelScores(1,4).toSeq.length === 1)

    assert(outside.top.enteredLabelScores(0,2).toSeq.length === 0)
    assert(outside.top.enteredLabelScores(1,3).toSeq.length === 0)
    assert((outside.bot.labelScore(0,1,"PRP", 0) - math.log(1.0/9.0)).abs < 1E-5, outside.bot.labelScore(0,1,"PRP", 0) + " " + math.log(1.0/9.0))

    assert((outside.top.labelScore(2,4, "NPu", 0) - math.log(1.0/6.0)).abs < 1E-6, outside.top.labelScore(2,4,"NPu", 0) -> "NP" -> outside.bot.labelScore(2,4,"NPb", 0))
    assert((outside.top.labelScore(2,4, "ADJPu", 0) - math.log(1.0/6.0)).abs < 1E-6, outside.bot.labelScore(2,4,"ADJPb", 0) -> "ADJPb")

    assert((outside.top.labelScore(0,1,"NPu", 0) - math.log(1.0/3.0)).abs < 1E-5, outside.top.enteredLabelScores(0,1).toIndexedSeq + "NPu")
    assert((outside.bot.labelScore(1,2,"VBZ", 0) - math.log(1.0/9.0)).abs < 1E-5, outside.bot.enteredLabelScores(1,2).toIndexedSeq + "VBZb")
    assert(outside.top.enteredLabelScores(1,2).toSeq.length === 1)
    assert((outside.bot.labelScore(2,3,"JJ", 0) - math.log(1.0/9.0)).abs < 1E-5, outside.bot.enteredLabelScores(2,3).toIndexedSeq + "JJ")
    assert(outside.bot.enteredLabelScores(2,3).toSeq.length === 1)
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

