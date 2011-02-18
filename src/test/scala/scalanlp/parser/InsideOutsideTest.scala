package scalanlp.parser

import org.junit.runner.RunWith;
import org.scalatest._;
import org.scalatest.junit._;
import org.scalatest.prop._;

/**
 * 
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class InsideOutsideTest extends FunSuite {
  test("Simple test from iobasics") {
    val grammar = DSLGrammar.simpleGrammar;
    val lexicon = DSLGrammar.simpleLexicon;
    val io = new InsideOutside("S",grammar,lexicon);
    val sent = "She eats pizza without anchovies" split " ";
    val counts = io.expectedCounts(sent);
    val (rules,unaries,words) = counts.decode(grammar);
    assert(rules("Sb",BinaryRule("Sb","NPu","VPu")) === 1.0);
    assert(rules("NPb",BinaryRule("NPb","Nu","PPu")) === 1.0);
    assert(rules("PPb",BinaryRule("PPb","Pu","Nu")) === 1.0);
    assert(rules("VPb",BinaryRule("VPb","Vu","NPu")) === 1.0);
    assert(words("P","without") === 1.0);
    assert(words("N","pizza") === 1.0);
    assert(words("N","She") === 1.0);
    assert(words("N","anchovies") === 1.0);
    assert(words("V","eats") === 1.0);
  }

  test("complex example") {
    val grammar = grammarForComplexExample;
    val lexicon = lexiconForComplexExample;
    val io = new InsideOutside("S",grammar,lexicon);
    val sent = "He has good control" split " ";
    val counts = io.expectedCounts(sent);
    val res@(rules,unaries, words) = counts.decode(grammar);

    assert(rules("Sb",BinaryRule("Sb","NPu","VPu")) === 1.0);
    assert(rules("VPb",BinaryRule("VPb","VBZu","NPu")) === 0.4999999999999997);
    assert(rules("VPb",BinaryRule("VPb","VBZu","ADJPu"))===0.4999999999999997);
    assert(rules("NPb",BinaryRule("NPb","JJu","NNu"))===0.49999999999999994);
    assert(unaries("NPu",UnaryRule("NPu","NN"))===0.49999999999999994);
    assert(unaries("NPu",UnaryRule("NPu","PRP"))===1.0);
    assert(rules("ADJPb",BinaryRule("ADJPb","JJu","NPu")) === 0.49999999999999994);
    assert(words("JJ","good") === 1.0);
    assert(words("NN","control") === 1.0);
    assert(words("VBZ","has") === 1.0);
    assert(words("PRP","He") === 1.0);
  }

  test("complex example inside") {
    val grammar = grammarForComplexExample;
    val lexicon = lexiconForComplexExample;
    val io = new InsideOutside("S",grammar,lexicon);
    val sent = "He has good control" split " ";
    val inside = io.parser.buildInsideChart(sent);
    assert(inside.bot.labelScore(0,1,"PRP") === 0.0)
    assert(inside.top.labelScore(0,1,"NPu") === math.log(1.0/3.0));
    assert(inside.top.enteredLabelScores(0,1).length === 1,"t01");
    assert(inside.bot.enteredLabelScores(0,1).length === 1,"b01");
    assert(inside.bot.labelScore(1,2, "VBZ") === 0.0);
    assert(inside.top.labelScore(1,2, "VBZu") === 0.0);
    assert(inside.bot.enteredLabelScores(1,2).length === 1,"t12");
    assert(inside.top.enteredLabelScores(1,2).length === 1,"b12");
    assert(inside.bot.labelScore(2,3, "JJ") === 0.0);
    assert(inside.top.labelScore(2,3, "JJu") === 0.0);
    assert(inside.bot.enteredLabelScores(2,3).toSeq.length === 1,"b23");
    assert(inside.top.enteredLabelScores(2,3).toSeq.length === 1,"t23");
    assert(inside.top.labelScore(3,4, "NPu") === math.log(1.0/3.0));
    assert(inside.bot.labelScore(3,4, "NN") === 0.0);
    assert(inside.top.labelScore(3,4, "NNu") === 0.0);
    assert(inside.bot.enteredLabelScores(3,4).toSeq.length === 1, "b34");
    assert(inside.top.enteredLabelScores(3,4).toSeq.length === 2, "t34");

    assert(inside.bot.enteredLabelScores(0,2).toSeq.length === 0);
    assert(inside.bot.enteredLabelScores(1,3).toSeq.length === 0);
    assert(inside.bot.labelScore(2,4, "NPb") === 0.0,"24npb");
    assert(inside.bot.labelScore(2,4, "ADJPb") === math.log(1.0/3.0),"24adjpb");
    assert(inside.top.labelScore(2,4, "ADJPu") === math.log(1.0/3.0),"24adjpu");
    assert(inside.top.labelScore(2,4, "NPu") === math.log(1.0/3.0),"24npu");
    assert(inside.top.enteredLabelScores(2,4).toSeq.length === 2);
    assert(inside.bot.enteredLabelScores(2,4).toSeq.length === 2);

    assert(inside.bot.enteredLabelScores(0,3).toSeq.length === 0);
    assert((inside.top.labelScore(1,4, "VPu") - math.log(1.0/3.0)).abs < 1E-4);
    assert(inside.bot.enteredLabelScores(1,4).toSeq.length === 1,"b14");

    assert((inside.top.labelScore(0,4, "S") - math.log(1.0/9.0)).abs < 1E-4);
    assert(inside.top.enteredLabelScores(0,4).toSeq.length === 1);
  }

  test("complex example outside") {
    val grammar = grammarForComplexExample;
    val lexicon = lexiconForComplexExample;
    val io = new InsideOutside("S",grammar,lexicon);
    val sent = "He has good control" split " ";
    val inside = io.parser.buildInsideChart(sent);
    val outside = io.parser.buildOutsideChart(inside);

    assert((outside.top.labelScore(0,4, "S") === 0.0));
    assert(outside.top.enteredLabelScores(0,4).toSeq.length === 1);

    assert(outside.top.enteredLabelScores(0,3).toSeq.length === 0);
    assert((outside.top.labelScore(1,4, "VPu") - math.log(1.0/3.0)).abs < 1E-4, outside.top.enteredLabelScores(1,4).toIndexedSeq);
    assert(outside.top.enteredLabelScores(1,4).toSeq.length === 1);

    assert(outside.top.enteredLabelScores(0,2).toSeq.length === 0);
    assert(outside.top.enteredLabelScores(1,3).toSeq.length === 0);
    assert((outside.bot.labelScore(0,1,"PRP") - math.log(1.0/9.0)).abs < 1E-5, outside.bot.labelScore(0,1,"PRP") + " " + math.log(1.0/9.0));

    assert((outside.top.labelScore(2,4, "NPu") - math.log(1.0/6.0)).abs < 1E-6, outside.top.labelScore(2,4,"NPu") -> "NP" -> outside.bot.labelScore(2,4,"NPb"));
    assert((outside.top.labelScore(2,4, "ADJPu") - math.log(1.0/6.0)).abs < 1E-6, outside.bot.labelScore(2,4,"ADJPb") -> "ADJPb");

    assert((outside.top.labelScore(0,1,"NPu") - math.log(1.0/3.0)).abs < 1E-5, outside.top.enteredLabelScores(0,1).toIndexedSeq + "NPu");
    assert((outside.bot.labelScore(1,2,"VBZ") - math.log(1.0/9.0)).abs < 1E-5, outside.bot.enteredLabelScores(1,2).toIndexedSeq + "VBZb");
    assert(outside.top.enteredLabelScores(1,2).toSeq.length === 1);
    assert((outside.bot.labelScore(2,3,"JJ") - math.log(1.0/9.0)).abs < 1E-5, outside.bot.enteredLabelScores(2,3).toIndexedSeq + "JJ");
    assert(outside.bot.enteredLabelScores(2,3).toSeq.length === 1);
    assert((outside.top.labelScore(3,4, "NPu") - math.log(1.0/6.0)).abs < 1E-5, outside.top.enteredLabelScores(3,4).toIndexedSeq + "NP");
    assert((outside.bot.labelScore(3,4,"NN") - math.log(1.0/9.0)).abs < 1E-5, outside.bot.labelScore(3,4,"NN") + "NN");
  }

  import DSLGrammar._;
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

    'XXX -> 'PUNCT -> 1.0
   );

  def lexiconForComplexExample = lexicon(
    'JJ -> "good" -> (1.0),
    'NN -> "control" -> (1.0),
    'VBZ -> "has" -> (1.0),
    'PRP -> "He" -> (1.0),
    'PUNCT -> "." -> (1.0)
  )


}

