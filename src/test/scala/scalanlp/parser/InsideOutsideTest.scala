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
class InsideOutsideTest extends FunSuite with Checkers {
  test("Simple test from iobasics") {
    val grammar = DSLGrammar.simpleGrammar;
    val lexicon = DSLGrammar.simpleLexicon;
    val io = new InsideOutside("S",grammar,lexicon);
    val sent = "She eats pizza without anchovies" split " ";
    val counts = io.expectedCounts(sent);
    val (rules,unaries,words) = counts.decode(grammar);
    assert(rules("S",BinaryRule("S","NP","VP")) === 1.0);
    assert(rules("NP",BinaryRule("NP","N","PP")) === 1.0);
    assert(rules("PP",BinaryRule("PP","P","N")) === 1.0);
    assert(rules("VP",BinaryRule("VP","V","NP")) === 1.0);
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
    val (rules,unaries, words) = counts.decode(grammar);

    assert(rules("S",BinaryRule("S","NP","VP")) === 1.0);
    assert(rules("VP",BinaryRule("VP","VBZ","NP")) === 0.4999999999999997);
    assert(rules("VP",BinaryRule("VP","VBZ","ADJP"))===0.4999999999999997);
    assert(rules("NP",BinaryRule("NP","JJ","NN"))===0.49999999999999994);
    assert(unaries("NP",UnaryRule("NP","NN"))===0.49999999999999994);
    assert(unaries("NP",UnaryRule("NP","PRP"))===1.0);
    assert(rules("ADJP",BinaryRule("ADJP","JJ","NP")) === 0.49999999999999994);
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
    assert(inside.labelScoreNoUnary(0,1,"PRP") === 0.0)
    assert(inside.labelScore(0,1,"NPu") === math.log(1.0/3.0));
    assert(inside.enteredLabelScores(0,1).toSeq.length === 2);
    assert(inside.labelScoreNoUnary(1,2, "VBZ") === 0.0);
    assert(inside.enteredLabelScores(1,2).toSeq.length === 1);
    assert(inside.labelScoreNoUnary(2,3, "JJ") === 0.0);
    assert(inside.enteredLabelScores(2,3).toSeq.length === 1);
    assert(inside.labelScore(3,4, "NPu") === math.log(1.0/3.0));
    assert(inside.labelScoreNoUnary(3,4, "NN") === 0.0);
    assert(inside.enteredLabelScores(3,4).toSeq.length === 2);

    assert(inside.enteredLabelScores(0,2).toSeq.length === 0);
    assert(inside.enteredLabelScores(1,3).toSeq.length === 0);
    assert(inside.labelScore(2,4, "NP") === math.log(1.0/3.0));
    assert(inside.labelScore(2,4, "ADJP") === math.log(1.0/3.0));
    assert(inside.enteredLabelScores(2,4).toSeq.length === 2);

    assert(inside.enteredLabelScores(0,3).toSeq.length === 0);
    assert((inside.labelScore(1,4, "VP") - math.log(1.0/3.0)).abs < 1E-4);
    assert(inside.enteredLabelScores(1,4).toSeq.length === 1);

    assert((inside.labelScore(0,4, "S") - math.log(1.0/9.0)).abs < 1E-4);
    assert(inside.enteredLabelScores(0,4).toSeq.length === 1);
  }

  test("complex example outside") {
    val grammar = grammarForComplexExample;
    val lexicon = lexiconForComplexExample;
    val io = new InsideOutside("S",grammar,lexicon);
    val sent = "He has good control" split " ";
    val inside = io.parser.buildInsideChart(sent);
    val outside = io.parser.buildOutsideChart(inside);

    assert((outside.labelScoreNoUnary(0,4, "S") === 0.0));
    assert(outside.enteredLabelScores(0,4).toSeq.length === 1);

    assert(outside.enteredLabelScores(0,3).toSeq.length === 0);
    assert((outside.labelScore(1,4, "VP") - math.log(1.0/3.0)).abs < 1E-4, outside.enteredLabelScores(1,4).toIndexedSeq);
    assert(outside.enteredLabelScores(1,4).toSeq.length === 1);

    assert(outside.enteredLabelScores(0,2).toSeq.length === 0);
    assert(outside.enteredLabelScores(1,3).toSeq.length === 0);
    assert((outside.labelScore(0,1,"PRP") - math.log(1.0/9.0)).abs < 1E-5, outside.labelScore(0,1,"PRP") + " " + math.log(1.0/9.0));

    assert((outside.labelScoreNoUnary(2,4, "NP") - math.log(1.0/6.0)).abs < 1E-6, outside.labelScore(2,4,"NP") -> "NP" -> outside.labelScoreNoUnary(2,4,"NP"));
    assert((outside.labelScoreNoUnary(2,4, "ADJP") - math.log(1.0/6.0)).abs < 1E-6, outside.labelScore(2,4,"ADJP") -> "ADJP");

    assert((outside.labelScoreNoUnary(0,1,"NP") - math.log(1.0/3.0)).abs < 1E-5, outside.enteredLabelScores(0,1).toIndexedSeq + "NP");
    assert((outside.labelScore(1,2,"VBZ") - math.log(1.0/9.0)).abs < 1E-5, outside.enteredLabelScores(1,2).toIndexedSeq + "VBZ");
    assert(outside.enteredLabelScores(1,2).toSeq.length === 1);
    assert((outside.labelScore(2,3,"JJ") - math.log(1.0/9.0)).abs < 1E-5, outside.enteredLabelScores(2,3).toIndexedSeq + "JJ");
    assert(outside.enteredLabelScores(2,3).toSeq.length === 1);
    assert((outside.labelScore(3,4, "NP") - math.log(1.0/6.0)).abs < 1E-5, outside.enteredLabelScores(3,4).toIndexedSeq + "NP");
    assert((outside.labelScore(3,4,"NN") - math.log(1.0/9.0)).abs < 1E-5, outside.labelScore(3,4,"NN") + "NN");
  }

  import DSLGrammar._;
  def grammarForComplexExample = grammar(
    'S -> 'Sb -> 1.0,
    'Sb -> ('NPu,'VPu) -> (1.0),

    'VPu -> 'VPb -> 1.0,
    'VPb -> ('VBZu,'NPu) -> (1.0),
    'VPb -> ('VBZu,'ADJPu) -> (1.0),

    'ADJPu-> 'ADJP -> 1.0,

    'ADJPb-> ('JJu,'NPu) -> (1.0),

    'NPu-> 'NPb -> 1.0,
    'NPu-> ('PRPb) -> (1.0),
    'NPu-> ('NNb) -> (1.0),

    'NPb-> ('JJu,'NNu) -> (1.0),

    'JJu -> 'JJ -> 1.0,
    'NNu -> 'NN -> 1.0,
    'VBZu -> 'VBZ -> 1.0,
    'PRPu -> 'PRP -> 1.0,

    'XXX -> 'PUNCT -> 1.0
   );

  def lexiconForComplexExample = lexicon(
    'JJ -> "good" -> (1.0),
    'NN -> "control" -> (1.0),
    'VBZ -> "has" -> (1.0),
    'PRP -> "He" -> (1.0),
    'PUNCT -> "." -> (1.0)
  )

  def grammarForMoreComplexExample = grammar(
    'S -> 'S -> 1.0,
    'S -> ('ADJP) -> 1.0,
    'S -> ('VP) -> 1.0,
    'S -> ('NP) -> 1.0,
    'S -> ('NP,'VP) -> 1.0,
    'S -> ('S,'XS) -> 1.0,

    'VP -> 'VP -> 1.0,
    'VP -> ('VBZ,'NP) -> 1.0,
    'VP -> ('VBZ,'S) -> 1.0,
    'VP -> ('NN) -> 1.0,
    'VP -> ('VBZ,'ADJP) -> 1.0,

    'ADJP -> 'ADJP -> 1.0,
    'ADJP -> ('JJ,'NP) -> 1.0,
    'ADJP -> ('JJ) -> 1.0,
    'NP -> 'NP -> 1.0,
    'NP -> ('NP,'VP) -> 1.0,
    'NP -> ('NP,'NP) -> 1.0,
    'NP -> ('NN) -> 1.0,
    'NP -> ('NP,'XNP) -> 1.0,
    'NP -> ('NP,'NN) -> 1.0,
    'NP -> ('JJ) -> 1.0,
    'NP -> ('NP,'ADJP) -> 1.0,
    'NP -> ('JJ,'XNP) -> 1.0,
    'NP -> ('JJ,'NN) -> 1.0,
    'NP -> ('PRP) -> 1.0,
    'NP -> ('ADJP,'NN) -> 1.0,
    'XS -> ('XS) -> 1.0,
    'XS -> ('NP,'XS) -> 1.0,
    'XS -> ('VP,'XS) -> 1.0,
    'XS -> ('S,'PUNCT) -> 1.0,
    'XS -> ('VP,'PUNCT) -> 1.0,
    'XS -> ('NP,'VP) -> 1.0,
    'XS -> ('S,'XS) -> 1.0,
    'ROOT -> ('S) -> 1.0,
    'XXX -> 'YYY -> 1.0
   );


  test("More complex example") {
    val grammar = grammarForMoreComplexExample;
    val lexicon = lexiconForComplexExample;
    val io = new InsideOutside("ROOT",grammar,lexicon);
    val sent = "He has good control ." split " ";
    val counts = io.expectedCounts(sent);
    val (rules, unaries, words) = counts.decode(grammar);


    assert((words("JJ","good") - 1.0).abs < 1E-6, words("JJ","good"));
    assert((words("NN","control") - 1.0).abs < 1E-6, words("NN","control"));
    assert((words("VBZ","has") - 1.0).abs < 1E-6, words("VBZ","has"));
    assert((words("PRP","He") - 1.0).abs < 1E-6, words("PRP","He"));
    assert((words("PUNCT",".") - 1.0).abs < 1E-6, words("PUNCT","."));
    assert((unaries("ROOT",UnaryRule("ROOT","S")) - 1.0).abs < 1E-6, unaries("ROOT",UnaryRule("ROOT","S")));
    assert((unaries("NP",UnaryRule("NP","PRP")) - 1.0).abs < 1E-6, unaries("NP",UnaryRule("NP","PRP")));
  }
}

