package scalanlp.parser
package projections

import org.junit.runner.RunWith;
import org.scalatest._;
import org.scalatest.junit._;
import org.scalatest.prop._
import scalanlp.trees.AnnotatedLabel
;

/**
 *
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class AnchoredRuleScorerTest  extends ParserTestHarness with FunSuite {

  test("We can parse using span scorer") {
    val gen = ParserTestHarness.simpleParser;
    val projections = GrammarProjections.identity(gen.builder.grammar)
    val f = new AnchoredRuleScorerFactory(gen.builder.grammar, gen, Double.NegativeInfinity);
    val zero = new CKYChartBuilder[ParseChart.LogProbabilityParseChart, AnnotatedLabel, String](gen.builder.root, new ZeroLexicon(gen.builder.lexicon), Grammar.zero(gen.builder.grammar), ParseChart.logProb);
    val fnext = new AnchoredRuleScorerFactory(zero.grammar, SimpleChartParser(zero), Double.NegativeInfinity);
    for( TreeInstance(_, t, w, _) <- getTestTrees()) try {
      val gent = gen(w);
      val scorer = f.mkSpanScorer(w);
      val xx = zero.buildInsideChart(w)
      assert(!xx.top.labelScore(0, w.length, AnnotatedLabel.TOP).isInfinite, "early")
      val ginside = zero.buildInsideChart(w, scorer);
      assert(!ginside.top.labelScore(0, w.length, AnnotatedLabel.TOP).isInfinite, "mid")
      val goutside = zero.buildOutsideChart(ginside, scorer)
      val scorer2 = fnext.buildSpanScorer(new ChartPair[ParseChart, AnnotatedLabel](ginside, goutside, ginside.top.labelScore(0, w.length, AnnotatedLabel.TOP), scorer))
      val ginside2 = zero.buildInsideChart(w, scorer2);
      lazy val goutside2 = zero.buildOutsideChart(ginside2, scorer2);
      val tree = SimpleViterbiDecoder(ParserTestHarness.simpleGrammar).extractBestParse(AnnotatedLabel.TOP, zero.grammar, ginside, goutside, w, scorer)
      val tree2 = SimpleViterbiDecoder(ParserTestHarness.simpleGrammar).extractBestParse(AnnotatedLabel.TOP, zero.grammar, ginside2, goutside2, w, scorer)
      assert(tree2 === tree, "late");
    } catch {
      case e: Exception =>
      throw new RuntimeException("Trouble with " + t.render(w), e);
    }

  }

  test("Parsing kind of works using it") {
    val gen = ParserTestHarness.simpleParser;
    val projections = GrammarProjections.identity(gen.builder.grammar)
    val f = new AnchoredRuleScorerFactory(gen.builder.grammar, gen, Double.NegativeInfinity);
    val zero = new CKYChartBuilder(gen.builder.root, new ZeroLexicon(gen.builder.lexicon), Grammar.zero(gen.builder.grammar), ParseChart.logProb);

    val parser = new Parser[AnnotatedLabel, String] {
      def bestParse(w: Seq[String], spanScorer: SpanScorer[AnnotatedLabel]) = {
        val scorer = f.mkSpanScorer(w);
        val ginside = zero.buildInsideChart(w, scorer);
        assert(!ginside.top.labelScore(0, w.length, AnnotatedLabel.TOP).isInfinite)
        val goutside = zero.buildOutsideChart(ginside, scorer)
        val tree = SimpleViterbiDecoder(ParserTestHarness.simpleGrammar).extractBestParse(AnnotatedLabel.TOP, zero.grammar, ginside, goutside, w, scorer)
        tree
      }
    }

    val res = evalParser(getTestTrees(), parser)
    assert(res.f1 > 0.5, res.f1)

  }

}