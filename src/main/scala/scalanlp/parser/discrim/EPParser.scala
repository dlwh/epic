package scalanlp.parser
package discrim

import scalanlp.trees._

import projections._;
import ParseChart._;

/**
 *
 * @author dlwh
 */
@serializable
@SerialVersionUID(1)
class EPParser[L,L2,W](val parsers: Seq[CKYChartBuilder[LogProbabilityParseChart,L2,W]], coarseParser: ChartBuilder[LogProbabilityParseChart,L,W],
                       val projections: Seq[ProjectionIndexer[L,L2]],
                       val maxEPIterations: Int= 1,
                       val damping: Double = 1.0) extends Parser[L,W] {
  case class ParsedSentenceData(inside: LogProbabilityParseChart[L2], outside: LogProbabilityParseChart[L2],
                                partition: Double, f0: SpanScorer[L2])



  def buildAllCharts(words: Seq[W],
                     initScorer: SpanScorer[L]=SpanScorer.identity[L],
                     tree: BinarizedTree[L]=null):Array[ParsedSentenceData] = {
    var currentF0 = initScorer;
    val corrections = Array.fill(parsers.length)( SpanScorer.divide(currentF0,parsers.length));

    val partitions = Array.fill(parsers.length)(0.0);
    val insideCharts = Array.fill[LogProbabilityParseChart[L2]](parsers.length)(null);
    val outsideCharts = Array.fill[LogProbabilityParseChart[L2]](parsers.length)(null);
    val scorers = Array.fill(parsers.length)(SpanScorer.identity[L2]);
    var changed = true;

    var lastF0 = currentF0;
    for(i <- 0 until maxEPIterations if changed) {
      val lastLastF0 = lastF0;
      lastF0 = currentF0;
      for(m <- 0 until parsers.length) {
        println(i + " " + m + words);
        val rescaledScorer = approximators(m).divideAndNormalize(currentF0, corrections(m), words);
        val projectedScorer = projectCoarseScorer(rescaledScorer, m); // TODO subtract out log numStates
        if(parsers.length == 1) {
          //sanify(rescaledScorer, words.length);
        }

        insideCharts(m) = parsers(m).buildInsideChart(words,projectedScorer);
        val outside = parsers(m).buildOutsideChart(insideCharts(m),projectedScorer);
        outsideCharts(m) = outside
        scorers(m) = projectedScorer;

        val newPartition = insideCharts(m).top.labelScore(0,words.length,parsers.last.root);
        assert(!newPartition.isInfinite);
        partitions(m) = newPartition;
        // project down the approximation
        currentF0 = approximators(m).project(insideCharts(m),outsideCharts(m), newPartition, projectedScorer, tree);
        corrections(m) = ScalingSpanScorer(currentF0,rescaledScorer,0.0,-1);
      }
      if(parsers.length == 1 || maxEPIterations == 1) {
        changed = false;
      } else {
        val maxChange = computeMaxChange(currentF0,lastF0,words.length);
        assert(!maxChange.isNaN)
        changed = maxChange.abs > 1E-4;
        if(!changed) {
          print(i + " ")
        } else if(i == maxEPIterations - 1) {

          print("F ")
        }
        println("<" + i +">" + maxChange);
      }
    }
    Array.tabulate(parsers.length)(m => ParsedSentenceData(insideCharts(m),
      outsideCharts(m), partitions(m),scorers(m)));
  }

  def computeMaxChange(scorer1: SpanScorer[L], scorer2: SpanScorer[L], length: Int):Double = {
    val changes = for {
      span <- 1 to length iterator;
      i <- 0 to (length - span) iterator;
      j = i + span;
      k <- (i+1) until j iterator;
      p <- 0 until projections(0).coarseIndex.size iterator;
      (b,bv) <- coarseParser.grammar.binaryRulesByIndexedParent(p) iterator;
      c <- bv.activeKeys
    } yield {
      // TODO: compute change that is consistent with all span scorers :-/
      val s1 = scorer1.scoreBinaryRule(i, k, j, p, b, c)
      val s2 = scorer2.scoreBinaryRule(i, k, j, p, b, c)
      val a = (s1 - s2);
      if(a.isNaN) 0.0 // from negative infinities...el
      else if(a.isInfinite) 1.0
      else  {
        a.abs
      }
    }

    changes.foldLeft(0.0)(math.max _ );
  }

  def projectCoarseScorer(coarseScorer: SpanScorer[L], model: Int):SpanScorer[L2] ={
    new ProjectingSpanScorer(projections(model), coarseScorer);
  }

  val approximators = (parsers zip projections).map{ case (parser,proj) =>
    new AnchoredRuleApproximator[L,L2,W](parser, coarseParser,proj, Double.NegativeInfinity);
  }
  val decoder = new ViterbiDecoder(projections.last);
  val f0Decoder = new SimpleViterbiDecoder(coarseParser.grammar);
  val f0Builder = new CKYChartBuilder[LogProbabilityParseChart,L,W](coarseParser.root,new ZeroLexicon(coarseParser.lexicon), new ZeroGrammar(coarseParser.grammar), ParseChart.logProb);
  val coarseFact = new AnchoredRuleScorerFactory[L,L,W](coarseParser, ProjectionIndexer(coarseParser.grammar.index,coarseParser.grammar.index,identity[L]), Double.NegativeInfinity);


  def bestParse(s: scala.Seq[W], spanScorer: SpanScorer[L]) = {
    val parserData = buildAllCharts(s,spanScorer).last
    val lastParser = parsers.last;
    decoder.extractBestParse(lastParser.root,lastParser.grammar,parserData.inside,parserData.outside,parserData.f0);
  }
}