package scalanlp.parser
package discrim

import scalanlp.trees._

import projections._
import ParseChart._
import scalanlp.trees.UnaryChainRemover.ChainReplacer
import java.io.{File, BufferedInputStream, FileInputStream, ObjectInputStream}
import collection.mutable.ArrayBuffer
import scalanlp.util._

/**
 * The EP Parser parses a product of grammars by projecting each down to an easy to parse grammar,
 * and using each grammar in sequence to evaluate
 * @author dlwh
 */
@SerialVersionUID(1)
class EPParser[L,L2,W](val parsers: Seq[ChartBuilder[LogProbabilityParseChart,L2,W]], coarseParser: ChartBuilder[LogProbabilityParseChart,L,W],
                       val projectionsSeq: Seq[GrammarProjections[L,L2]],
                       val maxEPIterations: Int= 1,
                       val damping: Double = 1.0) extends ChartParser[L,L2,W] with Serializable { outer =>

  case class EPResult(marginals: Seq[ParsedSentenceData], partition: Double, f0: SpanScorer[L])
  case class ParsedSentenceData(inside: LogProbabilityParseChart[L2], outside: LogProbabilityParseChart[L2],
                                partition: Double, correction: SpanScorer[L2])

  def fineParsers:Seq[ChartParser[L,L2,W]] = {
    for( ((parser,projection),i) <- parsers zip projectionsSeq zipWithIndex) yield {
      new ChartParser[L,L2,W] with Serializable {

        def charts(s: scala.Seq[W], spanScorer: SpanScorer[L]) = {
          val EPResult(parserDatas,partition,f0) = buildAllCharts(s,spanScorer)
          val parserData = parserDatas(i)
          new ChartPair[ParseChart,L2](parserData.inside, parserData.outside, parserData.correction)
        }

        def decoder = outer.decoder

        def projections = projection

        def root = parser.root

        protected def grammar = parser.grammar
      }
    }
  }

  def charts(s: scala.Seq[W], spanScorer: SpanScorer[L]) = {
    val EPResult(parserDatas,partition,f0) = buildAllCharts(s,spanScorer)
    val parserData = parserDatas.last
    new ChartPair[ParseChart,L2](parserData.inside, parserData.outside, parserData.correction)
  }

  def root = parsers.last.root
  def projections = projectionsSeq.last

  protected def grammar = parsers.last.grammar

  def buildAllCharts(words: Seq[W],
                     initScorer: SpanScorer[L]=SpanScorer.identity[L],
                     tree: BinarizedTree[L]=null):EPResult = {
    var currentF0 = initScorer
    val corrections = Array.fill(parsers.length)( SpanScorer.divide(currentF0,parsers.length))

    val partitions = Array.fill(parsers.length)(0.0)
    val insideCharts = Array.fill[LogProbabilityParseChart[L2]](parsers.length)(null)
    val outsideCharts = Array.fill[LogProbabilityParseChart[L2]](parsers.length)(null)
    val scorers = Array.fill(parsers.length)(SpanScorer.identity[L2])
    var changed = true

    var lastF0 = currentF0
    for(i <- 0 until maxEPIterations if changed) {
      val lastLastF0 = lastF0
      lastF0 = currentF0
      for(m <- 0 until parsers.length) {
        val rescaledScorer = approximators(m).divide(currentF0, corrections(m), words)
        val projectedScorer = projectCoarseScorer(rescaledScorer, m)

        insideCharts(m) = parsers(m).buildInsideChart(words,projectedScorer)
        outsideCharts(m) = parsers(m).buildOutsideChart(insideCharts(m), projectedScorer)
        scorers(m) = projectedScorer

        val newPartition = insideCharts(m).top.labelScore(0,words.length,parsers.last.root)
        assert(!newPartition.isInfinite)
        partitions(m) = newPartition
        // project down the approximation
        currentF0 = approximators(m).project(insideCharts(m),outsideCharts(m), newPartition, projectedScorer, tree)
        corrections(m) = ScalingSpanScorer(currentF0,rescaledScorer,0.0,-1)
      }
      if(parsers.length == 1 || maxEPIterations == 1) {
        changed = false
      } else {
        val maxChange = computeMaxChange(currentF0,lastF0,initScorer,words.length)
        assert(!maxChange.isNaN)
        changed = maxChange.abs > 1E-4
      }
    }


    val f0Partition = (f0Builder.buildInsideChart(words, currentF0).top.labelScore(0,words.length,f0Builder.root))
    val partition = partitions.sum + f0Partition
    val data = Array.tabulate(parsers.length)(m => ParsedSentenceData(insideCharts(m),
      outsideCharts(m), partitions(m),scorers(m)))
    EPResult(data,partition,currentF0)
  }

  def computeMaxChange(scorer1: SpanScorer[L], scorer2: SpanScorer[L], validSpan: SpanScorer[L], length: Int):Double = {
    val changes = for {
      span <- (1 to length).iterator
      i <- (0 to (length - span)).iterator
      j = i + span
      p <- (0 until projectionsSeq(0).labels.coarseIndex.size).iterator if !validSpan.scoreSpan(i,j,p).isNegInfinity
      k <- ((i + 1) until j).iterator
      r <- coarseParser.grammar.indexedBinaryRulesWithParent(p)
    } yield {
      // TODO: compute change that is consistent with all span scorers :-/
      val s1 = scorer1.scoreBinaryRule(i, k, j, r)
      val s2 = scorer2.scoreBinaryRule(i, k, j, r)
      val a = (s1 - s2)
      if(a.isNaN) 0.0 // from negative infinities...el
      else if(a < 0 && a.isInfinite) 1001.0
      else if(a.isInfinite) 10000.
      else if(s1.abs < 1E-4) a.abs
      else a.abs / (s1.abs + s2.abs)
    }

    changes.find( _.abs > 1E-4).getOrElse(0.0)
  }

  def projectCoarseScorer(coarseScorer: SpanScorer[L], model: Int) ={
    new ProjectingSpanScorer(projectionsSeq(model), coarseScorer)
  }

  val approximators = (parsers zip projectionsSeq).map{ case (parser,proj) =>
//    new AnchoredRuleApproximator[L,L2,W](parser, coarseParser,proj, Double.NegativeInfinity)
    new AnchoredRuleApproximator[L,L2,W](parser, coarseParser,proj, -10)
  }
  val decoder = new MaxConstituentDecoder[L,L2,W](projectionsSeq.last)
  val f0Decoder = new MaxConstituentDecoder[L,L,W](GrammarProjections.identity(coarseParser.grammar))
  val f0Builder = new CKYChartBuilder[LogProbabilityParseChart,L,W](coarseParser.root,new ZeroLexicon(coarseParser.lexicon), Grammar.zero(coarseParser.grammar), ParseChart.logProb)



  def f0parser:ChartParser[L,L,W] = {
    new ChartParser[L,L,W] with Serializable {

      def charts(w: Seq[W], spanScorer: SpanScorer[L]) = {
        val EPResult(parserDatas,partition,f0) = buildAllCharts(w,spanScorer)
        val f0Inside = f0Builder.buildInsideChart(w,SpanScorer.sum(spanScorer,f0))
        val f0Outside = f0Builder.buildOutsideChart(f0Inside,SpanScorer.sum(spanScorer,f0))
        new ChartPair[ParseChart,L](f0Inside,f0Outside,SpanScorer.sum(spanScorer,f0))
      }

      def decoder = f0Decoder

      def projections = GrammarProjections.identity(coarseParser.grammar)

      def root = f0Builder.root

      protected def grammar = f0Builder.grammar

    }
  }
}

/**
 *
 * @author dlwh
 */
object EPParserRunner extends ParserPipeline {

  case class Params(parser: ParserParams.BaseParser[String],
                    model0: File = null,
                    model1: File = null,
                    model2: File = null,
                    model3: File = null)
  protected val paramManifest = manifest[Params]

  def trainParser(trainTrees: IndexedSeq[TreeInstance[String,String]],
                  validate: Parser[String,String]=>ParseEval.Statistics,
                  params: Params) = {
    val parsers = new ArrayBuffer[SimpleChartParser[String,(String,Int),String]]
    var found = true
    var i = 0
    val paths = params.productIterator.buffered
    while(found && paths.hasNext) {
      found = false
      while(paths.hasNext && !paths.head.isInstanceOf[File]) paths.next
      if(paths.hasNext) {
        val path = paths.next.asInstanceOf[File]
        println(path)
        if(path ne null) {
          parsers += readObject(path)
          found = true
        }
        i += 1
      }
    }
    val coarseParser = params.parser.optParser

    val productParser = new EPParser(parsers.map(_.builder.withCharts(ParseChart.logProb)), coarseParser.get, parsers.map(_.projections), maxEPIterations = 5)
    Iterator.single( "EPIC" -> productParser)
  }


}


/**
 *
 * @author dlwh
 */
object EPParserParamRunner extends ParserPipeline {

  case class Params(parser: ParserParams.BaseParser[String],
                    epParser: File, useExact: Boolean = true)
  protected val paramManifest = manifest[Params]

  def trainParser(trainTrees: IndexedSeq[TreeInstance[String,String]],
                  validate: Parser[String,String]=>ParseEval.Statistics,
                  params: Params) = {


    val epParser = readObject[EPParser[String,(String,Int),String]](params.epParser)
    val parsers = epParser.parsers
    val projections = epParser.projectionsSeq

    val coarseParser = params.parser.optParser

    val eps = Iterator.tabulate(5) { i =>
      val productParser = new EPParser(parsers, coarseParser.get, projections, maxEPIterations = i+1)
      ("EP-" + i) -> productParser
    }
    val product = new ProductParser(parsers, coarseParser.get, projections)
    val raw = for ( ((par,pro),i) <- parsers zip epParser.projectionsSeq zipWithIndex)
                  yield ("Raw-" + i) -> new SimpleChartParser[String,(String,Int),String](par, new MaxConstituentDecoder(pro), pro)

    val teed = epParser.fineParsers
    val namedTeed = for ( (p,i) <- teed.zipWithIndex) yield ("Teed-" + i) -> p

    val exactStuff = if(params.useExact) {
      val exact = ExactParserExtractor.extractParser(parsers, coarseParser.get, projections)
      Iterator(("Exact" -> exact))
    } else Iterator.empty


    Iterator[(String,Parser[String,String])]("F0" -> epParser.f0parser) ++ raw.iterator ++ eps ++ Iterator("Product" -> product) ++ namedTeed ++ exactStuff
  }


}
