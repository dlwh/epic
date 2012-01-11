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
 *
 * @author dlwh
 */
trait EPModel[L,W] extends Serializable { outer =>
  type L2
  def projections: GrammarProjections[L,L2]

  trait Builder extends Serializable {
    type L2 = outer.L2
    def chartBuilder: ChartBuilder[LogProbabilityParseChart,L2,W]
    def projections:GrammarProjections[L,L2] = outer.projections
    def model = outer
  }
}

object EPModel {
  class FixedBuilderEPModel[L,L3,W](_builder: ChartBuilder[ParseChart,L3,W], proj: GrammarProjections[L,L3]) extends EPModel[L,W] {
    type L2 = L3

    def projections = proj
    def builder = new Builder {
      def chartBuilder = _builder.withCharts(ParseChart.logProb)
    }
  }
  def fromChartParser[L,L2,W](parser: SimpleChartParser[L,L2,W]) = {
    fromBuilderAndProjections(parser.builder,parser.projections)
  }

  def fromBuilderAndProjections[L,L2,W](builder: ChartBuilder[ParseChart,L2,W], proj: GrammarProjections[L,L2]) = {
    new FixedBuilderEPModel(builder,proj)
  }
}


/**
 * The EP Parser parses a product of grammars by projecting each down to an easy to parse grammar,
 * and using each grammar in sequence to evaluate
 * @author dlwh
 */
@SerialVersionUID(2)
class EPParser[L,W](val parsers: Seq[EPModel[L,W]#Builder], coarseParser: ChartBuilder[LogProbabilityParseChart,L,W],
                    val maxEPIterations: Int= 1,
                    val damping: Double = 1.0,
                    takeCounterMeasures: Boolean = true) extends Parser[L,W] with Serializable { outer =>


  def bestParse(s: Seq[W], spanScorer: SpanScorer[L]) = {
    val EPResult(parseData,partition,f0) = buildAllCharts(s,spanScorer)
    val theData = parseData.last
    val model = theData.model
    val decoder = new MaxConstituentDecoder[L,model.L2,W](model.projections)
    val bestParse = decoder.extractBestParse(model.chartBuilder.root,
      model.chartBuilder.grammar, theData.inside, theData.outside, s, theData.scorer);
    bestParse
  }

  case class EPResult(marginals: Seq[ModelData], partition: Double, f0: SpanScorer[L])

  class ModelData(val model: EPModel[L,W]#Builder) {
    import model.L2
    protected[EPParser] val approximator = {
      new AnchoredRuleApproximator[L,L2,W](model.chartBuilder, coarseParser,model.projections, -7)
    }
    def decoder = new MaxConstituentDecoder[L,L2,W](model.projections)
    var inside: LogProbabilityParseChart[L2] = _
    var outside: LogProbabilityParseChart[L2] = _
    var partition: Double = _
    var correction: SpanScorer[L] = _
    var scorer: SpanScorer[L2] = _
  }

//  def charts(s: scala.Seq[W], spanScorer: SpanScorer[L]) = {
//    val EPResult(parserDatas,partition,f0) = buildAllCharts(s,spanScorer)
//    val parserData = parserDatas.last
//    new ChartPair[ParseChart,L2](parserData.inside, parserData.outside, parserData.correction)
//  }

  def buildAllCharts(words: Seq[W],
                     initScorer: SpanScorer[L]=SpanScorer.identity[L],
                     tree: BinarizedTree[L]=null):EPResult = {
    var currentF0 = initScorer
    val marginals = parsers.map(new ModelData(_))
    marginals.foreach { _.correction = SpanScorer.divide(currentF0,parsers.length) }

    var changed = true

    var lastF0 = currentF0
    for(i <- 0 until maxEPIterations if changed) {
      lastF0 = currentF0
      for( (marg,m) <- marginals.zipWithIndex) {
        import marg._
        val p = model
        val proj:GrammarProjections[L,p.L2] = model.projections
        val rescaledScorer: SpanScorer[L] = approximator.divide(currentF0, correction, words)
        val projectedScorer: SpanScorer[p.L2] = new ProjectingSpanScorer(p.projections, rescaledScorer)

        marg.inside  = p.chartBuilder.buildInsideChart(words,projectedScorer)
        marg.outside = p.chartBuilder.buildOutsideChart(marg.inside, projectedScorer)
        marg.scorer = projectedScorer

        val newPartition = inside.top.labelScore(0,words.length,model.chartBuilder.root)
        if(newPartition.isInfinite) {
          sys.error("Couldn't parse" + words + " on iteration " + i + " with model " + m)
        }
        marg.partition = newPartition
        // project down the approximation
        currentF0 = approximator.project(inside, outside, newPartition, projectedScorer, tree)
        marg.correction = ScalingSpanScorer(currentF0,rescaledScorer,0.0,-1)
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
    val partition = marginals.map(_.partition).sum + f0Partition
    EPResult(marginals,partition,currentF0)
  }

  def computeMaxChange(scorer1: SpanScorer[L], scorer2: SpanScorer[L], validSpan: SpanScorer[L], length: Int):Double = {
    val changes = for {
      span <- (1 to length).iterator
      i <- (0 to (length - span)).iterator
      j = i + span
      p <- (0 until parsers(0).projections.labels.coarseIndex.size).iterator if !validSpan.scoreSpan(i,j,p).isNegInfinity
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
    val models = parsers.map(EPModel.fromChartParser(_)).map(_.builder)

    val productParser = new EPParser(models, coarseParser.get, maxEPIterations = 5)
    Iterator.single( "EPIC" -> productParser)
  }


}


/**
 *
 * @author dlwh
object EPParserParamRunner extends ParserPipeline {

  case class Params(parser: ParserParams.BaseParser[String],
                    epParser: File, useExact: Boolean = true)
  protected val paramManifest = manifest[Params]

  def trainParser(trainTrees: IndexedSeq[TreeInstance[String,String]],
                  validate: Parser[String,String]=>ParseEval.Statistics,
                  params: Params) = {


    val epParser = readObject[EPParser[String,(String,Int),String]](params.epParser)
    val parsers = epParser.parsers

    val coarseParser = params.parser.optParser

    val eps = Iterator.tabulate(5) { i =>
      val productParser = new EPParser(parsers, coarseParser.get, maxEPIterations = i+1)
      ("EP-" + i) -> productParser
    }
    val product = new ProductParser(parsers, coarseParser.get)
    val raw = for ( ((par,pro),i) <- parsers)
                  yield ("Raw-" + i) -> new SimpleChartParser[String,(String,Int),String](par, new MaxConstituentDecoder(pro), pro)

    val teed = epParser.fineParsers
    val namedTeed = for ( (p,i) <- teed.zipWithIndex) yield ("Teed-" + i) -> p

    val exactStuff = if(params.useExact) {
      val exact = ExactParserExtractor.extractParser(parsers, coarseParser.get)
      Iterator(("Exact" -> exact))
    } else Iterator.empty


    Iterator[(String,Parser[String,String])]("F0" -> epParser.f0parser) ++ raw.iterator ++ eps ++ Iterator("Product" -> product) ++ namedTeed ++ exactStuff
  }


} */

