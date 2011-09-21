package scalanlp.parser.discrim

import scalanlp.parser.ParseChart._
import scalanlp.parser.projections.GrammarProjections
import collection.mutable.ArrayBuffer
import scalanlp.parser._
import scalala.tensor.{Counter, Counter2}
import scalala.tensor.Counter
import scalala.tensor.::
import java.io.File
import scalanlp.trees.UnaryChainRemover.ChainReplacer
import scalanlp.util._
import collection.IndexedSeq

/**
 * 
 * @author dlwh
 */

object ExactParserExtractor {

  type Label[L,L2] = (L,Seq[L2])

  def extractParser[L,L2,W](parsers: Seq[ChartBuilder[LogProbabilityParseChart,L2,W]],
                            coarseParser: ChartBuilder[LogProbabilityParseChart,L,W],
                            projections: Seq[GrammarProjections[L,L2]]):ChartParser[L,(L,Seq[L2]),W] = {

    type MyLabel = Label[L,L2]

    def project(l: MyLabel) = l._1
    def refine(l: L):Seq[MyLabel] = {
      val options = IndexedSeq[IndexedSeq[L2]](Vector.empty)
      val r = projections.foldLeft(options){ (options,p) =>
        for(l2 <- p.labels.refinementsOf(l); o <- options) yield {
          val r: IndexedSeq[L2] = o :+ l2
          r
        }
      }
      r.map(l ->  _)
    }

    val myProjections = GrammarProjections(coarseParser.grammar, refine _,  project _)
    val grammars = parsers.map(_.grammar)

    val brules = Counter2[MyLabel,BinaryRule[(L,Seq[L2])], Double]()
    val urules = Counter2[MyLabel,UnaryRule[(L,Seq[L2])], Double]()
    for(r <- myProjections.rules.fineIndex) r match {
      case br@BinaryRule(a,b,c) =>
        val scores = for {
          i <- 0 until grammars.length;
          aa = a._2 apply i
          bb = b._2 apply i
          cc = c._2 apply i
          g = grammars(i)
          score = g.ruleScore(BinaryRule(aa,bb,cc))
        } yield score

        brules(a,br) = scores.sum
      case ur@UnaryRule(a,b) =>
        val scores = for {
          i <- 0 until grammars.length;
          aa = a._2 apply i
          bb = b._2 apply i
          g = grammars(i)
        } yield g.ruleScore(UnaryRule(aa,bb))
        urules(a,ur) = scores.sum

    }

    val grammar = Grammar(myProjections.labels.fineIndex, myProjections.rules.fineIndex, brules, urules)

    val lexicons = parsers.map(_.lexicon)

    val _knownTagWords = collection.mutable.Set[(MyLabel,W)]()
    val knownTags = coarseParser.lexicon.knownTagWords.map(_._1).flatMap(myProjections.labels.refinementsOf _).toSet
    val knownWords = coarseParser.lexicon.knownTagWords.map(_._2).toSet
    for( w <- knownWords; ll1 <- lexicons(0).tagScores(w).keysIterator; ref <- myProjections.labels.refinementsOf(projections(0).labels.project(ll1))) {
      _knownTagWords += (ref->w)
    }

    def scoreWord(label: (L,Seq[L2]), w: W):Double = {
      var score = 0.0
      for( (lex,l) <- lexicons zip label._2) {
        val s = lex.wordScore(l,w)
        if(s == Double.NegativeInfinity)
          return s
        score += s
      }
      score
    }

    val lexicon = new Lexicon[MyLabel,W] {

      def wordScore(label: MyLabel, w: W):Double = {
        scoreWord(label,w)
      }


      override def tagScores(w: W) = {
        val scores = lexicons.map(_.tagScores(w));
        val res = Counter[MyLabel,Double]()
        for( ll1 <- scores(0).keysIterator; ref <- myProjections.labels.refinementsOf(projections(0).labels.project(ll1))) {
          val allScores = for (
            (sc,label) <- scores zip ref._2
          ) yield sc(label)
          res(ref) = allScores.sum
        }
        res
      }

      def tags = knownTags.iterator

      def knownTagWords = _knownTagWords.iterator
    }

    val root = myProjections.labels.refinementsOf(coarseParser.root)(0)
    val builder = new CKYChartBuilder[ParseChart.LogProbabilityParseChart,MyLabel,W](root, lexicon, grammar, ParseChart.logProb)
    new ChartParser(builder, new MaxConstituentDecoder[L,MyLabel,W](myProjections), myProjections)

  }


}

object ExactRunner extends ParserTrainer {

  case class Params(parser: ParserParams.BaseParser,
                    model0: File = null,
                    model1: File = null,
                    model2: File = null,
                    model3: File = null)
  protected val paramManifest = manifest[Params]

  def trainParser(trainTrees: IndexedSeq[TreeInstance[String,String]],
                  devTrees: IndexedSeq[TreeInstance[String,String]],
                  unaryReplacer : ChainReplacer[String],
                  params: Params) = {
    val parsers = new ArrayBuffer[ChartParser[String,(String,Int),String]]
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

    val productParser = ExactParserExtractor.extractParser(parsers.map(_.builder.withCharts(ParseChart.logProb)), coarseParser.get, parsers.map(_.projections))
    Iterator.single( "Exact" -> productParser)
  }


}