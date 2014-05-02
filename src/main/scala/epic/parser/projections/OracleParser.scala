package epic.parser.projections

import epic.constraints.{CachedChartConstraintsFactory, ChartConstraints}
import epic.trees._
import epic.parser._
import epic.lexicon.{TagScorer, Lexicon}
import breeze.numerics.I
import com.typesafe.scalalogging.slf4j.LazyLogging
import scala.collection.{GenTraversableLike, GenTraversable, GenTraversableOnce}
import scala.collection.generic.CanBuildFrom
import epic.util.{SafeLogging, CacheBroker}
import epic.parser.RefinedAnchoring.StructureDelegatingAnchoring
import epic.parser.RefinedChartMarginal.Factory
import breeze.config.{Configuration, CommandLineParser, Help}
import java.io.File
import breeze.util._
import epic.parser.ParseExtractionException
import epic.parser.ViterbiDecoder
import epic.parser.ParseExtractionException
import epic.trees.ProcessedTreebank
import epic.trees.TreeInstance
import epic.parser.ViterbiDecoder
import epic.parser.ParserParams.XbarGrammar
import epic.trees.annotations.{Xbarize, TreeAnnotator}
import breeze.collection.mutable.TriangularArray
import epic.constraints.ChartConstraints.UnifiedFactory

/**
 * Finds the best tree (relative to the gold tree) s.t. it's reacheable given the current anchoring.
 * Best is measured as number of correct labeled spans, as usual.
 * @author dlwh
 */
class OracleParser[L, L2, W](val refinedGrammar: SimpleRefinedGrammar[L, L2, W]) extends SafeLogging {
  private val cache = CacheBroker().make[IndexedSeq[W], BinarizedTree[L2]]("OracleParser")

  private var problems  = 0
  private var total = 0
  import refinedGrammar.topology

  private def refinements = refinedGrammar.refinements

  def forTree(tree: BinarizedTree[L2],
              words: IndexedSeq[W],
              constraints: ChartConstraints[L]) = try {
    val projectedTree: BinarizedTree[L] = tree.map(refinements.labels.project)
    cache.getOrElseUpdate(words, {
      val treeconstraints = ChartConstraints.fromTree(topology.labelIndex, projectedTree)
      if(constraints.top.containsAll(treeconstraints.top) && constraints.bot.containsAll(treeconstraints.bot)) {
        synchronized(total += 1)
        tree
      } else try {
        val w = words
        val marg = AugmentedAnchoring(makeGoldPromotingAnchoring(w, tree, treeconstraints), constraints).maxMarginal

        val closest: BinarizedTree[(L, Int)] = new ViterbiDecoder[L,W]().extractMaxDerivationParse(marg)

        logger.warn {
          val stats = new ParseEval(Set.empty[L]).apply(closest.map(_._1), projectedTree)
          val ratio =  synchronized{problems += 1; total += 1; problems * 1.0 / total}
          f"Gold tree for $words not reachable. Best has score: $stats. $ratio%.2f are bad so far. "
        }

        val globalizedClosest: BinarizedTree[L2] = closest.map({refinements.labels.globalize(_:L, _:Int)}.tupled)
        logger.trace {
          s"Reachable: ${globalizedClosest.render(words, newline = true)}\nGold:${tree.render(words, newline = true)}"
        }
        globalizedClosest
      } catch {
        case ex:ParseExtractionException =>
          logger.error("Couldn't find a parse for " + words +": " + ex.getMessage, ex)
          tree
      }
    })
  } catch {
    case ex: Exception =>
      logger.error(s"while handling projectability for $tree $words: " + ex.getMessage, ex)
      throw ex
  }


  def makeGoldPromotingAnchoring(w: IndexedSeq[W],
                                 tree: BinarizedTree[L2],
                                 treeconstraints: ChartConstraints[L]): RefinedAnchoring[L, W] = {
    val correctRefinedSpans = GoldTagPolicy.goldTreeForcing(tree.map(refinements.labels.fineIndex))
    val correctUnaryChains = {
      val arr = TriangularArray.fill(w.length + 1)(IndexedSeq.empty[String])
      for(UnaryTree(a, btree, chain, span) <- tree.allChildren) {
        arr(span.begin, span.end) = chain
      }
      arr
    }
    new StructureDelegatingAnchoring[L, W] {
      protected val baseAnchoring: RefinedAnchoring[L, W] = refinedGrammar.anchor(w)

      def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int): Double = {
        0.1 * baseAnchoring.scoreBinaryRule(begin, split, end, rule, ref)
      }

      def scoreUnaryRule(begin: Int, end: Int, rule: Int, ref: Int): Double = {
        val top = grammar.parent(rule)
        val isAllowed = treeconstraints.top.isAllowedLabeledSpan(begin, end, top)
        val isRightRefined = isAllowed && {
          val parentRef = baseAnchoring.parentRefinement(rule, ref)
          val refTop = refinements.labels.globalize(top, parentRef)
          correctRefinedSpans.isGoldTopTag(begin, end, refTop)
        }

        val isCorrectChain = {
          isAllowed && grammar.chain(rule) == correctUnaryChains(begin, end)
        }

        200 * I(isAllowed) + 10 * I(isRightRefined) + 5 * I(isCorrectChain) + 0.1 * baseAnchoring.scoreUnaryRule(begin, end, rule, ref)
      }

      def scoreSpan(begin: Int, end: Int, tag: Int, ref: Int): Double = {
        val globalized = refinements.labels.globalize(tag, ref)
        200 * I(treeconstraints.bot.isAllowedLabeledSpan(begin, end, tag)) +
          10 * I(correctRefinedSpans.isGoldBotTag(begin, end, globalized)) + 0.1 * baseAnchoring.scoreSpan(begin, end, tag, ref)
      }
    }


  }

  def oracleMarginalFactory(trees: IndexedSeq[TreeInstance[L2, W]]):RefinedChartMarginal.Factory[L, W] = new Factory[L, W] {
    val knownTrees = trees.iterator.map(ti => ti.words -> ti.tree).toMap

    def apply(w: IndexedSeq[W], constraints: CoreAnchoring[L, W]): RefinedChartMarginal[L, W] = {
      val refAnchoring = knownTrees.get(w).map { t => makeGoldPromotingAnchoring(w, t, constraints.sparsityPattern)}.getOrElse(refinedGrammar.anchor(w))

      RefinedChartMarginal(AugmentedAnchoring(refAnchoring, constraints), true)
    }
  }

  def oracleParser(constraintGrammar: CoreGrammar[L, W], trees: IndexedSeq[TreeInstance[L2, W]]): Parser[L, W] = {
    new Parser(constraintGrammar, oracleMarginalFactory(trees), ViterbiDecoder())
  }
}

object OracleParser {
  case class Params(treebank: ProcessedTreebank,
                    @Help(text="Prefix for the name of the eval directory. Sentences are dumped for EVALB to read.")
                    name: String,
                    @Help(text="Path to the parser file. Look in parsers/")
                    parser: File = null,
                    implicit val cache: CacheBroker,
                    @Help(text="Print this and exit.")
                    help: Boolean = false,
                    @Help(text="How many threads to parse with. Default is whatever Scala wants")
                    threads: Int = -1)

  def main(args: Array[String]) {
    val params = CommandLineParser.readIn[Params](args)
    println("Command line arguments for recovery:\n" + Configuration.fromObject(params).toCommandLineString)
    println("Evaluating Parser...")
    import params._

    import params.treebank._

    val ann = GenerativeParser.defaultAnnotator()

    val initialParser = params.parser match {
      case null =>
        val (grammar, lexicon) = XbarGrammar().xbarGrammar(trainTrees)
        GenerativeParser.annotatedParser(grammar, lexicon, ann, trainTrees)
      //        GenerativeParser.annotatedParser(grammar, lexicon, Xbarize(), trainTrees)
      case f =>
        readObject[Parser[AnnotatedLabel, String]](f)
    }

    val constraints: ChartConstraints.Factory[AnnotatedLabel, String] = new UnifiedFactory({

      val maxMarginalized = initialParser.copy(marginalFactory=initialParser.marginalFactory match {
        case SimpleChartFactory(ref, mm) => SimpleChartFactory(ref, maxMarginal = true)
        case x => x
      })


      val uncached = new ParserChartConstraintsFactory[AnnotatedLabel, String](maxMarginalized, {(_:AnnotatedLabel).isIntermediate})
      new CachedChartConstraintsFactory[AnnotatedLabel, String](uncached)
    })

    val refGrammar = GenerativeParser.annotated(initialParser.grammar, initialParser.lexicon, ann, trainTrees)
    val coreGrammar = new ConstraintCoreGrammarAdaptor(initialParser.grammar, initialParser.lexicon, constraints)

    val annDevTrees = devTrees.map(ann)

    val parser = {
      new OracleParser(refGrammar).oracleParser(coreGrammar, annDevTrees)
    }

    for(ti <- devTrees.par) try {
      val m = TreeMarginal(AugmentedGrammar(refGrammar, coreGrammar), ti.words, ann.localized(refGrammar.refinements.labels)(ti.tree, ti.words))
      println(m.logPartition)
    } catch {
      case e: Exception => e.printStackTrace()
    }


    val name = params.name

    println("Parser " + name)

    {
      println("Evaluating Parser on dev...")
      val stats = ParserTester.evalParser(devTrees, parser, name+ "-dev", params.threads)
      import stats._
      println("Eval finished. Results:")
      println(stats)
    }


  }

}

