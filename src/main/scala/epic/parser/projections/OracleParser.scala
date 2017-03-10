package epic.parser.projections

import epic.constraints.{CachedChartConstraintsFactory, ChartConstraints}
import epic.trees._
import epic.parser._
import breeze.numerics.I
import epic.util.{Optional, CacheBroker}
import epic.parser.GrammarAnchoring.StructureDelegatingAnchoring
import breeze.config.{Configuration, CommandLineParser, Help}
import java.io.File
import breeze.util._
import epic.parser.ParseExtractionException
import epic.trees.ProcessedTreebank
import epic.trees.TreeInstance
import epic.parser.ViterbiDecoder
import epic.parser.ParserParams.XbarGrammar
import breeze.collection.mutable.TriangularArray
import epic.constraints.ChartConstraints.UnifiedFactory

/**
 * Finds the best tree (relative to the gold tree) s.t. it's reacheable given the current anchoring.
 * Best is measured as number of correct labeled spans, as usual. If the given treebank symbol is correct,
 * bonus points can be awarded for getting the right refinement.
 *
 * On the training set, the "best" reachable tree will not always (~5% of the time) be the correct tree,
 * because pruning will remove the right answer. We don't want to try to train towards an unreachable tree,
 * because the training algorithm will do bad things. Instead, we want the best possible tree that
 * our parser could conceivably produce. That is why this class exists.
 *
 * If backupGrammar is provided, it will be used to find such a tree in the case that no tree can be
 * found with grammar (given the current constraints).
 *
 * Typically, the first grammar will be a treebank grammar that has no horizontal markovization (i.e.
 * it is not forgetfully binarized) and it also remembers the functional tags like -TMP.
 * The backup grammar is usually the grammar
 * with which the pruning masks were produced; because of the way we prune, that parser will always
 * be able to find a tree (assuming that it was able to find a tree without pruning.)
 *
 * TODO: should be a cascade of grammars
 *
 * @author dlwh
 */
class OracleParser[L, L2, W](val grammar: SimpleGrammar[L, L2, W], backupGrammar: Optional[SimpleGrammar[L, L2, W]] = None) extends SerializableLogging {
  private val cache = CacheBroker().make[IndexedSeq[W], BinarizedTree[L2]]("OracleParser")

  private var problems  = 0
  private var total = 0

  def forTree(tree: BinarizedTree[L2],
              words: IndexedSeq[W],
              constraints: ChartConstraints[L]) = try {
    val projectedTree: BinarizedTree[L] = tree.map(grammar.refinements.labels.project)
    cache.getOrElseUpdate(words, {
      val treeconstraints = ChartConstraints.fromTree(grammar.topology.labelIndex, projectedTree)
      if (constraints.top.containsAll(treeconstraints.top) && constraints.bot.containsAll(treeconstraints.bot)) {
        synchronized(total += 1)
        tree
      } else try {

        logger.warn {
          val ratio = synchronized {
            problems += 1
            total += 1
            problems * 1.0 / total
          }
          f"Gold tree for $words not reachable. $ratio%.2f are bad so far. "
        }

        (IndexedSeq(grammar) ++ backupGrammar.iterator).iterator.flatMap { grammar =>
          try {
            val w = words
            val marg = makeGoldPromotingAnchoring(grammar, w, tree, treeconstraints, constraints).maxMarginal

            val closest: BinarizedTree[(L, Int)] = new ViterbiDecoder[L, W]().extractMaxDerivationParse(marg)

            val globalizedClosest: BinarizedTree[L2] = closest.map({
              grammar.refinements.labels.globalize(_: L, _: Int)
            }.tupled)
            logger.trace {
              s"Reachable: ${globalizedClosest.render(words, newline = true)}\nGold:${tree.render(words, newline = true)}"
            }
            Some(globalizedClosest)
          } catch {
            case ex: ParseExtractionException => None
          }
        }.next()

      } catch {
        case ex:NoSuchElementException =>
          logger.error("Couldn't find a tree for $words")
          tree
      }
    })
  } catch {
    case ex: Exception =>
      logger.error(s"while handling projectability for $tree $words: " + ex.getMessage, ex)
      throw ex
  }

  def makeGoldPromotingAnchoring(grammar: SimpleGrammar[L, L2, W],
                                 w: IndexedSeq[W],
                                 tree: BinarizedTree[L2],
                                 treeconstraints: ChartConstraints[L],
                                 constraints: ChartConstraints[L]): GrammarAnchoring[L, W] = {
    import grammar.{refinements => _, _}
    val correctRefinedSpans = GoldTagPolicy.goldTreeForcing(tree.map(grammar.refinements.labels.fineIndex))
    val correctUnaryChains = {
      val arr = TriangularArray.fill(w.length + 1)(IndexedSeq.empty[String])
      for(UnaryTree(a, btree, chain, span) <- tree.allChildren) {
        arr(span.begin, span.end) = chain
      }
      arr
    }
    new StructureDelegatingAnchoring[L, W] {
      protected val baseAnchoring: GrammarAnchoring[L, W] = grammar.anchor(w)
      override def addConstraints(cs: ChartConstraints[L]): GrammarAnchoring[L, W] = {
        makeGoldPromotingAnchoring(grammar, w, tree, treeconstraints, constraints & cs)
      }

      override def sparsityPattern: ChartConstraints[L] = constraints

      def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int): Double = {
        0.1 * baseAnchoring.scoreBinaryRule(begin, split, end, rule, ref)
      }

      def scoreUnaryRule(begin: Int, end: Int, rule: Int, ref: Int): Double = {
        val top = topology.parent(rule)
        val isAllowed = treeconstraints.top.isAllowedLabeledSpan(begin, end, top)
        val isRightRefined = isAllowed && {
          val parentRef = baseAnchoring.parentRefinement(rule, ref)
          val refTop = grammar.refinements.labels.globalize(top, parentRef)
          correctRefinedSpans.isGoldTopTag(begin, end, refTop)
        }

        val isCorrectChain = {
          isAllowed && topology.chain(rule) == correctUnaryChains(begin, end)
        }

        200 * I(isAllowed) + 10 * I(isRightRefined) + 5 * I(isCorrectChain) + 0.1 * baseAnchoring.scoreUnaryRule(begin, end, rule, ref)
      }

      def scoreSpan(begin: Int, end: Int, tag: Int, ref: Int): Double = {
        val globalized = grammar.refinements.labels.globalize(tag, ref)
        200 * I(treeconstraints.bot.isAllowedLabeledSpan(begin, end, tag)) +
          10 * I(correctRefinedSpans.isGoldBotTag(begin, end, globalized)) + 0.1 * baseAnchoring.scoreSpan(begin, end, tag, ref)
      }
    }

  }

  def oracleMarginalFactory(trees: IndexedSeq[TreeInstance[L2, W]]):ParseMarginal.Factory[L, W] = new ParseMarginal.Factory[L, W] {
    val knownTrees = trees.iterator.map(ti => ti.words -> ti.tree).toMap

    def apply(w: IndexedSeq[W], constraints: ChartConstraints[L]): RefinedChartMarginal[L, W] = {
      val refAnchoring = knownTrees.get(w).map { t =>
        val projectedTree: BinarizedTree[L] = t.map(grammar.refinements.labels.project)
        val treeconstraints = ChartConstraints.fromTree(grammar.topology.labelIndex, projectedTree)
        makeGoldPromotingAnchoring(grammar, w, t, treeconstraints, constraints)
      }.getOrElse(grammar.anchor(w))

      RefinedChartMarginal(refAnchoring, true)
    }
  }

  def oracleParser(constraintGrammar: ChartConstraints.Factory[L, W], trees: IndexedSeq[TreeInstance[L2, W]])(implicit deb: Debinarizer[L]): Parser[L, W] = {
    new Parser(grammar.topology, grammar.lexicon, constraintGrammar, oracleMarginalFactory(trees), ViterbiDecoder())
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
        case StandardChartFactory(ref, mm) => StandardChartFactory(ref, maxMarginal = true)
        case x => x
      })


      val uncached = new ParserChartConstraintsFactory[AnnotatedLabel, String](maxMarginalized, {(_:AnnotatedLabel).isIntermediate})
      new CachedChartConstraintsFactory[AnnotatedLabel, String](uncached)(params.cache)
    })

    val refGrammar = GenerativeParser.annotated(initialParser.topology, initialParser.lexicon, ann, trainTrees)

    val annDevTrees = devTrees.map(ann)

    val parser = {
      new OracleParser(refGrammar).oracleParser(constraints, annDevTrees)
    }

    for(ti <- devTrees.par) try {
      val m = TreeMarginal(refGrammar, ti.words, ann.localized(refGrammar.refinements.labels)(ti.tree, ti.words))
      println(m.logPartition)
    } catch {
      case e: Exception => e.printStackTrace()
    }

    val name = params.name

    println("Parser " + name)

    {
      println("Evaluating Parser on dev...")
      val stats = ParserTester.evalParser(devTrees, parser, name+ "-dev", params.threads)
      println("Eval finished. Results:")
      println(stats)
    }

  }

}

