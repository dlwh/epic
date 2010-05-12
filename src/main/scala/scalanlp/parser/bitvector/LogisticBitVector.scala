package scalanlp.parser.bitvector

import scalala.Scalala._;
import scalala.tensor.counters.LogCounters
import scalala.tensor.counters.Counters.PairedDoubleCounter
import scalala.tensor.counters.LogCounters.LogPairedDoubleCounter
import scalala.tensor.counters.LogCounters.LogDoubleCounter
import scalanlp.trees._
import scalanlp.concurrent.ParallelOps._;
import scalanlp.concurrent.ThreadLocal;
import scalanlp.config.Configuration
import scalanlp.parser.GenerativeGrammar
import scalanlp.parser.UnsmoothedLexicon
import scalanlp.parser._;
import scalala.tensor.counters.Counters._;
import scalanlp.util.Iterators
import scalanlp.util.Log

import scalanlp.parser.splitting.StateSplitting
import scalanlp.parser.splitting.StateSplitting._;
import scalanlp.util.CachedHashCode;

import BitUtils._;


object LogisticBitVector {

  type Substate = Int;
  type Context[L] = (L,Substate);
  sealed trait Decision[+L,+W];
  case class BinaryRuleDecision[L,W](left: L, leftState: Int, right: L, rightState: Int) extends Decision[L,W] with CachedHashCode;
  case class UnaryRuleDecision[L,W](child: L, state: Int) extends Decision[L,W] with CachedHashCode;
  case class WordDecision[L,W](word: W) extends Decision[L,W] with CachedHashCode;

  trait Feature[+L,+W] extends Product with CachedHashCode;

  case class RuleFeature[L](r: Rule[L]) extends Feature[L,Nothing] with CachedHashCode;
  case class LexicalFeature[L,W](parent: L, word: W) extends Feature[L,W] with CachedHashCode;
  case class SingleBitFeature[L,W](lbl: LabelOfBit, bitIndex: Int, toggled: Int) extends Feature[L,W] with CachedHashCode;
  case class UnionFeature[L,W](f1: Feature[L,W], f2: Feature[L,W]) extends Feature[L,W] with CachedHashCode;

  sealed class LabelOfBit(val index: Int);
  case object Parent extends LabelOfBit(0);
  case object LChild extends LabelOfBit(1);
  case object RChild extends LabelOfBit(2);
  case object UChild extends LabelOfBit(3);

  private[bitvector] val bitLabels = Seq(Parent,LChild,RChild,UChild);

}

import LogisticBitVector._

class LogisticBitVector[L,W](treebank: StateSplitting.Treebank[L,W],
                             root: L,
                             val initLexicon: PairedDoubleCounter[L,W],
                             val initProductions: PairedDoubleCounter[L,Rule[L]],
                             numBits: Int,
                             featurizer: Featurizer[L,W]) extends FeaturizedObjectiveFunction {
  type Context = LogisticBitVector.Context[L];
  type Decision = LogisticBitVector.Decision[L,W];
  type Feature = LogisticBitVector.Feature[L,W];
  lazy val numStates = (1 << numBits);


  def decisionsForContext(c: Context): Iterator[Decision] = {
    val lexDecisions = for( (w,_) <- initLexicon(c._1).iterator) yield LogisticBitVector.WordDecision[L,W](w);
    val ruleDecisions:  Iterator[Iterator[Decision]] = for( (r,_) <- initProductions(c._1).iterator) yield r match {
      case BinaryRule(par,left,right) => for(lS <- split(left).iterator; rS <- split(right).iterator)
        yield LogisticBitVector.BinaryRuleDecision(lS._1,lS._2,rS._1,rS._2);
      case UnaryRule(_,child) => for(s <- split(child).iterator)
        yield LogisticBitVector.UnaryRuleDecision(s._1,s._2);
    }

    lexDecisions ++ ruleDecisions.flatten;
  }

  def allContexts: Iterator[Context] = {
    val allLabels = Set.empty ++ initProductions.rows.map{_._1} ++ initLexicon.rows.map{_._1};
    for {
      l <- allLabels.iterator
      state <- split(l).iterator
    } yield state;
  }

  def features(d: Decision, c: Context):Seq[Feature] = {
    featurizer.features(d, c);
  }

  private def split(x: L) = {
    if(x == root) Seq((x,0))
    else {
      val maxStates = BitUtils.roundToNextPowerOfTwo(initLexicon(x).size + 100 * initProductions(x).size);
      (0 until (numStates min maxStates)) map { i => (x,i) };
    }
  }

  def expectedCounts(logThetas: LogPairedDoubleCounter[Context,Decision]) = {
    val (productions,wordProds) = extractGrammarAndLexicon(logThetas);
    val grammar = new ThreadLocal(new GenerativeGrammar(productions));
    val lexicon = new UnsmoothedLexicon(wordProds);
   // val ecounts = treebank.iterator.map { case (t,s) => StateSplitting.expectedCounts(grammar(),lexicon,t map split,s); }
     //     .reduceLeft(_ += _);
    val ecounts = treebank.par(100).mapReduce(
      { case (t,s) => StateSplitting.expectedCounts(grammar(),lexicon,t map split,s); },
      {( _:ExpectedCounts[W]) += (_: ExpectedCounts[W])} ); 

    val ExpectedCounts(binaryRuleCounts,unaryRuleCounts,wordCounts,logProb) = ecounts;
    val ruleCounts = StateSplitting.decodeRules(grammar.get, binaryRuleCounts, unaryRuleCounts);
    val lexCounts = StateSplitting.decodeWords(grammar.get, wordCounts);

    val eCounts = LogPairedDoubleCounter[Context,Decision]();

    for( (c,rules) <- ruleCounts.rows;
        eCtr = eCounts(c);
        (rule,v) <- rules ) rule match {
      case BinaryRule(_, (lchild,lstate), (rchild,rstate)) =>
        eCtr(BinaryRuleDecision(lchild,lstate,rchild,rstate)) = v;
      case UnaryRule(_, (child,state)) =>
        eCtr(UnaryRuleDecision(child,state))  = v;
    }

    for( (c,words:LogDoubleCounter[W]) <- lexCounts.rows;
        eCtr = eCounts(c);
        (w:W,v) <- words )  {
      eCtr(WordDecision(w)) = v;
    }

    (logProb,LogCounters.exp(eCounts));
  }

  def extractParser(logThetas: LogPairedDoubleCounter[Context,Decision],
                    features:DoubleCounter[Feature],
                    logNormalizers: LogDoubleCounter[Context]) = {
    val (prods,words) = extractGrammarAndLexicon(logThetas);
    val grammar = new GenerativeGrammar(prods);
    // words is normalized, and so we need to rescale it so that SimpleLexicon doesn't suck horribly.
   // val lex = new SimpleLexicon(LogCounters.exp(words + Math.log(initLexicon.total)));
    val transposed = LogPairedDoubleCounter[W,Context];
    transposed := words.transpose;
    val tags = Set.empty ++ words.activeKeys.map(_._1);
    // TODO: make less awful
    println{tags.map(t => (t,words(t).size))};
    val openTags = for( t <- tags if words(t).size > 50)  yield t;
    val lex = new BitVectorLexicon[L,W](featurizer, transposed, features, logNormalizers,tags, openTags.toSet);
    val parser = new GenerativeParser[(L,Int),W]((root,0),lex,grammar).map { (t:Tree[(L,Int)]) =>
      t.map(_._1);
    }
    parser;
  }

  def extractGrammarAndLexicon(logThetas: LogPairedDoubleCounter[Context,Decision]) = {
    // NB: Context is (L,Int), and is our parent for the production rules.
    val lexicon = LogPairedDoubleCounter[Context,W]();
    val productions = LogPairedDoubleCounter[Context,Rule[(L,Int)]]();
    for( (c,decCtr) <- logThetas.rows;
         (d,v) <- decCtr
         if v != Double.NegativeInfinity) {
      d match {
        case WordDecision(w) =>
          lexicon(c)(w) = v;
        case UnaryRuleDecision(child,state) =>
          val r = UnaryRule(c,(child,state));
          productions(c)(r) = v;
        case BinaryRuleDecision(lchild,lstate,rchild,rstate) =>
          val r = BinaryRule(c,(lchild,lstate),(rchild,rstate));
          productions(c)(r) = v;
      }
    }
    (productions,lexicon);
  }


  def initialFeatureWeight(feature: Feature) ={
    featurizer.initFeatureWeight(feature).get;
  }

}

object LogisticBitVectorTest extends ParserTester {


  def trainParser(trainTrees: Seq[(BinarizedTree[String],Seq[String])],
                  devTrees: Seq[(BinarizedTree[String],Seq[String])],
                  config: Configuration) = {

    val numBits = config.readIn[Int]("numBits",3);
    val (initLexicon,initProductions) = GenerativeParser.extractCounts(trainTrees.iterator);
    val factory = config.readIn[FeaturizerFactory[String,String]]("featurizerFactory");

    val featurizer = factory.getFeaturizer(config, initLexicon, initProductions);

    val obj = new LogisticBitVector(trainTrees,"",initLexicon,initProductions,numBits, featurizer);
    val iterationsPerEval = config.readIn("iterations.eval",25);
    val maxIterations = config.readIn("iterations.max",100);
    val maxMStepIterations = config.readIn("iterations.mstep.max",80);
    val stateIterator = obj.emIterations(maxMStepIterations = maxMStepIterations);

    var lastLL = Double.NegativeInfinity;
    var numBelowThreshold = 0;
    val maxBelowThreshold = 10;
    var totalIters = 0;
    var converged = false;
    val log = Log.globalLog;
    Iterators.fromProducer[(String,Parser[String,String])] {
      if(totalIters >= maxIterations) None
      else {
        var iter = 0;
        var stateOpt:Option[obj.State] = None;
        while(iter < iterationsPerEval && stateIterator.hasNext && !converged) {
          iter += 1;
          val state = stateIterator.next;
          totalIters += 1;
          log(Log.INFO)("Iteration " + totalIters + " finished.")
          val diff = (lastLL - state.marginalLikelihood)/lastLL;
          if(diff < 1E-4) {
            numBelowThreshold += 1;
          }
          log(Log.INFO)("Marginal likelihood: " + state.marginalLikelihood + " (Diff: " + diff + ")");
          lastLL = state.marginalLikelihood;
          stateOpt = Some(state);
        }
        for( state <- stateOpt) yield {
          val parser = obj.extractParser(state.logThetas,state.weights,state.weightLogNormalizers);
          (totalIters + "", parser);
        }
      }
    }

  }
}