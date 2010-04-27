package scalanlp.parser.bitvector

import scala.collection.mutable.HashMap
import scalala.Scalala._;
import scalala.tensor.counters.LogCounters
import scalala.tensor.counters.Counters.PairedDoubleCounter
import scalala.tensor.counters.LogCounters.LogPairedDoubleCounter
import scalala.tensor.counters.LogCounters.LogDoubleCounter
import scalanlp.trees._
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

  case class RuleFeature[L,W](r: Rule[L]) extends Feature[L,W] with CachedHashCode;
  case class LexicalFeature[L,W](parent: L, word: W) extends Feature[L,W] with CachedHashCode;
  case class SingleBitFeature[L,W](lbl: LabelOfBit, bitIndex: Int, toggled: Int) extends Feature[L,W] with CachedHashCode;
  case class UnionFeature[L,W](feats: Seq[Feature[L,W]]) extends Feature[L,W] with CachedHashCode;

  sealed trait LabelOfBit;
  case object Parent extends LabelOfBit;
  case object LChild extends LabelOfBit;
  case object RChild extends LabelOfBit;
  case object UChild extends LabelOfBit;

  def mkBitStati(numBits: Int, lbl: LabelOfBit, state: Substate) = {
    for( (bit,toggled) <- BitUtils.iterateBits(state,numBits)) yield {
      SingleBitFeature(lbl,bit,toggled);
    }
  } toSeq;
}

import LogisticBitVector._

class LogisticBitVector[L,W](treebank: StateSplitting.Treebank[L,W],
                             root: L,
                             numBits: Int,
                             featurizer: Featurizer[L,W]) extends FeaturizedObjectiveFunction {
  type Context = LogisticBitVector.Context[L];
  type Decision = LogisticBitVector.Decision[L,W];
  type Feature = LogisticBitVector.Feature[L,W];
  lazy val numStates = (1 << numBits);

  lazy val (initLexicon:PairedDoubleCounter[L,W],initProductions:PairedDoubleCounter[L,Rule[L]]) = GenerativeParser.extractCounts(treebank.iterator);

  def decisionsForContext(c: Context): Iterator[Decision] = {
    val lexDecisions = for( (w,_) <- initLexicon(c._1).iterator) yield LogisticBitVector.WordDecision[L,W](w);
    val ruleDecisions:  Iterator[Iterator[Decision]] = for( (r,_) <- initProductions(c._1).iterator) yield r match {
      case BinaryRule(par,left,right) => for(lS <- 0 until numStates iterator; rS <- 0 until numStates iterator)
        yield LogisticBitVector.BinaryRuleDecision(left,lS,right,rS);
      case UnaryRule(_,child) => for(s <- 0 until numStates iterator)
        yield LogisticBitVector.UnaryRuleDecision(child,s);
    }

    lexDecisions ++ ruleDecisions.flatten;
  }

  def allContexts: Iterator[Context] = {
    val allLabels = Set.empty ++ initProductions.rows.map{_._1} ++ initLexicon.rows.map{_._1};
    println(numStates);
    for {
      l <- allLabels.iterator
      state <- 0 until numStates iterator
    } yield (l,state);
  }

  def features(d: Decision, c: Context):Seq[Feature] = {
    featurizer.features(d, c);
  }

  private def split(x: L) = if(x == root) Seq((x,0)) else (0 until (1 << numBits) ) map { i => (x,i) };

  def expectedCounts(logThetas: LogPairedDoubleCounter[Context,Decision]) = {
    val (productions,wordProds) = extractGrammarAndLexicon(logThetas);
    val grammar = new GenerativeGrammar(productions);
    val lexicon = new UnsmoothedLexicon(wordProds);
    val results = for {
      (t:BinarizedTree[L],s) <- treebank.iterator
    } yield StateSplitting.expectedCounts(grammar,lexicon,t map split,s);

    val ExpectedCounts(binaryRuleCounts,unaryRuleCounts,wordCounts,logProb) = results.reduceLeft { _ += _ };
    val ruleCounts = StateSplitting.decodeRules(grammar, binaryRuleCounts, unaryRuleCounts);
    val lexCounts = StateSplitting.decodeWords(grammar, wordCounts);

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

  def extractParser(logThetas: LogPairedDoubleCounter[Context,Decision]) = {
    val (prods,words) = extractGrammarAndLexicon(logThetas);
    val grammar = new GenerativeGrammar(prods);
    // words is normalized, and so we need to rescale it so that SimpleLexicon doesn't suck horribly.
    val lex = new SimpleLexicon(LogCounters.exp(words + Math.log(initLexicon.total)));
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
         lexCtr = lexicon(c);
         prodCtr = productions(c);
         (d,v) <- decCtr) {
      d match {
        case WordDecision(w) =>
          lexCtr(w) = v;
        case UnaryRuleDecision(child,state) =>
          val r = UnaryRule(c,(child,state));
          prodCtr(r) = v;
        case BinaryRuleDecision(lchild,lstate,rchild,rstate) =>
          val r = BinaryRule(c,(lchild,lstate),(rchild,rstate));
          prodCtr(r) = v;
      }
    }
    (productions,lexicon);
  }


  def initialFeatureWeight(feature: Feature) ={
    featurizer.initFeatureWeight(this,feature).get;
  }

}

object LogisticBitVectorTest extends ParserTester {


  def trainParser(trainTrees: Iterable[(BinarizedTree[String],Seq[String])],
                  devTrees: Iterable[(BinarizedTree[String],Seq[String])],
                  config: Configuration) = {

    val numBits = config.readIn("numBits",3);
    val featurizer = config.readIn[Featurizer[String,String]]("featurizer");
    val obj = new LogisticBitVector(trainTrees,"",numBits, featurizer);
    val iterationsPerEval = config.readIn("iterations.eval",10);
    val maxIterations = config.readIn("iterations.max",100);
    val stateIterator = obj.emIterations();

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
          val parser = obj.extractParser(state.logThetas);
          (totalIters + "", parser);
        }
      }
    }

  }
}