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

import scalanlp.parser.splitting.StateSplitting
import scalanlp.parser.splitting.StateSplitting._;
import scalanlp.util.CachedHashCode;

import BitUtils._;

class LogisticBitVector[L,W](treebank: StateSplitting.Treebank[L,W], root: L, numBits: Int) extends FeaturizedObjectiveFunction {
  type Substate = Int;
  type Context = (L,Substate);
  type Decision = MyDecision;
  type Feature = MyFeature;
  lazy val numStates = (1 << numBits);

  sealed trait MyDecision;
  case class BinaryRuleDecision(left: L, leftState: Int, right: L, rightState: Int) extends MyDecision with CachedHashCode;
  case class UnaryRuleDecision(child: L, state: Int) extends MyDecision with CachedHashCode;
  case class WordDecision(word: W) extends MyDecision with CachedHashCode;

  trait MyFeature;
  case class RuleFeature(r: Rule[L]) extends MyFeature with CachedHashCode;
  case class SingleBitFeature(r: Rule[L], bit: BitStatus) extends MyFeature with CachedHashCode;
  case class BinaryBitFeature(r: Rule[L], bit1: BitStatus, bit2: BitStatus) extends MyFeature with CachedHashCode;

  case class LexicalFeature(parent: L, word: W) extends MyFeature with CachedHashCode;
  case class LexicalBitFeature(parent: L, word: W, bit: BitStatus) extends MyFeature with CachedHashCode;

  sealed trait LabelOfBit;
  case object Parent extends LabelOfBit;
  case object LChild extends LabelOfBit;
  case object RChild extends LabelOfBit;
  case object UChild extends LabelOfBit;

  final case class BitStatus(label: LabelOfBit, bitIndex: Int, toggled: Int);
  lazy val (initLexicon:PairedDoubleCounter[L,W],initProductions:PairedDoubleCounter[L,Rule[L]]) = GenerativeParser.extractCounts(treebank.iterator);

  def decisionsForContext(c: Context): Iterator[Decision] = {
    val lexDecisions = for( (w,_) <- initLexicon(c._1).iterator) yield WordDecision(w);
    val ruleDecisions:  Iterator[Iterator[MyDecision]] = for( (r,_) <- initProductions(c._1).iterator) yield r match {
      case BinaryRule(par,left,right) => for(lS <- 0 until numStates iterator; rS <- 0 until numStates iterator)
        yield BinaryRuleDecision(left,lS,right,rS);
      case UnaryRule(_,child) => for(s <- 0 until numStates iterator)
        yield UnaryRuleDecision(child,s);
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

  def features(d: Decision, c: Context):Seq[Feature] = d match {
    case WordDecision(w) =>
     val bitStates: Iterator[Feature] = for( (bit,toggled) <- BitUtils.iterateBits(c._2,numBits) )
       yield LexicalBitFeature(c._1,w,BitStatus(Parent,bit,toggled));
     (Iterator.single[Feature](LexicalFeature(c._1,w)) ++ bitStates).toSeq;
    case UnaryRuleDecision(child,state) =>
      val rule = UnaryRule(c._1,child);
      val ruleFeature = RuleFeature(rule);
      val childBitStates = mkBitStati(UChild,state);
      val parentBitStates = mkBitStati(Parent,c._2);
      val childFeatures:Seq[Feature] = childBitStates map { SingleBitFeature(rule, _) }
      val parentFeatures:Seq[Feature] = parentBitStates map { SingleBitFeature(rule, _) };
      val binFeatures = for(parent <- parentBitStates iterator; child <- childBitStates iterator)
        yield BinaryBitFeature(rule, parent, child);
      Seq[Feature](ruleFeature) ++ parentFeatures ++ childFeatures ++ binFeatures;
    case BinaryRuleDecision(left,lstate,right,rstate) =>
      val rule = BinaryRule(c._1,left,right);
      val ruleFeature = RuleFeature(rule);
      val lchildBitStates = mkBitStati(LChild,lstate);
      val rchildBitStates = mkBitStati(RChild,rstate);
      val parentBitStates = mkBitStati(Parent,c._2);
      val parentFeatures:Seq[Feature] = parentBitStates map { SingleBitFeature(rule, _) };
      val lchildFeatures:Seq[Feature] = lchildBitStates map { SingleBitFeature(rule, _) }
      val rchildFeatures:Seq[Feature] = rchildBitStates map { SingleBitFeature(rule, _) }
      val lbinFeatures = for(parent <- parentBitStates iterator; child <- lchildBitStates iterator)
        yield BinaryBitFeature(rule, parent, child);
      val rbinFeatures = for(parent <- parentBitStates iterator; child <- rchildBitStates iterator)
        yield BinaryBitFeature(rule, parent, child);
      Seq(ruleFeature) ++ parentFeatures ++ lchildFeatures ++ lbinFeatures ++ rchildFeatures ++ rbinFeatures;
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

  private def mkBitStati(lbl: LabelOfBit, state: Substate) = {
    for( (bit,toggled) <- BitUtils.iterateBits(state,numBits)) yield {
      BitStatus(lbl,bit,toggled);
    }
  } toSeq;

  def initialFeatureWeight(feature: Feature) = feature match {
    case RuleFeature(r) => Math.log(initProductions(r.parent, r));
    case SingleBitFeature(r, bit) => Math.log(Math.random * 0.2 + 0.9)
    case BinaryBitFeature(r, bit1, bit2) => Math.log(Math.random * 0.2 + 0.9)
    case LexicalFeature(parent, word) => Math.log(initLexicon(parent,word));
    case LexicalBitFeature(parent, word, bit) => Math.log(Math.random * 0.2 + 0.9)
  }

}

object LogisticBitVectorTest extends ParserTester {
  def trainParser(trainTrees: Iterable[(BinarizedTree[String],Seq[String])],
                  devTrees: Iterable[(BinarizedTree[String],Seq[String])],
                  config: Configuration):Parser[String,String] = {

    val obj = new LogisticBitVector(trainTrees,"",config.readIn("parser.bits",3));
    val finalThetas = obj.runEM();

    val (prods,words) = obj.extractGrammarAndLexicon(finalThetas);

    val grammar = new GenerativeGrammar(prods);
    val lex = new SimpleLexicon(LogCounters.exp(words));

    new GenerativeParser[(String,Int),String](("",0),lex,grammar).map { (t:Tree[(String,Int)]) =>
      t.map(_._1);
    }
  }
}