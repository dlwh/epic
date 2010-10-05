package scalanlp.parser

import scalala.tensor.counters.Counters._;
import scalala.tensor.counters.LogCounters;

/**
 * 
 * @author dlwh
 */
object DSLGrammar {
  def grammar(rewrites: DSLGrammarPart*) = {
    val result = PairedDoubleCounter[String,Rule[String]];
    for ( DSLGrammarPart(r,w) <- rewrites) {
      result(r.parent,r) += w;
    }

    new GenerativeGrammar(LogCounters.logNormalizeRows(result));
  }

  def lexicon(words: ((Symbol,String),Double)*) = {
    val result = PairedDoubleCounter[String,String];
    for( ((sym,str),w) <- words) {
      result(sym.name,str) = w;
    }
    new UnsmoothedLexicon(LogCounters.logNormalizeRows(result));
  }

  def simpleGrammar =  grammar(
    'S -> ('N,'V) -> 1.0,
    'V -> ('V, 'N) -> 1.0,
    'N -> ('N,'P) -> 1.0,
    'P -> ('PP, 'N) -> 1.0
  );

  def simpleLexicon =  lexicon(
    'N -> "She" -> 1.0,
    'V -> "eats" -> 1.0,
    'N -> "pizza" -> 1.0,
    'PP -> "without" -> 1.0,
    'N -> "anchovies" -> 1.0
  );

}

final case class DSLGrammarPart(rule: Rule[String], weight: Double);

object DSLGrammarPart {
  implicit def binaryRule(r: ((Symbol,(Symbol,Symbol)),Double)):DSLGrammarPart = r match {
    case ((a, (b,c)),w) => DSLGrammarPart(BinaryRule(a.name,b.name,c.name),w);
  }

  implicit def unaryRule(r: ((Symbol,Symbol),Double)):DSLGrammarPart = r match {
    case ((a, b),w) => DSLGrammarPart(UnaryRule(a.name,b.name),w);
  }
}