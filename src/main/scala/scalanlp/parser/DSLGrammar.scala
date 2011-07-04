package scalanlp.parser

import scalala.library.Library
import scalala.tensor.Counter2

/**
 * 
 * @author dlwh
 */
object DSLGrammar {
  def grammar(rewrites: DSLGrammarPart*) = {
    val binaryProductions = Counter2[String,BinaryRule[String], Double];
    val unaryProductions = Counter2[String,UnaryRule[String], Double];
    for ( DSLGrammarPart(r,w) <- rewrites) {
      r match {
        case br@BinaryRule(a,b,c) =>
          binaryProductions(a,br) = w;
        case ur@UnaryRule(a,b) =>
          unaryProductions(a,ur) = w;
      }
    }

    Grammar(Library.logAndNormalizeRows(binaryProductions),Library.logAndNormalizeRows(unaryProductions));
  }

  def lexicon(words: ((Symbol,String),Double)*) = {
    val result = Counter2[String,String,Double];
    for( ((sym,str),w) <- words) {
      result(sym.name,str) = w;
    }
    new UnsmoothedLexicon(Library.logAndNormalizeRows(result));
  }

  def simpleGrammar =  grammar(
    'S -> 'Sb -> 1.0,
    'Sb -> ('NPu,'VPu) -> 1.0,

    'VPu -> 'VPb -> 1.0,
    'VPb -> ('Vu, 'NPu) -> 1.0,

    'NPu -> 'NPb -> 1.0,
    'NPu -> 'N -> 1.0,

    'NPb -> ('Nu,'PPu) -> 1.0,

    'PPu -> 'PPb -> 1.0,
    'PPb -> ('Pu, 'Nu) -> 1.0,

    'Nu -> 'N -> 1.0,
    'Pu -> 'P -> 1.0,
    'Vu -> 'V -> 1.0
  );

  def simpleLexicon =  lexicon(
    'N -> "She" -> 1.0,
    'V -> "eats" -> 1.0,
    'N -> "pizza" -> 1.0,
    'P -> "without" -> 1.0,
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