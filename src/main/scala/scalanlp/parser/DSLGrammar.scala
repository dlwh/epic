package scalanlp.parser

import breeze.linalg._
import scalanlp.trees.{LexicalProduction, Rule, BinaryRule, UnaryRule}

/**
 * A DSL Grammar allows us to define simple weighted PCFG's quickly.
 * Useful for testing. See definition of simpleGrammar for an example.
 * @author dlwh
 */
object DSLGrammar {
  def grammar(rewrites: DSLGrammarPart*): AugmentedGrammar[String, String] = {
    val binaryProductions = Counter2[String, BinaryRule[String], Double]
    val unaryProductions = Counter2[String, UnaryRule[String], Double]
    val lexicon = Counter2[String, String, Double]
    rewrites foreach {
      case DSLBRule(br, w) =>
        binaryProductions(br.parent, br) = w
      case DSLURule(ur, w) =>
        unaryProductions(ur.parent, ur) = w
      case DSLLex(a, word, w) =>
        lexicon(a, word) = w
    }


    val grammar = BaseGrammar("S", binaryProductions, unaryProductions)
    val unsmoothed = new UnsmoothedLexicon(lexicon.keysIterator.map{ case (k,v) => LexicalProduction(k,v)}.toSet)
    AugmentedGrammar.fromRefined(RefinedGrammar.generative(grammar, unsmoothed, binaryProductions, unaryProductions, lexicon))
  }

  def simpleGrammar =  grammar(
    'S -> 'Sb -> 1.0,
    'Sb -> ('NPu, 'VPu) -> 1.0,

    'VPu -> 'VPb -> 1.0,
    'VPb -> ('Vu, 'NPu) -> 1.0,

    'NPu -> 'NPb -> 1.0,
    'NPu -> 'N -> 1.0,

    'NPb -> ('Nu, 'PPu) -> 1.0,

    'PPu -> 'PPb -> 1.0,
    'PPb -> ('Pu, 'Nu) -> 1.0,

    'Nu -> 'N -> 1.0,
    'Pu -> 'P -> 1.0,
    'Vu -> 'V -> 1.0,

  // lexicon
    'N -> "She" -> 1.0,
    'V -> "eats" -> 1.0,
    'N -> "pizza" -> 1.0,
    'P -> "without" -> 1.0,
    'N -> "anchovies" -> 1.0
  )

  val lexicon = IndexedSeq(
    LexicalProduction("N","She"),
    LexicalProduction("V","eats"),
    LexicalProduction("N","pizza"),
    LexicalProduction("P","without"),
    LexicalProduction("N","anchovies")
  )

}

/**
 * objects part of the dsl to create grammars nicely. You won't create these directly.
 */
sealed trait DSLGrammarPart
/** You won't use this directly. */
case class DSLBRule(rule: BinaryRule[String], weight: Double) extends DSLGrammarPart
/** You won't use this directly. */
case class DSLURule(rule: UnaryRule[String], weight: Double) extends DSLGrammarPart
/** You won't use this directly. */
case class DSLLex(sym: String, word: String, weight: Double) extends DSLGrammarPart

/**
 * contains implicits to create dsl grammars. You won't use this directly.
 */
object DSLGrammarPart {
  implicit def binaryRule(r: ((Symbol, (Symbol, Symbol)), Double)):DSLGrammarPart = r match {
    case ((a, (b, c)), w) => DSLBRule(BinaryRule(a.name, b.name, c.name), w)
  }

  implicit def unaryRule(r: ((Symbol, Symbol), Double)):DSLGrammarPart = r match {
    case ((a, b), w) => DSLURule(UnaryRule(a.name, b.name, Seq.empty), w)
  }

  implicit def lex(r: ((Symbol, String), Double)):DSLGrammarPart = r match {
    case ((a, b), w) => DSLLex(r._1._1.name, r._1._2, r._2)
  }
}