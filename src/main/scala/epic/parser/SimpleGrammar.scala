package epic.parser
/*
 Copyright 2012 David Hall

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/
import projections.GrammarRefinements
import epic.trees._
import collection.mutable.ArrayBuffer
import java.io.{FileInputStream, PrintWriter, Writer}
import epic.lexicon._
import scala.io.Source
import breeze.util.Index
import breeze.linalg.{DenseMatrix, Counter2}
import epic.trees.BinaryRule
import epic.trees.UnaryRule
import java.security.MessageDigest
import java.math.BigInteger
import scala.collection.immutable
import epic.constraints.ChartConstraints
import epic.features.EnglishWordClassGenerator

/**
 * A [[epic.parser.SimpleGrammar]] is a grammar where the topology of
 * the grammar (including the refinements) and the scores for the rules
 * does not vary with the input sentence. That is, this is the class for the standard
 * kind of PCFG/WCFG that are normally taught.
 *
 * @author dlwh
 */
@SerialVersionUID(2)
class SimpleGrammar[L, L2, W](val topology: RuleTopology[L],
                              val lexicon: Lexicon[L, W],
                              val refinements: GrammarRefinements[L, L2],
                              val refinedTopology: RuleTopology[L2],
                              val ruleScoreArray: Array[Double],
                              val tagScorer: TagScorer[L2, W]) extends Grammar[L, W] with Serializable {
  def ruleScore(r: Int, ruleRef: Int):Double = {
    val global = refinements.rules.globalize(r, ruleRef)
    ruleScoreArray(global)
  }

  def ruleScore(refinedRule: Int): Double = {
    ruleScoreArray(refinedRule)
  }

  def anchor(w: IndexedSeq[W], constr: ChartConstraints[L]) = new SimpleGrammar.Anchoring(this, w, constr)


  override def withPermissiveLexicon: Grammar[L, W] = new SimpleGrammar(topology, lexicon.morePermissive, refinements, refinedTopology, ruleScoreArray, tagScorer)

  /**
   * Writes a text representation of the grammar to the output.
   * @param out
   */
  def prettyPrint(out: Writer = new PrintWriter(System.out)) = {
    val printout = new PrintWriter(out)
    import printout._

    for( (cr,index) <- refinements.rules.coarseIndex.zipWithIndex; ref <- refinements.rules.refinementsOf(index)) {
      refinements.rules.fineIndex.get(ref) match {
        case BinaryRule(a,b,c) =>
          println(s"$a -> $b $c ${ruleScore(ref)}")
        case UnaryRule(a,b,chain) if chain.isEmpty =>
          println(s"$a -> $b ${ruleScore(ref)}")
        case UnaryRule(a,b,chain) =>
          println(s"$a --${chain.mkString("[","-","]")}--> $b ${ruleScore(ref)}")
      }

    }
  }

  def signature = {
    val md5 = MessageDigest.getInstance("MD5")
    md5.update(index.toString.getBytes("UTF-8"))
    md5.update(labelIndex.toString.getBytes("UTF-8"))
    md5.update(ruleScoreArray.mkString("{", ",", "}").getBytes("UTF-8"))
    val digest = md5.digest()
    val bigInt = new BigInteger(1,digest)
    val hashtext = bigInt.toString(16)
    hashtext
  }

  val insideTensor = SparseRuleTensor.forSimpleGrammarInside(this)
  /** Swaps left child and parent */
  val outsideTensor = SparseRuleTensor.forSimpleGrammarOutside(this)
}

object SimpleGrammar {

  object CloseUnaries extends Enumeration {
    val None, Sum, Viterbi = Value

  }

  private def doCloseUnaries(matrix: DenseMatrix[Double], closureType: CloseUnaries.Value, syms: Index[AnnotatedLabel]): immutable.IndexedSeq[(UnaryRule[AnnotatedLabel], Double)] = closureType match {
    case CloseUnaries.None =>
      val probs = breeze.numerics.log(matrix)
      val numsyms = matrix.rows
      for(i <- 0 until numsyms; j <- 0 until numsyms if probs(i,j) != Double.NegativeInfinity) yield {
        UnaryRule(syms.get(i), syms.get(j), IndexedSeq.empty) -> probs(i, j)
      }
    case CloseUnaries.Sum => ???
    case CloseUnaries.Viterbi =>
      val probs = breeze.numerics.log(matrix)
      val numsyms = matrix.rows
      val next = DenseMatrix.fill(numsyms, numsyms)(-1)
      for {
        k <- 0 until numsyms
        i <- 0 until numsyms
        if probs(i,k) != Double.NegativeInfinity
        j <- 0 until numsyms
        if probs(i, j) < probs(i,k) + probs(k,j)
      } {
        probs(i,j) = probs(i,k) + probs(k,j)
        next(i, j) = k
      }

    def reconstruct(i: Int, j: Int):Vector[Int] = {
      assert(probs(i, j) != Double.NegativeInfinity)
      next(i, j) match {
        case -1 => Vector.empty
        case k => reconstruct(i,k) ++ (k +: reconstruct(k, j))
      }
    }

    for(i <- 0 until numsyms; j <- 0 until numsyms if probs(i,j) != Double.NegativeInfinity) yield {
      UnaryRule(syms.get(i), syms.get(j), reconstruct(i,j).map(syms.get).map(_.label)) -> probs(i, j)
    }

  }

  def parseBerkeleyText(prefix: String,
                        threshold: Double = -12,
                        closeUnaries: CloseUnaries.Value = CloseUnaries.Viterbi): SimpleGrammar[AnnotatedLabel, AnnotatedLabel, String] = {
    val syms = Index[AnnotatedLabel]()
    val symsIn = Source.fromInputStream(new FileInputStream(prefix+".numstates"))
    for( line <- symsIn.getLines) {
      val Array(_sym, numStatesS) = line.split("\\s+")
      val sym: String = preprocessSymbol(_sym)
      for( sub <- 0 until numStatesS.toInt) {
        syms.index(AnnotatedLabel(s"${sym}_$sub"))
      }
    }
    symsIn.close()

    val rules = Index[Rule[AnnotatedLabel]]()
    val ruleScores = new ArrayBuffer[Double]()

    val binaryIn = Source.fromInputStream(new FileInputStream(prefix+".binary"))
    for ( line <- binaryIn.getLines()) {
      val Array(_a,_b,_c, score) = line.split("\\s+")
      val a = if (_a.startsWith("ROOT")) "TOP_0" else preprocessSymbol(_a)
      val b = if (_b.startsWith("ROOT")) "TOP_0" else preprocessSymbol(_b)
      val c = if (_c.startsWith("ROOT")) "TOP_0" else preprocessSymbol(_c)
      val logScore = math.log(score.toDouble)
      if (logScore >= threshold) {
        val ruleId = rules.index(BinaryRule(a,b,c).map(AnnotatedLabel(_)))
        if (ruleId == ruleScores.length)
          ruleScores += logScore
        syms.index(AnnotatedLabel(a))
        syms.index(AnnotatedLabel(b))
        syms.index(AnnotatedLabel(c))
        assert(rules.size == ruleScores.length)
      }

    }
    binaryIn.close()

    val unaryIn = Source.fromInputStream(new FileInputStream(prefix+".unary"))
    val unclosedUnaries: DenseMatrix[Double] = DenseMatrix.eye[Double](syms.size)
    for ( line <- unaryIn.getLines()) {
      val Array(_a, _b,score) = line.split("\\s+")
      val a = if (_a.startsWith("ROOT")) "TOP_0" else preprocessSymbol(_a)
      val b = if (_b.startsWith("ROOT")) "TOP_0" else preprocessSymbol(_b)
      val logScore = math.log(score.toDouble)
      if (logScore >= threshold) {
        val ai = syms(AnnotatedLabel(a))
        val bi = syms(AnnotatedLabel(b))
        require(ai >= 0 && bi >= 0, a + " " + b + " " + syms)
        unclosedUnaries(ai, bi) = score.toDouble
        assert(rules.size == ruleScores.length)
      }
    }
    for ((unary, unaryScore) <- doCloseUnaries(unclosedUnaries, closeUnaries, syms)) {
      rules.index(unary)
      ruleScores += unaryScore
    }
    unaryIn.close()

    val lexIn = Source.fromInputStream(new FileInputStream(prefix+".lexicon"))
    val lexCounts = Counter2[AnnotatedLabel, String, Double]()
    for(line <- lexIn.getLines()) {
      val Array(tag, w, scores) = line.split("\\s", 3)

      for((scoreString, index) <- scores.dropRight(1).drop(1).split(", ").zipWithIndex) {
        val sym = AnnotatedLabel(s"${tag}_$index")
        lexCounts(sym, w) = math.log(scoreString.toDouble)
      }

    }
    lexIn.close()

    def project(l: AnnotatedLabel): AnnotatedLabel = l.copy(label = l.label.takeWhile(_ != '_'))

    val coarseRules = Index(rules.map(_.map(project)))
    val coarseSyms = Index(syms.map(project))

    val coarseGrammar = RuleTopology(AnnotatedLabel.TOP, coarseSyms, coarseRules)
    val fineGrammar = RuleTopology(AnnotatedLabel.TOP, syms, rules)

    val refinements = GrammarRefinements(coarseGrammar, fineGrammar, project _)
    val lexicon = new SignatureLexicon(coarseGrammar.labelIndex, makeAllowedTags(coarseGrammar.labelIndex, lexCounts), {(w:String) => "UNK"+EnglishWordClassGenerator(w)})

    val scorer = new SignatureTagScorer[AnnotatedLabel, String](lexCounts, { (w:String) => "UNK"+EnglishWordClassGenerator(w)})

    Grammar.unanchored[AnnotatedLabel, AnnotatedLabel, String](coarseGrammar, lexicon,
      refinements,
      ruleScores.toArray,
      scorer)
  }

  def preprocessSymbol(_sym: String): String = {
    val sym = if (_sym == "ROOT") "TOP" else if (_sym == "PRT|ADVP") "PRT" else _sym
    sym.replaceAll("ROOT","TOP").replaceAll("PRT\\|ADVP_[0-9]*", "PRT_0")
  }

  private def makeAllowedTags(coarseLabelIndex: Index[AnnotatedLabel], counter: Counter2[AnnotatedLabel, String, Double]): Map[String, Set[Int]] = {
    val map = collection.mutable.Map[String, Set[Int]]().withDefaultValue(Set.empty[Int])
    for ( (l, w) <- counter.keysIterator; proj = coarseLabelIndex(l.copy(l.label.takeWhile(_ != '_')))) {
      assert(proj != -1, l)
      map(w) += proj
    }
    map.toMap
  }

  case class Anchoring[L, L2, W](grammar: SimpleGrammar[L, L2, W], words: IndexedSeq[W], override val sparsityPattern: ChartConstraints[L]) extends ProjectionsGrammarAnchoring[L, L2, W] {

    override def addConstraints(constraints: ChartConstraints[L]): GrammarAnchoring[L, W] = copy(sparsityPattern = sparsityPattern & constraints)

    def topology = grammar.topology
    def lexicon = grammar.lexicon
    def refinements = grammar.refinements
    def refinedTopology: RuleTopology[L2] = grammar.refinedTopology

    private val tagAnchoring = grammar.tagScorer.anchor(words)
    override def toString() = "SimpleRefinedGrammar.Anchoring(...)"

    def scoreSpan(begin: Int, end: Int, label: Int, ref: Int) = {
      val baseScore = if (begin + 1 == end) {
        val fullId = refinements.labels.globalize(label, ref)
        tagAnchoring.scoreTag(begin, refinements.labels.fineIndex.get(fullId))
      } else {
        0.0
      }
      baseScore
    }

    def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int) = {
      grammar.ruleScore(rule, ref)
    }

    def scoreUnaryRule(begin: Int, end: Int, rule: Int, ref: Int) = {
      grammar.ruleScore(rule, ref)
    }

  }

}
