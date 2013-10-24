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
import breeze.util.{MutableIndex, Index}
import epic.trees.BinaryRule
import epic.trees.UnaryRule
import breeze.linalg.{DenseMatrix, Counter2}
import chalk.text.analyze.EnglishWordClassGenerator
import epic.trees.BinaryRule
import epic.trees.UnaryRule
import java.security.MessageDigest
import java.math.BigInteger
import scala.collection.immutable

/**
 *
 * @author dlwh
 */
@SerialVersionUID(2)
class SimpleRefinedGrammar[L, L2, W](val grammar: BaseGrammar[L],
                                     val lexicon: Lexicon[L, W],
                                     val refinements: GrammarRefinements[L, L2],
                                     val refinedGrammar: BaseGrammar[L2],
                                     val ruleScoreArray: Array[Array[Double]],
                                     spanScoreArray: Array[Array[Double]],
                                     parentCompatibleRefinements: Array[Array[Array[Int]]],
                                     childCompatibleRefinements: Array[Array[Array[Int]]],
                                     val tagScorer: TagScorer[L2, W]) extends RefinedGrammar[L, W] with Serializable {
  def ruleScore(r: Int, ruleRef: Int):Double = ruleScoreArray(r)(ruleRef)
  def spanScore(l: Int, ref: Int):Double = spanScoreArray(l)(ref)

  def ruleScore(refinedRule: Int): Double = {
    val ref = refinements.rules.localize(refinedRule)
    val parent = refinements.rules.project(refinedRule)
    ruleScoreArray(parent)(ref)
  }


  private val coarseRulesGivenParentRefinement = Array.tabulate(grammar.labelIndex.size) { p =>
    // refinement -> rules
    val result = Array.fill(refinements.labels.refinementsOf(p).size)(ArrayBuffer[Int]())
    for(r <- grammar.indexedBinaryRulesWithParent(p); ref <- 0 until result.length) {
      if(parentCompatibleRefinements(r)(ref).nonEmpty) {
        result(ref) += r
      }
    }

    result.map(_.toArray)
  }


  private val parentRefinementsGivenCoarseRule:Array[Array[Int]] = Array.tabulate(grammar.index.size) { r =>
  // rules -> parent refinements
    refinements.rules.refinementsOf(r).map(refinedGrammar.parent(_)).toSet.toArray.map(refinements.labels.localize(_)).sorted
  }

  private val leftChildRefinementsGivenCoarseRule:Array[Array[Int]] = Array.tabulate(grammar.index.size) { r =>
    if(grammar.index.get(r).isInstanceOf[UnaryRule[_]]) Array.empty
    else  refinements.rules.refinementsOf(r).map(r => refinedGrammar.leftChild(r)).toSet.toArray.map(refinements.labels.localize(_)).sorted
  }

  private val rightChildRefinementsGivenCoarseRule:Array[Array[Int]] = Array.tabulate(grammar.index.size) { r =>
    if(grammar.index.get(r).isInstanceOf[UnaryRule[_]]) Array.empty
    else  refinements.rules.refinementsOf(r).map(r => refinedGrammar.rightChild(r)).toSet.toArray.map(refinements.labels.localize(_)).sorted
  }

  private val leftChildCompatibleRefinements: Array[Array[Array[Int]]] = Array.tabulate(grammar.index.size) { r =>
    if(grammar.index.get(r).isInstanceOf[UnaryRule[L]]) {
      null
    } else {

      val leftChild = grammar.leftChild(r)
      val leftChildRefs = Array.fill(refinements.labels.refinementsOf(leftChild).length){ArrayBuffer[Int]()}
      for(ruleRef <- refinements.rules.refinementsOf(r)) {
        val refParent = refinements.labels.localize(refinements.rules.fineIndex.get(ruleRef).asInstanceOf[BinaryRule[L2]].left)
        leftChildRefs(refParent) += refinements.rules.localize(ruleRef)
      }
      leftChildRefs.map(_.toArray)
    }
  }

  private val rightChildCompatibleRefinements: Array[Array[Array[Int]]] = Array.tabulate(grammar.index.size) { r =>
    if(grammar.index.get(r).isInstanceOf[UnaryRule[L]]) {
      null
    } else {

      val rightChild = grammar.rightChild(r)
      val rightChildRefs = Array.fill(refinements.labels.refinementsOf(rightChild).length){ArrayBuffer[Int]()}
      for(ruleRef <- refinements.rules.refinementsOf(r)) {
        val refParent = refinements.labels.localize(refinements.rules.fineIndex.get(ruleRef).asInstanceOf[BinaryRule[L2]].right)
        rightChildRefs(refParent) += refinements.rules.localize(ruleRef)
      }
      rightChildRefs.map(_.toArray)
    }
  }

  def anchor(w: IndexedSeq[W]) = new RefinedAnchoring[L, W] {
    val tagAnchoring = tagScorer.anchor(w)
    override def toString() = "SimpleAnchoring(...)"
    val grammar = SimpleRefinedGrammar.this.grammar
    val lexicon = SimpleRefinedGrammar.this.lexicon
    def words = w

    def scoreSpan(begin: Int, end: Int, label: Int, ref: Int) = {
      val baseScore = if(begin + 1 == end) {
        val fullId = refinements.labels.globalize(label, ref)
        tagAnchoring.scoreTag(begin, refinements.labels.fineIndex.get(fullId))
      } else {
        0.0
      }
      baseScore + spanScoreArray(label)(ref)
    }

    def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int) = {
      ruleScoreArray(rule)(ref)
    }

    def scoreUnaryRule(begin: Int, end: Int, rule: Int, ref: Int) = {
      ruleScoreArray(rule)(ref)
    }

    def validLabelRefinements(begin: Int, end: Int, label: Int) = {
      refinements.labels.localRefinements(label)
    }

    def validRuleRefinementsGivenParent(begin: Int, end: Int, rule: Int, parentRef: Int) = {
      parentCompatibleRefinements(rule)(parentRef)
    }

    def validRuleRefinementsGivenLeftChild(begin: Int, split: Int, completionBegin: Int, completionEnd: Int, rule: Int, childRef: Int): Array[Int] = {
      leftChildCompatibleRefinements(rule)(childRef)
    }

    def validRuleRefinementsGivenRightChild(completionBegin: Int, completionEnd: Int, split: Int, end: Int, rule: Int, childRef: Int): Array[Int] = {
      rightChildCompatibleRefinements(rule)(childRef)
    }

    def validUnaryRuleRefinementsGivenChild(begin: Int, end: Int, rule: Int, childRef: Int) = {
      childCompatibleRefinements(rule)(childRef)
    }

    def leftChildRefinement(rule: Int, ruleRef: Int) = {
      val refinedRuleId = refinements.rules.globalize(rule, ruleRef)
      refinements.labels.localize(refinedGrammar.leftChild(refinedRuleId))
    }

    def rightChildRefinement(rule: Int, ruleRef: Int) = {
      val refinedRuleId = refinements.rules.globalize(rule, ruleRef)
      refinements.labels.localize(refinedGrammar.rightChild(refinedRuleId))
    }

    def parentRefinement(rule: Int, ruleRef: Int) = {
      val refinedRuleId = refinements.rules.globalize(rule, ruleRef)
      refinements.labels.localize(refinedGrammar.parent(refinedRuleId))
    }

    def childRefinement(rule: Int, ruleRef: Int) = {
      val refinedRuleId = refinements.rules.globalize(rule, ruleRef)
      refinements.labels.localize(refinedGrammar.child(refinedRuleId))
    }

    // TODO: make this not so slow! Doesn't really matter, but still..
    def ruleRefinementFromRefinements(r: Int, refA: Int, refB: Int) = {
      val a = grammar.parent(r)
      val b = grammar.child(r)
      val a2 = refinements.labels.globalize(a, refA)
      val b2 = refinements.labels.globalize(b, refB)
      val rule = UnaryRule(refinements.labels.fineIndex.get(a2), refinements.labels.fineIndex.get(b2), grammar.chain(r))
      val refinedRuleIndex = refinements.rules.fineIndex(rule)
      if(refinedRuleIndex < 0) {
        -1
      } else {
        refinements.rules.localize(refinedRuleIndex)
      }
    }

    def ruleRefinementFromRefinements(r: Int, refA: Int, refB: Int, refC: Int) = {
      val a = grammar.parent(r)
      val b = grammar.leftChild(r)
      val c = grammar.rightChild(r)
      val a2 = refinements.labels.globalize(a, refA)
      val b2 = refinements.labels.globalize(b, refB)
      val c2 = refinements.labels.globalize(c, refC)
      refinements.rules.localize(refinements.rules.fineIndex(BinaryRule(refinements.labels.fineIndex.get(a2),
        refinements.labels.fineIndex.get(b2),
        refinements.labels.fineIndex.get(c2)
      ))  )
    }

    def numValidRefinements(label: Int) = refinements.labels.refinementsOf(label).length
    def numValidRuleRefinements(rule: Int) = refinements.rules.refinementsOf(rule).length

    def validCoarseRulesGivenParentRefinement(a: Int, refA: Int) = coarseRulesGivenParentRefinement(a)(refA)

    def validParentRefinementsGivenRule(begin: Int, splitBegin: Int, splitEnd: Int, end: Int, rule: Int): Array[Int] = {
      parentRefinementsGivenCoarseRule(rule)
    }

    def validLeftChildRefinementsGivenRule(begin: Int, end: Int, completionBegin: Int, completionEnd: Int, rule: Int): Array[Int] = {
      leftChildRefinementsGivenCoarseRule(rule)
    }

    def validRightChildRefinementsGivenRule(completionBegin: Int, completionEnd: Int, begin: Int, end: Int, rule: Int): Array[Int] = {
      rightChildRefinementsGivenCoarseRule(rule)
    }
  }


  /**
   * Writes a text representation of the grammar to the output.
   * @param out
   * @param includeSpanScores
   */
  def prettyPrint(out: Writer = new PrintWriter(System.out), includeSpanScores: Boolean = false) = {
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
    md5.update(ruleScoreArray.map(_.mkString("{", ", ", "}")).mkString("{", ",", "}").getBytes("UTF-8"))
    val digest = md5.digest()
    val bigInt = new BigInteger(1,digest);
    val hashtext = bigInt.toString(16);
    hashtext
  }
}

object SimpleRefinedGrammar {

  object CloseUnaries extends Enumeration {
    val None, Sum, Viterbi = Value

  }


  private def doCloseUnaries(matrix: DenseMatrix[Double], closureType: CloseUnaries.Value, syms: Index[AnnotatedLabel]): immutable.IndexedSeq[(UnaryRule[AnnotatedLabel], Double)] = {
    val probs = breeze.numerics.log(matrix)
    val numsyms = matrix.rows
    val next = DenseMatrix.fill(numsyms, numsyms)(-1)
    closureType match {
      case CloseUnaries.None => // do nothing
      case CloseUnaries.Sum => ???
      case CloseUnaries.Viterbi =>
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
                        threshold: Double = -10,
                        closeUnaries: CloseUnaries.Value = CloseUnaries.Viterbi): SimpleRefinedGrammar[AnnotatedLabel, AnnotatedLabel, String] = {
    val syms = Index[AnnotatedLabel]()
    val symsIn = Source.fromInputStream(new FileInputStream(prefix+".numstates"))
    for( line <- symsIn.getLines) {
      val Array(_sym, numStatesS) = line.split("\\s+")
      val sym = if(_sym == "ROOT") "TOP" else _sym
      if(!sym.startsWith("PRT|ADVP")) {
        for( sub <- 0 until numStatesS.toInt) {
          syms.index(AnnotatedLabel(s"${sym}_$sub"))
        }
      }
    }
    symsIn.close()

    val rules = Index[Rule[AnnotatedLabel]]()
    val ruleScores = new ArrayBuffer[Double]()

    val binaryIn = Source.fromInputStream(new FileInputStream(prefix+".binary"))
    for ( line <- binaryIn.getLines()) {
      val Array(a,b,c, score) = line.split("\\s+")
      if(!Array(a,b,c).exists(_.startsWith("PRT|ADVP"))) {
        val logScore = math.log(score.toDouble)
        if(logScore >= threshold) {
          ruleScores += logScore
          syms.index(AnnotatedLabel(a))
          syms.index(AnnotatedLabel(b))
          syms.index(AnnotatedLabel(c))
          rules.index(BinaryRule(a,b,c).map(AnnotatedLabel(_)))
          assert(rules.size == ruleScores.length)
        }
      }

    }
    binaryIn.close()

    val unaryIn = Source.fromInputStream(new FileInputStream(prefix+".unary"))
    val unclosedUnaries: DenseMatrix[Double] = DenseMatrix.eye[Double](syms.size)
    for ( line <- unaryIn.getLines()) {
      val Array(_a, _b,score) = line.split("\\s+")
      val a = if(_a.startsWith("ROOT")) "TOP_0" else _a
      val b = if(_b.startsWith("ROOT")) "TOP_0" else _b
      if(!Array(a,b).exists(_.startsWith("PRT|ADVP"))) {
        val logScore = math.log(score.toDouble)
        if(logScore >= threshold) {
          val ai = syms(AnnotatedLabel(a))
          val bi = syms(AnnotatedLabel(b))
          require(ai >= 0 && bi >= 0, a + " " + b + " " + syms)
          unclosedUnaries(ai, bi) = score.toDouble
          assert(rules.size == ruleScores.length)
        }
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

    val coarseGrammar = BaseGrammar(AnnotatedLabel.TOP, coarseSyms, coarseRules)
    val fineGrammar = BaseGrammar(AnnotatedLabel.TOP, syms, rules)

    val refinements = GrammarRefinements(coarseGrammar, fineGrammar, project _)
    val lexicon = new SignatureLexicon(coarseGrammar.labelIndex, makeAllowedTags(coarseGrammar.labelIndex, lexCounts), {(w:String) => "UNK-"+EnglishWordClassGenerator(w)})

    val scorer = new SignatureTagScorer[AnnotatedLabel, String](lexCounts, { (w:String) => "UNK-"+EnglishWordClassGenerator(w)})

    RefinedGrammar.unanchored[AnnotatedLabel, AnnotatedLabel, String](coarseGrammar, lexicon,
      refinements,
      ruleScores.toArray,
      new Array(fineGrammar.labelIndex.size),
      scorer)
  }


  private def makeAllowedTags(coarseLabelIndex: Index[AnnotatedLabel], counter: Counter2[AnnotatedLabel, String, Double]): Map[String, Set[Int]] = {
    val map = collection.mutable.Map[String, Set[Int]]().withDefaultValue(Set.empty[Int])
    for ( (l, w) <- counter.keysIterator; proj = coarseLabelIndex(l.copy(l.label.takeWhile(_ != '_')))) {
      assert(proj != -1, l)
      map(w) += proj
    }
    map.toMap
  }
}
