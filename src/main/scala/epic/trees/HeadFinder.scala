package epic.trees

/*
 Copyright 2012 David Hall

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/

import epic.trees.HeadRules.{Dir, Left, Right}
import breeze.util.Lens

/**
 * Implements HeadFinding as in the Collins parser.
 * You can use HeadFinder.left[L] or right[L] to
 * not use any head rules
 *
 * Based on Aria's code.
 *
 * @author dlwh
 */
object HeadFinder {
  def left[L]: HeadFinder[L] = new RuleBasedHeadFinder[L](Left, HeadRules.empty)
  def right[L]: HeadFinder[L] = new RuleBasedHeadFinder[L](Right, HeadRules.empty)
  val collins = new RuleBasedHeadFinder(Left, rules = HeadRules.collinsHeadRules)
  implicit def lensed[L, U](hf: HeadFinder[L])(implicit lens: Lens[U, L]) = hf.projected(lens.get(_: U))
}

trait HeadFinder[L] {

  def findHeadChild(l: L, children: L*): Int

  def findHeadChild(r: Rule[L]): Int = r match {
    case UnaryRule(_, _, _) => 0
    case BinaryRule(a, b, c) =>
      findHeadChild(a, b, c)
  }

  def findHeadChild(t: Tree[L]): Int = findHeadChild(t.label, t.children.map(c => c.label): _*)

  def findHeadWord[W](t: Tree[L], words: Seq[W]) = words(findHeadWordIndex(t))

  def findHeadWordIndex(t: Tree[L]): Int = {
    if (t.isLeaf) t.span.begin
    else {
      findHeadWordIndex(t.children(findHeadChild(t)))
    }
  }

  def findHeadTag(t: Tree[L]): L = {
    if (t.isLeaf) t.label
    else {
      findHeadTag(t.children(findHeadChild(t)))
    }
  }

  def annotateHeadIndices[W](t: Tree[L]): Tree[(L, Int)] = t match {
    case t:BinarizedTree[L] => annotateHeadIndices(t)
    case Tree(l, children, span) if children.isEmpty => Tree(l -> t.span.begin, IndexedSeq.empty, t.span)
    case Tree(l, children, span) =>
      val headChild = findHeadChild(t)
      val rec = children.map(annotateHeadIndices(_))
      Tree(l -> rec(headChild).label._2, rec, t.span)
  }

  def annotateHeadIndices(t: BinarizedTree[L]): BinarizedTree[(L, Int)] = t match {
    case NullaryTree(l, span) =>  NullaryTree(l -> t.span.begin, t.span)
    case u@UnaryTree(a, b, chain, span) =>
      val rec = annotateHeadIndices(b)
      u.copy(a -> rec.label._2, rec)
    case BinaryTree(a, b, c, span) =>
      val headChild = findHeadChild(t)
      val recB = annotateHeadIndices(b)
      val recC = annotateHeadIndices(c)
      val head = if (headChild == 0) recB.label._2 else recC.label._2
      BinaryTree(a -> head, recB, recC, t.span)
  }

  def annotateHeadTags[W](t: BinarizedTree[L]): BinarizedTree[(L, L)] = t match {
    case NullaryTree(l, span) =>
      NullaryTree(l -> l, t.span)
    case u@UnaryTree(a, b, chain, span) =>
      val rec: BinarizedTree[(L, L)] = annotateHeadTags(b)
      u.copy(a -> rec.label._2, rec)
    case BinaryTree(a, b, c, span) =>
      val headChild = findHeadChild(t)
      val recB: BinarizedTree[(L, L)] = annotateHeadTags(b)
      val recC: BinarizedTree[(L, L)] = annotateHeadTags(c)
      val head = if (headChild == 0) recB.label._2 else recC.label._2
      BinaryTree(a -> head, recB, recC, t.span)
  }

  def projected[U](f: U => L): HeadFinder[U]
}

/**
 * Can annotate a tree with the head word. Usually
 * you should just use HeadFinder.collinsHeadFinder
 *
 * @author dlwh
 */
 @SerialVersionUID(1)
class RuleBasedHeadFinder[L](defaultDirection: Dir = Left, rules: HeadRules[L]) extends HeadFinder[L]  with Serializable {
  override def findHeadChild(l: L, children: L*): Int = {
    val result = rules.findMatchIndex(l, children: _*) match {
      case None if defaultDirection == Left => 0
      case None => children.size - 1
      case Some(i) => i
    }
    result
  }

  def annotateHeadWords[W](t: Tree[L], words: Seq[W]): Tree[(L, W)] = t match {
    case Tree(l, children, span) if children.isEmpty => Tree(l -> words(t.span.begin), IndexedSeq.empty, t.span)
    case Tree(l, children, span) =>
      val headChild = findHeadChild(t)
      val rec = children.map(annotateHeadWords(_, words))
      Tree(l -> rec(headChild).label._2, rec, t.span)
  }

  def projected[U](f: U => L): HeadFinder[U] = new RuleBasedHeadFinder[U](defaultDirection, rules.projected(f))

  def lensed[U](implicit lens: Lens[U, L]) = HeadFinder.lensed(this)
}

/**
 * Based on Aria's comments:
 *
 * Basically, you're looking for the head label by searching in Dir
 * for each parent -> rule expansion.
 *
 * Dir is whether or not to look left to right or right to left
 * Dis determines whether you are looking for the first match
 * of any of the categories, or if you're looking for any match
 * of the first category, then the second, etc. etc.
 *
 */
case class HeadRule[L](dir: Dir, dis: Boolean, heads: Seq[L]) { rule =>
  val headSet = heads.toSet

  /**
   * @param children
   * @return index of head child, or -1 on miss
   */
  def findMatchIndex(children: L*) = {
    if (rule.dis) {
      if (rule.dir == Left) children.indexWhere(rule.headSet)
      else children.lastIndexWhere(rule.headSet contains _)
    } else {
      val candidates = for (l <- rule.heads.iterator) yield {
        if (rule.dir == Left) children.indexOf(l)
        else children.lastIndexOf(l)
      }
      candidates.find(_ >= 0) getOrElse -1
    }
  }
}

@SerialVersionUID(1L)
trait HeadRules[L] extends Serializable { outer =>
  protected type InnerLabel

  protected def findRules(l: InnerLabel): Seq[HeadRule[InnerLabel]]

  protected def proj(l: L): InnerLabel

  def findMatchIndex(parent: L, children: L*): Option[Int] = {
    val myRules: Seq[HeadRule[InnerLabel]] = findRules(proj(parent))
    val mapped = children.map(proj)
    val answers = myRules.view.map(_.findMatchIndex(mapped: _*)).filterNot(_ == -1)
    answers.headOption
  }

  def lensed[U](implicit lens: Lens[U, L]) = projected(lens.get(_: U))

  def projected[U](f: U => L): HeadRules[U] = new HeadRules[U] {
    type InnerLabel = outer.InnerLabel

    protected def proj(l: U): InnerLabel = outer.proj(f(l))

    protected def findRules(l: this.type#InnerLabel) = outer.findRules(l)
  }

}

object HeadRules {

  /**
   * Search direction for the match.
   */
  sealed trait Dir
  case object Left extends Dir
  case object Right extends Dir

  def empty[L]: HeadRules[L] = fromMap[L](Map.empty)

  def fromMap[L](map: Map[L, Seq[HeadRule[L]]]): HeadRules[L] = new HeadRules[L] {
    protected type InnerLabel = L
    protected def findRules(l: L) = map.getOrElse(l, Seq.empty)
    protected def proj(l: L) = l
  }

  private def shr[L](dir: Dir, dis: Boolean, heads: L*) = HeadRule(dir, dis, heads)

  val collinsHeadRules = fromMap[String] {
    val allNonTerms = shr(Right, false, "ROOT", "TOP", "ADJP", "ADVP", "CONJP", "FRAG", "S", "INTJ", "LST", "NAC", "NX", "PP", "PRN", "PRT", "QP", "RRC", "S", "SBAR", "SBARQ", "SINV", "SQ", "UCP", "VP", "WHADJP", "WHADVP", "WHNP", "WHPP", "X", "NML", "NP", "NN", "NNP", "NNPS", "NNS", "VB", "VBZ", "VBG", "VBD", "JJ", "JJR", "JJS", "CC", "VBP", "PRP", "PRP$", "PRPS", "CD", "IN", "TO", "WDT", "WP", "WP$", "WRB", "RB", "SYM", "RB", "UH", "RP", "RBR", "RBS", "DT")
    val allNonLeft = allNonTerms.copy(Left)
    val basic = Map[String, Seq[HeadRule[String]]](
      "" -> Seq(shr(Left, false, "S", "SINV")),
      "ROOT" -> Seq(shr(Left, false, "S", "SINV")),
      "TOP" -> Seq(shr(Left, false, "S", "SINV")),
      "ADJP" -> Seq(shr(Left, false,
        "NNS", "QP", "NN", "$", "JJ", "ADVP", "VBN", "VBG", "ADJP", "JJR",
        "NP", "JJS", "DT", "FW", "RBR", "RBS", "SBAR", "RB")),
      "ADVP" -> Seq(shr(Right, false,
        "RB", "RBR", "RBS", "FW", "ADVP", "TO", "CD",
        "JJR", "JJ", "IN", "NP", "JJS", "NN")),
      "CONJP" -> Seq(shr(Right, false, "CC", "RB", "IN")),
        "FRAG" -> Seq(shr(Right, false, "ROOT", "TOP", "ADJP", "ADVP", "CONJP", "FRAG", "S", "INTJ", "LST", "NAC", "NX", "PP", "PRN", "PRT", "QP", "RRC", "S", "SBAR", "SBARQ", "SINV", "SQ", "UCP", "VP", "WHADJP", "WHADVP", "WHNP", "WHPP", "X", "NML", "NP")),
      "S" -> Seq(shr(Left, false, "TO", "IN", "VP", "S", "SBAR", "ADJP", "UCP", "NP")),
      "INTJ" -> Seq(allNonLeft),
      "LST" -> Seq(shr(Right, false, "LS", ":"), allNonTerms),
      "NAC" -> Seq(shr(Left, false,
        "NN", "NNS", "NNP", "NNPS", "NAC", "EX", "$", "CD", "QP",
        "PRP", "VBG", "JJ", "JJS", "JJR", "ADJP", "FW")),
      "NX" -> Seq(allNonLeft),
      "PP" -> Seq(shr(Right, false, "IN", "TO", "VBG", "VBN", "RP", "FW")),
      "PRN" -> Seq(allNonLeft),
      "PRT" -> Seq(shr(Right, false, "RP")),
      "QP" -> Seq(shr(Left, false,
        "$", "IN", "NNS", "NN", "JJ", "RB", "DT", "CD", "NCD", "QP", "JJR", "JJS")),
      "RRC" -> Seq(shr(Right, false, "VP", "NP", "ADVP", "ADJP", "PP")),
      "S" -> Seq(shr(Left, false, "TO", "IN", "VP", "S", "SBAR", "ADJP", "UCP", "NP")),
      "SBAR" -> Seq(shr(Left, false,
        "WHNP", "WHPP", "WHADVP", "WHADJP", "IN", "DT", "S", "SQ", "SBAR", "FRAG")),
      "SBARQ" -> Seq(shr(Left, false, "SQ", "S", "SINV", "SBARQ", "FRAG")),
      "SINV" -> Seq(shr(Left, false, "VBZ", "VBD", "VBP", "VB", "MD", "VP", "S", "SINV", "ADJP", "NP")),
      "SQ" -> Seq(shr(Left, false, "VBZ", "VBD", "VBP", "VB", "MD", "VP", "SQ")),
      "UCP" -> Seq(allNonTerms),
      "VP" -> Seq(shr(Left, false,
        "TO", "VBD", "VBN", "MD", "VBZ", "VB", "VBG", "VBP", "AUX",
        "AUXG", "VP", "ADJP", "NN", "NNS", "NP")),
      "WHADJP" -> Seq(shr(Right, false, "CC", "WRB", "JJ", "ADJP")),
      "WHADVP" -> Seq(shr(Right, false, "CC", "WRB")),
      "WHNP" -> Seq(shr(Left, false, "WDT", "WP", "WP$", "WHADJP", "WHPP", "WHNP")),
      "WHPP" -> Seq(shr(Right, false, "IN", "TO", "FW")),
      "X" -> Seq(allNonTerms),
      "NML" -> Seq(shr(Right, true, "NN", "NNP", "NNPS", "NNS", "NX", "POS", "JJR"),
        shr(Left, false, "NP"),
        shr(Right, false, "CD"),
        shr(Right, true, "JJ", "JJS", "RB", "QP")),
      "NP" -> Seq(shr(Right, true, "NN", "NNP", "NNPS", "NNS", "NX", "POS", "JJR"),
        shr(Left, false, "NP"),
        shr(Right, false, "CD"),
        shr(Right, true, "JJ", "JJS", "RB", "QP"))
    )

    //add in binarized symbols, and look for the binarized symbol first
    (basic ++ basic.map {
      case (k, v) => ("@" + k, v)
    }) map {
      case (lbl, rules) =>
        // add defaults for binarized rules
        if (!lbl.startsWith("@"))
          lbl -> (shr(Left, false, '@' + lbl) +: rules :+ allNonTerms)
        else
          lbl -> (shr(Left, false, lbl) +: rules :+ allNonTerms)
    } : Map[String, Seq[HeadRule[String]]]
  }

}

/*
object NegraHeadFinder extends HeadFinder[AnnotatedLabel] {
  def findHeadChild(l: AnnotatedLabel, children: AnnotatedLabel*): Int = l.label match {
    case "ISU" =>
      var index = children.indexWhere(a => a.hasAnnotation(FunctionalTag("UC")))
      if (index < 0) {
        children.length - 1
      } else {
        index
      }
    case "DL" =>
      var index = children.indexWhere(a => a.hasAnnotation(FunctionalTag("HD")) || a.hasAnnotation(FunctionalTag("DH")))
      if (index < 0) {
        children.length - 1
      } else {
        index
      }
    case _ =>
      var index = children.indexWhere(a => a.hasAnnotation(FunctionalTag("HD")) || a.hasAnnotation(FunctionalTag("PH")))
      if (index < 0) {
        index = children.length - 1
      }
      index

  }
}
*/
