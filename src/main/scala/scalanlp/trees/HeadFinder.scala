package scalanlp.trees

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

import scalanlp.trees.HeadRules.{Dir, Left, Right}
import scalanlp.util.Lens

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
  def left[L]: HeadFinder[L] = new HeadFinder[L](Left, HeadRules.empty)

  def right[L]: HeadFinder[L] = new HeadFinder[L](Right, HeadRules.empty)

  val collins = new HeadFinder(Left, rules = HeadRules.collinsHeadRules);

  implicit def lensed[L, U](hf: HeadFinder[L])(implicit lens: Lens[U, L]) = hf.projected(lens.get(_: U))
}


/**
 * Can annotate a tree with the head word. Usually
 * you should just use HeadFinder.collinsHeadFinder
 *
 * @author dlwh
 */
class HeadFinder[L](defaultDirection: Dir = Left,
                    rules: HeadRules[L]) extends Serializable {


  def findHeadChild(r: Rule[L]): Int = r match {
    case UnaryRule(_, _) => 0
    case BinaryRule(a, b, c) =>
      findHeadChild(a, b, c)
  }

  def findHeadChild(t: Tree[L]): Int = findHeadChild(t.label, t.children.map(c => c.label): _*)

  def findHeadChild(l: L, children: L*) = {
    rules.findMatchIndex(l, children: _*) match {
      case None if defaultDirection == Left => 0
      case None => children.size - 1
      case Some(i) => i
    }
  }

  def findHeadWordIndex(t: Tree[L]): Int = {
    if (t.isLeaf) t.span.start;
    else {
      findHeadWordIndex(t.children(findHeadChild(t)));
    }
  }

  def findHeadTag(t: Tree[L]): L = {
    if (t.isLeaf) t.label
    else {
      findHeadTag(t.children(findHeadChild(t)));
    }
  }

  def findHeadWord[W](t: Tree[L], words: Seq[W]) = words(findHeadWordIndex(t));

  def annotateHeadWords[W](t: Tree[L], words: Seq[W]): Tree[(L, W)] = t match {
    case Tree(l, children) if children.length == 0 => Tree(l -> words(t.span.start), IndexedSeq.empty)(t.span)
    case Tree(l, children) =>
      val headChild = findHeadChild(t)
      val rec = children.map(annotateHeadWords(_, words))
      Tree(l -> rec(headChild).label._2, rec)(t.span)
  }
  
  
  def annotateHeadIndices[W](t: BinarizedTree[L]): BinarizedTree[(L, Int)] = t match {
    case NullaryTree(l) =>  NullaryTree(l -> t.span.start)(t.span)
    case UnaryTree(a, b) => 
      val rec = annotateHeadIndices(b)
      UnaryTree(a -> rec.label._2, rec)(t.span)
    case BinaryTree(a, b, c) =>
      val headChild = findHeadChild(t)
      val recB = annotateHeadIndices(b)
      val recC = annotateHeadIndices(c)
      val head = if(headChild == 0) recB.label._2 else recC.label._2
      BinaryTree(a -> head, recB, recC)(t.span)
  }


  def projected[U](f: U => L): HeadFinder[U] = new HeadFinder[U](defaultDirection, rules.projected(f))

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
case class HeadRule[L](dir: Dir, dis: Boolean, heads: Seq[L]) {
  rule =>
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
      };
      candidates.find(_ >= 0) getOrElse -1;
    }

  }
}

trait HeadRules[L] extends Serializable { outer =>
  protected type InnerLabel

  protected def findRules(l: InnerLabel): Seq[HeadRule[InnerLabel]]

  protected def proj(l: L): InnerLabel

  def findMatchIndex(parent: L, children: L*): Option[Int] = {
    val myRules: Seq[HeadRule[InnerLabel]] = findRules(proj(parent))
    val mapped = children.map(proj)
    val answers = myRules.iterator.map(_.findMatchIndex(mapped: _*)).filterNot(_ == -1)
    if (answers.hasNext) Some(answers.next)
    else None
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
  sealed trait Dir;

  case object Left extends Dir;

  case object Right extends Dir;

  def empty[L]: HeadRules[L] = fromMap[L](Map.empty)

  def fromMap[L](map: Map[L, Seq[HeadRule[L]]]): HeadRules[L] = new HeadRules[L] {
    protected type InnerLabel = L

    protected def findRules(l: L) = map.getOrElse(l, Seq.empty)

    protected def proj(l: L) = l

  }

  private def shr[L](dir: Dir, dis: Boolean, heads: L*) = HeadRule(dir, dis, heads);

  val collinsHeadRules = fromMap {
    val basic = Map[String, Seq[HeadRule[String]]](
      "" -> Seq(shr(Left, false, "S", "SINV")),
      "ROOT" -> Seq(shr(Left, false, "S", "SINV")),
      "TOP" -> Seq(shr(Left, false, "S", "SINV")),
      "ADJP" -> Seq(shr(Left, false,
        "NNS", "QP", "NN", "$", "ADVP", "JJ", "VBN", "VBG", "ADJP", "JJR",
        "NP", "JJS", "DT", "FW", "RBR", "RBS", "SBAR", "RB")),
      "ADVP" -> Seq(shr(Right, false,
        "RB", "RBR", "RBS", "FW", "ADVP", "TO", "CD",
        "JJR", "JJ", "IN", "NP", "JJS", "NN")),
      "CONJP" -> Seq(shr(Right, false, "CC", "RB", "IN")),
      "FRAG" -> Seq(shr(Right, false)),
      "INTJ" -> Seq(shr(Left, false)),
      "LST" -> Seq(shr(Right, false, "LS", ":")),
      "NAC" -> Seq(shr(Left, false,
        "NN", "NNS", "NNP", "NNPS", "NAC", "EX", "$", "CD", "QP",
        "PRP", "VBG", "JJ", "JJS", "JJR", "ADJP", "FW")),
      "NX" -> Seq(shr(Left, false)),
      "PP" -> Seq(shr(Right, false, "IN", "TO", "VBG", "VBN", "RP", "FW")),
      "PRN" -> Seq(shr(Left, false)),
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
      "UCP" -> Seq(shr(Right, false)),
      "VP" -> Seq(shr(Left, false,
        "TO", "VBD", "VBN", "MD", "VBZ", "VB", "VBG", "VBP", "AUX",
        "AUXG", "VP", "ADJP", "NN", "NNS", "NP")),
      "WHADJP" -> Seq(shr(Right, false, "CC", "WRB", "JJ", "ADJP")),
      "WHADVP" -> Seq(shr(Right, false, "CC", "WRB")),
      "WHNP" -> Seq(shr(Left, false, "WDT", "WP", "WP$", "WHADJP", "WHPP", "WHNP")),
      "WHPP" -> Seq(shr(Right, false, "IN", "TO", "FW")),
      "X" -> Seq(shr(Right, false)),
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
          lbl -> (shr(Left, false, '@' + lbl) +: rules)
        else
          lbl -> (shr(Left, false, lbl) +: rules)
    }
  }

}



