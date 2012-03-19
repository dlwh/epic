package scalanlp.trees
/*
 Copyright 2010 David Hall

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


/**
 * Implements HeadFinding as in the Collins parser.
 *
 * Based on Aria's code.
 *
 * @author dlwh
 */
object HeadFinder {

  /**
   * Search direction for the match.
   */
  sealed trait Dir;
  case object Left extends Dir;
  case object Right extends Dir;


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
    val headSet = Set.empty ++ heads;
  }
  private def shr[L](dir: Dir, dis: Boolean, heads: L*) = HeadRule(dir,dis,heads);

  val collinsHeadRules ={
    val basic = Map[String,Seq[HeadRule[String]]](
      "" -> Seq(shr(Left,false,"S","SINV")),
      "ROOT" -> Seq(shr(Left,false,"S","SINV")),
      "TOP" -> Seq(shr(Left,false,"S","SINV")),
      "ADJP" -> Seq(shr(Left, false,
        "NNS", "QP", "NN","$", "ADVP", "JJ", "VBN", "VBG", "ADJP", "JJR",
        "NP", "JJS", "DT","FW", "RBR", "RBS", "SBAR", "RB")),
      "ADVP" -> Seq(shr(Right, false,
        "RB", "RBR", "RBS","FW", "ADVP", "TO", "CD",
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
      "SQ" -> Seq(shr(Left, false, "VBZ", "VBD", "VBP","VB", "MD", "VP", "SQ")),
      "UCP" -> Seq(shr(Right, false)),
      "VP" -> Seq(shr(Left, false,
        "TO", "VBD", "VBN", "MD", "VBZ", "VB", "VBG", "VBP", "AUX",
        "AUXG", "VP", "ADJP", "NN", "NNS", "NP")),
      "WHADJP" -> Seq(shr(Right, false, "CC", "WRB", "JJ", "ADJP")),
      "WHADVP" -> Seq(shr(Right, false, "CC", "WRB")),
      "WHNP" -> Seq(shr(Left, false, "WDT", "WP", "WP$","WHADJP", "WHPP", "WHNP")),
      "WHPP" -> Seq(shr(Right, false, "IN", "TO", "FW")),
      "X" -> Seq(shr(Right, false)),
      "NP" -> Seq(shr(Right, true, "NN", "NNP", "NNPS", "NNS", "NX", "POS", "JJR"),
        shr(Left, false, "NP"),
        shr(Right, false, "CD"),
        shr(Right, true, "JJ", "JJS", "RB", "QP"))
    )
    //add in binarized symbols, and look for the binarized symbol first
    (basic ++ basic.map{ case (k,v) => ("@" + k,v) }) map {  case (lbl,rules) =>
      // add defaults for binarized rules
      if(!lbl.startsWith("@"))
        lbl -> (shr(Left,false, '@' + lbl) +: rules)
      else
        lbl -> (shr(Left,false, lbl) +: rules)
    }
  }

  val collinsHeadFinder = new HeadFinder(collinsHeadRules);
}

import HeadFinder._
import scalanlp.parser.Rule
import scalanlp.parser.UnaryRule
import scalanlp.parser.BinaryRule

/**
 * Can annotate a tree with the head word. Usually
 * you should just use HeadFinder.collinsHeadFinder
 *
 * @author dlwh
 */
class HeadFinder[L](rules: Map[L,Seq[HeadRule[L]]]) extends Serializable {

  def findHeadChild[F](r: Rule[F], proj: F=>L):Int = r match {
    case UnaryRule(_,_) => 0
    case BinaryRule(a,b,c) => 
      findHeadChild(proj(a),proj(b),proj(c))
  }

  def findHeadChild(l: L, children: L*) = {
    val myRules:Seq[HeadRule[L]] = rules.getOrElse(l,Seq(shr(Left,false)));
    val childLabels:IndexedSeq[L] = children.toIndexedSeq

    val answers = for(rule <- myRules.iterator) yield {
      val answer = if(rule.dis) {
        if(rule.dir == Left) childLabels.indexWhere(rule.headSet contains _)
        else childLabels.lastIndexWhere(rule.headSet contains _)
      } else {
        val candidates = for (l <- rule.heads.iterator) yield {
          if(rule.dir == Left) childLabels.indexOf(l)
          else childLabels.lastIndexOf(l)
        };
        candidates.find( _ >= 0) getOrElse -1;
      }

      answer
    }

    answers.find(_ >= 0) getOrElse 0;
  }

  def findHeadChild[F](t: Tree[F], proj: F=>L):Int = {
    findHeadChild(proj(t.label),t.children.map(c => proj(c.label)):_*)
  }

  def findHeadWordIndex[F](t: Tree[F], proj: F=>L):Int = {
    if(t.isLeaf) t.span.start;
    else {
      findHeadWordIndex(t.children(findHeadChild(t,proj)),proj);
    }
  }

  def findHeadTag[F](t: Tree[F], proj: F=>L):F = {
    if(t.isLeaf) t.label
    else {
      findHeadTag(t.children(findHeadChild(t,proj)), proj);
    }
  }

  def findHeadWord[W](t: Tree[L], words: Seq[W]) = words(findHeadWordIndex[L](t,identity _));
  def findHeadWord[F,W](t: Tree[F], words: Seq[W], proj: F=>L) = words(findHeadWordIndex(t,proj));

  def annotateHeadWords[W](t: Tree[L], words: Seq[W]): Tree[(L,W)] = t match {
    case Tree(l,children) if children.length == 0 => Tree(l -> words(t.span.start),IndexedSeq.empty)(t.span)
    case Tree(l,children) if children.length == 0 =>
      val headChild = findHeadChild(t,identity[L] _)
      val rec = children.map(annotateHeadWords(_,words))
      Tree( l -> rec(headChild).label._2, rec)(t.span)
  }

}
