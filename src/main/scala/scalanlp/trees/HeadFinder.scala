package scalanlp.trees

object HeadFinder {

  sealed trait Dir;
  case object Left extends Dir;
  case object Right extends Dir;

  case class HeadRule[L](dir: Dir, dis: Boolean, heads: Seq[L]) {
    val headSet = Set.empty ++ heads;
  }
  private def shr(dir: Dir, dis: Boolean, heads: String*) = HeadRule(dir,dis,heads);

  val collinsHeadRules = Map(
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
  );

  val collinsHeadFinder = new HeadFinder(collinsHeadRules);
}

import HeadFinder._;

class HeadFinder[L](rules: Map[L,Seq[HeadRule[L]]]) {

  def findHeadChild(t: Tree[L]) = {
    val myRules = rules(t.label);

    val answers = for(rule <- myRules.iterator) yield {
      val children = t.children;
      val childLabels = children.map(_.label);

      val answer = if(rule.dis) {
        if(rule.dir == Left) childLabels.findIndexOf(rule.headSet contains _)
        else childLabels.findLastIndexOf(rule.headSet contains _)
      } else {
        val candidates = for (l <- rule.heads.iterator) yield {
          if(rule.dir == Left) childLabels.findIndexOf(_ == l)
          else childLabels.findLastIndexOf(_ == l)
        };
        candidates.find( _ >= 0) getOrElse -1;
      }

      answer
    }

    answers.find(_ >= 0) getOrElse 0;
  }

  def findHeadWordIndex(t: Tree[L]):Int = {
    if(t.isLeaf) t.span.start;
    else {
      findHeadWordIndex(t.children(findHeadChild(t)));
    }
  }

  def findHeadWord[W](t: Tree[L], words: Seq[W]) = words(findHeadWordIndex(t));

  def annotateHeadWords[W](t: Tree[L], words: Seq[W]): Tree[(L,W)] = t.extend{tree => (tree.label,findHeadWord(tree,words)) }
}
