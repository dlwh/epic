package scalanlp.trees

import collection.generic.CanBuildFrom
import collection.mutable.ArrayBuffer

import UnaryChainRemover._
import scalala.tensor.counters.Counters;

/**
 * Removes unaries chains A -> B -> ... -> C, replacing them with A -> C and remembering the most likely
 * chain for any A C pair.
 *
 * @author dlwh
 */
class UnaryChainRemover[L] {
  def removeUnaryChains[W](trees: Iterator[(BinarizedTree[L],Seq[W])]):(IndexedSeq[(BinarizedTree[L],Seq[W])],ChainReplacer[L]) = {
    val buf = new ArrayBuffer[(BinarizedTree[L],Seq[W])];
    val counts = Counters.PairedIntCounter[(L,L),Seq[L]];

    def transform(t: BinarizedTree[L]):BinarizedTree[L] = t match {
      case UnaryTree(l,c) =>
        val (chain,cn) = stripChain(c);
        counts(l -> cn.label, chain) += 1;
        UnaryTree(l,transform(cn))(t.span);
      case BinaryTree(l,lchild,rchild) =>
        BinaryTree(l,transform(lchild),transform(rchild))(t.span);
      case t => t;
    }


    for( (t,w) <- trees) {
      val tn = transform(t)
      buf += (tn -> w);
    }

    (buf,chainReplacer(counts));
  }

  private def stripChain(t: BinarizedTree[L]):(List[L],BinarizedTree[L]) = t match {
    case UnaryTree(l,c) =>
      val (chain,tn) = stripChain(c);
      (l :: chain, tn);
    case _ => (List.empty,t);
  }
}

object UnaryChainRemover {
  private def chainReplacer[L](counts: Counters.PairedIntCounter[(L,L),Seq[L]]) = {
    val maxes = counts.rows.map{ case (labels,map) => (labels -> map.argmax)}.toMap;

    new ChainReplacer[L]  {
      def replacementFor(parent: L, child: L) = maxes.getOrElse(parent -> child, Seq.empty);
    }

  }
  trait ChainReplacer[L] {
    def replacementFor(parent: L, child: L):Seq[L];

    def replaceUnaries(t: Tree[L]) = t match {
      case UnaryTree(a,child) =>
        val c = child.label;
        val replacements = replacementFor(a,c);
        val withChain = replacements.foldRight(child)( (lbl,child) => UnaryTree(lbl,child)(child.span));
        UnaryTree(a,withChain)(t.span);
      case t => t;
    }
  }
}