package scalanlp.trees

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

/**
 * Removes unaries chains A -> B -> ... -> C, replacing them with A -> C and modifying C's AnnotatedLabel
 * to add in the unary chain
 *
 *
 * @author dlwh
 */
object UnaryChainRemover {

  def removeUnaryChains(tree: BinarizedTree[AnnotatedLabel]) = {

    def transform(t: BinarizedTree[AnnotatedLabel],parentWasUnary:Boolean):BinarizedTree[AnnotatedLabel] = t match {
      case UnaryTree(l,c, chain, span) =>
        val (chain,cn) = stripChain(c)
        UnaryTree(l,transform(cn,true), chain, t.span)
      case BinaryTree(l,lchild,rchild, span) =>
        if(parentWasUnary) BinaryTree(l,transform(lchild,false),transform(rchild,false), t.span)
        else UnaryTree(l,BinaryTree(l,transform(lchild,false),transform(rchild,false), t.span), Seq.empty, t.span)
      case NullaryTree(l, span) =>
        if(parentWasUnary) NullaryTree(l, t.span)
        else UnaryTree(l,NullaryTree(l, t.span), Seq.empty, t.span)
      case t => t
    }

    transform(tree,true)
  }

  private def stripChain(t: BinarizedTree[AnnotatedLabel]):(List[String],BinarizedTree[AnnotatedLabel]) = t match {
    case UnaryTree(l, c, _, span) =>
      val (chain,tn) = stripChain(c)
      (l.label :: chain, tn)
    case _ => (List.empty,t)
  }
}

trait UnaryChainReplacer[L] {
  def replaceUnaries(t: Tree[L]):Tree[L]
}

object AnnotatedLabelChainReplacer extends UnaryChainReplacer[AnnotatedLabel] {
  def replaceUnaries(t: Tree[AnnotatedLabel]):Tree[AnnotatedLabel] = t match {
    case UnaryTree(a, child, chain, span) if a.label == child.label.label && chain.isEmpty =>
      replaceUnaries(child)
    case UnaryTree(a, child, chain, span) =>
      val deunaried = replaceUnaries(child).asInstanceOf[BinarizedTree[AnnotatedLabel]]
      val withChain = chain.foldRight(deunaried){ (label, child) =>
        UnaryTree(AnnotatedLabel(label), child.asInstanceOf[BinarizedTree[AnnotatedLabel]], Seq.empty, span)
      }
      UnaryTree(a,withChain, Seq.empty, t.span)
    case t@BinaryTree(a, lchild, rchild, span) =>
      BinaryTree(a,
        replaceUnaries(lchild).asInstanceOf[BinarizedTree[AnnotatedLabel]],
        replaceUnaries(rchild).asInstanceOf[BinarizedTree[AnnotatedLabel]], t.span)
    case t => t
  }
}