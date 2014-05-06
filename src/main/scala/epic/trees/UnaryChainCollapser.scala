package epic.trees

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
 * Removes unaries chains A -> B -> ... -> C, replacing them with A -> C and modifying the tree
 * to know about the unaries
 *
 *
 * @author dlwh
 */
object UnaryChainCollapser {

  def collapseUnaryChains(tree: BinarizedTree[AnnotatedLabel], keepChains: Boolean = true): BinarizedTree[AnnotatedLabel] = {

    def transform(t: BinarizedTree[AnnotatedLabel],parentWasUnary:Boolean):BinarizedTree[AnnotatedLabel] = t match {
      case UnaryTree(l,c, _chain, span) =>
        val (chain,cn) = stripChain(c)
        UnaryTree(l,transform(cn,true), if(keepChains) _chain ++ chain.toIndexedSeq else IndexedSeq.empty, t.span)
      case BinaryTree(l,lchild,rchild, span) =>
        if(parentWasUnary) BinaryTree(l,transform(lchild,false),transform(rchild,false), t.span)
        else UnaryTree(l,BinaryTree(l,transform(lchild,false),transform(rchild,false), t.span), IndexedSeq.empty, t.span)
      case NullaryTree(l, span) =>
        if(parentWasUnary) NullaryTree(l, t.span)
        else UnaryTree(l,NullaryTree(l, t.span), IndexedSeq.empty, t.span)
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



