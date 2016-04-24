package epic.trees

import epic.framework.Feature

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

sealed trait Production[@specialized(Int) +L, +W] extends Feature {
  def parent: L
  def map[A](f: L => A): Production[A, W]
}



sealed trait Rule[@specialized(Int) +L] extends Production[L, Nothing] {
  def parent: L
  def children: Seq[L]
  def symbols = parent +: children
  def map[A](f: L => A): Rule[A]
  def mapChildren[A >: L](f: L => A): Rule[A]
}

@SerialVersionUID(8613629952079423488L)
final case class BinaryRule[@specialized(Int) +L](parent: L, left: L, right: L) extends Rule[L] {
  def children = Seq(left, right)
  def map[A](f: L => A) = BinaryRule(f(parent), f(left), f(right))
  def mapChildren[A >: L](f: L => A) = BinaryRule(parent, f(left), f(right))
}

@SerialVersionUID(8559479322874082992L)
final case class UnaryRule[@specialized(Int) +L](parent: L, child: L, chain: IndexedSeq[String]) extends Rule[L] {
  def children = Seq(child)
  def map[A](f: L => A) = UnaryRule(f(parent), f(child), chain)
  def mapChildren[A >: L](f: L => A) = UnaryRule(parent, f(child), chain)
  def isIdentity = chain.isEmpty && parent == child
}

final case class LexicalProduction[@specialized(Int) +L, +W](parent: L, word: W) extends Production[L, W] {
  def map[A](f: L => A) = LexicalProduction(f(parent), word)
}

case class NullRule[@specialized(Int) +L](parent: L) extends Production[L, Nothing] {
  def map[A](f: (L) => A): NullRule[A] = NullRule(f(parent))
}

object BinaryRule {
  def leftChildFirstOrdering[L:Ordering]:Ordering[BinaryRule[L]] = Ordering.Tuple3[L, L, L].on(br => (br.left, br.right, br.parent))
  def parentFirstOrdering[L:Ordering]:Ordering[BinaryRule[L]] = Ordering.Tuple3[L, L, L].on(br => (br.parent, br.left, br.right))
}

object UnaryRule {
  def childFirstOrdering[L:Ordering]:Ordering[UnaryRule[L]] = Ordering.Tuple2[L, L].on(br => (br.child, br.parent))
  def parentFirstOrdering[L:Ordering]:Ordering[UnaryRule[L]] = Ordering.Tuple2[L, L].on(br => (br.parent, br.child))
}
