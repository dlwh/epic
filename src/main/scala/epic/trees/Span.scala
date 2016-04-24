package epic.trees

import spire.syntax.cfor

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


class Span(val encoded: Long) extends AnyVal with Serializable {
  def toPair: (Int, Int) = (begin, end)

  def begin = (encoded >>> 32).toInt
  def end = encoded.toInt

  def isEmpty = begin == end
  def nonEmpty = !isEmpty

  def length = end - begin

  def map[U](f: Int => U) = Range(begin, end).map(f)

  @inline
  def foreach(f: Int => Unit) = {
    cfor.cfor(begin)(_ < end, _ +1) { f }
  }

  def iterator = toRange.iterator

  def toRange = Range(begin, end)

  def contains(pos: Int) = pos >= begin && pos < end

  /**
   * Returns true if this and other overlap but containment or equality does not hold.
   * @param other
   * @return
   */
  def crosses(other: Span) = (
    (begin < other.begin && end < other.end && end > other.begin)
    ||  (other.begin < begin && other.end < end && other.end > begin)
  )

//  override def hashCode(): Int = {
//    (begin, end).hashCode()
//  }
//
//  override def equals(obj: scala.Any): Boolean = this match {
//    case other: Span => this.begin == other.begin && this.end == other.end
//    case _ => false
//  }

  /**
  * Return true if this' range contains the other range.
  */
  def contains(other: Span) = {
    begin <= other.begin && end >= other.end
  }

  def toIndexedSeq = Range(begin, end)

  override def toString = s"Span($begin, $end)"
}

object Span {
  def apply(begin: Int, end: Int) = new Span((begin.toLong << 32) | (end.toLong&0xFFFFFFFFL))
  def unapply(span: Span) = Some((span.begin, span.end))
}
