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


case class Span(st: Int, e: Int) extends Range(st,e,1)  {
  require(start <= end)


  /**
   * Returns true if this and other overlap but containment or equality does not hold.
   * @param other
   * @return
   */
  def crosses(other: Span) = (
    (start < other.start && end < other.end && end > other.start)
    ||  (other.start < start && other.end < end && other.end > start)
  )



  /**
  * Return true if this' range contains the other range.
  */
  def contains(other:Span) = {
    start <= other.start && end >= other.end
  }

  override def toString = "Span("+start + "," + end + ")"
}
