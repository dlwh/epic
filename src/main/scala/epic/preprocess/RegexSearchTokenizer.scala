/*
 Copyright 2009 David Hall, Daniel Ramage

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
package epic.preprocess

import epic.slab._
import scala.runtime.ScalaRunTime
import epic.trees.Span

/**
 * Finds all occurrences of the given pattern in the document.
 *
 * @author dlwh
 * @author dramage
 */
case class RegexSearchTokenizer(pattern : String) extends Tokenizer {
  private val compiled = pattern.r

  def apply(sentence: String): Vector[Token] = {
    compiled.findAllMatchIn(sentence).map{ m => Token(Span(m.start, m.end))}.toVector
  }
  override def toString: String = ScalaRunTime._toString(this)
}
