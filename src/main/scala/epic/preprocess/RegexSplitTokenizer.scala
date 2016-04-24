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

import java.util.regex.Pattern

import epic.slab._
import epic.slab.Token
import epic.trees.Span

import scala.collection.mutable.ArrayBuffer

/**
 * Splits the input document according to the given pattern.  Does not
 * return the splits.
 *
 * @author dramage
 */
case class RegexSplitTokenizer(pattern : String) extends Tokenizer {

  private val regex = Pattern.compile(pattern)

  override def apply[In](slab: StringSlab[In]): StringSlab[In with Token] = {
    val m = regex.matcher(slab.content)

    val spans = new ArrayBuffer[(Span, Token)]()

    var start = 0
    while (m.find()) {
      val end = m.start()
      if (end - start >= 1)
        spans += (Span(start, end) -> Token(slab.content.substring(start, end)))
      start = m.end()
    }
    if (start != slab.content.length)
      spans += Span(start, slab.content.length) -> Token(slab.content.substring(start, slab.content.length))
    slab.addLayer[Token](spans)
  }

}

