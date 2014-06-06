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
import epic.slab.Token
import epic.slab.Sentence

/**
 * Splits the input document according to the given pattern.  Does not
 * return the splits.
 *
 * @author dramage
 */
case class RegexSplitTokenizer(pattern : String) extends Tokenizer {
  val compiled = pattern.r
  override def apply[In <: Sentence](slab: StringSlab[In]): StringSlab[In with Token] = {
    slab.++[Token](slab.iterator[Sentence].flatMap { s =>
      val content = s.in(slab).content
      var last = 0
      compiled.findAllMatchIn(content).map{ m =>
        val ret = Token(last, m.start(0), slab.content.substring(last + s.begin, m.start(0) + s.begin))
        last += m.end(0)
        ret
      }
    })
  }
}

