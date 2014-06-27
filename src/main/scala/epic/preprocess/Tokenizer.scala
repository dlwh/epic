/*
 Copyright 2009 David Hall, Daniel Ramage

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
package epic.preprocess

import epic.slab._


/**
 * Abstract trait for tokenizers, which annotate sentence-segmented text with tokens. Tokenizers work
 * with both raw strings and [[epic.slab.StringSlab]]s.
 *
 * @author dlwh
 */
@SerialVersionUID(1)
trait Tokenizer extends StringAnalysisFunction[Sentence, Token] with Serializable with (String=>IndexedSeq[String]) {
  override def toString() = getClass.getName +"()"

  def apply(a: String):IndexedSeq[String] = {
    apply(Slab(a).+(Sentence(0, a.length))).iterator[Token].map(_.token).toIndexedSeq
  }

}


