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
import Utils._
import shapeless._
import ops.hlist._
import epic.trees.Span

/**
 * Abstract trait for tokenizers, which annotate sentence-segmented
 * text with tokens. The trait offsets the returned tokens according
 * to the Sentence.
 *
 * @author dlwh
 * @author reactormonk
 */
@SerialVersionUID(1)
trait Tokenizer extends AnalysisFunction11[String, Sentence, Token] {
  override def toString() = getClass.getName +"()"

  override def apply(content: String, sentences: Vector[epic.slab.Sentence]): Vector[epic.slab.Token] = {
    sentences.map({ sentence => 
      apply(content.substring(sentence.span.begin, sentence.span.end))
        .map(t => t.offset(sentence.span.begin))
    }).flatten
  }

  def apply(sentence: String): Vector[Token]
}

object Tokenizer {
  def apply(tokenizer: (String => Vector[Token])): Tokenizer = new Tokenizer {
    def apply(sentence: String): Vector[Token] = tokenizer(sentence)
  }
}
