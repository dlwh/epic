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


/**
 * Abstract trait for tokenizers, which act as functions from a String
 * to an Iterable[String].  See companion object for instructions on
 * registering new subtypes outside of the current package.
 *
 * @author dramage
 */
@SerialVersionUID(1)
trait Tokenizer extends (String => Iterable[String]) with Serializable {

  override def toString = getClass.getName
}

/**
 * Companion object for Tokenizer that supports automatic TextSerialization
 * of Tokenizer and its subtypes.  Tokenizers not in breeze.text.tokenizers
 * need to call <code>Tokenizer.register[CustomTokenizer]("CustomTokenizer")</code>
 * in order for toString and fromString on Tokenizers to recognize the new type.
 *
 * @author dramage
 */
object Tokenizer  {


}
