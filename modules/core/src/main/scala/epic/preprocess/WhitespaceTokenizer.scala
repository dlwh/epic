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

/**
 * Tokenizes by splitting on the regular expression \s+.
 *
 * @author dramage
 */
class WhitespaceTokenizer() extends RegexSplitTokenizer("\\s+")

object WhitespaceTokenizer {
  def apply() : WhitespaceTokenizer = new WhitespaceTokenizer
  private val _instance : WhitespaceTokenizer = apply()
  def apply(in : String) : Iterable[String] = _instance(in)
}


