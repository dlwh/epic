package scalanlp.parser;
/*
 Copyright 2010 David Hall

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


import scalanlp.trees._;


/**
 * A Parser produces a syntactic representation of a sentence, called a [[scalanlp.trees.Tree], which
 * has internal nodes that demarcate syntactic functions
 *
 * @author dlwh
 */
trait Parser[L,W] extends (Seq[W]=>Tree[L]) {
  /**
   * Returns the best parse (calls bestParse) for the sentence
   *
   * @param s the sentence
   */
  def apply(s: Seq[W]) = bestParse(s);

  /**
   * Returns the best parse for the sentence. Optionally takes a [[scalanlp.parser.CoreAnchoring]], which
   * can be used to provide additional information about the weight of particular spans or
   * rules in a sentence.
   *
   * @param s sentence
   */
  def bestParse(s: Seq[W]):Tree[L];
}
