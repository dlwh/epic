package epic.ontonotes

import epic.framework.Example
import epic.trees.{AnnotatedLabel, Tree}


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

/**
 * Represents an ontonotes document (a single file, or portion thereof)
 */
case class Document(id: String, sentences: IndexedSeq[Sentence]) extends Example[IndexedSeq[OntoAnnotations], IndexedSeq[IndexedSeq[String]]] {
  def dspans = sentences.flatMap(_.dspans)
  def words: IndexedSeq[IndexedSeq[String]] = sentences.map(_.words)
  def features = words
  lazy val label: IndexedSeq[OntoAnnotations] = sentences.map(_.label)
  lazy val trees: IndexedSeq[Tree[AnnotatedLabel]] = sentences.map(_.tree)
  lazy val ner: Map[DSpan, NerType.Value] = sentences.map(_.ner).reduceLeft(_ ++ _)
  lazy val coref: Map[DSpan, Mention] = sentences.map(_.coref).reduceLeft(_ ++ _)
}


