package epic.trees
package annotations
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
 * More or less the annotator that does Klein&Manning 2003 annotations.
 * @author dlwh
 */

case class KMAnnotator( horizontal: Int = 2, vertical: Int = 2) extends TreeAnnotator[AnnotatedLabel, String, AnnotatedLabel] {

  val pipeline =
    FilterAnnotations[String](Set(FunctionalTag("TMP"))) andThen
    Markovize[String](horizontal,vertical) andThen
    SplitAuxiliary() andThen
    SplitVP() andThen
    SplitIN[String]() andThen
    SplitPossNP[String]() andThen
    AnnotateBaseNP[String]() andThen
    AnnotateRightRecNP[String]() andThen
    MarkNonIdentityUnaries[String]() andThen
    MarkExternalUnaries[String]()  andThen
    DominatesV[String]()

  def apply(tree: BinarizedTree[AnnotatedLabel], words: Seq[String]) = pipeline(tree, words)

}
