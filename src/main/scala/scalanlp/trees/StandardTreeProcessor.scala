package scalanlp.trees

import breeze.util.Interner


/*
 Copyright 2012 David Hall

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

/**
 *
 * @author dlwh
 */
case class StandardTreeProcessor(headFinder: HeadFinder[AnnotatedLabel] = HeadFinder.collins) extends (Tree[String]=>BinarizedTree[AnnotatedLabel]) {
  import Trees.Transforms._
  private val ens = new EmptyNodeStripper[String]
  private val xox = new XOverXRemover[String]
  private val interner = new Interner[AnnotatedLabel]

  def apply(tree: Tree[String]):BinarizedTree[AnnotatedLabel] = {
    val transformed = xox(ens(tree).get)
    val ann = transformed.map { label =>
      val fields = if(label.startsWith("-")) Array(label) else label.split("[-=]")
      val split = fields.filterNot(s => s.nonEmpty && s.charAt(0).isDigit)
      interner.intern(AnnotatedLabel(split(0).intern,
        features= split.iterator.drop(1).map(_.intern).map(FunctionalTag(_)).toSet
      ))
    }
    val r = Trees.binarize(ann, {(l:AnnotatedLabel) => l.copy("@"+l.label)}, headFinder)
    r.relabelRoot(_ => AnnotatedLabel.TOP)
  }
}
