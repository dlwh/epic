package epic.trees

import breeze.util.Interner
import java.io.{ObjectInputStream, IOException}


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
  private def ens = new EmptyNodeStripper[String]
  private def xox = new XOverXRemover[String]
  @transient
  private var interner = new Interner[AnnotatedLabel]
  @transient
  private var functionalTagInterner = new Interner[FunctionalTag]


  // Don't delete.
  @throws(classOf[IOException])
  @throws(classOf[ClassNotFoundException])
  private def readObject(oin: ObjectInputStream) {
    oin.defaultReadObject()
    interner = new Interner
    functionalTagInterner = new Interner
  }


  def apply(tree: Tree[String]):BinarizedTree[AnnotatedLabel] = {
    val transformed = xox(ens(tree).get)
    val ann = transformed.map { label =>
      val fields = splitLabel(label)
      val split = fields.filterNot(s => s.nonEmpty && s.charAt(0).isDigit)
      interner.intern(AnnotatedLabel(split(0).intern,
        features= split.iterator.drop(1).map(tag => functionalTagInterner.intern(FunctionalTag(tag))).toSet
      ))
    }

    def makeIntermediate(l: AnnotatedLabel, tag: AnnotatedLabel) = {
       l.copy("@"+l.label, headTag = Some(tag.baseLabel))
    }

    def extend(a: AnnotatedLabel, sib: Either[AnnotatedLabel, AnnotatedLabel]) = sib match {
      case Left(s) => a.copy(siblings = a.siblings :+ Left(s.baseLabel))
      case Right(s) => a.copy(siblings = a.siblings :+ Right(s.baseLabel))
    }

    val r = Trees.binarize(ann, makeIntermediate, extend, headFinder)
    val x = r.relabelRoot(_ => AnnotatedLabel.TOP)
//    println(tree.toString(newline = true) + "\n" + x.toString(newline = true) +"\n============================================================")
    x
  }

  def splitLabel(label: String): Array[String] = {
    if (label.startsWith("-"))
      Array(label)
    else if (label.contains("#")) {
      val splits = label.split("#").filter(_.nonEmpty)
      val nonmorphSplits = splits.head.split("[-=]")
      val morphSplits = splits.tail.flatMap(_.split("[|]")).filter("_" != _)
      nonmorphSplits ++ morphSplits
    } else {
      label.split("[-=#]")
    }
  }
}
