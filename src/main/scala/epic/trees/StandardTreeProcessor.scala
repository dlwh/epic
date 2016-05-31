package epic.trees

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
case class StandardTreeProcessor(headFinder: HeadFinder[AnnotatedLabel] = HeadFinder.collins, removeTraces: Boolean = true) extends (Tree[AnnotatedLabel]=>BinarizedTree[AnnotatedLabel]) {
  import Trees.Transforms._
  private def xox = new XOverXRemover[AnnotatedLabel]

  private val traceProcessor: (Tree[AnnotatedLabel]) => Tree[AnnotatedLabel] = {
    if (removeTraces) {
      new TraceRemover[AnnotatedLabel, String](_.label == "-NONE-")
    } else {
      new TraceToSlashCategoryConverter
    }
  }

  // Don't delete.
  @throws(classOf[IOException])
  @throws(classOf[ClassNotFoundException])
  private def readObject(oin: ObjectInputStream) {
    oin.defaultReadObject()
  }

  def apply(rawTree: Tree[AnnotatedLabel]):BinarizedTree[AnnotatedLabel] = {
    // val ann = tree.map { AnnotatedLabel.parseTreebank }
    var detraced = traceProcessor(rawTree)
    if (removeTraces) {
      detraced = detraced.map(_.copy(index = -1))
    }
    var transformed = xox(detraced)
    transformed = if (transformed.children.length != 1) {
      Tree(AnnotatedLabel.TOP, IndexedSeq(transformed), transformed.span)
    } else {
      transformed
    }

    def makeIntermediate(l: AnnotatedLabel, tag: AnnotatedLabel) = {
       l.copy("@"+l.label, headTag = Some(tag.baseLabel))
    }

    def extend(a: AnnotatedLabel, sib: Either[AnnotatedLabel, AnnotatedLabel]) = sib match {
      case Left(s) => a.copy(siblings = a.siblings :+ Left(s.baseLabel))
      case Right(s) => a.copy(siblings = a.siblings :+ Right(s.baseLabel))
    }

    val binarized = Trees.binarize(transformed, makeIntermediate, extend, headFinder).relabelRoot(_ => AnnotatedLabel.TOP)
    binarized
  }
}

object StandardTreeProcessor
