package epic.trees
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
import java.io._
import java.net.URL

import scala.io.Codec

/**
 * A SimpleTreebank can be easily specified by paths to the trees in Penn treebank format
 * @author dlwh
 */
class SimpleTreebank(trainUrls: Map[String,URL],
                     devUrls: Map[String,URL],
                     testUrls: Map[String,URL])(implicit enc: Codec) extends Treebank[String] {
  def this(train:File, dev: File, test: File) = {
    this(Map(train.getName -> train.toURI.toURL),
      Map(dev.getName -> dev.toURI.toURL),
      Map(test.getName -> test.toURI.toURL))
  }

  def this(train:IndexedSeq[File], dev: IndexedSeq[File], test: IndexedSeq[File]) = {
    this(train.map(t => t.getName -> t.toURI.toURL).toMap,
      dev.map(t => t.getName -> t.toURI.toURL).toMap,
      test.map(t => t.getName -> t.toURI.toURL).toMap)

  }

  def treesFromSection(sec: String) = {
    val url = (trainUrls orElse devUrls orElse testUrls)(sec)
    val stream = url.openStream()
    val pennReader = new PennTreeReader(new InputStreamReader(stream))
    pennReader
  }

  val train = Portion("train", trainUrls.keys.toIndexedSeq)
  val test =  Portion("test", testUrls.keys.toIndexedSeq)
  val dev =  Portion("dev", devUrls.keys.toIndexedSeq)

  val sections = train.sections ++ test.sections ++ dev.sections
}

object SimpleTreebank {
  def fromTrainDevTestDirs(baseDir: File, extension: String = ".parse"): Treebank[String] = {

    def listRecursively(f: File): Iterator[File] = {
      f.listFiles().iterator.flatMap {
        case f if f.isDirectory => listRecursively(f)
        case f if f.getName.endsWith(extension) =>  Iterator(f)
        case _ => Iterator.empty
      }
    }

    def mapify(iter: Iterator[File]) = iter.map(f => f.getName -> f.toURI.toURL).toMap

    new SimpleTreebank(mapify(listRecursively(new File(baseDir, "train"))),
      mapify(listRecursively(new File(baseDir, "dev"))),
      mapify(listRecursively(new File(baseDir, "test"))))

  }

  def writeSimpleTreebank(trees: Treebank[String], dir: File) = {
    dir.mkdirs()
    def writeToFile(file: File, trees: Iterator[(Tree[String],IndexedSeq[String])]) = {
      val outTrain = new PrintStream(new BufferedOutputStream(new FileOutputStream(file)))
      for( (tree,words) <- trees) {
        outTrain.println(Trees.Transforms.StandardStringTransform(tree).render(words,false))
      }

      outTrain.close()
    }
    writeToFile(new File(dir,"train"),trees.train.trees)
    writeToFile(new File(dir,"dev"),trees.dev.trees)
    writeToFile(new File(dir,"test"),trees.test.trees)

    new SimpleTreebank(new File(dir,"train"),new File(dir,"dev"), new File(dir,"test"))

  }
}