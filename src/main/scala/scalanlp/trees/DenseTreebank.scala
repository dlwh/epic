package scalanlp.trees
/*
 Copyright 2010 David Hall

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


import breeze.serialization.DataSerialization
import java.util.zip.{ZipEntry, ZipFile, ZipOutputStream}
import java.io._
import breeze.serialization.DataSerialization._
import breeze.util.Index

/**
 * DenseTreebank provides a way of serializing treebanks
 *
 * @author dlwh
 */
object DenseTreebank {
  /**
   * Produces a densetreebank in path from a current Treebank.
   */
  def compressTreebank(treebank: Treebank[String], path: File) = {
    val zipWriter = new ZipOutputStream(new BufferedOutputStream(new FileOutputStream(path)))
    val symbolIndex = Index[String]()
    val wordIndex = Index[String]()

    val treeHandler = Tree.treeSerializationReadWritable[Int]

    for(section <- treebank.sections) {
      zipWriter.putNextEntry(new ZipEntry("sections/"+section))
      val dos = new DataOutputStream(zipWriter)
      for( (tree,words) <- treebank.treesFromSection(section)) {
        val indexedTree = tree.map(symbolIndex index _)
        val indexedWords = words.map(wordIndex index _)
        write(dos,true)
        write(dos,indexedTree)
        write(dos,indexedWords)
      }
      write(dos,false)
      dos.flush()
      zipWriter.closeEntry()
    }


    // Metadata:
    zipWriter.putNextEntry(new ZipEntry("metadata"))
    val dos = new DataOutputStream(zipWriter)
    import treebank._
    write(dos,symbolIndex:Index[String])
    write(dos,wordIndex:Index[String])
    val metadata :Metadata = (sections,test.sections,train.sections,dev.sections)
    write(dos,metadata)
    dos.close()
    zipWriter.close()
  }

  private type Metadata = (Seq[String],Seq[String],Seq[String],Seq[String])

  /**
   * Read in a zip file to a treebank.
   */
  def fromZipFile(path: File): Treebank[String] = {
    val zipFile = new ZipFile(path)
    val metadata = zipFile.getEntry("metadata")
    val metaIn = new DataInputStream(zipFile.getInputStream(metadata))

    new Treebank[String] {
      val symbolIndex = read[Index[String]](metaIn)
      val wordIndex = read[Index[String]](metaIn)
      implicit def seqReadWritable = DataSerialization.seqReadWritable[String]
      implicit def metaReadWritable:ReadWritable[Metadata] = DataSerialization.tuple4ReadWritable(seqReadWritable, seqReadWritable, seqReadWritable, seqReadWritable)
      val (sections,testSections,trainSections,devSections) = read[Metadata](metaIn)
      metaIn.close()

      val test = Portion("test",testSections)
      val dev = Portion("dev",devSections)
      val train = Portion("train",trainSections)

      def treesFromSection(sec: String): Iterator[(Tree[String],Seq[String])] = {
        val iterator = {
          val section = zipFile.getEntry("sections/"+sec)
          val sectionIn = zipFile.getInputStream(section)
          val data = new DataInputStream(new BufferedInputStream(sectionIn))
          val iterator = if(!read[Boolean](data)) Iterator.empty
          else Iterator.continually {
            val indexedTree = read[Tree[Int]](data)(Tree.treeSerializationReadWritable[Int])
            val indexedWords = read[IndexedSeq[Int]](data)(DataSerialization.indexedSeqReadWritable[Int])
            val continue = read[Boolean](data)
            (indexedTree.map(symbolIndex.get _), indexedWords.map(wordIndex.get _),continue)
          } takeWhile( _._3)

          for( (tree,words, _ ) <- iterator) yield (tree,words)
        }

        iterator

      }
    }
  }

  def main(args: Array[String]) = {
    val penn = Treebank.fromPennTreebankDir(new File(args(0)))
    DenseTreebank.compressTreebank(penn, new File(args(1)))
    val dense = DenseTreebank.fromZipFile(new File(args(1)))
    dense.treesFromSection(dense.sections.last) foreach println
  }
}
