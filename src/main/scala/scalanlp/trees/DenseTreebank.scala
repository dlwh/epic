package scalanlp.trees

import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.io.DataInputStream
import java.io.DataOutputStream
import java.io.File
import java.io.FileOutputStream
import java.util.zip.ZipEntry
import java.util.zip.ZipFile
import java.util.zip.ZipOutputStream
import scalanlp.io.Serialization
import scalanlp.util.Index

object DenseTreebank {
  def compressTreebank(treebank: Treebank, path: File) = {
    val zipWriter = new ZipOutputStream(new BufferedOutputStream(new FileOutputStream(path)));
    val symbolIndex = Index[String]();
    val wordIndex = Index[String]();

    import scalanlp.io.Serialization.Handlers._;
    import scalanlp.io.Serialization.ScalanlpHandlers._;

    val treeHandler = Tree.treeSerializationHandler[Int];
    val wordsHandler = seqHandler[Int];

    for(section <- treebank.sections) {
      zipWriter.putNextEntry(new ZipEntry("sections/"+section));
      val dos = new DataOutputStream(zipWriter);
      for( (tree,words) <- treebank.treesFromSection(section)) {
        val indexedTree = tree.map(symbolIndex index _);
        val indexedWords = words.map(wordIndex index _);
        Serialization.write(true,dos);
        Serialization.write(indexedTree,dos);
        Serialization.write(indexedWords,dos);
      }
      Serialization.write(false,dos);
      dos.flush();
      zipWriter.closeEntry();
    }


    // Metadata:
    zipWriter.putNextEntry(new ZipEntry("metadata"));
    val dos = new DataOutputStream(zipWriter);
    import treebank._;
    Serialization.write(symbolIndex:Index[String],dos);
    Serialization.write(wordIndex:Index[String],dos);
    val metadata :Metadata = (sections,testSections,trainSections,devSections);
    Serialization.write(metadata,dos);
    dos.close();
    zipWriter.close();
  }

  private type Metadata = (Seq[String],Seq[String],Seq[String],Seq[String]);

  def fromZipFile(path: File): Treebank = {
    val zipFile = new ZipFile(path);
    val metadata = zipFile.getEntry("metadata");
    val metaIn = new DataInputStream(zipFile.getInputStream(metadata));

    import scalanlp.io.Serialization.Handlers._;
    import scalanlp.io.Serialization.ScalanlpHandlers._;

    new Treebank {
      val symbolIndex = Serialization.read[Index[String]](metaIn);
      val wordIndex = Serialization.read[Index[String]](metaIn);
      val (sections,testSections,trainSections,devSections) = Serialization.read[Metadata](metaIn);
      metaIn.close();

      def treesFromSection(sec: String): Iterator[(Tree[String],Seq[String])] = {
        val section = zipFile.getEntry("sections/"+sec);
        val sectionIn = zipFile.getInputStream(section);
        val data = new DataInputStream(new BufferedInputStream(sectionIn));
        val iterator = if(!Serialization.read[Boolean](data)) Iterator.empty;
        else Iterator.continually {
          val indexedTree = Serialization.read[Tree[Int]](data);
          val indexedWords = Serialization.read[Seq[Int]](data);
          val continue = Serialization.read[Boolean](data);
          (indexedTree.map(symbolIndex.get _), indexedWords.map(wordIndex.get _),continue);
        } takeWhile( _._3);
        
        for( (tree,words, _ ) <- iterator) yield (tree,words);
      }
    };
  }

  def main(args: Array[String]) = {
    val penn = Treebank.fromPennTreebankDir(new File(args(0)));
    DenseTreebank.compressTreebank(penn, new File(args(1)));
    val dense = DenseTreebank.fromZipFile(new File(args(1)));
    dense.treesFromSection(dense.sections apply (0)) foreach println
  }
}
