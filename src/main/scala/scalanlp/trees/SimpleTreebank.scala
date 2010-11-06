package scalanlp.trees

import java.net.URL
import io.Source
import java.io._

/**
 * 
 * @author dlwh
 */
class SimpleTreebank(trainUrls: Map[String,URL], devUrls: Map[String,URL], testUrls: Map[String,URL]) extends Treebank {
  def treesFromSection(sec: String) = {
    val url = (trainUrls orElse devUrls orElse testUrls)(sec) ;
    val stream = url.openStream();
    val pennReader = new PennTreeReader();
    val trees = pennReader.readTrees(Source.fromInputStream(stream).mkString(""));
    trees.fold( x=>x, x => error("error in " + url + " " + x.toString)).iterator
  }

  val trainSections = trainUrls.keys.toSeq;
  val testSections = testUrls.keys.toSeq;
  val devSections = devUrls.keys.toSeq;
  val sections = trainSections ++ testSections ++ devSections;
}

object SimpleTreebank {
  def writeSimpleTreebank(trees: Treebank, dir: File) = {
    dir.mkdirs();
    def writeToFile(file: File, trees: Iterator[(Tree[String],Seq[String])]) = {
      val outTrain = new PrintStream(new BufferedOutputStream(new FileOutputStream(file)));
      for( (tree,words) <- trees) {
        outTrain.println(tree.render(words,false));
      }

      outTrain.close();
    }
    writeToFile(new File(dir,"train"),trees.trainTrees);
    writeToFile(new File(dir,"dev"),trees.devTrees);
    writeToFile(new File(dir,"test"),trees.testTrees);

    new SimpleTreebank(Map("train"->new File(dir,"train").toURI.toURL),
      Map("dev"->new File(dir,"dev").toURI.toURL),
      Map("test"->new File(dir,"test").toURI.toURL)
    );

  }
}