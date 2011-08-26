package scalanlp.trees

import java.net.URL
import java.io._
import io.{Codec, Source}

/**
 * A SimpleTreebank can be easily specified by paths to the trees in Penn treebank format
 * @author dlwh
 */
class SimpleTreebank(trainUrls: Map[String,URL],
                     devUrls: Map[String,URL],
                     testUrls: Map[String,URL])(implicit enc: Codec) extends Treebank {
  def this(train:File, dev: File, test: File) = {
    this(Map(train.getName -> train.toURI.toURL),
      Map(dev.getName -> dev.toURI.toURL),
      Map(test.getName -> test.toURI.toURL))
  };

  def treesFromSection(sec: String) = {
    val url = (trainUrls orElse devUrls orElse testUrls)(sec) ;
    val stream = url.openStream();
    val pennReader = new PennTreeReader(new InputStreamReader(stream));
    pennReader
  }

  val train = Portion("train", trainUrls.keys.toSeq)
  val test =  Portion("test", testUrls.keys.toSeq);
  val dev =  Portion("dev", devUrls.keys.toSeq);

  val sections = train.sections ++ test.sections ++ dev.sections;
}

object SimpleTreebank {
  def writeSimpleTreebank(trees: Treebank, dir: File) = {
    dir.mkdirs();
    def writeToFile(file: File, trees: Iterator[(Tree[String],Seq[String])]) = {
      val outTrain = new PrintStream(new BufferedOutputStream(new FileOutputStream(file)));
      for( (tree,words) <- trees) {
        outTrain.println(Trees.Transforms.StandardStringTransform(tree).render(words,false));
      }

      outTrain.close();
    }
    writeToFile(new File(dir,"train"),trees.train.trees);
    writeToFile(new File(dir,"dev"),trees.dev.trees);
    writeToFile(new File(dir,"test"),trees.test.trees);

    new SimpleTreebank(new File(dir,"train"),new File(dir,"dev"), new File(dir,"test"));

  }
}