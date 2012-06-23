package epic.trees

import java.io.File

/**
 * A Treebank that uses a few number of training and test sentences.
 *
 * @author dlwh
 */
class SubsampledTreebank(base: Treebank[String], numTrain: Int, numDev:Int, numTest: Int) extends Treebank[String] {
  def sections = Seq("train","test","dev");

  val test = new Portion("test",Seq("test"));
  val dev = new Portion("dev",Seq("dev"));
  val train = new Portion("train",Seq("train"));

  def treesFromSection(sec: String) = sec match {
    case "train" => downSample(base.train.trees,numTrain);
    case "test" => downSample(base.test.trees,numTest);
    case "dev" => downSample(base.dev.trees,numDev);
    case _ => sys.error("unknown section: " + sec);
  }

  private def downSample[K](trees: Iterator[K], num: Int) = {
    if(num < 0) trees
    else {
      // TODO: maybe randomly sample
      trees.take(num);
    }
  }
}

/**
 * Main class to subsample a treebank; useful for development
 *
 * @author dlwh
 */
object ReduceTreebank {
  def main(args: Array[String]) = {
    val penn = Treebank.fromPennTreebankDir(new File(args(0)));
    val numTrain = args(2).toInt;
    val numTest = args(3).toInt;
    val numDev = args(4).toInt;
    DenseTreebank.compressTreebank(new SubsampledTreebank(penn,numTrain,numDev,numTest), new File(args(1)));
    val dense = DenseTreebank.fromZipFile(new File(args(1)));
    dense.treesFromSection(dense.sections apply (0)) foreach println
  }
}