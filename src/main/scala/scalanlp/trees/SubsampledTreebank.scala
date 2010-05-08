package scalanlp.trees

import java.io.File

class SubsampledTreebank(base: Treebank, numTrain: Int, numDev:Int, numTest: Int) extends Treebank {
  def sections = Seq("train","test","dev");
  def trainSections: Seq[String] = Seq("train");
  def testSections = Seq("test");
  def devSections = Seq("dev");
  def treesFromSection(sec: String) = sec match {
    case "train" => downSample(base.trainTrees,numTrain);
    case "test" => downSample(base.testTrees,numTest);
    case "dev" => downSample(base.devTrees,numDev);
    case _ => error("unknown section: " + sec);
  }

  private def downSample[K](trees: Seq[K], num: Int) = {
    if(num < 0) trees
    else {
      // TODO: maybe randomly sample
      trees.take(num);
    }
  }
}

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