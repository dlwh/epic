package epic.features

import epic.trees.SimpleTreebank
import epic.trees.PartialTreeProcessor
import epic.trees.Treebank
import java.io.File
import epic.trees.HeadFinder

object HackyHeadFinderTest {

  def main(args: Array[String]) {
//    val treebank = new SimpleTreebank(new File(ptbPath), new File(ptbPath), new File(ptbPath));
    val treebank = Treebank.fromPennTreebankDir(new File("data/wsj"));
    
    val process = PartialTreeProcessor();
    val treesWords = treebank.train.trees.toSeq;
    val processedTrees = treesWords.map(treeWordsPair => process(treeWordsPair._1));
    val hf = HeadFinder.collins;
    val processedTreesWithIndices = processedTrees.map(hf.annotateHeadIndices(_));
    for (i <- 0 until 10) {
      println(processedTrees(i).render(treesWords(i)._2, false));
      println(processedTreesWithIndices(i).render(treesWords(i)._2, false));
    }
    
//    hf.annotateHeadWords(t, words)
//    hf.findHeadChild(l, children);
//    val trees = 
  }
}