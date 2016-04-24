package epic.features

import epic.trees.SimpleTreebank
import epic.trees.PartialTreeProcessor
import epic.trees.Treebank
import java.io.File
import epic.trees.HeadFinder
import breeze.linalg.Counter2
import epic.trees.Tree
import breeze.linalg.Counter
import scala.collection.mutable.HashMap

object HackyHeadFinderTest {

  def main(args: Array[String]) {
    // val treebank = new SimpleTreebank(new File(ptbPath), new File(ptbPath), new File(ptbPath))
    val treebank = Treebank.fromPennTreebankDir(new File("data/wsj"))
    
    val process = PartialTreeProcessor()
    val treesWords = treebank.train.trees.toSeq
    val processedTreesWords = treesWords.map(treeWordsPair => (process(treeWordsPair._1), treeWordsPair._2))
    
    println("Training lexicon")
    var sentIdx = 0
    val trainWordTagCounts = Counter2[String,String,Double]
    for ((tree, words) <- processedTreesWords) {
      if (sentIdx % 1000 == 0) {
        println("Sentence: " + sentIdx)
      }
      (tree.leaves zip words).foreach { case (treeLeaf, word) =>
        trainWordTagCounts(treeLeaf.label, words) += 1.0
      }
      sentIdx += 1
    }
    val wordToTagMap = new HashMap[String,String]
    for (word <- trainWordTagCounts.keysIterator.map(_._2)) {
      var bestTag = ""
      var bestTagCount = 0.0
      val tagCounts = trainWordTagCounts(::, word).iterator
      for ((tag, count) <- tagCounts) {
        if (count > bestTagCount) {
          bestTag = tag
          bestTagCount = count
        }
      }
      wordToTagMap.put(word, bestTag)
    }
    println("Done training lexicon")

    val hf = HeadFinder.collins
    val hackyHeadFinder = new RuleBasedHackyHeadFinder
    
    var correct = Counter[String,Int]
    var correctPredTags = Counter[String,Int]
    var total = Counter[String,Int]
    def rec(tree: Tree[(String,Int)], words: Seq[String]): Unit = {
      if (!tree.isLeaf && !tree.label._1.isEmpty) {
        val headIdx = tree.label._2 - tree.begin
        val hhfHead = hackyHeadFinder.findHead(tree.label._1, tree.leaves.map(_.label._1).toSeq)
        val predTags = words.slice(tree.begin, tree.end).map(word => if (wordToTagMap.contains(word)) wordToTagMap(word) else "NN")
        val hhfHeadPredTags = hackyHeadFinder.findHead(tree.label._1, predTags)
        if (hhfHead == headIdx) {
          correct(tree.label._1) += 1
        }
        if (hhfHeadPredTags == headIdx) {
          correctPredTags(tree.label._1) += 1
        } else {
          println(tree.label + " => " + tree.leaves.map(_.label._1).toIndexedSeq + "\n      " + predTags + "; gold = " + headIdx + ", pred (gold) = " + hhfHead + ", pred (pred) = " + hhfHeadPredTags)
        }
        total(tree.label._1) += 1
      }
      if (!tree.isLeaf) {
        tree.children.foreach(rec(_, words))
      }
    }
    
    val devTreesWords = treebank.dev.trees.toSeq.map(treeWordsPair => (hf.annotateHeadIndices(process(treeWordsPair._1)), treeWordsPair._2))
    for (i <- 0 until 100) {
      val tree = devTreesWords(i)._1
      val words = devTreesWords(i)._2
      rec(tree, words)
      // println(tree.render(devTreesWords(i)._2, false))
      // println(processedTrees(i).render(treesWords(i)._2, false))
      // println(processedTreesWithIndices(i).render(treesWords(i)._2, false))
    }
    var totalAcc = 0
    var totalCount = 0
    for (key <- total.keySet) {
      println(key + ": " + correctPredTags(key) + " / " + total(key))
      totalAcc += correctPredTags(key)
      totalCount += total(key)
    }
    println(totalAcc + " / " + totalCount)
  }
}