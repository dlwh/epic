package epic.trees

import breeze.linalg.Counter2
import java.io.File
import java.io.BufferedReader
import scala.collection.mutable.ArrayBuffer
import java.io.InputStreamReader

class SupervisedHeadFinder[L](innards: SupervisedHeadFinderInnards[L,_]) extends HeadFinder[L] {
  
  def findHeadChild(l: L, children: L*): Int = {
    val head = innards.findHeadChild(l, children.toSeq)
    head
  }
  
  def projected[U](f: U => L): HeadFinder[U] = {
    new SupervisedHeadFinder[U](innards.projected(f))
  }
}

trait SupervisedHeadFinderInnards[L,B] extends Serializable { outer =>
  
  protected def proj(l: L): B
  
  protected def getHeadDB: HeadDB[B]
  
  def findHeadChild(l: L, children: Seq[L]) = {
    val b = proj(l)
    val bChildren = children.map(child => proj(child))
    getHeadDB.findHeadChild(b, bChildren)
  }
  
  def projected[U](f: U => L): SupervisedHeadFinderInnards[U,B] = new SupervisedHeadFinderInnards[U,B] {
    
    protected def proj(l: U): B = outer.proj(f(l))

    protected def getHeadDB = outer.getHeadDB
  }
}

object SupervisedHeadFinderInnards extends Serializable {
  
  def fromHeadDB[B](db: HeadDB[B]): SupervisedHeadFinderInnards[B,B] = new SupervisedHeadFinderInnards[B,B] {
    
    protected def proj(l: B) = l

    protected def getHeadDB: HeadDB[B] = db
  }
}

case class HeadDB[B](symbolArityHeadChildCounts: Counter2[(B,Int),Int,Int],
                     ruleHeadChildCounts: Counter2[(B,Seq[B]),Int,Int],
                     defaultToLeft: Boolean = true) {
  def findHeadChild(l: B, children: Seq[B]): Int = {
    // Manual arg-max because I suck at using Counter2
    var best = -1
    var bestCount = 0
    children.indices.foreach { i =>
      if (ruleHeadChildCounts((l,children),i) > bestCount) {
        best = i
        bestCount = ruleHeadChildCounts((l,children),i)
      }
    }
    if (best == -1) {
      // Else, the rule has never been seen before, so try just the symbol+arity
      children.indices.foreach { i =>
        if (symbolArityHeadChildCounts((l,children.size),i) > bestCount) {
          best = i
          bestCount = ruleHeadChildCounts((l,children),i)
        }
      }
    }
    if (best == -1) {
      best = if (defaultToLeft) 0 else children.size - 1
    }
    best
  }
}

object SupervisedHeadFinder {
  
  def trainHeadFinderFromFiles(ptbPath: String, conllPath: String): HeadFinder[String] = {
    println("Training supervised head finder from PTB trees at " + ptbPath + " and CoNLL trees at " + conllPath)
    val treebank = new SimpleTreebank(new File(ptbPath), new File(ptbPath), new File(ptbPath))
    val process = PartialTreeProcessor()
    val processedTrees = treebank.train.trees.toSeq.map(treeWordsPair => process(treeWordsPair._1))
    val conllTrees = readDepTrees(conllPath)
    if (processedTrees.size != conllTrees.size) {
      throw new RuntimeException("Error in training the supervised head finder: dep and const trees don't match: " +
                                 processedTrees.size + " const but " + conllTrees.size + " dep")
    }
    val symbolArityHeadChildCounts = Counter2[(String,Int),Int,Int]()
    val ruleHeadChildCounts = Counter2[(String,Seq[String]),Int,Int]()

    def rec(tree: Tree[String], conllTree: Seq[Int]) {
      if (!tree.isLeaf) {
        val label = tree.label
        // Find the head under this span which has its label outside the span
        var headIdx = -1
        for (idx <- tree.span.begin until tree.span.end) {
          if (conllTree(idx) < tree.span.begin || conllTree(idx) >= tree.span.end) {
            headIdx = idx
          }
        }
        if (headIdx != -1) {
          // Now identify which child contains the head and make that the head child
          var childIdx = 0
          while (tree.children(childIdx).span.end <= headIdx) {
            childIdx += 1
          }
          symbolArityHeadChildCounts(label -> tree.children.size, childIdx) += 1
          ruleHeadChildCounts(label -> tree.children.map(_.label), childIdx) += 1
        }
        tree.children.foreach(rec(_, conllTree))
      }
    }
    
    var numMatched = 0
    (conllTrees zip processedTrees).foreach { case (conllTree, constTree) =>
      if (conllTree.size == constTree.span.length) {
        rec(constTree, conllTree)
        numMatched += 1
      }
    }
    println("Head finder trained; lengths matched on " + numMatched + " / " + conllTrees.size + " trees")
    new SupervisedHeadFinder[String](SupervisedHeadFinderInnards.fromHeadDB(new HeadDB(symbolArityHeadChildCounts, ruleHeadChildCounts)))
    // HeadFinder.collins
  }
  
  // Reads in a vector of parents, 0-indexed, with the root being -1
  def readDepTrees(conllPath: String): Seq[Seq[Int]] = {
    val in = breeze.io.FileStreams.input(new File(conllPath))
    val br = new BufferedReader(new InputStreamReader(in, "UTF-8"))
    // val sents = new ArrayBuffer[Seq[Seq[String]]]()
    val trees = new ArrayBuffer[Seq[Int]]()
    var currSent = new ArrayBuffer[Seq[String]]
    var i = 0
    while (br.ready()) {
      val line = br.readLine()
      if (line.trim.isEmpty) {
        if (currSent.nonEmpty) {
          trees += conllToTree(currSent)
        }
        currSent = new ArrayBuffer[Seq[String]]
      } else {
        currSent += line.split("\\s+")
      }
      i += 1
    }
    if (currSent.nonEmpty) {
      trees += conllToTree(currSent)
    }
    trees
  }
  
  def conllToTree(sent: Seq[Seq[String]]) = sent.map(_(6).toInt - 1)
}