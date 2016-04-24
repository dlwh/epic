package epic.trees

import java.io.File

import scala.collection.mutable.ArrayBuffer
import scala.util.Try

/**
 * Removes traces from the word sequence, and makes the tree have empty spans
 *
 * @author dlwh
 **/
class TraceToSlashCategoryConverter extends (Tree[AnnotatedLabel] =>Tree[AnnotatedLabel]) {

  def apply(tree: Tree[AnnotatedLabel]):Tree[AnnotatedLabel] = {
    // list is pairs of (empty constituent, its id)
    def recursive(tree: Tree[AnnotatedLabel], cCommandIndices: Set[Int]):(Tree[AnnotatedLabel], Seq[(String, Int)]) = {
      tree match {
        case Tree(label, IndexedSeq(Tree(child, IndexedSeq(Tree(trace, _, _)), _)), span) if span.isEmpty =>
          // traces either show up as (NP-1 *NONE*) or as (NP *T*-1) need to catch both
          var id = trace.index
          if (id == -1) {
            id = label.index
          }
          val list = id match {
            case -1 => List.empty
            case i if cCommandIndices(i) => List(label.label -> i)
            case _ => List.empty
          }
//          println(tree.toString(true), list)
          Tree(label, IndexedSeq.empty, span) -> list
        case Tree(label, IndexedSeq(), span) =>
          assert(span.nonEmpty)
          Tree(label, IndexedSeq.empty, span) -> List.empty
        case Tree(label, children, span) =>
          val resolvedIndices = children.filterNot(_.span.isEmpty).map(_.label.index).toSet
          val (newChildren, gapsList) = tree.children.filterNot(_.label.label == "-NONE-").map(recursive(_, resolvedIndices ++ cCommandIndices)).unzip
          val gaps: IndexedSeq[(String, Int)] = gapsList.flatten.distinct
          val unresolvedGaps =  gaps.filterNot(pair => resolvedIndices(pair._2))
          val newLabel = label.copy(siblings = label.siblings ++ unresolvedGaps.map(pair => Left(pair._1)))
          // if (unresolvedGaps.nonEmpty) {
          //   println(unresolvedGaps, newLabel)
          // }
          Tree(newLabel, newChildren, span)  -> unresolvedGaps
      }
    }

    val (newTree, gaps) = recursive(tree, Set.empty)
    assert(gaps.isEmpty, gaps + "\n" + newTree.toString(true))
    newTree
  }
}

object TraceToSlashCategoryConverter {
  def main(args: Array[String]):Unit = {

    def listRecursively(f: File): Iterator[File] = {
      f.listFiles().iterator.flatMap {
        case f if f.isDirectory => listRecursively(f)
        case f if f.getName.endsWith(".parse") || f.getName.endsWith(".mrg") =>  Iterator(f)
        case _ => Iterator.empty
      }
    }

    val treeFiles = listRecursively(new File(args(0)))

    val rem = new TraceToSlashCategoryConverter()

    for(f <- treeFiles; (tree, words) <- new PennTreeReader(f)) try {
        println(tree.render(words))
        println("----")
        val nt = rem.apply(tree.map(AnnotatedLabel.parseTreebank))
        println(nt.render(words))
        println("==============")
    } catch {
      case e: AssertionError => e.printStackTrace()
    }

  }
}