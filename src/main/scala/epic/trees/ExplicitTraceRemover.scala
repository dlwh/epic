package epic.trees

import java.io.File

import scala.collection.mutable.ArrayBuffer
import scala.util.Try

/**
 * Removes traces from the word sequence, and makes the tree
 *
 * @author dlwh
 **/
class ExplicitTraceRemover(emptyPosTags: Set[String] = Set("-NONE-"), tracesToKeep: Set[String] = Set("*T*")) {
  def apply(tree: Tree[AnnotatedLabel], words: IndexedSeq[String]):(Tree[AnnotatedLabel], IndexedSeq[String]) = {
    val newWords = ArrayBuffer[String]()
    // list is pairs of (empty constituent, its id)
    def recursive(tree: Tree[AnnotatedLabel]):(Tree[AnnotatedLabel], Seq[(String, Int)]) = {
      tree match {
        case Tree(label, IndexedSeq(Tree(child, IndexedSeq(), _)), span) if emptyPosTags(child.label) =>
          val trace = words(span.begin)
          val ignoreTrace = !tracesToKeep.exists(trace.startsWith)
          // traces either show up as (NP-1 *NONE*) or as (NP *T*-1) need to catch both
          val id = parseId(trace).getOrElse(-1)//.getOrElse(label.index)
          val list = id match {
            case _ if ignoreTrace => List.empty
            case -1 => List.empty
            case _ => List(label.label -> id)
          }
          Tree(label, IndexedSeq.empty, Span(newWords.length, newWords.length)) -> list
        case Tree(label, IndexedSeq(), span) =>
          val newBegin = newWords.length
          newWords ++= span.map(words)

          Tree(label, IndexedSeq.empty, Span(newBegin, newWords.length)) -> List.empty

        case Tree(label, children, span) =>
          val (newChildren, gapsList) = tree.children.map(recursive).unzip
          val resolvedIndices = newChildren.map(_.label.index).toSet
          val gaps = gapsList.flatten
          val unresolvedGaps =  gaps.filterNot(pair => resolvedIndices(pair._2))


          val newLabel = label.copy(siblings = label.siblings ++ unresolvedGaps.map(pair => Left(pair._1)))

          if(unresolvedGaps.nonEmpty) println("QQQ",unresolvedGaps, newLabel)

          Tree(newLabel, newChildren, Span(children.head.span.begin, children.last.span.end))  -> unresolvedGaps
      }

    }

    val (newTree, gaps) = recursive(tree)
    assert(gaps.isEmpty, newTree.render(newWords) + " " + gaps)
    newTree -> newWords
  }

  def parseId(trace: String):Option[Int] = {
    if(trace == "0") {
      None
    } else {
      Try {
        trace.split("-").last.toInt
      }.toOption
    }
  }
}


object ExplicitTraceRemover {
  def main(args: Array[String]):Unit = {

    def listRecursively(f: File): Iterator[File] = {
      f.listFiles().iterator.flatMap {
        case f if f.isDirectory => listRecursively(f)
        case f if f.getName.endsWith(".parse") || f.getName.endsWith(".mrg") =>  Iterator(f)
        case _ => Iterator.empty
      }
    }

    val treeFiles = listRecursively(new File(args(0)))

    val rem = new ExplicitTraceRemover()

    for(f <- treeFiles; (tree, words) <- new PennTreeReader(f)) try {
        println(tree.render(words))
        println("----")
        val (nt, nw) = rem.apply(tree.map(AnnotatedLabel.parseTreebank), words)
        println(nt.render(nw))
        println("==============")
    } catch {
      case e: AssertionError => e.printStackTrace()
    }

  }
}