package epic.trees

import java.io.File

import scala.collection.mutable.ArrayBuffer
import scala.util.Try

/**
 * Removes traces from the word sequence, and makes the tree have empty spans
 *
 * @author dlwh
 **/
class TraceToSlashCategoryConverter(emptyPosTags: Set[String] = Set("-NONE-")) extends ( (Tree[AnnotatedLabel], IndexedSeq[String]) =>(Tree[AnnotatedLabel], IndexedSeq[String]) ) {

  def apply(tree: Tree[AnnotatedLabel], words: IndexedSeq[String]):(Tree[AnnotatedLabel], IndexedSeq[String]) = {
    val newWords = ArrayBuffer[String]()
    // list is pairs of (empty constituent, its id)
    def recursive(tree: Tree[AnnotatedLabel], cCommandIndices: Set[Int]):(Tree[AnnotatedLabel], Seq[(String, Int)]) = {
      tree match {
        case Tree(label, IndexedSeq(Tree(child, IndexedSeq(), _)), span) if emptyPosTags(child.label) =>
          val trace = words(span.begin)
          val ignoreTrace = false
          // traces either show up as (NP-1 *NONE*) or as (NP *T*-1) need to catch both
          val id = parseId(trace).getOrElse(-1)//.getOrElse(label.index)
          val list = id match {
            case _ if ignoreTrace => List.empty
            case -1 => List.empty
            case i if cCommandIndices(i) => List(label.label -> id)
            case _ => List.empty
          }
          Tree(label, IndexedSeq.empty, Span(newWords.length, newWords.length)) -> list
        case Tree(label, IndexedSeq(), span) =>
          val newBegin = newWords.length
          newWords ++= span.map(words)

          Tree(label, IndexedSeq.empty, Span(newBegin, newWords.length)) -> List.empty

        case Tree(label, children, span) =>
          val resolvedIndices = children.map(_.label.index).toSet
          val (newChildren, gapsList) = tree.children.filterNot(_.label.label == "-NONE-").map(recursive(_, resolvedIndices ++ cCommandIndices)).unzip
          val gaps: IndexedSeq[(String, Int)] = gapsList.flatten.distinct
          val unresolvedGaps =  gaps.filterNot(pair => resolvedIndices(pair._2))


          val newLabel = label.copy(siblings = label.siblings ++ unresolvedGaps.map(pair => Left(pair._1)))


//          if(unresolvedGaps.nonEmpty) {
//            println(unresolvedGaps, newLabel)
//          }

          Tree(newLabel, newChildren, Span(newChildren.head.span.begin, newChildren.last.span.end))  -> unresolvedGaps
      }

    }

    val (newTree, gaps) = recursive(tree, Set.empty)
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
        val (nt, nw) = rem.apply(tree.map(AnnotatedLabel.parseTreebank), words)
        println(nt.render(nw))
        println("==============")
    } catch {
      case e: AssertionError => e.printStackTrace()
    }

  }
}