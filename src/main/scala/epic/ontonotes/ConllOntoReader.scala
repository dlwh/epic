package epic.ontonotes

/*
 Copyright 2012 David Hall

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/
import java.io.File
import java.nio.charset.MalformedInputException

import epic.trees.{AnnotatedLabel, Span, Tree}

import scala.collection.mutable.{ArrayBuffer, Stack}
import scala.collection.{IndexedSeq, Iterator}
import scala.io.Source

/**
 * Reads the Conll 2011 shared task format. See http://conll.cemantix.org/2011/data.html
 * @author dlwh
 */

object ConllOntoReader {

  def readDocuments(file: File): IndexedSeq[Document] = try {
    val docIterator = new RawDocumentIterator(Source.fromFile(file).getLines())
    for ((rawSentences_ :IndexedSeq[IndexedSeq[String]], docIndex: Int) <- docIterator.zipWithIndex.toIndexedSeq) yield {
      val rawSentences = rawSentences_.collect { case seq if seq.nonEmpty =>
        seq.map(_.split("\\s+").toIndexedSeq)
      }

      val sentences = for( (s,sentenceIndex) <- rawSentences.zipWithIndex) yield {
      val words = s.map(_(3))
      val tags = s.map(_(4))

      val stringTree =  {
        val parseBits = s.map(_(5))
        val b = new StringBuilder()
        parseBits.indices.foreach { i =>
          b ++= parseBits(i).replace("*","( "+ tags(i) + " " + words(i) + " )")
        }
        Tree.fromString(b.toString)._1
      }

      val entities = collection.mutable.Map[(Int,Int), NerType.Value]()
      var currentChunkStart = -1
      var currentChunkType = NerType.OutsideSentence
      s.indices.foreach { i =>
        val chunk = s(i)(10)
        if (chunk.startsWith("(")) {
          assert(currentChunkStart < 0)
          currentChunkStart = i
          currentChunkType = NerType.fromString(chunk.replaceAll("[()*]",""))
        }

        if (chunk.endsWith(")")) {
          assert(currentChunkStart >= 0)
          entities += ((currentChunkStart -> (i+1)) -> currentChunkType)
          currentChunkStart = -1
        }
      }

      val lemmas = s.map(_(6))
      val frames = s.map(_(7))

      val srl = for(column <- 11 until (s.head.length-1)) yield {
        val lastValue = collection.mutable.Stack[(String, Int)]()
        val arguments = ArrayBuffer[Argument]()
        var verb = -1
        s.indices.foreach { i =>
          if (s(i)(column).startsWith("(")) {
            val trimmed = s(i)(column).substring(1, s(i)(column).lastIndexOf("*"))
            for(name <- trimmed.split("[(]"))
              lastValue.push(name.trim -> i)
          }
          if (s(i)(column).endsWith(")")) {
            for(close <- 0 until s(i)(column).count(_ == ')')) {
              assert(lastValue.nonEmpty, s.map(_(column)).mkString(",") + " " + i)
              val (name, start) = lastValue.pop()
              if (name == "V") {
                assert(start == i)
                verb = i
              } else {
                arguments += Argument(name, Span(start, i+1))
              }
            }
          }
        }

        assert(verb != -1,  s.map(_(column)).mkString(",") )
        assert(lastValue.isEmpty, s.map(_(column)).mkString(",") )
        Frame(lemmas(verb), verb, frames(verb).toInt, arguments)
      }

      val mentions = collection.mutable.Map[(Int,Int), Mention]()
      // stupid nested mentions. It's not quite a stack. I don't know why they did it this way.
      // (entity id -> stack of open parens for that id
      val stack = new collection.mutable.HashMap[Int, Stack[Int]]() {
        override def default(key: Int) = getOrElseUpdate(key,new Stack())
      }
      s.indices.foreach { i =>
        val chunk = s(i).last
        if (chunk != "-")
          for( id <- chunk.split("\\|")) {
            val tid = id.replaceAll("[()*]","").toInt
            if (id.startsWith("(")) {
              stack(tid).push(i)
            }
            if (id.endsWith(")")) {
              val start = stack(tid).pop()
              mentions(start -> (i+1)) = mention(tid)
            }
          }
      }

      val docId = s"${file.getName}-$docIndex"
      val tree = stringTree.extend { t => AnnotatedLabel(t.label) }
      val ner = Map.empty ++ entities.map { case ((beg,end),v) => DSpan(docId,sentenceIndex,beg,end) -> v}
      val coref = Map.empty ++ mentions.map { case ((beg,end),v) => DSpan(docId,sentenceIndex,beg,end) -> v}
      val speaker = s.map(_(9)).find(_ != "-")
      val annotations = OntoAnnotations(tree, ner, coref, srl, speaker)

      Sentence(docId, sentenceIndex,words, annotations)
    }

      Document(s"${file.toString}-$docIndex",sentences.toIndexedSeq)
    }

  } catch {
    case ex: MalformedInputException =>
      throw new RuntimeException("Error while processing " + file, ex)
  }

  private val mentionCache = Array.tabulate(100)(i => Mention(i))

  private def mention(id: Int) = if (id < mentionCache.length) mentionCache(id) else Mention(id)

  private class RawDocumentIterator(it: Iterator[String]) extends Iterator[IndexedSeq[IndexedSeq[String]]] {
    def hasNext = it.hasNext

    def next(): IndexedSeq[IndexedSeq[String]] = {
      var doneOuter = false
      val outBuf = new ArrayBuffer[IndexedSeq[String]]
      while (it.hasNext && !doneOuter) {
        val buf = new ArrayBuffer[String]
        var done = false
        var seenSomethingNotBlank = false
        while (it.hasNext && !done) {
          val next = it.next()
          if (next.startsWith("#begin")) {
            // pass
          } else if (next.startsWith("#end")) {
            doneOuter = true
          } else if (next.trim != "") {
            seenSomethingNotBlank = true
            buf += next.trim
          } else if (seenSomethingNotBlank) {
            done = true
          }
        }
        outBuf += buf
      }
      outBuf
    }
  }
}

