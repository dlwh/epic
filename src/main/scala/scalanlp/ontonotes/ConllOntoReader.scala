package scalanlp.ontonotes

import io.Source
import collection.{IndexedSeq, Iterator}
import java.lang.String
import scalanlp.trees.{HeadFinder, Tree}
import scalala.tensor.Counter
import collection.mutable.{MultiMap, Stack, ArrayBuffer}
import collection.immutable.HashMap
import java.io.{PrintStream, FileOutputStream, BufferedOutputStream, File}

/**
 * REads the Conll 2011 shared task format. See http://conll.cemantix.org/2011/data.html
 * @author dlwh
 */

object ConllOntoReader {
  def readDocuments(file: File):IndexedSeq[Document] = {
    for ( (rawSentences_ :IndexedSeq[IndexedSeq[String]]) <- new RawDocumentIterator(Source.fromFile(file).getLines()).toIndexedSeq) yield {
      val rawSentences = rawSentences_.collect { case seq if seq.nonEmpty =>
        seq.map(_.split("\\s+").toIndexedSeq)
      }

    val sentences = for( (s,index) <- rawSentences.zipWithIndex) yield {
      val words = s.map(_(3))
      val tags = s.map(_(4))

      val stringTree =  {
        val parseBits = s.map(_(5))
        val b = new StringBuilder()
        for(i <- 0 until parseBits.length) {
          b ++= parseBits(i).replace("*","( "+ tags(i) + " " + words(i) + " )")
        }
        Tree.fromString(b.toString)._1
      }

      val entities = collection.mutable.Map[(Int,Int), NERType.Value]()
      var currentChunkStart = -1
      var currentChunkType = NERType.NotEntity
      for(i <- 0 until s.length) {
        val chunk = s(i)(10)
        if(chunk.startsWith("(")) {
          assert(currentChunkStart < 0)
          currentChunkStart = i
          currentChunkType = NERType.fromString(chunk.replaceAll("[()*]",""))
        }

        if(chunk.endsWith(")")) {
          assert(currentChunkStart >= 0)
          entities += ((currentChunkStart -> (i+1)) -> currentChunkType)
          currentChunkStart = -1
        }
      }

      // TODO: lemmas
      // TODO: SRL

      val mentions = collection.mutable.Map[(Int,Int), Mention]()
      // stupid nested mentions. It's not quite a stack. I don't know why they did it this way.
      val stack = new collection.mutable.HashMap[Int, Stack[Int]]() {
        override def default(key: Int) = getOrElseUpdate(key,new Stack())
      }
      for(i <- 0 until s.length) {
        val chunk = s(i).last
        if(chunk != "-")
          for( id <- chunk.split("\\|")) {
            val tid = id.replaceAll("[()*]","").toInt
            if(id.startsWith("(")) {
              stack(tid).push(i)
            }
            if(id.endsWith(")")) {
              val start = stack(tid).pop()
              mentions(start -> (i+1)) = Mention(tid)
            }
          }
      }

      val tree = stringTree.extend { t =>
        if(t.isLeaf) OntoLabel(t.label)
        else {
          val mention = mentions.get(t.span.start -> t.span.end)
          val entity = entities.getOrElse(t.span.start -> t.span.end, NERType.NotEntity)

          OntoLabel(t.label,mention=mention,entity=entity)
        }
      }

      Sentence(file.getName + "-" + index,words,tree)
    }

      Document(file.getName,sentences.toIndexedSeq)
    }
  }

  private class RawDocumentIterator(it: Iterator[String]) extends Iterator[IndexedSeq[IndexedSeq[String]]] {
    def hasNext = it.hasNext

    def next():IndexedSeq[IndexedSeq[String]] = {
      var doneOuter = false
      val outBuf = new ArrayBuffer[IndexedSeq[String]]
      while(it.hasNext && !doneOuter) {
        val buf = new ArrayBuffer[String]
        var done = false
        var seenSomethingNotBlank = false
        while(it.hasNext && !done) {
          val next = it.next()
          if(next.startsWith("#begin")) {
            // pass
          } else if(next.startsWith("#end")) {
            doneOuter = true
          } else if(next.trim != "") {
            seenSomethingNotBlank = true
            buf += next.trim
          } else if(seenSomethingNotBlank) {
            done = true
          }
        }
        outBuf += buf
      }
      outBuf
    }
  }

}

object ReadAndDumpConll extends App {
  val gender = Map("he" -> "M", "his" -> "M", "him" -> "M", "it" -> "N", "its" -> "N", "her" -> "F", "she" -> "F", "their" -> "P", "our" -> "P", "we" -> "P", "you" -> "U", "they" -> "P", "them" -> "P")
//  val c = Corpus.fromXMLDirectory(new File(args(0)))
  new File("out").mkdirs()
  for( f <- new File(args(0)).listFiles if f.getName.endsWith("conll")){
    val out = new PrintStream(new BufferedOutputStream(new FileOutputStream(new File("out/" + f.getName.split("[.]")(0) +".feats"))))
    val parseOut = new PrintStream(new BufferedOutputStream(new FileOutputStream(new File("out/" + f.getName.split("[.]")(0) +".parse"))))
    for( (d:Document) <- ConllOntoReader.readDocuments(f)) {
      println(d.id)
      val allMentions = for {
        s <- d.sentences.map(_.stripTraces);
        c <- s.tree.allChildren;
        m <- c.label.mention.iterator
        tag = HeadFinder.collinsHeadFinder.findHeadTag(c,{(_:OntoLabel).tag}).tag
        if tag.startsWith("N")
      } yield {
        val hw = HeadFinder.collinsHeadFinder.findHeadWordIndex(c,{(_:OntoLabel).tag})
        val w = s.words(hw)
        (m.id,w)
      }
      val grouped = allMentions.groupBy(_._1)
      val groupedHeadWords = grouped.mapValues(_.map(_._2)).mapValues(Counter.count(_: _*))

      def mkFeature(w: String, words: Counter[String, Int]) = {
        if(words.isEmpty) w
        else {
          words.argmax
        }
      }

      for(s <- d.sentences.map(_.stripTraces)) {
        val mentions = new collection.mutable.HashMap[Int,Int]()
        for(c <- s.tree.allChildren; m <- c.label.mention) {
          val hw = HeadFinder.collinsHeadFinder.findHeadWordIndex(c,{(_:OntoLabel).tag})
          mentions(hw) = m.id
        }
        for(i <- 0 until s.words.length) {
          val features = mentions.get(i).map{m => mkFeature(s.words(i),groupedHeadWords.getOrElse(m,Counter[String,Int]()))}.getOrElse(s.words(i))
          out.println(s.words(i) + " " + features)
        }
        out.println()
        parseOut.println(s.tree.map(_.tag) render s.words)
      }
    }
    parseOut.close()
    out.close()
  }

}