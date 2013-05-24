package epic.coref

import java.io.{PrintStream, FileOutputStream, File}
import io.{Codec, Source}
import collection.mutable.ArrayBuffer
import epic.ontonotes.{DSpan, Document}

/**
 *
 * @author dlwh
 */
object ConllEval {
  /**
   * Dumps conll files to evaluate. Copies both gold and guess files into one file each
   * Assumes existence of perl and that the files were read in by ConllOntoReader, and
   * parses the IDs. Sorry, bad coding form, I know.
   * @param prefix prefix of path names
   * @param docs
   */
  def evaluate(prefix: String, docs: IndexedSeq[(Document,Seq[Set[DSpan]])]) {
    val goldOutF = new File(prefix + "_gold")
    val guessOutF = new File(prefix + "_guess")
    goldOutF.getParentFile.mkdirs()

    // sort the documents by their id, so we can write them out correctly.
    // file-docId
    def filePart(id: String) = id.takeWhile(_ != '-')
    val groupedAndSortedDocs = docs.sortBy(_._1.id).groupBy(doc => filePart(doc._1.id))

    val goldOut = new PrintStream(new FileOutputStream(goldOutF))
    val out = new PrintStream(new FileOutputStream(guessOutF))
    for((file,docs) <- groupedAndSortedDocs) {
      // copy gold file file
      println(file)

      val rows: Iterator[String] = Source.fromFile(new File(file))(Codec.UTF8).getLines().map(_.trim)

      case class MentionMapEntry(sentId: String,
                                 word: String,
                                 var oneWordMention: Option[Int] = None,
                                 starts:ArrayBuffer[Int] = ArrayBuffer[Int](),
                                 ends:ArrayBuffer[Int] = ArrayBuffer[Int]())

      // need to know where to fill in mentions
      // doc -> sentence -> pos -> (word, single-word mentions, mentions starting at pos, mentions ending at pos)
      // word is for checking we have the right alignment
      val mentionMap: IndexedSeq[IndexedSeq[IndexedSeq[MentionMapEntry]]] =  {
        for( (doc,mentions) <- docs) yield {
          val mentionMapForDoc =  doc.sentences.map(s => s.words.map(w => MentionMapEntry(s.id, w)))
          for((set,clusterId) <- mentions.zipWithIndex; mc <- set) {
            if(mc.span.begin == mc.span.end - 1) {
              assert(mentionMapForDoc(mc.sentence)(mc.span.begin).oneWordMention == None,
                s"Two one-word mentions on same pos?!?! ${mentionMapForDoc(mc.sentence)(mc.span.begin)} $mc ${doc.id}")
              mentionMapForDoc(mc.sentence)(mc.span.begin).oneWordMention = Some(clusterId)
            } else {
              mentionMapForDoc(mc.sentence)(mc.span.begin).starts += clusterId
              mentionMapForDoc(mc.sentence)(mc.span.end-1).ends += clusterId
            }
          }
          mentionMapForDoc
        }
      }

      // we can actually just flatten the whole thing :-)
      val flattenedMentions = mentionMap.flatMap(_.flatten)
      for ( mm@MentionMapEntry(sentId, w, oneWordMention, starts,ends) <- flattenedMentions) {
        var line = rows.next()
        goldOut.println(line)
        while(line.isEmpty || line.startsWith("#")) {
          out.println(line)
          line = rows.next()
          goldOut.println(line)
        }
        line = line.reverse.dropWhile(!_.isSpaceChar).reverse
        assert(line.split(" +")(3) == w, line + " " + mm)
        val items = starts.map("(" + _) ++ oneWordMention.map("(" + _ + ")") ++ ends.map(_ + ")")
        val fullString = if(items.isEmpty) line + " -" else items.mkString(line + " ","|","")
        out.println(fullString)
      }

      while(rows.hasNext) {
        val line = rows.next()
        assert(line.isEmpty || line.startsWith("#"))
        out.println(line)
        goldOut.println(line)
      }


    }
    out.close()

  }

}
