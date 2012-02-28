package scalanlp.coref

import scalanlp.ontonotes._
import java.io.File

/**
 * 
 * @author dlwh
 */
object SimpleCoref extends App {
  val documents = Corpus.fromXMLDirectory(new File(args(0))).trainDocuments

  for( (d:Document) <- documents) {
    println(d.id)
    for(sx <- d.sentences; s = sx.stripTraces; c <- s.tree.allChildren; m <- c.label.mention) yield {
      println(m + " " + (s.words.slice(c.span.start,c.span.end)))
    }
    println()
    println()
    println()
  }

}