//package chalk.slab
//
//import akka.actor.{Actor,ActorLogging,ActorSystem,Props}
//import akka.pattern.ask
//import akka.util.Timeout
//import scala.collection.mutable.ListBuffer
//import scala.concurrent.duration._
//
//case class ActorSlab(
//  content: String,
//  annotations: Map[String,Seq[Span]] = Map[String, Seq[Span]]()
//) {
//  
//  override def toString = {
//    val annotationString = (for ((attr, spans) <- annotations) yield {
//      "  " + attr + ": " + spans.map(s=>"["+content.substring(s.start,s.end)+"]").mkString(" ")
//    }).mkString("\n\n")
//    s"$content\n\n$annotationString"
//  }
//}
//
//object ActorSlab {
//
//  def update(slab: ActorSlab, key: String, spansForKey: Seq[Span]) =
//    ActorSlab(slab.content, slab.annotations ++ Map(key -> spansForKey.toSeq))
//
//}
//  
//case class Span(start: Int, end: Int)
//
//trait AnalysisComponent extends Actor with ActorLogging
//
//object AnalysisComponent {
//  case class Process(slab: ActorSlab)
//}
//
//class RegexSentenceDetector extends AnalysisComponent {
//
//  import AnalysisComponent._
//  
//  def receive = {
//
//    case Process(slab) =>
//      val highestIndex = slab.content.length
//      val matches = """[.?!]""".r.findAllMatchIn(slab.content)
//      val enderIndices = ListBuffer(matches.map(_.end).toList: _*)
//      if (enderIndices.last < highestIndex) 
//        enderIndices += highestIndex
//      
//      val paired = enderIndices.toList.flatMap(i=> List(i,i+1))
//      val spans = for (List(start,end) <- (0 :: paired).grouped(2)) yield Span(start,end)
//      sender ! ActorSlab(slab.content, slab.annotations ++ Map("sentences" -> spans.toSeq))
//  }
//
//}
//
//class WhitespaceTokenizer extends AnalysisComponent {
//
//  import AnalysisComponent._
//  import scala.util.matching.Regex.Match
//  
//  def receive = {
//    case Process(slab) =>
//      val highestIndex = slab.content.length
//      val tokenSpans = for {
//        sentenceSpan <- slab.annotations("sentences")
//        sentence = slab.content.substring(sentenceSpan.start,sentenceSpan.end)
//        wsMatches = "\\s+".r.findAllMatchIn(sentence)
//        span <- gappedSpans(wsMatches.toSeq, sentenceSpan.start, sentence.length)
//      } yield {
//        span
//      }
//      sender ! ActorSlab(slab.content, slab.annotations ++ Map("tokens" -> tokenSpans.toSeq))
//  }
//
//  private def gappedSpans(foundMatches: Seq[Match], offset: Int, highestIndex: Int) = {
//    val flattenedMatches = ListBuffer(foundMatches.flatMap(m => Seq(m.start,m.end)): _*)
//    val allSpans = for {
//      List(start,end) <- (0 +: flattenedMatches :+ highestIndex).toList.grouped(2)
//    } yield {
//      Span(start+offset,end+offset)
//    }
//    allSpans
//  }
//
//}
//
//class DumbNer extends AnalysisComponent {
//
//  import AnalysisComponent._
//
//  def receive = {
//
//    case Process(slab) =>
//      val dumbNerSpans = for {
//        tokenSpan <- slab.annotations("tokens")
//        token = slab.content.substring(tokenSpan.start, tokenSpan.end)
//        if (token.head.isUpper)
//      } yield {
//        tokenSpan
//      }
//      sender ! ActorSlab.update(slab, "entities", dumbNerSpans)
//  }
//  
//}
//
//
//object AnalysisEngine {
//
//  import AnalysisComponent._
//
//  def main(args: Array[String]) {
//    val text = "Here is an example text. It has four sentences and it mentions Jimi Hendrix and Austin, Texas! In this third sentence, it also brings up Led Zeppelin and Radiohead, but does it ask a question? It also has a straggler sentence that doesn't end with punctuation"
//
//    val slab = ActorSlab(text)
//    val system = ActorSystem("ChalkSystem")
//
//    implicit val ec = system.dispatcher
//    implicit val timeout = Timeout(10 seconds)    
//
//    val sentenceDetector = system.actorOf(Props[RegexSentenceDetector])
//    val tokenizer = system.actorOf(Props[WhitespaceTokenizer])
//    val ner = system.actorOf(Props[DumbNer])
//
//    for {
//      slab1 <- (sentenceDetector ? Process(slab)).mapTo[ActorSlab]
//      slab2 <- (tokenizer ? Process(slab1)).mapTo[ActorSlab]
//      slab3 <- (ner ? Process(slab2)).mapTo[ActorSlab]
//    } {
//      println(slab3)
//      system.shutdown
//    }
//    
//  }
//
//}
