package chalk.slab

import akka.actor.{Actor,ActorLogging,ActorSystem,Props}
import akka.pattern.ask
import akka.util.Timeout
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration._

trait AnalysisComponent[X,Y,-Z<:Y,+W<:Y] extends Actor with ActorLogging {

  import AnalysisComponent._

  def process(slab: Slab[X,Y,Z]): Slab[X,Y,Y with W]

  def receive = {
    case Process(slab) =>
      sender ! process(slab.asInstanceOf[Slab[X,Y,Z]])
  }

}

object AnalysisComponent {
  case class Process[X,Y,Z<:Y](slab: Slab[X,Y,Z])
}

class SentenceSegmenterActor[AnnotationTypes <: StringAnnotation]
    extends AnalysisComponent[String, StringAnnotation, AnnotationTypes, Sentence] {

  def process(slab: Slab[String, StringAnnotation, AnnotationTypes]) = 
    slab ++ "[^\\s.!?]+[^.!?]+[.!?]".r.findAllMatchIn(slab.content).map(m => Sentence(m.start, m.end))
}

class TokenizerActor[AnnotationTypes <: Sentence]
    extends AnalysisComponent[String, StringAnnotation, AnnotationTypes, Token] {

  def process(slab: Slab[String, StringAnnotation, AnnotationTypes]) =
    slab ++ slab.iterator[Sentence].flatMap(sentence =>
      "\\p{L}+|\\p{P}+|\\p{N}+".r.findAllMatchIn(sentence.in(slab).content).map(m =>
        Token(sentence.begin + m.start, sentence.begin + m.end)))
  
}


object AnalysisEngine {

  import AnalysisComponent._
  import StringAnnotation._

  def main(args: Array[String]) {
    val text = "Here is an example text. It has four sentences and it mentions Jimi Hendrix and Austin, Texas! In this third sentence, it also brings up Led Zeppelin and Radiohead, but does it ask a question? It also has a straggler sentence that doesn't end with punctuation"

    val slab = Slab(text)
    val system = ActorSystem("ChalkSystem")

    implicit val ec = system.dispatcher
    implicit val timeout = Timeout(10 seconds)    

    
    val sentenceSegmenter = system.actorOf(Props[SentenceSegmenterActor[StringAnnotation]])
    val tokenizer = system.actorOf(Props[TokenizerActor[Sentence]])

    for {
      slab1 <- (sentenceSegmenter ? Process(slab)).mapTo[Slab[String,StringAnnotation,Sentence]]
      slab2 <- (tokenizer ? Process(slab1)).mapTo[Slab[String,StringAnnotation,Sentence with Token]]
    } {

      // Notice that the last sentence (lacking EOS char) is missing.
      val sentences = slab2.iterator[Sentence].toList
      println("\nSENTENCES\n\n" + sentences.map(_.in(slab2).content).mkString("\n"))
      
      val tokens = slab2.iterator[Token].toList
      println("\nTOKENS\n\n" + tokens.map(_.in(slab2).content).mkString("\n"))

      system.shutdown
    }
    
  }

}
