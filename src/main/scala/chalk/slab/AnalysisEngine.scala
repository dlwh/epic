package chalk.slab

import akka.actor.{Actor,ActorLogging,ActorSystem,Props}
import akka.pattern.ask
import akka.util.Timeout
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration._

/**
  * An analysis function that takes a Slab with declared annotation types in it and outputs
  * a new Slab with additional annotations of a new type.
  *
  * Documentation for the type variables:
  *   C = Content type
  *   B = Base annonation type
  *   I = Input annotation type
  *   O = Output annotation type
  */ 
trait AnalysisFunction[C,B,-I<:B,+O<:B] extends (Slab[C,B,I] => Slab[C,B,B with O])

/**
  * An actor that mixes-in an AnalysisFunction and hands Slabs contained in Process messages over
  * to the function.
  */
trait AnalysisComponent[C,B,-I<:B,+O<:B] extends Actor with ActorLogging with AnalysisFunction[C,B,I,O] {
  import AnalysisComponent._
  def receive = {
    case Process(slab) => sender ! apply(slab.asInstanceOf[Slab[C,B,I]])
  }
}

/**
  * Companion object, e.g. to hold messages that can be processed by an AnalysisComponent actor.
  */
object AnalysisComponent {
  case class Process[C,B,I<:B](slab: Slab[C,B,I])
}

/**
  * A simple regex sentence segmenter.
  */
trait SentenceSegmenter extends AnalysisFunction[String, StringAnnotation, StringAnnotation, Sentence] {
  def apply(slab: Slab[String, StringAnnotation, StringAnnotation]) =
    slab ++ "[^\\s.!?]+[^.!?]+[.!?]".r.findAllMatchIn(slab.content).map(m => Sentence(m.start, m.end))
}

/**
  * An actor that uses SentenceSegmenter.
  */
class SentenceSegmenterActor extends SentenceSegmenter
    with AnalysisComponent[String,StringAnnotation,StringAnnotation,Sentence]

/**
  * A simple regex tokenizer.
  */
trait Tokenizer extends AnalysisFunction[String, StringAnnotation, Sentence, Token] {
  def apply(slab: Slab[String, StringAnnotation, Sentence]) =
    slab ++ slab.iterator[Sentence].flatMap(sentence =>
      "\\p{L}+|\\p{P}+|\\p{N}+".r.findAllMatchIn(sentence.in(slab).content).map(m =>
        Token(sentence.begin + m.start, sentence.begin + m.end)))
}

/**
  * An actor that uses Tokenizer.
  */
class TokenizerActor extends AnalysisComponent[String, StringAnnotation, Sentence, Token] with Tokenizer

/**
  * Example application doing actor based Slab processing.
  */
object AnalysisEngine {

  import AnalysisComponent._
  import StringAnnotation._

  def main(args: Array[String]) {
    val text = "Here is an example text. It has four sentences and it mentions Jimi Hendrix and Austin, Texas! In this third sentence, it also brings up Led Zeppelin and Radiohead, but does it ask a question? It also has a straggler sentence that doesn't end with punctuation"

    val slab = Slab(text)
    val system = ActorSystem("ChalkSystem")

    implicit val ec = system.dispatcher
    implicit val timeout = Timeout(10 seconds)    

    
    val sentenceSegmenter = system.actorOf(Props[SentenceSegmenterActor])
    val tokenizer = system.actorOf(Props[TokenizerActor])

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
