package chalk.slab

import akka.actor.{Actor,ActorLogging,ActorSystem,Props}
import akka.pattern.{ask,pipe}
import akka.util.Timeout
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration._
import scala.concurrent.Future

/**
  * An actor that mixes-in an AnalysisFunction and hands Slabs contained in Process messages over
  * to the function.
  */
trait AnalysisComponent[C,B,I<:B,O<:B] extends Actor with ActorLogging with AnalysisFunction[C,B,I,O] {
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

trait StringAnalysisComponent[I<:Span,O<:Span]
    extends AnalysisComponent[String,Span,I,O]

/**
  * An actor that uses SentenceSegmenter.
  */
class SentenceSegmenterActor extends SentenceSegmenter[Span]
    with StringAnalysisComponent[Span,Sentence]

/**
  * An actor that uses Tokenizer.
  */
class TokenizerActor extends Tokenizer[Sentence] with StringAnalysisComponent[Sentence, Token]


/**
  * An analysis engine that runs Slabs through a pipeline of AnalysisComponents. It currently
  * requires explicit declaration of the analyzers, but this would ideally be done through
  * configuration. No compile-time consistency check for the types in the pipeline is performed.
  * Anyhoo, this gives the basic idea for how an actor based AnalysisEngine might work, so
  * it should be a good starting point.
  */
class AnalysisEngine extends Actor with ActorLogging {

  import AnalysisComponent._
  import AnalysisEngine._
  import Span._
  implicit val ec = context.dispatcher
  implicit val timeout = Timeout(10 seconds)
  
  val sentenceSegmenter = context.system.actorOf(Props[SentenceSegmenterActor])
  val tokenizer = context.system.actorOf(Props[TokenizerActor])

  def receive = {
    case Process(slab) =>
      log.info("Processing slab:\n " + slab.content)
      (for {
        slab1 <- (sentenceSegmenter ? Process(slab)).mapTo[Slab[String,Span,Sentence]]
        slab2 <- (tokenizer ? Process(slab1)).mapTo[Slab[String,Span,Sentence with Token]]
      } yield {
        slab2
      }) pipeTo sender

    case ProcessCorpus(texts) =>
      Future.traverse(texts)(text => self ? Process(Slab(text))) pipeTo sender
  }
}

/**
  * Example application doing actor based Slab processing.
  */
object AnalysisEngine {

  case class ProcessCorpus(corpus: Iterator[String])
  
  import AnalysisComponent._
  import Span._

  val text1 = "Here is an example text. It has four sentences and it mentions Jimi Hendrix and Austin, Texas! In this third sentence, it also brings up Led Zeppelin and Radiohead, but does it ask a question? It also has a straggler sentence that doesn't end with punctuation"

  val text2 = "How much wood can a woodchuck chuck? Can a woodchuck chuck wood?"

  val text3 = "The Association for Computational Linguistics is proud to present its first Lifetime Achievement Award to Prof. Aravind Joshi of the University of Pennsylvania. Aravind Joshi was born in 1929 in Pune, India, where he completed his secondary education as well as his first degree in Mechanical and Electrical Engineering, the latter in 1950. He worked as a research assistant in Linguistics at Penn from 1958-60, while completing his Ph.D. in Electrical Engineering, in 1960. Joshi's work and the work of his Penn colleagues at the frontiers of Cognitive Science was rewarded in 1991 by the establishment of a National Science Foundation Science and Technology Center for Research in Cognitive Science, which Aravind Joshi co-directed until 2001. Dr. Joshi has supervised thirty-six Ph.D. theses to-date, on topics including information and coding theory, and also pure linguistics."
  
  def main(args: Array[String]) {

    val system = ActorSystem("ChalkSystem")
    implicit val ec = system.dispatcher
    implicit val timeout = Timeout(10 seconds)

    val engine = system.actorOf(Props[AnalysisEngine])
    val corpus = Iterator(text1,text2,text3)
    
    for {
      slabs <- (engine ? ProcessCorpus(corpus)).mapTo[Iterator[Slab[String,Span,Sentence with Token]]]
      slab <- slabs
    } {
      // Notice that the last sentence (lacking EOS char) is missing.
      val sentences = slab.iterator[Sentence].toList
      println("\nSENTENCES\n\n" + sentences.map(_.in(slab).content).mkString("\n"))
      
      val tokens = slab.iterator[Token].toList
      println("\nTOKENS\n\n" + tokens.map(_.in(slab).content).mkString("\n"))
    }
    Thread.sleep(3000)
    system.shutdown
  }

}
