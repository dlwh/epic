package epic.util

import java.io.{FileInputStream, File}
import breeze.config.{CommandLineParser, Help}
import breeze.util._
import epic.preprocess._
import scala.io.{Codec, Source}
import epic.sequences.CRF
import java.util.concurrent.{LinkedBlockingDeque, TimeUnit, ThreadPoolExecutor}

/**
 * TODO
 *
 * @author dlwh
 **/
trait ProcessTextMain[Model, AnnotatedType] {
  import ProcessTextMain._

  def render(model: Model, ann: AnnotatedType, tokens: IndexedSeq[String]): String

  def renderFailed(model: Model, tokens: IndexedSeq[String], reason: Throwable): String = {
    s"### Could not tag $tokens, because ${reason.getMessage}... ${reason.getStackTrace.take(2).mkString(";")}".replaceAll("\n", " ")
  }

  def annotate(model: Model, text: IndexedSeq[String]):AnnotatedType

  def classPathLoad(language: String):Model

  def main(args: Array[String]) = {
    val (baseConfig, files) = CommandLineParser.parseArguments(args)
    val config = baseConfig
    val params: Params = try {
      config.readIn[Params]("")
    } catch {
      case e:Exception =>
        e.printStackTrace()
        System.err.println(breeze.config.GenerateHelp[Params](config))
        sys.exit(1)
    }

    val model = try {
      epic.models.deserialize[Model](params.model.toString)
    } catch {
      case ex: Exception =>
        // BT 20150514 - skanky hack to get message to screen, would be nicer if could say "enable debug messages for full trace"
        System.err.println(s"Couldn't deserialize model due to exception, ${ex.getCause.getMessage}. Trying classPathLoad...")
        classPathLoad(params.model.toString)
    }

    val sentenceSegmenter = {
      val base = params.sentences.toLowerCase match {
        case "java" => new JavaSentenceSegmenter()
        case "default" | "trained" => MLSentenceSegmenter.bundled().get
        case "newline" => new NewLineSentenceSegmenter()
      }
      new StreamSentenceSegmenter(base)
    }
    val tokenizer = params.tokens.toLowerCase match {
      case "default" | "treebank" => new TreebankTokenizer
      case "none" | "whitespace" => new WhitespaceTokenizer
    }

    implicit val context = if (params.threads > 0) {
      scala.concurrent.ExecutionContext.fromExecutor(new ThreadPoolExecutor(1, params.threads, 1, TimeUnit.SECONDS, new LinkedBlockingDeque[Runnable]()))
    } else {
      scala.concurrent.ExecutionContext.global
    }

    val iter = if (files.isEmpty) Iterator(System.in) else files.iterator.map(new FileInputStream(_))

    for(src <- iter) {
      val queue = FIFOWorkQueue(sentenceSegmenter.sentences(src)){sent =>
        val tokens = tokenizer(sent).toIndexedSeq
        try {
          if (tokens.length > params.maxLength) {
            throw new SentenceTooLongException(tokens.length)
          }
          val tree = annotate(model, tokens)
          render(model, tree, tokens)
        } catch {
          case e: Exception =>
           renderFailed(model, tokens, e)
        }
      }
      for(result <- queue) {
        println(result)
      }
      src.close()

    }
  }

}

object ProcessTextMain {
  case class Params(model: File,
                    @Help(text="How many threads to parse with. Default is whatever Scala wants")
                    threads: Int = -1,
                    @Help(text="Kind of sentence segmenter to use. Default is the trained one. Options are: default, newline, java")
                    sentences: String = "default",
                    @Help(text="Kind of word tokenizer to use. Default is the treebank. Can also choose whitespace/none for pre-tokenized text.")
                    tokens: String = "default",
                    maxLength: Int = 1000)

  case class SentenceTooLongException(length: Int) extends Exception("Sentence too long: " + length)
}
