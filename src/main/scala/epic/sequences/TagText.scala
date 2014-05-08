package epic.sequences

import java.io.File
import breeze.config.{Help, CommandLineParser}
import breeze.util._
import epic.trees.AnnotatedLabel
import io.{Codec, Source}
import chalk.text.LanguagePack
import epic.preprocess.TreebankTokenizer
import scala.collection.parallel.ForkJoinTaskSupport
import epic.util.FIFOWorkQueue
import java.util.concurrent.{LinkedBlockingDeque, TimeUnit, ThreadPoolExecutor}

/**
 * Simple class that reads in a bunch of files and parses them. Output is dumped to standard out.
 * @author dlwh
 */
object TagText {

  case class Params(model: File, maxLength: Int = 500,
                    @Help(text="How many threads to parse with. Default is whatever Scala wants")
                    threads: Int = -1)

  def main(args: Array[String]) {
    val (baseConfig, files) = CommandLineParser.parseArguments(args)
    val config = baseConfig
    val params: Params = try {
      config.readIn[Params]("test")
    } catch {
      case e:Exception =>
        e.printStackTrace()
        System.err.println(breeze.config.GenerateHelp[Params](config))
        sys.exit(1)
    }

    val parser = readObject[CRF[Any,String]](params.model)

    val sentenceSegmenter = LanguagePack.English.sentenceSegmenter
    val tokenizer = new TreebankTokenizer

    implicit val context = if(params.threads > 0) {
      scala.concurrent.ExecutionContext.fromExecutor(new ThreadPoolExecutor(1, params.threads, 1, TimeUnit.SECONDS, new LinkedBlockingDeque[Runnable]()))
    } else {
      scala.concurrent.ExecutionContext.global
    }


    val iter = if(files.length == 0) Iterator(Source.fromInputStream(System.in)) else files.iterator.map(Source.fromFile(_)(Codec.UTF8))

    for(src <- iter) {
      val text = src.mkString
      val queue = FIFOWorkQueue(sentenceSegmenter(text)){sent =>
        try {
          val tokens = tokenizer(sent).toIndexedSeq
          if(tokens.length <= params.maxLength) {
            val tree = parser.bestSequence(tokens)

            tree.render
          } else {
            "(())"
          }
        } catch {
          case e: Exception =>
            e.printStackTrace()
            "(())"
        }
      }
      for(result <- queue) {
        println(result)
      }
      src.close()

    }
  }

}
