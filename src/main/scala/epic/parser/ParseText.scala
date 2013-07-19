package epic.parser

import java.io.File
import breeze.config.{Help, CommandLineParser}
import breeze.util._
import epic.trees.AnnotatedLabel
import io.{Codec, Source}
import chalk.text.LanguagePack

/**
 * Simple class that reads in a bunch of files and parses them. Output is dumped to standard out.
 * @author dlwh
 */
object ParseText {

  case class Params(parser: File, maxLength: Int = 60,
                    @Help(text="How many threads to parse with. Default is whatever Scala wants")
                    threads: Int = -1)

  def main(args: Array[String]) {
    val (baseConfig, files) = CommandLineParser.parseArguments(args)
    val config = baseConfig
    val params = try {
      config.readIn[Params]("test")
    } catch {
      case e:Exception =>
        e.printStackTrace()
        System.err.println(breeze.config.GenerateHelp[Params](config))
        sys.exit(1)
    }

    val parser = readObject[Parser[AnnotatedLabel,String]](params.parser)
    if(params.threads >= 1) {
//      collection.parallel.ForkJoinTasks.defaultForkJoinPool.setParallelism(params.threads)
    }

    val sentenceSegmenter = LanguagePack.English.sentenceSegmenter
    val tokenizer = LanguagePack.English.treebankTokenizer.get

    for(f <- files) {
      val text = Source.fromFile(f)(Codec.UTF8).mkString
      val parsed = sentenceSegmenter(text).par.map { sent =>
        val tokens = tokenizer(sent).toIndexedSeq

        try {
          val tree = parser.bestParse(tokens)

          tree.render(tokens, newline = false)
        } catch {
          case e: Exception =>
          e.printStackTrace()
          "(())"
        }
      }

      parsed foreach println
    }
  }

}
