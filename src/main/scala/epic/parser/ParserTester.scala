package epic.parser
/*
 Copyright 2012 David Hall

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/
import breeze.config.{Help, CommandLineParser, Configuration}
import java.io.File
import breeze.util._
import epic.trees.{TreeInstance, ProcessedTreebank, AnnotatedLabel, AnnotatedLabelChainReplacer}

/**
 * ParserTester just tests a grammar
 * reading in the treebank and params and such
 */
object ParserTester {
  /**
   * The type of the parameters to read in via dlwh.epic.config
   */
  case class Params(treebank: ProcessedTreebank,
                    @Help(text="Prefix for the name of the eval directory. Sentences are dumped for EVALB to read.")
                    name: String,
                    @Help(text="Path to the parser file. Look in parsers/")
                    parser: File,
                    @Help(text="Should we evaluate on the test set? Or just the train set?")
                    evalOnTest: Boolean = false,
                    @Help(text="Print this and exit.")
                    help: Boolean = false)

  /**
   * Evaluates a parser on dev and possibly test.
   */
  def main(args: Array[String]) {
    val (baseConfig,files) = CommandLineParser.parseArguments(args)
    val config = baseConfig backoff Configuration.fromPropertiesFiles(files.map(new File(_)))
    val params = try {
      config.readIn[Params]("test")
    } catch {
      case e =>
      println(breeze.config.GenerateHelp[Params](config))
      sys.exit(1)
    }

    if(params.help) {
      println(breeze.config.GenerateHelp[Params](config))
      System.exit(1)
    }
    println("Evaluating Parser...")
    println(params)

    import params.treebank._

    val parser = readObject[Parser[AnnotatedLabel,String]](params.parser)

    import params._

    val name = params.name

    println("Parser " + name)

    {
      println("Evaluating Parser on dev...")
      val stats = evalParser(devTrees,parser,name+ "-dev")
      import stats._
      println("Eval finished. Results:")
      println( "P: " + precision + " R:" + recall + " F1: " + f1 +  " Ex:" + exact + " Tag Accuracy: " + tagAccuracy)
    }

    if (params.evalOnTest) {
      println("Evaluating Parser on test...")
      val stats = evalParser(testTrees,parser,name+ "-test")
      import stats._
      println("Eval finished. Results:")
      println( "P: " + precision + " R:" + recall + " F1: " + f1 +  " Ex:" + exact + " Tag Accuracy: " + tagAccuracy)
    }
  }

  def evalParser(testTrees: IndexedSeq[TreeInstance[AnnotatedLabel,String]],
                 parser: Parser[AnnotatedLabel,String], name: String):ParseEval.Statistics = {
    ParseEval.evaluateAndLog(testTrees, parser, name, AnnotatedLabelChainReplacer)
  }

}