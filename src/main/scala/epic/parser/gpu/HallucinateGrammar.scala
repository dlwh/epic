package epic.parser.gpu

import epic.parser.{GenerativeParser, ParserParams}
import breeze.config.{Configuration, CommandLineParser}
import java.io._
import epic.parser.ParserParams.JointParams
import epic.trees.annotations.{FilterAnnotations, TreeAnnotator}
import epic.trees.{UnaryRule, BinaryRule, AnnotatedLabel}

/**
 * 
 * @author dlwh
 */
object HallucinateGrammar {
  case class Params(annotator: TreeAnnotator[AnnotatedLabel, String, AnnotatedLabel] = FilterAnnotations(),
                    useGPU: Boolean = true, numToParse: Int = 1000, numGrammars: Int = 8, numStates: Int = 2)

  def main(args: Array[String]) {
    import ParserParams.JointParams

    val (baseConfig, files) = CommandLineParser.parseArguments(args)
    val config = baseConfig backoff Configuration.fromPropertiesFiles(files.map(new File(_)))
    val params = try {
      config.readIn[JointParams[Params]]("test")
    } catch {
      case e =>
        e.printStackTrace()
        println(breeze.config.GenerateHelp[JointParams[Params]](config))
        sys.exit(1)
    }

    import params._
    import params.trainer._
    println("Training Parser...")
    println(params)
    val annotator = FilterAnnotations[String]()
    val transformed = params.treebank.trainTrees.par.map { ti => annotator(ti) }.seq.toIndexedSeq
    val grammar = GenerativeParser.extractGrammar(AnnotatedLabel.TOP, transformed)

    def score = math.exp(math.random * 5 - 10)
    val out = new PrintStream(new BufferedOutputStream(new FileOutputStream("grammar.out")))

    var visited = Set[(AnnotatedLabel, AnnotatedLabel)]()
    for(r <- grammar.index) {
      r match {
        case BinaryRule(a, b, c) =>
          for(g <- 0 until numGrammars; aa <- 0 until numStates; bb <- 0 until numStates; cc <- 0 until numStates)
            out.println("%s_%d_%d -> %s_%d_%d %s_%d_%d %f".format(a,g,aa,b,g,bb,c,g,cc,score))
        case r@UnaryRule(a, b, _) =>
          if(!visited(a->b))
            if(a == b)
              for(g <- 0 until numGrammars; aa <- 0 until numStates)
                out.println("%s_%d_%d -> %s_%d_%d %f".format(a,g,aa,b,g,aa,score))
            else
              for(g <- 0 until numGrammars; aa <- 0 until numStates; bb <- 0 until numStates)
                out.println("%s_%d_%d -> %s_%d_%d %f".format(a,g,aa,b,g,bb,score))
          visited += (a->b)
      }
    }

    out.close()
  }

}
