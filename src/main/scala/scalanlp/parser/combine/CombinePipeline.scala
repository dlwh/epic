package scalanlp.parser.combine

import scalanlp.config._
import scalanlp.data.Example
import io.Source
import java.io.{BufferedReader, FileReader, File}
import scalanlp.trees._
import scalanlp.parser._
import collection.immutable.IndexedSeq
import scalanlp.optimize.FirstOrderMinimizer.OptParams
import scalanlp.util._
import collection.mutable.ArrayBuffer
import collection._

case class CombineParams(treebank: TreebankParams,
                         dir: File,
                         kbestDir: File = null,
                         k: Int = 20,
                         trainSections:String = "all",
                         systems:String = "all",
                         testSection: String = "23",
                         devSection: String = "22",
                         iterationsPerEval: Int = 50,
                         actualTest: Boolean = false,
                         useRuleFeatures: Boolean = true,
                         useLabelFeatures: Boolean = true,
                         opt: OptParams,
                         oldWeights: File = null,
                         maxLength: Int = 1000000
                          )

/**
 * 
 * @author dlwh
 */

trait CombinePipeline {
  type Params = CombineParams


  def main(args: Array[String]) {
    val (baseConfig,files) = scalanlp.config.CommandLineParser.parseArguments(args)
    val config = baseConfig backoff Configuration.fromPropertiesFiles(files.map(new File(_)))
    val params = config.readIn[CombineParams]("");
    println(params)
    import params._
    val trainSections = {
      if(params.trainSections == "all" && !params.actualTest) Set("00","01","24")
      else if (params.trainSections == "all") Set("00","01","24","22")
      else params.trainSections.split(",").toSet
    }

    val acceptableSystems = if(systems == "all") Set.empty[String] else Set(systems.split(","):_*)
    println(acceptableSystems)

    val treebank = params.treebank.treebank

    val process = new StandardTreeProcessor()

    val inputTrees: IndexedSeq[TreeBundle[AnnotatedLabel, String]] = {
      for {
        (section:String) <- trainSections.toIndexedSeq
        tb <- readCandidatesFromSection(section, acceptableSystems, dir, kbestDir, k, treebank, process, maxLength)
      } yield tb
    }

    val testTrees: IndexedSeq[TreeBundle[AnnotatedLabel, String]] = {
      val section = if(actualTest) params.testSection else params.devSection
      for {
        tb <- readCandidatesFromSection(section, acceptableSystems, dir, kbestDir, k, treebank, process, maxLength)
      } yield tb
    }.toIndexedSeq

    // Remove unary chains and remember the replacer.
    // we can use system output on test sentences, just not gold output!
    val fixedTrain = inputTrees.map(_.mapTrees({UnaryChainRemover.removeUnaryChains _}, mapGold=true))
    val fixedTest = testTrees.map(_.mapTrees({UnaryChainRemover.removeUnaryChains _}, mapGold=false)  )

    // train the parse, removing the gold trees, just in case.
    val parsers = trainParser(fixedTrain, fixedTest.map(_.copy(goldTree=null)), params);

    for((name,parser) <- parsers) {
      println("Parser " + name);

      println("Evaluating Parser...");
      val stats = evalParser(fixedTest.map(_.goldInstance),parser,name);
      import stats._;
      println("Eval finished. Results:");
      println( "P: " + precision + " R:" + recall + " F1: " + f1 +  " Ex:" + exact + " Tag Accuracy: " + tagAccuracy);
      val outDir = new File("parsers/");
      outDir.mkdirs();
      val out = new File(outDir,name +".parser")
      writeObject(out,parser);
    }


  }


  /**
   * The main point of entry for implementors. Should return a sequence
   * of parsers
   */
  def trainParser(trainTrees: IndexedSeq[TreeBundle[AnnotatedLabel,String]],
                  goldTrees: IndexedSeq[TreeBundle[AnnotatedLabel,String]],
//                  validate: Parser[String,String]=>ParseEval.Statistics,
                  params: Params):Iterator[(String,Parser[AnnotatedLabel,String])];




  /*
  val model = new CombineModel(basicParser)
  val obj = new ModelObjective(model, fixedTrain)
  val cached = new CachedBatchDiffFunction(obj)
  for( (state,iteration) <- opt.iterations(cached)) {

  }
  */



  // helper methods


  private def readCandidatesFromSection[L](section: String,
                                           parsers: Set[String],
                                           dir: File,
                                           kbestDir: File,
                                           k: Int,
                                           tb: Treebank[String],
                                           process: Tree[String]=>BinarizedTree[L],
                                           maxLength: Int):Iterable[TreeBundle[L,String]] = {
    val filesToRead = for {
      f <- dir.listFiles() ++ {if(kbestDir ne null) kbestDir.listFiles() else Iterable.empty}
      if f.getName.startsWith("wsj" + section)
      systemName = f.getName.split("[._]")(1)
      if (parsers.isEmpty || parsers.contains(systemName))
    } yield f

    val iterators: Array[(String, Iterator[IndexedSeq[(Tree[String], Seq[String])]])] = for(f <- filesToRead) yield {
      val parserName = f.getName.split("[.]")(1)
      val isKBest = f.getParentFile == kbestDir
      val iter: Iterator[IndexedSeq[(Tree[String], Seq[String])]] = if(parserName.contains("stanford")) {
        newStanfordIterator(f).buffered.map(_.toIndexedSeq)
      } else if (isKBest && parserName.contains("charniak")) {
        newCharniakKBestIterator(f).buffered
      } else if (isKBest && parserName.contains("berkeley")) {
        newBerkeleyKBestIterator(f).buffered
      } else if(isKBest) {
        newKBestIterator(f).buffered
      } else {
        newTreeIterator(f).buffered.map(_.toIndexedSeq)
      }
      val retName = if(isKBest) parserName +"-kbest" else parserName
      retName -> iter
    }

    val trees = tb.treesFromSection(section)

    def fullProcess(t: Tree[String]) = process(Tree("ROOT",t.children)(t.span))


    val treeIterator = for( ((t,words),i) <- trees.zipWithIndex) yield {
      val guessTrees = for {
        (pname,iter) <- iterators
        kbestList = iter.next().take(k)
        if kbestList.nonEmpty
        trees = kbestList.map(_._1)
        w = kbestList.head._2
        if w == words
      } yield (pname,trees.map(fullProcess))

      val rerooted = fullProcess(t)
      TreeBundle(section+ "-" +i, rerooted, guessTrees.map(x => (x._1 -> x._2)).toMap, words.map(_.intern))
    }

    treeIterator.toIndexedSeq.filter(_.words.length < maxLength)
  }

  def newKBestIterator(file: File): Iterator[IndexedSeq[(Tree[String], Seq[String])]] = {
    val lines = Source.fromFile(file).getLines()
    scalanlp.util.Iterators.fromProducer {
      if(!lines.hasNext) None
      else {
        val myLines = new ArrayBuffer[String]
        var ok = true
        while(lines.hasNext && ok) {
          val line = lines.next()
          if(line.trim != "") {
            ok = true
            myLines += line
          } else {
            ok = false
          }
        }
//        println(myLines)
        Some(myLines.map(Tree.fromString(_)).toIndexedSeq)
      }
    }
  }

  def newBerkeleyKBestIterator(file: File): Iterator[IndexedSeq[(Tree[String], Seq[String])]] = {
    val lines = Source.fromFile(file).getLines()
    scalanlp.util.Iterators.fromProducer {
      if(!lines.hasNext) None
      else {
        val myLines = new ArrayBuffer[String]
        var ok = true
        while(lines.hasNext && ok) {
          val line = lines.next()
          if(line.trim != "") {
            ok = true
            myLines += line.split("\t")(1)
          } else {
            ok = false
          }
        }
        //        println(myLines)
        Some(myLines.map(Tree.fromString(_)).toIndexedSeq)
      }
    }
  }

  def newCharniakKBestIterator(file: File): Iterator[IndexedSeq[(Tree[String], Seq[String])]] = {
    val lines = Source.fromFile(file).getLines()
    val line = lines.next() // drop first
    scalanlp.util.Iterators.fromProducer {
      if(!lines.hasNext) None
      else {
        val myLines = new ArrayBuffer[String]
        var ok = true
        while(lines.hasNext && ok) {
          if(line.charAt(0).isDigit) {
            ok = false
          } else if (line.startsWith("(")) {
            myLines += line
          }
        }
        Some(myLines.map(Tree.fromString(_)).toIndexedSeq)
      }
    }
  }

  def newTreeIterator(file: File) = {
    val lines = Source.fromFile(file).getLines()
    for (line <- lines) yield {
      if (line.trim().isEmpty || line.trim() == "(())") {
        None
      } else {
        Some(Tree.fromString(line))
      }
    }
  }

  def newStanfordIterator(file: File) = {
    val reader = new PennTreeReader(new BufferedReader(new FileReader(file)))
    for( (tree,words) <- reader) yield {
      Some(tree -> words)
    }
  }


  def evalParser(testTrees: IndexedSeq[TreeInstance[AnnotatedLabel,String]],
                 parser: Parser[AnnotatedLabel,String], name: String):ParseEval.Statistics = {
    val stats = ParseEval.evaluateAndLog[AnnotatedLabel](testTrees,parser,name,AnnotatedLabelChainReplacer);
    stats
  }


}

case class TreeBundle[L,W](id: String, goldTree: BinarizedTree[L], outputs: Map[String,IndexedSeq[BinarizedTree[L]]], words: Seq[W]) extends Example[Tree[L], Seq[W]] {
  def features = words

  def label = goldTree

  def goldInstance = TreeInstance(id, goldTree, words)
  def treeInstances(withGold: Boolean=false): Iterator[TreeInstance[L,W]] = {
    val allTrees = if(withGold) outputs.iterator ++ Iterator("Gold" -> IndexedSeq(goldTree)) else outputs.iterator
    for( (sys,trees) <- allTrees; t <- trees) yield TreeInstance(id+"-"+sys, t, words)
  }

  def mapTrees(f: BinarizedTree[L]=>BinarizedTree[L], mapGold: Boolean = false) = {
    TreeBundle(id, if(mapGold) f(goldTree) else goldTree, outputs.mapValues(_.map(f)), words)
  }
}