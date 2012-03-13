package scalanlp.parser.combine

import scalanlp.config._
import scalanlp.data.Example
import io.Source
import java.io.{BufferedReader, FileReader, File}
import scalanlp.trees._
import scalanlp.parser._
import collection.immutable.IndexedSeq
import collection.{Iterable, BufferedIterator, Iterator}
import scalanlp.optimize.FirstOrderMinimizer.OptParams
import scalanlp.util._
import scalanlp.trees.UnaryChainRemover.ChainReplacer

case class CombineParams(treebank: TreebankParams,
                  kbestDir: File,
                  trainSections:String = "all",
                  systems:String = "all",
                  testSection: String = "23",
                  devSection: String = "22",
                  iterationsPerEval: Int = 50,
                  actualTest: Boolean = false,
                  opt: OptParams
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

    val treebank = params.treebank.treebank

    val process = Trees.Transforms.StandardStringTransform andThen {Trees.headBinarize(_, HeadFinder.collinsHeadFinder)}

    val inputTrees: IndexedSeq[TreeBundle[String, String]] = {
      for {
        (section:String) <- trainSections.toIndexedSeq
        tb <- readCandidatesFromSection(section, acceptableSystems, kbestDir, treebank, process)
        goldTree = process(tb.goldTree)
      } yield tb.copy[String,String](goldTree=goldTree)
    }

    val testTrees: IndexedSeq[TreeBundle[String, String]] = {
      val section = if(actualTest) params.testSection else params.devSection
      for {
        tb <- readCandidatesFromSection(section, acceptableSystems, kbestDir, treebank, process)
        goldTree = process(tb.goldTree)
      } yield tb.copy[String,String](goldTree=goldTree)
    }.toIndexedSeq

    // Remove unary chains and remember the replacer.
    // we can use system output on test sentences, just not gold output!
    val chainRemover = new UnaryChainRemover[String](identity)
    val chainReplacer = chainRemover.removeUnaryChains(testTrees.flatMap(_.treeInstances(withGold = false)))._2
    val fixedTrain = inputTrees.map(_.mapTrees({chainRemover.justRemoveChains _}, mapGold=true))
    val fixedTest = testTrees.map(_.mapTrees({chainRemover.justRemoveChains _}, mapGold=false)  )

    // train the parse, remove the gold trees, just in case.
    val parsers = trainParser(fixedTrain, fixedTest.map(_.copy[String,String](goldTree=null)), params);

    for((name,parser) <- parsers) {
      println("Parser " + name);

      println("Evaluating Parser...");
      val stats = evalParser(fixedTest.map(_.goldInstance),parser,name,chainReplacer);
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
  def trainParser(trainTrees: IndexedSeq[TreeBundle[String,String]],
                  goldTrees: IndexedSeq[TreeBundle[String,String]],
//                  validate: Parser[String,String]=>ParseEval.Statistics,
                  params: Params):Iterator[(String,Parser[String,String])];




  /*
  val model = new CombineModel(basicParser)
  val obj = new ModelObjective(model, fixedTrain)
  val cached = new CachedBatchDiffFunction(obj)
  for( (state,iteration) <- opt.iterations(cached)) {

  }
  */



  // helper methods


  private def readCandidatesFromSection(section: String,
                                        parsers: Set[String],
                                        kbestDir: File,
                                        tb: Treebank[String],
                                        process: Tree[String]=>BinarizedTree[String]):Iterable[TreeBundle[String,String]] = {
    val filesToRead = for {
      f <- kbestDir.listFiles()
      if f.getName.startsWith("wsj" + section)
      if (parsers.isEmpty || parsers.exists(f.getName contains _))
    } yield f

    val iterators = for(f <- filesToRead) yield {
      val parserName = f.getName.substring(6,f.getName.lastIndexOf('.'))
      val iter: BufferedIterator[Option[(Tree[String], Seq[String])]] = if(parserName.contains("stanford")) {
        newStanfordIterator(f).buffered
      } else {
        newTreeIterator(f).buffered
      }
      parserName -> iter
    }

    val trees = tb.treesFromSection(section)

    val filteredTrees = for {
      (t,w) <- trees
      if w == iterators.view.map(_._2.head).collectFirst { case Some( (t2,w2)) => w2}.get
    } yield (t,w)


    val treeIterator = for( ((t,words),i) <- filteredTrees.zipWithIndex) yield {
      val guessTrees = for {
        (pname,iter) <- iterators
        (guessTree,w2) <- iter.next
      } yield (pname -> guessTree)
      assert(words.length == t.span.end)
      guessTrees.foreach{ case (sys,t) => assert(words.length == t.span.end, sys -> words -> t)}
      val rerooted = process(Tree("ROOT",t.children)(t.span))
      TreeBundle(section+ "-" +i, rerooted, guessTrees.toMap.mapValues(process), words)
    }

    treeIterator.toIndexedSeq
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


  def evalParser(testTrees: IndexedSeq[TreeInstance[String,String]],
                 parser: Parser[String,String], name: String, chainReplacer: ChainReplacer[String]):ParseEval.Statistics = {
    val stats = ParseEval.evaluateAndLog[String](testTrees,parser,name,chainReplacer);
    stats
  }


}

case class TreeBundle[L,W](id: String, goldTree: BinarizedTree[L], outputs: Map[String,BinarizedTree[L]], words: Seq[W]) extends Example[Tree[L], Seq[W]] {
  def features = words

  def label = goldTree

  def goldInstance = TreeInstance(id, goldTree, words)
  def treeInstances(withGold: Boolean=false) = {
    val allTrees = if(withGold) outputs.iterator ++ Iterator("Gold" -> goldTree) else outputs.iterator
    for( (sys,t) <- allTrees) yield TreeInstance(id+"-"+sys, t, words)
  }

  def mapTrees(f: BinarizedTree[L]=>BinarizedTree[L], mapGold: Boolean = false) = {
    TreeBundle(id, if(mapGold) f(goldTree) else goldTree, outputs.mapValues(f), words)
  }
}