package epic.sequences

import java.io._
import breeze.config.{Configuration, CommandLineParser}
import epic.ontonotes.{NerType, ConllOntoReader}
import collection.mutable.ArrayBuffer
import breeze.linalg.DenseVector
import epic.framework.{Example, ModelObjective}
import breeze.optimize._
import breeze.util.Encoder
import epic.trees.Span
import breeze.optimize.FirstOrderMinimizer.OptParams
import breeze.util.Implicits._
import com.typesafe.scalalogging.slf4j.LazyLogging
import epic.preprocess.TreebankTokenizer
import epic.corpora.CONLLSequenceReader
import epic.framework.Example


/**
 *
 * @author dlwh
 */
object SemiNerPipeline extends LazyLogging {

  case class Params(path: File,
                    modelOut: File = new File("ner.model.gz"),
                    nfiles: Int = 100000,
                    iterPerEval: Int = 20,
                    nthreads: Int = -1,
                    opt: OptParams,
                    checkGradient: Boolean = false,
                    lowerCaseAndStripPunct: Boolean = false,
                    exclude: String = "")

  def main(args: Array[String]) {
    val params = CommandLineParser.readIn[Params](args)
    logger.info("Command line arguments for recovery:\n" + Configuration.fromObject(params).toCommandLineString)
    val excludeSet = params.exclude.split(",").map(NerType.fromString(_)).toSet
    val (train, test) = {
      var instances =  for {
        file <- params.path.listFiles take params.nfiles
        doc <- ConllOntoReader.readDocuments(file)
        s <- doc.sentences
      } yield s.nerSegmentation

      instances = instances.map(_.filterLabels(x => !excludeSet(x)))

      if (params.lowerCaseAndStripPunct) {
        instances = instances.map(_.filterWords(_.exists(_.isLetterOrDigit)).mapWords(_.toLowerCase))
      }
      instances.splitAt(instances.length * 9 / 10)
    }


    val gazetteer =  None//Gazetteer.ner("en")

    // build feature Index
    val model = new SegmentationModelFactory(gazetteer = gazetteer).makeModel(train)
    val obj = new ModelObjective(model, train, params.nthreads)
    val cached = new CachedBatchDiffFunction(obj)
    if(params.checkGradient) {
      GradientTester.test(cached, obj.initialWeightVector(true), toString = {(x: Int) => model.featureIndex.get(x).toString})
    }

    def eval(state: FirstOrderMinimizer[DenseVector[Double], BatchDiffFunction[DenseVector[Double]]]#State) {
      val crf = model.extractCRF(state.x)
      println("Eval + " + (state.iter+1) + " " + SegmentationEval.eval(crf, test))
    }

    val finalState = params.opt.iterations(cached, obj.initialWeightVector(randomize=false)).tee(state => if((state.iter +1) % params.iterPerEval == 0) eval(state)).take(params.opt.maxIterations).last
    eval(finalState)

    breeze.util.writeObject(params.modelOut, model.extractCRF(finalState.x))

  }

}



object SemiConllNerPipeline extends LazyLogging {

  def makeSegmentation(ex: Example[IndexedSeq[String],IndexedSeq[IndexedSeq[String]]]): Segmentation[String, String]  = {
    val labels = ex.label
    val words = ex.features.map(_ apply 0)
    assert(labels.length == words.length)
    val out = new ArrayBuffer[(String, Span)]()
    var start = labels.length
    var i = 0
    while(i < labels.length) {
      val l = labels(i)
      l(0) match {
        case 'O' =>
          if(start < i)
            out += (labels(start).replaceAll(".-","").intern -> Span(start, i))
//          out += ("O".intern -> Span(i, i+1))
          start = i + 1
        case 'B' =>
          if(start < i)
            out += (labels(start).replaceAll(".-","").intern -> Span(start, i))
          start = i
        case 'I' =>
          if(start >= i) {
            start = i
          } else if(labels(start) != l){
            out += (labels(start).replaceAll(".-","").intern -> Span(start, i))
            start = i
          } // else, still in a field, do nothing.
        case _  =>
          sys.error("weird label?!?" + l)
      }

      i += 1
    }
    if(start < i)
      out += (labels(start).replaceAll(".-","").intern -> Span(start, i))

//    assert(out.nonEmpty && out.last._2.end == words.length, out + " " + words + " " + labels)
    Segmentation(out, words, ex.id)
  }



  case class Params(train: File,
                    test: File,
                    nsents: Int = 100000,
                    nthreads: Int = -1,
                    iterPerEval: Int = 20,
                    modelOut: File = new File("ner-conll.ser.gz"),
                    opt: OptParams,
                    checkGradient: Boolean = false)

  def main(args: Array[String]) {
    val params:Params = CommandLineParser.readIn[Params](args)
    logger.info("Command line arguments for recovery:\n" + Configuration.fromObject(params).toCommandLineString)
    val (train,test) = {
      val standardTrain = CONLLSequenceReader.readTrain(new FileInputStream(params.train), params.train.getName).toIndexedSeq
      val standardTest = CONLLSequenceReader.readTrain(new FileInputStream(params.test), params.train.getName).toIndexedSeq

      standardTrain.take(params.nsents).map(makeSegmentation) -> standardTest.map(makeSegmentation)
    }


    // build feature Index
    val model: SemiCRFModel[String, String] = new SegmentationModelFactory(/*, gazetteer = Gazetteer.ner("en" )*/).makeModel(train)
    val obj = new ModelObjective(model, train, params.nthreads)
    val cached = new CachedBatchDiffFunction(obj)

    if(params.checkGradient) {
      GradientTester.test(cached, obj.initialWeightVector(true), toString={(i: Int) => model.featureIndex.get(i).toString})
    }

    //
    def eval(state: FirstOrderMinimizer[DenseVector[Double], BatchDiffFunction[DenseVector[Double]]]#State) = {
      val out = new PrintWriter(new BufferedOutputStream(new FileOutputStream("weights.txt")))
      Encoder.fromIndex(model.featureIndex).decode(state.x).iterator.toIndexedSeq.sortBy(-_._2.abs).takeWhile(_._2.abs > 1E-4) foreach {case (x, v) => out.println(v + "\t" + x)}
      val crf: SemiCRF[String, String] = model.extractCRF(state.x)
      println("Eval + " + (state.iter+1))
      val stats = SegmentationEval.eval(crf, test)
      println("Final: " + stats)
      out.close()
      stats
    }

    val weights = params.opt.iterations(cached, obj.initialWeightVector(randomize=false)).tee(state => if((state.iter +1) % params.iterPerEval == 0) eval(state)).take(params.opt.maxIterations).last
    val stats = eval(weights)
    breeze.util.writeObject(params.modelOut, model.extractCRF(weights.x))
    println(stats)


  }

}
