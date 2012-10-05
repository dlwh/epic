package epic.sequences

import java.io._
import breeze.config.{Configuration, CommandLineParser}
import epic.ontonotes.ConllOntoReader
import epic.everything.NERType
import collection.mutable.ArrayBuffer
import breeze.linalg.DenseVector
import epic.framework.ModelObjective
import breeze.optimize._
import breeze.corpora.CONLLSequenceReader
import breeze.data.Example
import breeze.util.Encoder
import epic.trees.Span
import breeze.optimize.FirstOrderMinimizer.OptParams
import epic.everything.DSpan


/**
 *
 * @author dlwh
 */
object SemiNERPipeline {

  def makeSegmentation(ner: Map[DSpan, NERType.Value],
                       words: IndexedSeq[String],
                       id: String): Segmentation[NERType.Value, String]  = {
    val sorted = ner.toIndexedSeq.sortBy((_: (DSpan, NERType.Value))._1.begin)
    var out = new ArrayBuffer[(NERType.Value, Span)]()
    var last = 0
    for( (dspan, label) <- sorted ) {
      assert(last <= dspan.begin)
      while(dspan.begin != last) {
        out += (NERType.NotEntity -> Span(last,last+1))
        last += 1
      }
      out += (label -> Span(dspan.begin, dspan.end))
      last = dspan.end
    }
    while(words.length != last) {
      out += (NERType.NotEntity -> Span(last,last+1))
      last += 1
    }

    Segmentation(out, words, id)
  }



  case class Params(path: File,
                    name: String = "eval/ner",
                    nfiles: Int = 100000,
                    iterPerEval: Int = 20,
                    opt: OptParams)

  def main(args: Array[String]) {
    val (baseConfig, files) = CommandLineParser.parseArguments(args)
    val config = baseConfig backoff Configuration.fromPropertiesFiles(files.map(new File(_)))
    val params = config.readIn[Params]("")
    val (train, test) = {
      val instances =  for {
        file <- params.path.listFiles take params.nfiles
        doc <- ConllOntoReader.readDocuments(file)
        s <- doc.sentences
      } yield makeSegmentation(s.ner, s.words, s.id)
      val train = instances.take(instances.length * 9 / 10)
      val test = instances.drop(instances.length * 9 / 10)
      (train,test)
    }



    // build feature Index
    val model = new SegmentationModelFactory(NERType.OutsideSentence, gazetteer = Gazetteer.ner("en")).makeModel(train)
    val obj = new ModelObjective(model, train)
    val cached = new CachedBatchDiffFunction(obj)

    val checking = new RandomizedGradientCheckingFunction[Int, DenseVector[Double]](cached, toString={ (i: Int) => model.featureIndex.get(i).toString})
//
    def eval(state: FirstOrderMinimizer[DenseVector[Double], BatchDiffFunction[DenseVector[Double]]]#State) {
      val crf = model.extractCRF(state.x)
      println("Eval + " + (state.iter+1) + " " + SegmentationEval.eval(crf, test, NERType.NotEntity))
    }

    val weights = params.opt.iterations(cached, obj.initialWeightVector(randomize=true)).tee(state => if((state.iter +1) % params.iterPerEval == 0) eval(state)).take(params.opt.maxIterations).last
    eval(weights)




  }

}



object SemiConllNERPipeline {

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
          out += ("O".intern -> Span(i, i+1))
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

    assert(out.nonEmpty && out.last._2.end == words.length, out + " " + words + " " + labels)
    Segmentation(out, words, ex.id)
  }



  case class Params(path: File,
                    test: File,
                    name: String = "eval/ner",
                    nfiles: Int = 100000,
                    iterPerEval: Int = 20,
                    opt: OptParams)

  def main(args: Array[String]) {
    val (baseConfig, files) = CommandLineParser.parseArguments(args)
    val config = baseConfig backoff Configuration.fromPropertiesFiles(files.map(new File(_)))
    val params = config.readIn[Params]("")
    val (train,test) = {
          val standardTrain = CONLLSequenceReader.readTrain(new FileInputStream(params.path), params.path.getName).toIndexedSeq
          val standardTest = CONLLSequenceReader.readTrain(new FileInputStream(params.test), params.path.getName).toIndexedSeq

          standardTrain.map(makeSegmentation) -> standardTest.map(makeSegmentation)
    }




    // build feature Index
    val model = new SegmentationModelFactory("##", gazetteer = Gazetteer.ner("en")).makeModel(train)
    val obj = new ModelObjective(model, train)
    val cached = new CachedBatchDiffFunction(obj)

    val checking = new RandomizedGradientCheckingFunction[Int, DenseVector[Double]](cached, toString={ (i: Int) => model.featureIndex.get(i).toString})
//
    def eval(state: FirstOrderMinimizer[DenseVector[Double], BatchDiffFunction[DenseVector[Double]]]#State) {
      val out = new PrintWriter(new BufferedOutputStream(new FileOutputStream("weights.txt")))
      Encoder.fromIndex(model.featureIndex).decode(state.x).iterator foreach {case (x, v) if v.abs > 1E-6 => out.println(x -> v) case _ => }
      val crf: SemiCRF[String, String] = model.extractCRF(state.x)
      println("Eval + " + (state.iter+1) + " " + SegmentationEval.eval(crf, test, "O"))
      out.close()
    }

    val weights = params.opt.iterations(cached, obj.initialWeightVector(randomize=true)).tee(state => if((state.iter +1) % params.iterPerEval == 0) eval(state)).take(params.opt.maxIterations).last
    eval(weights)



  }

}


