package epic.sequences

import java.io.File
import breeze.config.{Configuration, CommandLineParser}
import epic.ontonotes.ConllOntoReader
import epic.everything.NERType
import collection.mutable.ArrayBuffer
import breeze.linalg.DenseVector
import epic.framework.ModelObjective
import breeze.optimize._
import breeze.optimize.FirstOrderMinimizer.OptParams
import epic.trees.Span
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



  case class Params(path: File, name: String = "eval/ner", nfiles: Int = 100000,
                    iterPerEval: Int = 20,
                    opt: OptParams)

  def main(args: Array[String]) {
    val (baseConfig, files) = CommandLineParser.parseArguments(args)
    val config = baseConfig backoff Configuration.fromPropertiesFiles(files.map(new File(_)))
    val params = config.readIn[Params]("")
    val instances: Array[Segmentation[NERType.Value, String]] = for {
      file <- params.path.listFiles take params.nfiles
      doc <- ConllOntoReader.readDocuments(file)
      s <- doc.sentences
    } yield makeSegmentation(s.ner, s.words, s.id)

    val train = instances.take(instances.length * 9 / 10)
    val test = instances.drop(instances.length * 9 / 10)


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
//    eval(weights)
    val weights2 = new LBFGS[DenseVector[Double]].minimize(checking, weights.x)



  }

}

