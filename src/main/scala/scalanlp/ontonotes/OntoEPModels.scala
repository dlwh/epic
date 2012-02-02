package scalanlp.ontonotes
import scalanlp.parser._
import discrim._
import projections.GrammarProjections
import scalala.tensor._
import scalanlp.trees.{NullaryTree, UnaryTree, BinaryTree, BinarizedTree}
import scalala.library.Library
import java.io.File
import scalanlp.parser.ParseChart._
import scalanlp.util._
import scalala.library.Library._
import collection.IndexedSeq

/**
 * 
 * @author dlwh
 */

object OntoEPModels;

class StrippingOntoEPModel(inner: EPModelFactory[OntoLabel,String]) extends EPModelFactory[OntoLabel,String] {
  def make(coarseParser: ChartBuilder[ParseChart.LogProbabilityParseChart, OntoLabel, String],
           trainTrees: IndexedSeq[TreeInstance[OntoLabel, String]],
           initLexicon: Counter2[OntoLabel, String, Double],
           initBinaries: Counter2[OntoLabel, BinaryRule[OntoLabel], Double],
           initUnaries: Counter2[OntoLabel, UnaryRule[OntoLabel], Double]) = {

    val strippedTrees = for (ti <- trainTrees) yield {
      new TreeInstance(ti.id, ti.tree.map(l => OntoLabel(l.tag)), ti.words)
    }
    inner.make(coarseParser,strippedTrees,initLexicon,initBinaries,initUnaries)
  }
}

case class KMOntoDiscEPModelFactory(pipeline: KMPipeline,
                                    featurizerFactory: FeaturizerFactory[AnnotatedLabel,String] = new PlainFeaturizerFactory[AnnotatedLabel],
                                    addRawRules: Boolean = true,
                                    oldWeights: File = null,
                                    locked: Boolean = false) extends EPModelFactory[OntoLabel,String] {
   def make(coarseParser: ChartBuilder[LogProbabilityParseChart,OntoLabel,String],
            trainTrees: IndexedSeq[TreeInstance[OntoLabel,String]],
            initLexicon: Counter2[OntoLabel,String,Double],
            initBinaries: Counter2[OntoLabel,BinaryRule[OntoLabel],Double],
            initUnaries: Counter2[OntoLabel,UnaryRule[OntoLabel],Double]) = {
     val transformed = trainTrees.par.map { ti =>
       val t = pipeline(ti.tree.map(_.tag),ti.words)
       TreeInstance(ti.id,t,ti.words)
     }.seq
     val (words,binary,unary) = GenerativeParser.extractCounts(transformed);
     if(addRawRules) {
       for( (l,w) <- initLexicon.keysIterator) {
         words(AnnotatedLabel(l.tag),w) += 1.0
       }

       for( (l,rule) <- initBinaries.keysIterator) {
         binary(AnnotatedLabel(l.tag),rule.map(a => AnnotatedLabel(a.tag))) += 1.0
       }

       for( (l,rule) <- initUnaries.keysIterator) {
         unary(AnnotatedLabel(l.tag),rule.map(a => AnnotatedLabel(a.tag))) += 1.0
       }
     }
     val grammar = Grammar(Library.logAndNormalizeRows(binary),Library.logAndNormalizeRows(unary));
     val lexicon = new SignatureLexicon(words, EnglishWordClassGenerator, 5);
     val proj = GrammarProjections(coarseParser.grammar,grammar,{(l: AnnotatedLabel) => OntoLabel((l:AnnotatedLabel).label)})

    val featurizer = featurizerFactory.getFeaturizer(words, binary, unary);

    val finalFeat = if(oldWeights != null) {
      val weights = readObject[(Any,Counter[Feature[AnnotatedLabel,String],Double])](oldWeights)._2
      new CachedWeightsFeaturizer(featurizer,weights,randomize=false,randomizeZeros=false)
    } else {
      featurizer
    }
    val indexed = FeatureIndexer(finalFeat,lexicon,grammar)

    val openTags: Set[AnnotatedLabel] = Set.empty ++ {
      for(t <- words.nonzero.keys.iterator.map(_._1) if words(t, ::).size > 5) yield t;
    }

    val closedWords:Set[String] = Set.empty ++ {
      val wordCounts = sum(words)
      wordCounts.nonzero.pairs.iterator.filter(_._2 > 10).map(_._1);
    }

     import KMOntoDiscEPModelFactory._
     new KMDiscEPModel[OntoLabel,AnnotatedLabel,String](proj,annotate(pipeline),indexed,openTags,closedWords,transformed.head.tree.label,locked)
   }
}

object KMOntoDiscEPModelFactory {
  def annotate(pipeline: KMPipeline) = (t: BinarizedTree[OntoLabel], w: Seq[String]) => pipeline(t.map(_.tag),w)

}

case class NEREPModelFactory[W](featurizerFactory: FeaturizerFactory[AnnotatedLabel,W] = new PlainFeaturizerFactory[AnnotatedLabel]()) extends EPModelFactory[OntoLabel,W] {
  import NEREPModelFactory._
  def make(coarseParser: ChartBuilder[ParseChart.LogProbabilityParseChart,OntoLabel,W],
           trainTrees: IndexedSeq[TreeInstance[OntoLabel,W]],
           initLexicon: Counter2[OntoLabel,W,Double],
           initBinaries: Counter2[OntoLabel,BinaryRule[OntoLabel],Double],
           initUnaries: Counter2[OntoLabel,UnaryRule[OntoLabel],Double]):DiscEPModel[OntoLabel,W] = {
    val nerTrees: IndexedSeq[TreeInstance[AnnotatedLabel, W]] = for( ti <- trainTrees) yield {
      TreeInstance(ti.id,nerify(ti.tree),ti.words)
    }
    val (words,binary,unary) = GenerativeParser.extractCounts(nerTrees);
    val grammar = Grammar(Library.logAndNormalizeRows(binary),Library.logAndNormalizeRows(unary));
    val lexicon = new SimpleLexicon(words);
    val proj = GrammarProjections(coarseParser.grammar,grammar,{(l:AnnotatedLabel) => OntoLabel(l.label)})


    val featurizer = featurizerFactory.getFeaturizer(words,binary,unary)
    val indexed = FeatureIndexer(featurizer,lexicon,grammar)


    val openTags: Set[AnnotatedLabel] = Set.empty ++ {
      for(t <- words.nonzero.keys.iterator.map(_._1) if words(t, ::).size > 5) yield t;
    }

    val closedWords = Set.empty ++ {
      val wordCounts = sum(initLexicon)
      wordCounts.nonzero.pairs.iterator.filter(_._2 > 5).map(_._1);
    }

    new KMDiscEPModel[OntoLabel,AnnotatedLabel,W](proj,
      sillyclosure _,
      indexed,
      openTags,
      closedWords,
      nerTrees.head.tree.label
    )

  }



}

object NEREPModelFactory {
  // annotations:
  case class ContinueNER(x: NERType) extends Annotation
  case object ContinuingNER extends Annotation

  case class StartNER(x: NERType) extends Annotation
  case object StartingNER extends Annotation

  private def sillyclosure(t: BinarizedTree[OntoLabel], w: Any) = nerify(t)
  def nerify(tree: BinarizedTree[OntoLabel], incomingNER: NERType = NERType.NotEntity):BinarizedTree[AnnotatedLabel] = {
    val (features:Set[Annotation],next) = tree.label.entity match {
      case NERType.NotEntity =>
        if(incomingNER == NERType.NotEntity) Set.empty[Annotation] -> NERType.NotEntity
        else Set[Annotation](ContinueNER(incomingNER),ContinuingNER) -> incomingNER
      case x =>
        Set[Annotation](StartNER(x),StartingNER) -> x
    }

    tree match {
      case BinaryTree(a,b,c) =>
        val newLabel = AnnotatedLabel(a.tag,features=features)
        BinaryTree(newLabel,nerify(b,next),nerify(c,next))(tree.span)
      case UnaryTree(a,b) =>
        val newLabel = AnnotatedLabel(a.tag,features=features)
        UnaryTree(newLabel,nerify(b,next))(tree.span)
      case NullaryTree(a) =>
        val newLabel = AnnotatedLabel(a.tag,features=features)
        NullaryTree(newLabel)(tree.span)
    }
  }
}