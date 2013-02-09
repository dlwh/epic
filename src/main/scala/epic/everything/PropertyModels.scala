package epic.everything

import breeze.util.{Index, Lens}
import epic.ontonotes.NERType
import epic.trees.AnnotatedLabel
import epic.everything.PropertyPropagation._
import epic.framework.Feature
import epic.parser.features.IndicatorFeature

/**
 * 
 *
 * @author dlwh
 */
object PropertyModels {

  val nerLens: Lens[SpanBeliefs,Beliefs[NERType.Value]] = Lens({_.ner}, {(a,b) => a.copy(ner=b)})
  val syntaxLens: Lens[SpanBeliefs,Beliefs[Option[AnnotatedLabel]]] = Lens({_.label}, {(a,b) => a.copy(label=b)})
  val governorLens: Lens[SpanBeliefs,Beliefs[Int]] = Lens({_.governor}, {(a,b) => a.copy(governor=b)})

  def nerPacket(nerIndex: Index[NERType.Value]):AssociationPacket[NERType.Value] = new AssociationPacket[NERType.Value] {
    def lens: Lens[SpanBeliefs, Beliefs[NERType.Value]] = nerLens

    val featureIndex: Index[_] = nerIndex

    val cachedFeatures = Array.tabulate(featureIndex.size)(i => Array(i))

    def featuresFor(fs: FeaturizedSentence, b: SpanBeliefs, begin: Int, end: Int, assignment: Int): Array[Int] = {
      cachedFeatures(assignment)
    }
  }

  def syntaxPacket(labelIndex: Index[Option[AnnotatedLabel]]):AssociationPacket[Option[AnnotatedLabel]] = new AssociationPacket[Option[AnnotatedLabel]] {
    def lens: Lens[SpanBeliefs, Beliefs[Option[AnnotatedLabel]]] = syntaxLens

    val featureIndex: Index[_] = labelIndex

    val cachedFeatures = Array.tabulate(featureIndex.size)(i => Array(i))

    def featuresFor(fs: FeaturizedSentence, b: SpanBeliefs, begin: Int, end: Int, assignment: Int): Array[Int] = {
      cachedFeatures(assignment)
    }
  }

  def governorPacket(wordFeatures: Index[Feature]):AssociationPacket[Int] = new AssociationPacket[Int] {
    def lens: Lens[SpanBeliefs, Beliefs[Int]] = governorLens

    val featureIndex: Index[_] = wordFeatures


    def featuresFor(fs: FeaturizedSentence, b: SpanBeliefs, begin: Int, end: Int, assignment: Int): Array[Int] = {
      fs.featuresForWord(assignment)
    }
  }



  def nerSyntaxModel(fs: FeaturizedDocument.Factory, beliefsFactory: SentenceBeliefs.Factory):PropertyPropagation.Model[_, _] = {
    val nerp = nerPacket(beliefsFactory.nerLabelIndex)
    val synp = syntaxPacket(beliefsFactory.optionLabelProp.index)
    val govp = governorPacket(fs.wordFeatureIndex)

    PropertyPropagation.packetModel(beliefsFactory, nerp, IndexedSeq(synp, govp))
  }

}

