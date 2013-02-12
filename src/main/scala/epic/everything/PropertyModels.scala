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
  val srlLens: Lens[SpanBeliefs,IndexedSeq[Beliefs[Option[String]]]] = Lens({_.frames}, {(a,b) => a.copy(frames=b)})

  def nerPacket(nerIndex: Index[NERType.Value]):AssociationPacket[NERType.Value] = new AssociationPacket[NERType.Value] {
    def lens = nerLens

    val featureIndex: Index[_] = nerIndex

    val cachedFeatures = Array.tabulate(featureIndex.size)(i => Array(i))

    def featuresFor(fs: FeaturizedSentence, b: SpanBeliefs, begin: Int, end: Int, assignment: Int): Array[Int] = {
      cachedFeatures(assignment)
    }
  }

  def syntaxPacket(labelIndex: Index[Option[AnnotatedLabel]]):AssociationPacket[Option[AnnotatedLabel]] = new AssociationPacket[Option[AnnotatedLabel]] {
    def lens = syntaxLens

    val featureIndex: Index[_] = labelIndex

    val cachedFeatures = Array.tabulate(featureIndex.size)(i => Array(i))

    def featuresFor(fs: FeaturizedSentence, b: SpanBeliefs, begin: Int, end: Int, assignment: Int): Array[Int] = {
      cachedFeatures(assignment)
    }
  }

  def governorPacket(wordFeatures: Index[Feature]):AssociationPacket[Int] = new AssociationPacket[Int] {
    def lens = governorLens

    val featureIndex: Index[_] = Index(wordFeatures.iterator ++ Iterator(IndicatorFeature("-ROOT-"), IndicatorFeature("-NOTGOVERNED-")))

    val notGovernedArray = Array(featureIndex.size - 1)
    val rootArray = Array(featureIndex.size - 2)


    def featuresFor(fs: FeaturizedSentence, b: SpanBeliefs, begin: Int, end: Int, assignment: Int): Array[Int] = {
      if (assignment > fs.length) notGovernedArray
      else if (assignment == fs.length) rootArray
      else fs.featuresForWord(assignment)
    }
  }

  def srlPacket(wordFeatures: Index[Feature], srlComponents: Index[Option[String]]):SequencePacket[Option[String]] = new SequencePacket[Option[String]] {
    def lens = srlLens

    val featureIndex: Index[_] = Index(wordFeatures ++ srlComponents)

    val srlOffset = wordFeatures.size


    def featuresFor(fs: FeaturizedSentence, b: SpanBeliefs, begin: Int, end: Int, component: Int, assignment: Int): Array[Int] = {
      val fi = fs.featuresForWord(fs.frames(component).pos)
      fi :+ (assignment + srlOffset)
    }
  }




  def nerSyntaxModel(fs: FeaturizedDocument.Factory, beliefsFactory: SentenceBeliefs.Factory):PropertyPropagation.Model[_, _] = {
    val nerp = nerPacket(beliefsFactory.nerLabelIndex)
    val synp = syntaxPacket(beliefsFactory.optionLabelProp.index)
    val govp = governorPacket(fs.wordFeatureIndex)

    PropertyPropagation.packetModel(beliefsFactory, nerp, IndexedSeq(synp, govp))
  }

  def srlSyntaxModel(fs: FeaturizedDocument.Factory, beliefsFactory: SentenceBeliefs.Factory):PropertyPropagation.Model[_, _] = {
    val synp = syntaxPacket(beliefsFactory.optionLabelProp.index)
    val srlp = srlPacket(fs.wordFeatureIndex, beliefsFactory.srlProp.index)

    PropertyPropagation.sequencePacketModel(beliefsFactory, synp, srlp)
  }


  def srlNerModel(fs: FeaturizedDocument.Factory, beliefsFactory: SentenceBeliefs.Factory):PropertyPropagation.Model[_, _] = {
    val nerp = nerPacket(beliefsFactory.nerLabelIndex)
    val srlp = srlPacket(fs.wordFeatureIndex, beliefsFactory.srlProp.index)

    PropertyPropagation.sequencePacketModel(beliefsFactory, nerp, srlp)
  }

  def srlGovernorModel(fs: FeaturizedDocument.Factory, beliefsFactory: SentenceBeliefs.Factory):PropertyPropagation.Model[_, _] = {
    val synp = governorPacket(fs.wordFeatureIndex)
    val srlp = srlPacket(fs.wordFeatureIndex, beliefsFactory.srlProp.index)
    val packet1 = synp
    val packet2 = srlp
    val fi = Index[Feature]()
    val featureMatrices =
      Array.tabulate(packet1.featureIndex.size, packet2.featureIndex.size) {(p1, p2) =>
       fi.index(AssociationFeature(packet1.featureIndex.get(p1), packet2.featureIndex.get(p2)))
      }

    val (agree, disagree) = fi.index(new IndicatorFeature("SRL governed by lemma")) -> fi.index(new IndicatorFeature("SRL not governed by lemma"))

    type T = Int
    type U =Option[String]
    val assoc = new AssociationFeaturizer[T, U] {
      def centralLens: Lens[SpanBeliefs, Beliefs[T]] = packet1.lens
      override val satelliteLenses = packet2.lens

      val featureIndex: Index[Feature] =  fi


      def anchor(sent: FeaturizedSentence, spanBeliefs: SpanBeliefs, begin: Int, end: Int): AssociationAnchoring[T, U] = new AssociationAnchoring[T, U] {
        def featuresFor(p1: Int, satIndex: Int, p2: Int):Array[Int] = {
          val feats1 = packet1.featuresFor(sent, spanBeliefs, begin, end, p1)
          val feats2 = packet2.featuresFor(sent, spanBeliefs, begin, end, satIndex, p2)
          val arr = new Array[Int](feats1.length * feats2.length + 1)
          var off = 0
          var i = 0
          while(i < feats1.length) {
            var j = 0
            val featureRow = featureMatrices(feats1(i))
            while(j < feats2.length) {
              arr(off) = featureRow(feats2(j))
              off += 1
              j += 1
            }
            i += 1
          }
          arr(off) = if (p1 == sent.frames(satIndex).pos) agree else disagree
          arr
        }

      }

    }

    new Model(beliefsFactory, assoc)
  }
}

