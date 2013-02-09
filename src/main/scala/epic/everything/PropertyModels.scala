package epic.everything

import breeze.util.{Index, Lens}
import epic.ontonotes.NERType
import epic.trees.AnnotatedLabel
import epic.everything.PropertyPropagation._
import epic.framework.Feature

/**
 * 
 *
 * @author dlwh
 */
object PropertyModels {

  val nerLens: Lens[SpanBeliefs,Beliefs[NERType.Value]] = Lens({_.ner}, {(a,b) => a.copy(ner=b)})
  val syntaxLens: Lens[SpanBeliefs,Beliefs[Option[AnnotatedLabel]]] = Lens({_.label}, {(a,b) => a.copy(label=b)})
  val governorLens: Lens[SpanBeliefs,Beliefs[Int]] = Lens({_.governor}, {(a,b) => a.copy(governor=b)})

  private val emptyArray = Array.empty[Feature]

  def nerSyntaxModel(beliefsFactory: SentenceBeliefs.Factory, data: IndexedSeq[FeaturizedSentence]):PropertyPropagation.Model[_, _] = {

    val lenses = IndexedSeq(syntaxLens, governorLens).asInstanceOf[IndexedSeq[Lens[SpanBeliefs, Beliefs[Any]]]]
    pivotModel(beliefsFactory, nerLens, lenses, data){ new Featurizer[NERType.Value, Any] {
      def anchor(sent: FeaturizedSentence, spanBeliefs: SpanBeliefs, begin: Int, end: Int): FeatureAnchoring[NERType.Value, Any] = new FeatureAnchoring[NERType.Value, Any] {
        def featuresFor(p1: Int, satIndex: Int, p2: Int): Array[Feature] = satIndex match {
          case 0 =>
            // syntax
            Array(AssociationFeature(spanBeliefs.ner.property.index.get(p1), spanBeliefs.label.property.index.get(p2)))
          case 1 =>
            // governor
            if (p2 < begin || p2 >= end) emptyArray
            else {
              for (f <- sent.featuresForWord(p2)) yield {
                AssociationFeature(spanBeliefs.ner.property.index.get(p1), f)
              }
            }.toArray
        }
      }
    }}
  }

  def pivotModel[T, U](beliefsFactory: SentenceBeliefs.Factory,
                    lens1: Lens[SpanBeliefs, Beliefs[T]],
                    lenses2: IndexedSeq[Lens[SpanBeliefs, Beliefs[U]]],
                    data: IndexedSeq[FeaturizedSentence]
                     )(featurizer: Featurizer[T,_]):PropertyPropagation.Model[T, U] = {

    val fi = Index[Feature]
    for (fs<- data) {
      val beliefs = beliefsFactory(fs)
      for (begin <- 0 until fs.length; end <- (begin+1) until fs.length) {
        val spanBeliefs = beliefs.spanBeliefs(begin, end)
        if (spanBeliefs ne null) {
          val anchoring = featurizer.anchor(fs, spanBeliefs, begin, end)
          val p1 = lens1.get(spanBeliefs)
          for (i <- 0 until lenses2.length) {
            val p2 = lenses2(i).get(spanBeliefs)
            for (a1 <- 0 until p1.property.size; a2 <- 0 until p2.property.size; f <- anchoring.featuresFor(a1, i, a2)) {
              fi.index(f)
            }
          }
        }
      }
    }


    val assoc = new AssociationFeaturizer[T, U] {
      def centralLens: Lens[SpanBeliefs, Beliefs[T]] = lens1
      def satelliteLenses = lenses2

      val featureIndex: Index[Feature] =  fi


      def anchor(sent: FeaturizedSentence, spanBeliefs: SpanBeliefs, begin: Int, end: Int): AssociationAnchoring[T, U] = new AssociationAnchoring[T, U] {
        val unindexed = featurizer.anchor(sent, spanBeliefs, begin, end)
        def featuresFor(p1: Int, satIndex: Int, p2: Int):Array[Int] = {
          unindexed.featuresFor(p1, satIndex, p2).map(featureIndex).filter(_ != -1)
        }

      }

    }

    new Model(beliefsFactory, assoc)
  }

}

