package epic.everything.models

import epic.parser.models._
import epic.trees.{BinarizedTree, TreeInstance, Tree, AnnotatedLabel}
import breeze.util.{Lens, Index}
import epic.framework.{StandardExpectedCounts, ProjectableInference, Feature}
import breeze.linalg.DenseVector
import epic.everything.{ProcessedDocument, DocumentAnnotator}
import epic.parser._
import projections.LexGovernorProjector
import breeze.collection.mutable.TriangularArray


object DocLexParser {
  type L = AnnotatedLabel
  type W = String
  /**
   *
   * @author dlwh
   */
  class Model(factory: DocumentBeliefs.Factory,
              bundle: LexGrammarBundle[L, W],
              reannotate: (BinarizedTree[L], Seq[W])=>BinarizedTree[L],
              indexed: IndexedLexFeaturizer[L, W],
              initFeatureValue: Feature=>Option[Double] = {(f: Feature) => None}) extends DocumentAnnotatingModel {
    type ExpectedCounts = StandardExpectedCounts[Feature]
    type Marginal = DocLexParser.Marginal[ParseMarginal[AnnotatedLabel, String]]
    type Inference = DocLexParser.Inference

    def featureIndex: Index[Feature] = indexed.index

    def initialValueForFeature(f: Feature): Double = initFeatureValue(f).getOrElse(0.0)

    def expectedCountsToObjective(ecounts: ExpectedCounts): (Double, DenseVector[Double]) = {
      (ecounts.loss, ecounts.counts)
    }

    def inferenceFromWeights(weights: DenseVector[Double]) = {
      val gram = bundle.makeGrammar(indexed, weights)
      def ann(tree: BinarizedTree[L], words: Seq[W]):BinarizedTree[(L, Int)] = {
        val reannotated = reannotate(tree, words)
        val headed = bundle.headFinder.annotateHeadIndices(reannotated)
        headed

      }
      new Inference(factory, gram, ann _, indexed)
    }



  }

  case class Marginal[+Inner<:epic.framework.Marginal](sentences: IndexedSeq[Inner]) extends epic.framework.Marginal {
    val logPartition = sentences.map(_.logPartition).sum
  }

  class Inference(beliefsFactory: DocumentBeliefs.Factory,
                  grammar: LexGrammar[AnnotatedLabel, String],
                  reannotate: (BinarizedTree[AnnotatedLabel], Seq[String])=>BinarizedTree[(AnnotatedLabel, Int)],
                  featurizer: IndexedLexFeaturizer[AnnotatedLabel, String]) extends DocumentAnnotatingInference with ProjectableInference[ProcessedDocument, DocumentBeliefs] {

    type Marginal = DocLexParser.Marginal[ParseMarginal[AnnotatedLabel, String]]
    type ExpectedCounts = StandardExpectedCounts[Feature]
    def emptyCounts = StandardExpectedCounts.zero(featurizer.index)

    def baseAugment(doc: ProcessedDocument): DocumentBeliefs = {
      beliefsFactory(doc)
    }


    def marginal(doc: ProcessedDocument, aug: DocumentBeliefs): (Marginal, Double) = {
      val trees = doc.treeInstances
      val lenses = for( (b, s) <- aug.sentenceBeliefs zip doc.sentences) yield adapt(s.words, b)

      val marginals = for (i <- 0 until trees.length) yield {
        AugmentedAnchoring(lenses(i), doc.sentences(i).sparsity).marginal
      }

      Marginal(marginals) -> marginals.map(_.logPartition).sum
    }

    def projectGold(v: ProcessedDocument, m: Marginal, oldAugment: DocumentBeliefs): DocumentBeliefs = {
      project(v, m, oldAugment)
    }


    def goldMarginal(doc: ProcessedDocument, aug: DocumentBeliefs): (Marginal, Double) = {
      val trees = doc.treeInstances
      val lenses = for( (b, s) <- aug.sentenceBeliefs zip doc.sentences) yield adapt(s.words, b)

      val marginals = for (i <- 0 until trees.length) yield {
        new TreeMarginal(AugmentedAnchoring(lenses(i), doc.sentences(i).sparsity), reannotate(trees(i).tree, trees(i).words))
      }

      Marginal(marginals) -> marginals.map(_.logPartition).sum
    }

    def countsFromMarginal(doc: ProcessedDocument,
                           marg: Marginal,
                           counts: ExpectedCounts,
                           scale: Double): ExpectedCounts = {
      for( i <- (0 until marg.sentences.length)) {
        marg.sentences(i).expectedCounts(featurizer, counts, scale)
      }
      counts
    }


    def adapt(words: IndexedSeq[String], t: SentenceBeliefs): RefinedAnchoring[AnnotatedLabel, String] = {
      new PropertyParsingAnchoring(grammar, words, t)
    }

    def project(v: ProcessedDocument, marg: Marginal, oldAugment: DocumentBeliefs): DocumentBeliefs = {
      val sentences = for ( (old, (s, m)) <- oldAugment.sentenceBeliefs.zip(v.sentences zip marg.sentences)) yield {
        val info = new LexGovernorProjector(grammar).apply(m.anchoring.refined, m)
        val words = Array.tabulate(s.length) { w =>
          val oldW = old.wordBeliefs(w)
          oldW.copy(governor=oldW.governor.copy(beliefs=info.wordGovernor(w)),
//            span=oldW.span.copy(beliefs=info.governedSpan(w)),
            tag=oldW.tag.copy(beliefs=info.wordTag(w)),
            maximalLabel=oldW.maximalLabel.copy(beliefs=info.maximalLabelType(w))
                    )

        }

        val spans = TriangularArray.tabulate(m.length+1) { (r, c) =>
          if(old.spanBeliefs(r, c) == null) null
          else {
            val span: SpanBeliefs = old.spanBeliefs(r, c)
            span.copy(governor=span.governor.copy(beliefs = info.spanGovernor(r, c)),
            label=span.label.copy(beliefs = info.spanType(r,c))
            )
          }
        }

        old.copy(spans=spans, wordBeliefs=words)
      }
      new DocumentBeliefs(sentences)
    }

    def apply(v1: ProcessedDocument, v2: DocumentBeliefs): ProcessedDocument = error("TODO")
  }


}




