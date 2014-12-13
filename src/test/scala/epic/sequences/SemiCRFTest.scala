package epic
package sequences

import java.util.BitSet

import breeze.collection.mutable.TriangularArray
import breeze.features.FeatureVector
import breeze.linalg.{Counter2, DenseVector, norm}
import breeze.optimize.GradientTester
import breeze.util.{Encoder, Index, OptionIndex}
import epic.constraints.LabeledSpanConstraints
import epic.constraints.LabeledSpanConstraints.{Factory, SimpleConstraints}
import epic.features.{CrossProductIndex, IndexedSurfaceFeaturizer, SurfaceFeaturizer}
import epic.framework.{Feature, ModelObjective}
import epic.sequences.SemiCRF.Marginal
import epic.sequences.SemiCRFModel.{BIEOFeatureAnchoring, BIEOFeaturizer}
import epic.trees.Span
import org.scalatest.FunSuite

/**
 *
 * @author dlwh
 */
class SemiCRFTest extends FunSuite {
  test("wikipedia basic hmm test") {
    val transitions = Counter2(
      ('Rainy,'Rainy,0.7),
      ('Rainy,'Sunny,0.3),
      ('Rainy,'Start,0.0),

      ('Sunny,'Start,0.0),
      ('Sunny,'Rainy,0.3),
      ('Sunny,'Sunny,0.7),

      ('Start,'Sunny,0.5),
      ('Start,'Rainy,0.5),
      ('Start,'Start,0.0)
    )

    val emissions = Counter2(
      ('Rainy,'U,0.9),
      ('Rainy,'N,0.1),

      ('Sunny,'U,0.2),
      ('Sunny,'N,0.8),

      ('Start,'U,0.0),
      ('Start,'N,0.0)
    )

    val hmm = SemiCRF.fromCRF(HMM[Symbol,Symbol]('Start, transitions, emissions, smoothEmissions=false))
    val cal: Marginal[Symbol, Symbol] = hmm.marginal(IndexedSeq('U,'U,'N,'U,'U))
//    val cal: Marginal[Symbol, Symbol] = hmm.marginal(IndexedSeq('U))
    val marginals = (0 until cal.length).map(pos => cal.spanMarginal(pos, pos+1)).map(Encoder.fromIndex(hmm.labelIndex).decode(_))

    assert( (marginals(0)(Some('Rainy)) - 0.8673).abs < 1E-4, marginals(0))
    assert( (marginals(1)(Some('Rainy)) - 0.8204).abs < 1E-4)
    assert( (marginals(2)(Some('Rainy)) - 0.3075).abs < 1E-4)
    assert( (marginals(3)(Some('Rainy)) - 0.8204).abs < 1E-4)
    assert( (marginals(4)(Some('Rainy)) - 0.8673).abs < 1E-4)
    for (begin <- 0 until cal.length; end <- (begin+2) to cal.length ) {
      assert(cal.spanMarginal(begin, end).values.forall(_ == 0.0))
    }
  }


  test("weird gradient thing") {
    val train = IndexedSeq(Segmentation(Array('SK -> Span(0, 2), 'O -> Span(2, 3) ), "sco") )

    val allowedSymSet:Map[IndexedSeq[Char], Set[Symbol]] = Map("sc".toIndexedSeq -> Set('SK), "o".toIndexedSeq -> Set('O))

    val (index, cons) = makeConstraintFactory[Char](allowedSymSet)

    val model = new SemiCRFModel(makeFeaturizer(index, cons, train), cons)

    val obj = new ModelObjective(model, train)

    val grad = GradientTester.test[Int, DenseVector[Double]](obj, obj.initialWeightVector(false) := 0.0, randFraction = 1.0, toString = i => model.featureIndex.get(i).toString)
    assert(norm(grad, Double.PositiveInfinity) < 1E-4, grad)
  }

  private def makeFeaturizer[P, G](index: OptionIndex[Symbol],
                                   consFactory: LabeledSpanConstraints.Factory[Symbol, G],
                                   train: IndexedSeq[Segmentation[Symbol, G]]):SemiCRFModel.BIEOFeaturizer[Symbol, G] = {

    val surfaceFeaturizer =  SurfaceFeaturizer.apply[G]((_, _) => Array.empty)

    val indexed = IndexedSurfaceFeaturizer.fromData[G](surfaceFeaturizer, train.map(_.words), consFactory)
    val cpIndexBuilder = new CrossProductIndex.Builder(index, indexed.featureIndex)
    val cpIndex = cpIndexBuilder.result()


    new BIEOFeaturizer[Symbol, G] {
      override def anchor(w: IndexedSeq[G]): BIEOFeatureAnchoring[Symbol, G] = {
        new BIEOFeatureAnchoring[Symbol, G] {
          val cons = consFactory.constraints(w)
          override def words: IndexedSeq[G] = w

          val surfFeat = indexed.anchor(w)

          val emptyFeatureVector = new FeatureVector(Array())

          override def featuresForBegin(prev: Int, cur: Int, pos: Int): FeatureVector = {
            emptyFeatureVector
          }

          override def featuresForInterior(cur: Int, pos: Int): FeatureVector = emptyFeatureVector

          override def featuresForSpan(prev: Int, cur: Int, beg: Int, end: Int): FeatureVector = {
            new FeatureVector(cpIndex.crossProduct(Array(cur), surfFeat.featuresForSpan(beg, end)))
          }


          override def featureIndex: Index[Feature] = cpIndex
        }

      }

      override def labelIndex: OptionIndex[Symbol] = index

      override def featureIndex: Index[Feature] = cpIndex
    }
  }

  private def makeConstraintFactory[G](syms: Map[IndexedSeq[G], Set[Symbol]]) = {
    val symIndex = Index[Symbol]()

    val allowedSyms = syms.map { case (g, ps) =>
      g -> (new BitSet() ++= ps.map(symIndex.index) )
    }

    allowedSyms.values.foreach(_ += symIndex.size)

    val factory:Factory[Symbol, G] = new Factory[Symbol, G] {
      override def constraints(w: IndexedSeq[G]): LabeledSpanConstraints[Symbol] = {
        val maxAllowedLengthForPhrase = Array.fill[Int](symIndex.size)(w.length)

        val spanConstraints = TriangularArray.tabulate(w.length + 1) { (b, e) =>
          allowedSyms.getOrElse(w.slice(b, e), null)
        }

        val maxLengthForPos = Array.tabulate(w.length){i => w.length}
        new SimpleConstraints(maxLengthForPos, maxAllowedLengthForPhrase, spanConstraints)
      }
    }

    (new OptionIndex(symIndex), factory)


  }
}
