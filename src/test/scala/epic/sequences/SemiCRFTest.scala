package epic.sequences

import org.scalatest.FunSuite
import breeze.util.Encoder
import breeze.linalg.Counter2
import epic.sequences.SemiCRF.Marginal

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

    val hmm = SemiCRF.fromCRF(new CRF(HMM[Symbol,Symbol]('Start,transitions,emissions, smoothEmissions=false)))
    val cal: Marginal[Symbol, Symbol] = hmm.marginal(IndexedSeq('U,'U,'N,'U,'U))
//    val cal: Marginal[Symbol, Symbol] = hmm.marginal(IndexedSeq('U))
    val marginals = (0 until cal.length).map(pos => cal.spanMarginal(pos, pos+1)).map(Encoder.fromIndex(hmm.labelIndex).decode(_))
    assert( (marginals(0)('Rainy) - 0.8673).abs < 1E-4)
    assert( (marginals(1)('Rainy) - 0.8204).abs < 1E-4)
    assert( (marginals(2)('Rainy) - 0.3075).abs < 1E-4)
    assert( (marginals(3)('Rainy) - 0.8204).abs < 1E-4)
    assert( (marginals(4)('Rainy) - 0.8673).abs < 1E-4)
    for (begin <- 0 until cal.length; end <- (begin+2) to cal.length ) {
      assert(cal.spanMarginal(begin, end).values.forall(_ == 0.0))
    }
  }
}
