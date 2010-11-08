package scalanlp.parser
package projections

/**
 * A trait for projecting charts from coarse to fine.
 * @author dlwh
 */
trait ChartProjector[Chart[X]<:ParseChart[X],C,F] {
  def projectChart(coarse: Chart[C]):Chart[F];
}

object ChartProjector {
  def apply[Chart[X]<:ParseChart[X],C,F](chartFactory: ParseChart.Factory[Chart],
                                         fineGrammar: Grammar[F],
                                         proj: Int=>Int):ChartProjector[Chart,C,F]  = new ChartProjector[Chart,C,F] {
    def projectChart(coarse: Chart[C]) = {
      val fine = chartFactory(fineGrammar, coarse.length);
      for(start <- 0 until coarse.length; end <- (start+1) until coarse.length;
          (a,score) <- coarse.enteredLabelScores(start,end)) {
        fine.enter(start,end,proj(a),score);
      }
      fine;
    }
  }
}