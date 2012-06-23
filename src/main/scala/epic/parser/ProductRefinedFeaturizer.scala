package epic.parser

import breeze.util.EitherIndex

/**
 *
 * @author dlwh
 */

class ProductRefinedFeaturizer[L, W, Feat1, Feat2](sf1: RefinedGrammar[L, W],
                                        sf2: RefinedGrammar[L, W],
                                        feat1: RefinedFeaturizer[L, W, Feat1],
                                        feat2: RefinedFeaturizer[L, W, Feat2]) extends RefinedFeaturizer[L, W, Either[Feat1, Feat2]] {
  def index: EitherIndex[Feat1, Feat2] = feat1.index | feat2.index

  def anchor(w: Seq[W]):Anchoring = {
    val s1 = sf1.anchor(w)
    val s2 = sf2.anchor(w)
    val f1 = feat1.anchor(w)
    val f2 = feat1.anchor(w)
    new ProductRefinementsHandler[L, W](s1, s2) with Anchoring {
      def words = w

      def featuresForBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int) = {
        val arr1 = f1.featuresForBinaryRule(begin, split, end, rule, rule1Ref(rule, ref))
        val arr2 = f2.featuresForBinaryRule(begin, split, end, rule, rule2Ref(rule, ref))
        arr1 ++ arr2.map(_ + index.rightOffset)
      }

      def featuresForUnaryRule(begin: Int, end: Int, rule: Int, ref: Int) = {
        val arr1 = f1.featuresForUnaryRule(begin, end, rule, rule1Ref(rule, ref))
        val arr2 = f2.featuresForUnaryRule(begin, end, rule, rule2Ref(rule, ref))
        arr1 ++ arr2.map(_ + index.rightOffset)
      }

      def featuresForSpan(begin: Int, end: Int, label: Int, ref: Int) = {
        val arr1 = f1.featuresForSpan(begin, end, label, label1Ref(label, ref))
        val arr2 = f2.featuresForSpan(begin, end, label, label2Ref(label, ref))
        arr1 ++ arr2.map(_ + index.rightOffset)
      }
    }
  }
}
