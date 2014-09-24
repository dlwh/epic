package epic.trees.annotations

import epic.trees.{AnnotatedLabel, HeadFinder, BinarizedTree}
import epic.trees.annotations.TreeAnnotations.HeadTagAnnotation

/**
 * 
 * @author dlwh
 */
class HeadTagAnnotator[W] extends TreeAnnotator[AnnotatedLabel, W, AnnotatedLabel] {
  val headFinder = HeadFinder.collins.lensed[AnnotatedLabel]
  def apply(tree: BinarizedTree[AnnotatedLabel], words: Seq[W]): BinarizedTree[AnnotatedLabel] = {
    headFinder.annotateHeadTags(tree).map{ l => l._1.annotate(HeadTagAnnotation(l._2.label))}
  }
}
