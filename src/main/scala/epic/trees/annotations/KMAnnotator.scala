package epic.trees
package annotations

/**
 *
 * @author dlwh
 */

case class KMAnnotator() extends TreeAnnotator[AnnotatedLabel, String, AnnotatedLabel] {
  val pipeline = (
    StripAnnotations[String]() andThen
    AddMarkovization[String](2,2) andThen
    SplitAuxiliary() andThen
    SplitVP() andThen
    SplitIN[String]() andThen
    SplitPossNP[String]() andThen
    AnnotateBaseNP[String]() andThen
    AnnotateRightRecNP[String]() andThen
    MarkNonIdentityUnaries[String]() andThen
    MarkExternalUnaries[String]() andThen
    DominatesV[String]()
    )


  def apply(tree: BinarizedTree[AnnotatedLabel], words: Seq[String]) = pipeline(tree, words)


}
