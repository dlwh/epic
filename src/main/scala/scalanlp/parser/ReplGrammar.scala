package scalanlp.parser


import scalanlp.trees._;

/**
 * This class just provides some objects I commonly want on the REPL.
 * @author dlwh
 */
class ReplGrammar(treebankPath: String, isDenseTreebank:Boolean=true, binarizationKind: String = "xbar") {
  lazy val treebank = if(isDenseTreebank) DenseTreebank.fromZipFile(new java.io.File(treebankPath))
  else Treebank.fromPennTreebankDir(new java.io.File(treebankPath));

  val binarize = {
    val headRules = binarizationKind match {
      case "xbar" | "right" => HeadFinder.right[String]
      case "leftXbar" | "left" => HeadFinder.left[String]
      case "head" => HeadFinder.collins
      case _ => HeadFinder.collins
    }
    Trees.binarize((_:Tree[String]), headRules)
  }

  val maxLength = 15;

  val xform = Trees.Transforms.StandardStringTransform;

  lazy val trainTrees = IndexedSeq.empty ++ (for( (tree,words) <- treebank.train.trees.filter(_._2.length <= maxLength))
    yield TreeInstance(words.toString(),binarize(xform(tree)),words)).toSeq;

  lazy val devTrees = IndexedSeq.empty ++ (for( (tree,words) <- treebank.dev.trees.filter(_._2.length <= maxLength))
    yield TreeInstance(words.toString(),binarize(xform(tree)),words)).toSeq;
}