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
    if(binarizationKind == "xbar") Trees.xBarBinarize(_:Tree[String],false);
    else Trees.binarize(_:Tree[String]);
  }

  val maxLength = 15;

  val xform = Trees.Transforms.StandardStringTransform;

  lazy val trainTrees = IndexedSeq.empty ++ (for( (tree,words) <- treebank.train.trees.filter(_._2.length <= maxLength))
    yield TreeInstance(words.toString(),binarize(xform(tree)),words)).toSeq;

  lazy val devTrees = IndexedSeq.empty ++ (for( (tree,words) <- treebank.dev.trees.filter(_._2.length <= maxLength))
    yield TreeInstance(words.toString(),binarize(xform(tree)),words)).toSeq;

  lazy val (lexicon,grammar) = GenerativeParser.extractLexiconAndGrammar(trainTrees);
}