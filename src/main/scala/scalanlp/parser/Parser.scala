package scalanlp.parser;

import scalanlp.trees._;
import scalanlp.classify.Classifier;
import scalanlp.data._;

trait Parser[L,W] extends Classifier[Tree[L],Seq[W]] {
  def bestParse(s: Seq[W]) = classify(s);
}
