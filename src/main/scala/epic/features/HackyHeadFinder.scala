package epic.features


trait HackyHeadFinder[L,T] {
  def findHead(label: L, preterminals: Seq[T]): Int;
}

case class RuleBasedHackyHeadFinder() extends HackyHeadFinder[String,String] {
  
  def findHead(label: String, preterminals: Seq[String]): Int = {
    0;
  }
  
}

object RuleBasedHackyHeadFinder {
  
  
  
}
