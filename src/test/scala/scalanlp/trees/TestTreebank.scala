package scalanlp.trees

/**
 * 
 * @author dlwh
 */
object TstTreebank {
  val treebank =  {
    val train = TstTreebank.getClass.getClassLoader.getResource("treebanks/smallbank/train");
    val test = TstTreebank.getClass.getClassLoader.getResource("treebanks/smallbank/test");
    val dev = TstTreebank.getClass.getClassLoader.getResource("treebanks/smallbank/dev");

    new SimpleTreebank(Map("train"->train),Map("dev"->dev),Map("test"->test));
  }
}