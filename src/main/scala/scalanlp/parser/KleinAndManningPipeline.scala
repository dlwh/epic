package scalanlp.parser
import scalanlp.trees._

import projections.GrammarProjections
import scalanlp.parser.ParseEval.Statistics
import scalanlp.trees.Trees
import scalala.library.Library
import scalanlp.parser.ParseChart._
import collection.IndexedSeq

/**
 * 
 * @author dlwh
 */

object KleinAndManningPipeline extends ParserPipeline {
  protected val paramManifest = manifest[Params]
  case class Params(parser: ParserParams.BaseParser,
                    horizontal: Int = 2,
                    vertical: Int = 2)

  def trainParser(trainTrees: IndexedSeq[TreeInstance[String, String]],
                  validate: (Parser[String, String]) => Statistics,
                  params: Params) = {

    val (initLexicon,initBinaries,initUnaries) = GenerativeParser.extractCounts(trainTrees);

    val xbarParser = params.parser.optParser getOrElse {
      val grammar = Grammar(Library.logAndNormalizeRows(initBinaries),Library.logAndNormalizeRows(initUnaries));
      val lexicon = new SimpleLexicon(initLexicon);
      new CKYChartBuilder[LogProbabilityParseChart,String,String]("",lexicon,grammar,ParseChart.logProb);
    }

    val transformed: IndexedSeq[TreeInstance[String, String]] = trainTrees.map { ti =>
      val tree = (
        {identity[BinarizedTree[String]](_)}
          andThen {Trees.annotateParentsBinarized(_,params.vertical)}
          andThen {Trees.addHorizontalMarkovization(_,params.horizontal)}
          andThen {splitAux(_:BinarizedTree[String],ti.words)}
          andThen {splitVP(_)}
          andThen {splitIN(_)}
          andThen {splitPossNP(_)}
          andThen {annotateBaseNP(_)}
//          andThen {annotateRightRecNP(_)}
          andThen {markNonIdentityUnaries(_)}
          andThen {markExternalUnaries(_)}
          andThen {markDominates(_,">DV",l => l.startsWith("V") || l.startsWith("MD"))}
      )(ti.tree)
      ti.copy(tree=tree)
    }
    val (words,binary,unary) = GenerativeParser.extractCounts(transformed);
    val grammar = Grammar(Library.logAndNormalizeRows(binary),Library.logAndNormalizeRows(unary));
    println(grammar.labelIndex)
    val lexicon = new SignatureLexicon(words, EnglishWordClassGenerator, 5);
    val builder = CKYChartBuilder("",lexicon,grammar).withCharts(ParseChart.logProb)
    val proj = GrammarProjections(xbarParser.grammar,grammar,Trees.deannotateLabel _)
    val decoder = new MaxConstituentDecoder[String,String,String](proj)
    val parser = new SimpleChartParser[String,String,String](builder,decoder,proj)
    Iterator("Markovized" -> parser)
  }

  def markNonIdentityUnaries(tree: BinarizedTree[String]):BinarizedTree[String] = tree match {
    case BinaryTree(label,lc,rc) => BinaryTree(label,markNonIdentityUnaries(lc),markNonIdentityUnaries(rc))(tree.span)
    case NullaryTree(label) => tree
    case UnaryTree(label,c) if label != "" && label != c.label => UnaryTree(label +">U",markNonIdentityUnaries(c))(tree.span)
    case UnaryTree(label,c) => UnaryTree(label,markNonIdentityUnaries(c))(tree.span)
  }


  def markExternalUnaries(tree: BinarizedTree[String], shouldAnnotate: String=>Boolean = Set("RB","DT")):BinarizedTree[String] = tree match {
    case BinaryTree(label,lc,rc) => BinaryTree(label,markExternalUnaries(lc),markExternalUnaries(rc))(tree.span)
    case NullaryTree(label) => tree
    case UnaryTree(label,c) if label != "" && label != c.label && shouldAnnotate(c.label) =>
      UnaryTree(label,markExternalUnaries(c).relabelRoot(_ + "^U"))(tree.span)
    case UnaryTree(label,c) => UnaryTree(label,markExternalUnaries(c))(tree.span)
  }

  val beVerbs = Set("be","is","are","were","am","was","been","being")
  val hasVerbs = Set("has","have","had")

  def splitAux(tree: BinarizedTree[String], words: Seq[String]) = tree.extend[String] { (t:BinarizedTree[String]) =>
    t match {
      case UnaryTree(label,NullaryTree(lbl2)) if label == lbl2 =>
        val w = words(t.span.start)
        if (beVerbs.contains(w.toLowerCase)) label+">BE"
        else if (hasVerbs.contains(w.toLowerCase)) label+">HAVE"
        else label
      case NullaryTree(label) =>
        val w = words(t.span.start)
        if (beVerbs.contains(w.toLowerCase)) label+">BE"
        else if (hasVerbs.contains(w.toLowerCase)) label+">HAVE"
        else label
      case t:BinarizedTree[String] => t.label
    }
  }

  def baseLabel(l: String) = l.dropWhile(_ == '@').takeWhile(c => c != '>' && c != '^')

  val activeVerbs = Set("VBZ","VBD","VBP","MD")
  def splitVP(tree: BinarizedTree[String]) = tree.extend{ (t: BinarizedTree[String]) =>
    if(baseLabel(t.label) != "VP") t.label
    else {
      val headTag = HeadFinder.collinsHeadFinder.findHeadTag(t,baseLabel)
      val base = baseLabel(headTag)
      if (activeVerbs(base)) {
        t.label + ">VBF"
      } else {
        t.label + ">" + base
      }
    }
  }

  def splitPossNP(tree: BinarizedTree[String]) = tree.extend{ (t: BinarizedTree[String]) =>
    if(baseLabel(t.label) != "NP") t.label
    else {
      val headTag = HeadFinder.collinsHeadFinder.findHeadTag(t,baseLabel)
      val base = baseLabel(headTag)
      if (base == "POS") {
        t.label + ">P"
      } else {
        t.label
      }
    }
  }

  def markDominates(tree: BinarizedTree[String], label: String, pred: String=>Boolean) = {
    def dominates(x: BinarizedTree[String]) = x.leaves.exists { t => pred(t.label) }
    tree.extend { (t:BinarizedTree[String]) =>
      if(t.label == "") t.label
      else if(dominates(t)) t.label + label
      else t.label
    }
  }

  def splitIN(tree: BinarizedTree[String],
              parent: Option[String] = None,
              grandParent: Option[String] = None):BinarizedTree[String] = {
    val blbl = baseLabel(tree.label)
    tree match {
      case tree@NullaryTree(lbl) if blbl == "IN" =>
        if(grandParent.isEmpty || grandParent.exists(_ == "") || parent.exists(_ == "")) {
          tree
        } else if (grandParent.exists(_(0) == 'N') && (parent.exists(s => s(0) == 'P' || s(0) == 'A'))) {
          tree.copy(lbl + ">N")(tree.span)
        } else if (parent.exists(_(0) == 'Q') && (grandParent.exists(s => s(0) == 'N' || s.startsWith("ADJP")))) {
          tree.copy(lbl + ">Q")(tree.span)
        } else if(grandParent.exists(_ == "S")) {
          if(parent.exists(_ == "SBAR")) {
            tree.copy(lbl + ">SCC")(tree.span)
          } else {
            tree.copy(lbl + ">SC")(tree.span)
          }
        } else {
          tree
        }
      case UnaryTree(lbl,c) =>
        if(blbl != "IN") {
          if(parent.exists(_ != blbl))
            UnaryTree(lbl,splitIN(c,Some(blbl),parent))(tree.span)
          else
            UnaryTree(lbl,splitIN(c,parent,grandParent))(tree.span)
        } else {
          val nc = splitIN(c,parent,grandParent)
          UnaryTree(nc.label,nc)(tree.span)
        }
      case BinaryTree(lbl,l,r) =>
        BinaryTree(lbl,splitIN(l,Some(blbl),parent),splitIN(r,Some(blbl),parent))(tree.span)
      case _ => tree
    }

  }

  def annotateBaseNP(tree: BinarizedTree[String]) = {
    // boolean is whether or not it's a "base"
    def rec(tree: BinarizedTree[String]):(BinarizedTree[String],Boolean) = tree match {
      case t:NullaryTree[String] => t -> true
      case t@UnaryTree(lbl1, NullaryTree(lbl2)) if baseLabel(lbl1) == baseLabel(lbl2) =>
        t -> true
      case t@UnaryTree(lbl1, child) =>
        val (newchild,ok) = rec(child)
        if(ok && baseLabel(lbl1) == "NP") {
          UnaryTree(lbl1+">B",newchild)(t.span) -> true
        } else {
          UnaryTree(lbl1,newchild)(t.span) -> false
        }
      case t@BinaryTree(lbl,lc,rc) =>
        val (newlc,lok) = rec(lc)
        val (newrc,rok) = rec(rc)
        if(lok && rok && baseLabel(lbl) == "NP") {
          BinaryTree(lbl+">B",newlc,newrc)(t.span) -> true
        } else {
          BinaryTree(lbl,newlc,newrc)(t.span) -> false
        }

    }
    rec(tree)._1

  }

  def annotateRightRecNP(tree: BinarizedTree[String]) = {
    // boolean is whether or not it has a right-most np
    def rec(tree: BinarizedTree[String]):(BinarizedTree[String],Boolean) = tree match {
      case t:NullaryTree[String] => t -> false
      case t@UnaryTree(lbl1, child) =>
        val (newchild,ok) = rec(child)
        if(ok && baseLabel(lbl1) == "NP") {
          UnaryTree(lbl1+">RRNP",newchild)(t.span) -> true
        } else {
          UnaryTree(lbl1,newchild)(t.span) -> (ok||baseLabel(lbl1) == "NP" && !lbl1.startsWith("@"))
        }
      case t@BinaryTree(lbl,lc,rc) =>
        val (newlc,_) = rec(lc)
        val (newrc,rok) = rec(rc)
        if(rok && baseLabel(lbl) == "NP") {
          BinaryTree(lbl+">RRNP",newlc,newrc)(t.span) -> true
        } else {
          BinaryTree(lbl,newlc,newrc)(t.span) -> (rok || (baseLabel(lbl) == "NP" && !lbl.startsWith("@")))
        }

    }
    rec(tree)._1

  }


}