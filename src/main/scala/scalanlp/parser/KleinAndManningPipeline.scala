package scalanlp.parser

import projections.{ProjectingSpanScorer, GrammarProjections}
import scalanlp.trees._

import scalanlp.parser.ParseEval.Statistics
import scalanlp.trees.Trees
import scalala.library.Library
import scalanlp.parser.ParseChart._
import collection.IndexedSeq

case class AnnotatedLabel(label: String,
                          parents: Seq[String] = Seq.empty,
                          siblings: Seq[Either[String,String]] = Seq.empty,
                          features: Set[Symbol] = Set.empty) {
  def annotate(sym: Symbol) = copy(features = features + sym)
  def isIntermediate = label.nonEmpty && label.charAt(0) == '@'
  def baseLabel = label.dropWhile(_ == '@')

  override lazy val hashCode = {
    scala.runtime.ScalaRunTime._hashCode(this)
  }
}

@SerialVersionUID(1)
class KMPipeline(horizontal: Int = 2, vertical: Int = 2) extends ((BinarizedTree[String],Seq[String])=>BinarizedTree[AnnotatedLabel]) with Serializable {

  def apply(tree: BinarizedTree[String], words: Seq[String]) =  {
    val treeOut = (
        {(_:BinarizedTree[String]).map(AnnotatedLabel(_))}
          andThen {annVert _}
          andThen {annHorz _}
          andThen {splitAux(_,words)}
          andThen {splitVP(_)}
          andThen {splitIN(_)}
          andThen {splitPossNP(_)}
          andThen {annotateBaseNP(_)}
          andThen {annotateRightRecNP(_)}
          andThen {markNonIdentityUnaries(_)}
          andThen {markExternalUnaries(_)}
          andThen {markDominates(_,"V",l => l.startsWith("V") || l.startsWith("MD"))}
      )(tree)
    treeOut
  }

  private def annVert(tree: BinarizedTree[AnnotatedLabel]):BinarizedTree[AnnotatedLabel] = {
    def join(base: AnnotatedLabel, parent: AnnotatedLabel) = {
      base.copy(parents = base.parents :+ parent.label)
    }
    Trees.annotateParentsBinarized(tree,join,{!(_:AnnotatedLabel).isIntermediate},vertical)
  }

  private def annHorz(tree: BinarizedTree[AnnotatedLabel]):BinarizedTree[AnnotatedLabel] = {
    def join(base: AnnotatedLabel, siblings: Seq[Either[AnnotatedLabel,AnnotatedLabel]]) = {
      val news = siblings.map {
        case Left(x) => Left(x.label)
        case Right(x) => Right(x.label)
      }

      base.copy(siblings = news)
    }
    Trees.addHorizontalMarkovization(tree,horizontal,join,{(_:AnnotatedLabel).isIntermediate})
  }

  val beVerbs = Set("be","is","are","were","am","was","been","being")
  val hasVerbs = Set("has","have","had")

  private def splitAux(tree: BinarizedTree[AnnotatedLabel], words: Seq[String]): BinarizedTree[AnnotatedLabel] = {
    tree.extend { (t:BinarizedTree[AnnotatedLabel]) =>
      t match {
        case UnaryTree(label,NullaryTree(lbl2)) if label.baseLabel == lbl2.baseLabel =>
          val w = words(t.span.start)
          if (beVerbs.contains(w.toLowerCase)) label.annotate('AuxBe).annotate('Aux)
          else if (hasVerbs.contains(w.toLowerCase)) label.annotate('AuxHave).annotate('Aux)
          else label
        case NullaryTree(label) =>
          val w = words(t.span.start)
          if (beVerbs.contains(w.toLowerCase)) label.annotate('AuxBe).annotate('Aux)
          else if (hasVerbs.contains(w.toLowerCase)) label.annotate('AuxHave).annotate('Aux)
          else label
        case _ => t.label
      }
    }
  }

  val activeVerbs = Set("VBZ","VBD","VBP","MD")
  private def splitVP(tree: BinarizedTree[AnnotatedLabel]) = tree.extend{ (t: BinarizedTree[AnnotatedLabel]) =>
    if(t.label.baseLabel != "VP") t.label
    else {
      val headTag = HeadFinder.collinsHeadFinder.findHeadTag(t,{(_:AnnotatedLabel).baseLabel})
      val base = headTag.baseLabel
      if (activeVerbs(base)) {
        t.label.annotate('VPisVBF)
      } else {
        t.label.annotate(Symbol("VPis"+base))
      }
    }
  }


  private def splitIN(tree: BinarizedTree[AnnotatedLabel],
              parent: Option[String] = None,
              grandParent: Option[String] = None):BinarizedTree[AnnotatedLabel] = {
    val blbl = tree.label.baseLabel
    tree match {
      case tree@NullaryTree(lbl) if blbl == "IN" =>
        if(grandParent.isEmpty || grandParent.exists(_ == "") || parent.exists(_ == "")) {
          tree
        } else if (grandParent.exists(_(0) == 'N') && (parent.exists(s => s(0) == 'P' || s(0) == 'A'))) {
          tree.copy(lbl.annotate('IN_N))(tree.span)
        } else if (parent.exists(_(0) == 'Q') && (grandParent.exists(s => s(0) == 'N' || s.startsWith("ADJP")))) {
          tree.copy(lbl.annotate('IN_Q))(tree.span)
        } else if(grandParent.exists(_ == "S")) {
          if(parent.exists(_ == "SBAR")) {
            tree.copy(lbl.annotate('IN_SCC))(tree.span)
          } else {
            tree.copy(lbl.annotate('IN_SC))(tree.span)
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

  def splitPossNP(tree: BinarizedTree[AnnotatedLabel]) = tree.extend{ (t: BinarizedTree[AnnotatedLabel]) =>
    if(t.label.baseLabel != "NP") t.label
    else {
      val headTag = HeadFinder.collinsHeadFinder.findHeadTag(t,{(_:AnnotatedLabel).baseLabel})
      val base = headTag.baseLabel
      if (base == "POS") {
        t.label.annotate('NP_Possessive)
      } else {
        t.label
      }
    }
  }


  def annotateBaseNP(tree: BinarizedTree[AnnotatedLabel]) = {
    // boolean is whether or not it's a "base"
    def rec(tree: BinarizedTree[AnnotatedLabel]):(BinarizedTree[AnnotatedLabel],Boolean) = tree match {
      case t:NullaryTree[AnnotatedLabel] => t -> true
      case t@UnaryTree(lbl1, NullaryTree(lbl2)) if lbl1.baseLabel == lbl2.baseLabel =>
        t -> true
      case t@UnaryTree(lbl1, child) =>
        val (newchild,ok) = rec(child)
        if(ok && lbl1.baseLabel == "NP") {
          UnaryTree(lbl1.annotate('BaseNP),newchild)(t.span) -> true
        } else if(lbl1.label == "") {
          UnaryTree(lbl1,newchild)(t.span) -> false
        } else {
        UnaryTree(lbl1,newchild)(t.span) -> false
        }
      case t@BinaryTree(lbl,lc,rc) =>
        val (newlc,lok) = rec(lc)
        val (newrc,rok) = rec(rc)
        if(lok && rok && lbl.baseLabel == "NP") {
          BinaryTree(lbl.annotate('BaseNP),newlc,newrc)(t.span) -> true
        } else {
          BinaryTree(lbl,newlc,newrc)(t.span) -> false
        }

    }
    rec(tree)._1

  }

  // TODO: fix
  def annotateRightRecNP(tree: BinarizedTree[AnnotatedLabel]) = {
    // boolean is whether or not it has a right-most np
    def rec(tree: BinarizedTree[AnnotatedLabel]):(BinarizedTree[AnnotatedLabel],Boolean) = tree match {
      case t:NullaryTree[AnnotatedLabel] => t -> false
      case t@UnaryTree(lbl1, child) =>
        val (newchild,ok) = rec(child)
        if(ok && lbl1.baseLabel == "NP") {
          UnaryTree(lbl1.annotate('RRNP),newchild)(t.span) -> true
        } else {
          UnaryTree(lbl1,newchild)(t.span) -> (ok||lbl1.label == "NP")
        }
      case t@BinaryTree(lbl,lc,rc) =>
        val (newrc,rok) = rec(rc)
        if(rok && lbl.baseLabel == "NP") {
          val (newlc,_) = rec(lc)
          val lclc = annotateDownwards(newlc)
          BinaryTree(lbl.annotate('RRNP),lclc,newrc)(t.span) -> true
        } else {
          val (newlc,_) = rec(lc)
          BinaryTree(lbl,newlc,newrc)(t.span) -> (rok || (lbl.label == "NP"))
        }

    }

    def annotateDownwards(tree: BinarizedTree[AnnotatedLabel]):BinarizedTree[AnnotatedLabel] = tree match {
      case t:NullaryTree[AnnotatedLabel] => t
      case UnaryTree(lbl,child) if lbl.baseLabel == "NP" =>
        UnaryTree(lbl.annotate('RRNP), annotateDownwards(child))(tree.span)
      case BinaryTree(lbl,lc,rc) if lbl.baseLabel == "NP" =>
        BinaryTree(lbl.annotate('RRNP), annotateDownwards(lc), annotateDownwards(rc))(tree.span)
      case _ => tree
    }
    rec(tree)._1

  }


  def markNonIdentityUnaries(tree: BinarizedTree[AnnotatedLabel]):BinarizedTree[AnnotatedLabel] = tree match {
    case BinaryTree(label,lc,rc) => BinaryTree(label,markNonIdentityUnaries(lc),markNonIdentityUnaries(rc))(tree.span)
    case NullaryTree(label) => tree
    case UnaryTree(label,c) if label.label != "" && label.label != c.label.label => UnaryTree(label.annotate('RealUnary),markNonIdentityUnaries(c))(tree.span)
    case UnaryTree(label,c) => UnaryTree(label,markNonIdentityUnaries(c))(tree.span)
  }

  def markExternalUnaries(tree: BinarizedTree[AnnotatedLabel], shouldAnnotate: String=>Boolean = Set("RB","DT")):BinarizedTree[AnnotatedLabel] = tree match {
    case BinaryTree(label,lc,rc) => BinaryTree(label,markExternalUnaries(lc),markExternalUnaries(rc))(tree.span)
    case NullaryTree(label) => tree
    case UnaryTree(label,c) if label.label != "" && label.label != c.label.label && shouldAnnotate(c.label.label) =>
      UnaryTree(label,markExternalUnaries(c).relabelRoot(_.annotate('ExternalUnary)))(tree.span)
    case UnaryTree(label,c) => UnaryTree(label,markExternalUnaries(c))(tree.span)
  }

  def markDominates(tree: BinarizedTree[AnnotatedLabel], label: String, pred: String=>Boolean) = {
    def dominates(x: BinarizedTree[AnnotatedLabel]) = x.leaves.exists { t => pred(t.label.label) }
    tree.extend { (t:BinarizedTree[AnnotatedLabel]) =>
      if(t.label.label == "") t.label
      else if(dominates(t)) t.label.annotate(Symbol("Dom"+label))
      else t.label
    }
  }
}

/**
 * 
 * @author dlwh
 */

object KleinAndManningPipeline extends ParserPipeline {
  protected val paramManifest = manifest[Params]
  case class Params(parser: ParserParams.BaseParser[String],
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

    val pipeline = new KMPipeline(params.horizontal, params.vertical)

    val transformed = trainTrees.par.map { ti =>
      val t = pipeline(ti.tree,ti.words)
      TreeInstance(ti.id,t,ti.words)
    }.seq
    val (words,binary,unary) = GenerativeParser.extractCounts(transformed);
    val grammar = Grammar(Library.logAndNormalizeRows(binary),Library.logAndNormalizeRows(unary));
    println(grammar.labelIndex)
    val lexicon = new SignatureLexicon(words, EnglishWordClassGenerator, 5);
    val builder = CKYChartBuilder(AnnotatedLabel(""),lexicon,grammar).withCharts(ParseChart.logProb)
    val proj = GrammarProjections(xbarParser.grammar,grammar,{(_:AnnotatedLabel).label})
    val decoder = new MaxConstituentDecoder[String,AnnotatedLabel,String](proj)
    val parser = new SimpleChartParser[String,AnnotatedLabel,String](builder,decoder,proj)
    val maxV = new MaxVariationalDecoder[String,AnnotatedLabel,String](xbarParser.grammar,xbarParser.lexicon,proj,builder)
    val maxVParser = new SimpleChartParser[String,AnnotatedLabel,String](builder,maxV,proj)
    val vit = new ViterbiDecoder[String,AnnotatedLabel,String](proj.labels)
    val viterbiParser = new SimpleChartParser[String,AnnotatedLabel,String](builder,vit,proj)
    Iterator("maxV" -> maxVParser, "Viterbi" -> viterbiParser, "Markovized" -> parser)
  }

}