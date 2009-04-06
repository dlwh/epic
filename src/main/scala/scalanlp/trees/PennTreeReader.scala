package scalanlp.trees;

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.lexical._;
import scala.util.parsing.input._;

object PennTreeReader extends StdLexical with ImplicitConversions with Scanners {
  def lparen = (ws ~> accept('(')) <~ ws;
  def rparen = (ws ~> accept(')')) <~ ws;
  def other = acceptIf( c => !c.isWhitespace && c != '(' && c != ')')( "'" + _ + "'not expected");
  def ws = whitespace;

  def tok = rep(other) ^^ { x => x.mkString("")};

  def seqTree(pos:Int):Parser[(List[Tree[String]],Seq[String])] = (
    tree(pos) >> { case (tree,words) => 
        seqTree(pos+words.length) ^^ { case (restTrees,restWords) => 
          (tree :: restTrees, words.toList ++ restWords.toList)
        }
    }
  | tree(pos) ^^ { case (tree,words) => (tree :: Nil, words) }
  )

  def tree(pos:Int):Parser[(Tree[String],Seq[String])] = ( 
   ( (lparen ~> tok <~ ws) ~ tok <~ rparen ^^ {
      case (label ~ word) => (Tree(label,Seq())(Span(pos,pos+1)),Seq(word))
    })
    |(lparen ~> opt(tok) ) ~ (seqTree(pos) <~ rparen) ^^ { case (mbLabel ~ children) =>
      val words = children._2;
      (Tree(mbLabel.getOrElse(""),children._1)(Span(pos,pos + words.length)), words)
    }
  )


  def apply(input: String): Either[(Tree[String],Seq[String]),ParseResult[(Tree[String],Seq[String])]] = {
      phrase(tree(0))(new CharSequenceReader(input)) match {
        case Success( result, _) => Left( result )
        case x => Right(x);
      }
  }

}
