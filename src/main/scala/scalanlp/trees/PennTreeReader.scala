package scalanlp.trees;

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.lexical._;
import scala.util.parsing.input._;

object PennTreeReader2 {
  def readTree(text: String) = {
    
  }
}

object PennTreeReader extends StdLexical with ImplicitConversions with Scanners {
  private def lparen = (ws ~> accept('(')) <~ ws;
  private def rparen = (ws ~> accept(')')) <~ ws;
  private def other = acceptIf( c => !c.isWhitespace && c != '(' && c != ')')( "'" + _ + "'not expected");
  private def ws = whitespace;

  private def tok = rep(other) ^^ { x => x.mkString("")};

  private def seqTree(pos:Int):Parser[(List[Tree[String]],Seq[String])] = (
    tree(pos) >> { case (tree,words) => 
      seqTree(pos+words.length).? ^^ { 
        case Some( (restTrees,restWords) ) => 
          (tree :: restTrees, words.toList ++ restWords.toList)
        case None =>(tree :: Nil, words) }
    }
  )

  private def tree(pos:Int):Parser[(Tree[String],Seq[String])] = ( 
   ( (lparen ~> tok <~ ws) ~ tok <~ rparen ^^ {
      case (label ~ word) => (Tree(label,Seq())(Span(pos,pos+1)),Seq(word))
      
    })
    |(lparen ~> opt(tok) ) ~ (seqTree(pos) <~ rparen) ^^ { case (mbLabel ~ children) =>
      val words = children._2;
      (Tree(mbLabel.getOrElse(""),children._1)(Span(pos,pos + words.length)), words)
    }
  )

  def readTrees(input: String): Either[List[(Tree[String],Seq[String])],ParseResult[List[(Tree[String],Seq[String])]]] = {
    phrase(rep1(tree(0)))(new CharSequenceReader(input)) match {
        case Success( result, _) => Left( result )
        case x => Right(x);
      }
  }


  def readTree(input: String): Either[(Tree[String],Seq[String]),ParseResult[(Tree[String],Seq[String])]] = {
      phrase(tree(0))(new CharSequenceReader(input)) match {
        case Success( result, _) => Left( result )
        case x => Right(x);
      }
  }

}
