package epic.util

import breeze.util.SerializableLogging
import scala.annotation.switch

/**
 * based on http://languagelog.ldc.upenn.edu/myl/ldc/morph/buckwalter.html
 *
 * http://www.qamus.org/transliteration.htm
 *
 * @author dlwh
 **/
object ArabicNormalization extends SerializableLogging {

  def handleTreebankThings(s: String):Option[String] = {
    if (!s.startsWith("-")) {
      None
    } else {
      s match {
        case "-LRB-" => Some(s)
        case "-RRB-" => Some(s)
        case "-LSB-" => Some(s)
        case "-RSB-" => Some(s)
        case "-LCB-" => Some(s)
        case "-RCB-" => Some(s)
        case "-PLUS-" => Some(s)
        case "-MINUS-" => Some(s)
        case _ => None
      }
    }
  }

  def buckwalterToUnicode(buckwalter: String): String = {
    handleTreebankThings(buckwalter) match {
      case Some(x) => x
      case None =>
        val out = new StringBuilder()
        out.sizeHint(buckwalter.length)
        var i = 0
        while (i < buckwalter.length) {
          out += { ( buckwalter(i): @switch) match {
            case '\'' => '\u0621'
            case '|' => '\u0622'
            case '>' | 'O' => '\u0623'
            case '&' | 'W' => '\u0624'
            case '<' | 'I' => '\u0625'
            case '}' => '\u0626'
            case 'A' => '\u0627'
            case 'b' => '\u0628'
            case 'p' => '\u0629'
            case 't' => '\u062A'
            case 'v' => '\u062B'
            case 'j' => '\u062C'
            case 'H' => '\u062D'
            case 'x' => '\u062E'
            case 'd' => '\u062F'
            case '*' => '\u0630'
            case 'r' => '\u0631'
            case 'z' => '\u0632'
            case 's' => '\u0633'
            case '$' => '\u0634'
            case 'S' => '\u0635'
            case 'D' => '\u0636'
            case 'T' => '\u0637'
            case 'Z' => '\u0638'
            case 'E' => '\u0639'
            case 'g' => '\u063A'
            case '_' => '\u0640'
            case 'f' => '\u0641'
            case 'q' => '\u0642'
            case 'k' => '\u0643'
            case 'l' => '\u0644'
            case 'm' => '\u0645'
            case 'n' => '\u0646'
            case 'h' => '\u0647'
            case 'w' => '\u0648'
            case 'Y' => '\u0649'
            case 'y' => '\u064A'
            case 'F' => '\u064B'
            case 'N' => '\u064C'
            case 'K' => '\u064D'
            case 'a' => '\u064E'
            case 'u' => '\u064F'
            case 'i' => '\u0650'
            case '~' => '\u0651'
            case 'o' => '\u0652'
            case '`' => '\u0670'
            case '{' => '\u0671'
            case '.' | '?' | '!' | ',' | '"' | '%' | '-' | '/' | ':' | ';' | '=' => buckwalter(i)
            case x =>
              if (!x.isDigit)
                logger.warn("Unknown buckwalter character: " + x)
              x
          }}
          i += 1
        }
        out.result()
    }
  }

}
