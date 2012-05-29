package scalanlp.trees;
/*
 Copyright 2011 David Hall

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/


import scala.collection.JavaConversions._
import java.io.PushbackReader
import java.io.Reader
import java.io.StringReader
import scala.collection.mutable.ArrayBuffer

/**
 * PennTreeReader due to Adam Pauls.
 *
 * @author adpauls
 * @author dlwh
 */
class PennTreeReader(val reader : Reader,
                     val lowercase : Boolean = false,
                     val ROOT_LABEL : String = "",
                     dropLabels:Set[String]=Set("-NONE-")) extends Iterator[(Tree[String],IndexedSeq[String])] {

  private val in = new PushbackReader(reader, 4)

  private var nextTree = readRootTree()

  def hasNext = (nextTree != null);

  def next() = {
    if (!hasNext) throw new NoSuchElementException();
    val tree = nextTree;

    nextTree = readRootTree();

    tree;
  }

  private def readRootTree() : (Tree[String], IndexedSeq[String]) = {
    readWhiteSpace();
    if (!isLeftParen(peek())) null
    else {
      val tree = readTree(true, 0)
      tree
    }

  }


  private def readTree(isRoot : Boolean, pos : Int) : (Tree[String],IndexedSeq[String]) = {
    readLeftParen();
    val label = {
      val labelx = readLabel();
      if (isRoot && labelx.length == 0) ROOT_LABEL else labelx
    }

    val (children,words) = readChildren(pos);
    val spanEnd = pos + words.length
    readRightParen();
    if (!lowercase || children.size > 0) {
      new Tree[String](label, children, Span(pos, spanEnd)) -> words
    } else {
      new Tree[String](label.toLowerCase.intern, children, Span(pos, spanEnd)) -> words
    }
  }

  private def readLabel() = {
    readWhiteSpace();
    readText(false, false);
  }

  private def readText(atLeastOnex : Boolean, skipLeftParen : Boolean) = {
    var atLeastOne = atLeastOnex
    val sb = new StringBuilder();
    var ch = in.read();
    while (atLeastOne || (!isWhiteSpace(ch) && (skipLeftParen || !isLeftParen(ch)) && !isRightParen(ch) && ch != -1)) {
      sb.append(ch.toChar);
      ch = in.read();
      atLeastOne = false;
    }

    in.unread(ch);
    sb.toString().intern();
  }

  private def readChildren(pos : Int) : (IndexedSeq[Tree[String]],IndexedSeq[String]) = {
    val words = ArrayBuffer[String]();
    var currPos = pos
    readWhiteSpace();
    val children = new ArrayBuffer[Tree[String]]();
    while (!isRightParen(peek())) {
      readWhiteSpace();
      if (isLeftParen(peek())) {
        if (isTextParen()) {
          words += readLeaf()
          currPos += 1
        } else {
          val (tree,w) = readTree(false, currPos)
          if(!dropLabels(tree.label)) {
            currPos = tree.span.e
            words ++= w
            children.add(tree);
          }
        }
      } else if (peek() == 65535) {
        throw new RuntimeException("Unmatched parentheses in tree input.");
      } else {
        words += readLeaf()
        currPos += 1
      }
      readWhiteSpace();
    }
    children -> words
  }

  private def isTextParen() = {

    var numRead = 0;
    var ch = in.read()
    while (isLeftParen(ch)) {
      numRead += 1;
      ch = in.read()
    }
    val yes = numRead > 0 && (isRightParen(ch));
    in.unread(ch);
    for (i <- 0 until numRead) {
      in.unread('(');
    }
    yes;
  }

  private def peek() = {
    val ch = in.read();
    in.unread(ch);
    ch;
  }

  private def readLeaf() = {
    var label = readText(true, true);
    if (lowercase) label = label.toLowerCase;
    label.intern()
  }

  private def readLeftParen() = {
    readWhiteSpace();
    val ch = in.read();
    if (!isLeftParen(ch)) throw new RuntimeException("Format error reading tree. Expected '(' but got " + ch);
  }

  private def readRightParen() = {
    readWhiteSpace();
    val ch = in.read();
    if (!isRightParen(ch)) throw new RuntimeException("Format error reading tree.");
  }

  private def readWhiteSpace() = {
    var ch = in.read();
    while (isWhiteSpace(ch)) {
      ch = in.read();
    }
    in.unread(ch);
  }

  private def isWhiteSpace(ch : Int) = {
    (ch == ' ' || ch == '\t' || ch == '\f' || ch == '\r' || ch == '\n');
  }

  private def isLeftParen(ch : Int) = {
    ch == '(';
  }

  private def isRightParen(ch : Int) = {
    ch == ')';
  }

}

object PennTreeReader {
  /**
   * Reads a tree on a single line and returns null if there was a
   * problem.
   *
   * @param lowercase
   */
  def parseEasy(treeString : String, lowercase : Boolean = false) = {
    try {
      parseHard(treeString, lowercase);
    } catch {
      case e : RuntimeException => null;
    }
  }

  def parseHard(treeString : String, lowercase : Boolean = false) = {
    val sr = new StringReader(treeString);
    val reader = new PennTreeReader(sr, lowercase = lowercase);
    reader.next();
  }
}



