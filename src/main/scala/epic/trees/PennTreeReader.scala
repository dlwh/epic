package epic.trees
/*
 Copyright 2012 David Hall

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/


import java.io._

import epic.preprocess.TreebankTokenizer

import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer

/**
 * PennTreeReader due to Adam Pauls.
 *
 * This reader returns empty categories as leaves of the tree below the -NONE-. These leaves
 * span 0 words.
 *
 * For example,
 *     (TOP (S-IMP (NP-SBJ (-NONE- *PRO*))
                (VP (VB Look)
                    (PP-DIR (IN at)
                            (NP (DT that))))
                (. /.)))

 * will return (TOP[0:4] (S-IMP[0:4] (NP-SBJ[0:0] (-NONE-[0:0] (*PRO*[0:0]))) (VP[0:4]...)
 *
 * @author adpauls
 * @author dlwh
 */
class PennTreeReader(reader: Reader,
                     isEmptyCategory: String=>Boolean = _ == "-NONE-",
                     rootLabel : String = "TOP",
                     unescapeTokens: Boolean = true) extends Iterator[(Tree[String],IndexedSeq[String])] {

  def this(f: File) = this(new BufferedReader(new FileReader(f)))

  private val in = new PushbackReader(reader, 4)

  private var nextTree = readRootTree()

  def hasNext = nextTree != null

  def next() = {
    if (!hasNext) throw new NoSuchElementException()
    val tree = nextTree
    nextTree = readRootTree()
    if (nextTree == null) {
      in.close()
    }
    tree
  }

  private def readRootTree() : (Tree[String], IndexedSeq[String]) = {
    readWhiteSpace()
    if (!isLeftParen(peek())) null
    else {
      val tree = readTree(true, 0)
      tree
    }
  }

  private def readTree(isRoot : Boolean, pos : Int) : (Tree[String],IndexedSeq[String]) = {
    readLeftParen()
    val label = {
      val labelx = readLabel()
      if (isRoot && labelx.length == 0) rootLabel else labelx
    }

    if (isEmptyCategory(label)) {
      val emptyChild = readLeaf()
      readRightParen()
      Tree(label, IndexedSeq(Tree(emptyChild, IndexedSeq.empty, Span(pos, pos))), Span(pos, pos)) -> IndexedSeq.empty
    } else {
      val (children,words) = readChildren(pos)
      val spanEnd = pos + words.length
      readRightParen()
      Tree[String](label, children, Span(pos, spanEnd)) -> words
    }
  }

  private def readLabel() = {
    readWhiteSpace()
    readText(false, false)
  }

  private def readText(atLeastOnex : Boolean, skipLeftParen : Boolean) = {
    var atLeastOne = atLeastOnex
    val sb = new StringBuilder()
    var ch = in.read()
    while (atLeastOne || (!isWhiteSpace(ch) && (skipLeftParen || !isLeftParen(ch)) && !isRightParen(ch) && ch != -1)) {
      sb.append(ch.toChar)
      ch = in.read()
      atLeastOne = false
    }
    in.unread(ch)
    sb.toString()
  }

  private def readChildren(pos : Int) : (IndexedSeq[Tree[String]],IndexedSeq[String]) = {
    val words = ArrayBuffer[String]()
    var currPos = pos
    readWhiteSpace()
    val children = new ArrayBuffer[Tree[String]]()
    while (!isRightParen(peek())) {
      readWhiteSpace()
      if (isLeftParen(peek())) {
        if (isTextParen()) {
          words += readLeaf()
          currPos += 1
        } else {
          val (tree,w) = readTree(isRoot = false, pos = currPos)
          currPos = tree.end
          words ++= w
          children.add(tree)
        }
      } else if (peek() == 65535) {
        throw new RuntimeException("Unmatched parentheses in tree input.")
      } else {
        words += readLeaf()
        currPos += 1
      }
      readWhiteSpace()
    }
    children -> words
  }

  private def isTextParen() = {
    var numRead = 0
    var ch = in.read()
    while (isLeftParen(ch)) {
      numRead += 1
      ch = in.read()
    }
    val yes = numRead > 0 && isRightParen(ch)
    in.unread(ch)
    for (i <- 0 until numRead) {
      in.unread('(')
    }
    yes
  }

  private def peek() = {
    val ch = in.read()
    in.unread(ch)
    ch
  }

  private def readLeaf() = {
    var label = readText(true, true)
    if (unescapeTokens)
      label = TreebankTokenizer.treebankTokenToToken(label)
    if (label.startsWith("/") && label.length == 2 && label(1) != '/') {
      label = label.drop(1) // ontonotes escapes periods as /.
    }
    label
  }

  private def readLeftParen() = {
    readWhiteSpace()
    val ch = in.read()
    if (!isLeftParen(ch)) throw new RuntimeException("Format error reading tree. Expected '(' but got " + ch)
  }

  private def readRightParen() = {
    readWhiteSpace()
    val ch = in.read()
    if (!isRightParen(ch)) throw new RuntimeException("Format error reading tree.")
  }

  private def readWhiteSpace() = {
    var ch = in.read()
    while (isWhiteSpace(ch)) {
      ch = in.read()
    }
    in.unread(ch)
  }

  private def isWhiteSpace(ch : Int) = {
    ch == ' ' || ch == '\t' || ch == '\f' || ch == '\r' || ch == '\n'
  }

  private def isLeftParen(ch : Int) = {
    ch == '('
  }

  private def isRightParen(ch : Int) = {
    ch == ')'
  }

}
