package epic.parser.gpu

import epic.trees.{BinaryRule, UnaryRule}
import collection.mutable.ArrayBuffer
import com.nativelibs4java.opencl._
import java.lang.{Float=>JFloat, Integer=>JInt}
import java.io.FileWriter
import collection.immutable

class InsideKernel[C, L](ruleStructure: RuleStructure[C, L], numGrammars: Int)(implicit context: CLContext) {
  import ruleStructure._

  def insidePass(numSentences: Int,
                 insideBot: CLBuffer[JFloat],
                 insideTop: CLBuffer[JFloat],
                 posTags: CLBuffer[JFloat],
                 offsets: CLBuffer[JInt],
                 lengths: CLBuffer[JInt],
                 maxLength: Int,
                 lengthOffsets: CLBuffer[JInt],
                 rules: CLBuffer[JFloat],
                 events: CLEvent*)(implicit queue: CLQueue) = synchronized {
    binaries.foreach(_.setArgs(insideBot, insideTop, offsets, lengths, Integer.valueOf(1), rules))
    termBinaries.setArgs(insideBot, insideTop, posTags, offsets, lengths, lengthOffsets, Integer.valueOf(1), rules)
    unaries.setArgs(insideBot, insideTop, offsets, lengths, Integer.valueOf(1), rules)
    val iu, ib, it = new ArrayBuffer[CLEvent]()
    var lastU:CLEvent = null
    lastU = unaries.enqueueNDRange(queue, Array(numSentences, maxLength, numGrammars), Array(1, 1, numGrammars), events:_*)
    iu += lastU

    // TODO: retrofit inside/outside binaries and unaries to look at posTagsPointer....
    // TODO: also get ecounts...
    for (len <- 2 to maxLength) {
      binaries.foreach(_.setArg(4, len))
      val b = binaries.map(_.enqueueNDRange(queue, Array(numSentences, maxLength + 1 - len, numGrammars), Array(1, 1, numGrammars), lastU))
      ib ++= b

      termBinaries.setArg(6, len)
      val t = termBinaries.enqueueNDRange(queue, Array(numSentences, maxLength + 1 - len, numGrammars), Array(1, 1, numGrammars), b:_*)
      it += t

      unaries.setArg(4, len)
      lastU = unaries.enqueueNDRange(queue, Array(numSentences, maxLength + 1 - len, numGrammars), Array(1, 1, numGrammars), t)
      iu += lastU
    }

    if(queue.getProperties.contains(CLDevice.QueueProperties.ProfilingEnable)) {
      queue.finish()
      val iuCount = iu.map(e => e.getProfilingCommandEnd - e.getProfilingCommandStart).sum / 1E9
      val ibCount = ib.map(e => e.getProfilingCommandEnd - e.getProfilingCommandStart).sum / 1E9
      val itCount = it.map(e => e.getProfilingCommandEnd - e.getProfilingCommandStart).sum / 1E9
      println("inside: " + iuCount + " " + ibCount + " " + itCount)
    }

    lastU

  }

  private lazy val binaries = Array.tabulate(partitionsParent.length)(i => program.createKernel("inside_binaries_" + i))
  private lazy val termBinaries = program.createKernel("inside_term_binaries")
  private lazy val unaries = program.createKernel("inside_unaries")



  lazy val text = GrammarHeader.header(ruleStructure, numGrammars) +
    """


 __kernel void inside_term_binaries(
              __global parse_cell * inside_bots,
              __global const parse_cell * inside_tops,
              __global const parse_cell * pos_tags,
              __global const int* offsets,
              __global const int* lengths,
              __global const int* lengthOffsets,
              const int spanLength,
              __global const rule_cell* rules) {
  const int sentence = get_global_id(0);
  const int begin = get_global_id(1);
  const int gram = get_global_id(2);
  const int end = begin + spanLength;
  const int length = lengths[sentence];
//  float out[NUM_SYMS];
  float out[NUM_SYMS], right[NUM_SYMS];
  if (end <= length) {
    __global const parse_cell* chart_top =  inside_tops + offsets[sentence];
    for(int i = 0; i < NUM_SYMS; ++i) {
      out[i] = 0.0f;
    }

    __global const parse_cell * left =  CELL(inside_tops + offsets[sentence], begin, end-1);
    __global const parse_cell * right =  CELL(inside_tops + offsets[sentence], begin+1, end);
    __global const parse_cell * leftTerm = pos_tags + lengthOffsets[sentence] + begin;
    __global const parse_cell * rightTerm = pos_tags + lengthOffsets[sentence] + (end - 1);
      %s
    // out has a scale factor of (2^SCALE_FACTOR)^((end-split) + (split-begin) - 2) = (2^SCALE_FACTOR)^(end-begin-2)
    // multiply in a 2^SCALE_FACTOR to reachive balance.
    __global parse_cell* gout = CELL(inside_bots + offsets[sentence], begin, end);
    for(int i = 0; i < NUM_SYMS; ++i) {
      gout->syms[i][gram] += ldexp(out[i], SCALE_FACTOR);
    }
  }
}


__kernel void inside_unaries(__global const parse_cell * inside_bots,
              __global parse_cell * inside_tops,
              __global const int* offsets,
              __global const int* lengths,
              const int spanLength,
              __global const rule_cell* rules) {
  const int sentence = get_global_id(0);
  const int begin = get_global_id(1);
  const int gram = get_global_id(2);
  const int end = begin + spanLength;
  const int length = lengths[sentence];

  if (end <= length) {
    __global parse_cell* top = CELL(inside_tops + offsets[sentence], begin, end);
    __global const parse_cell* bot = CELL(inside_bots + offsets[sentence], begin, end);
    %s
  }
}

    """.stripMargin.format(
      insideTermRuleUpdates,
//      insideRuleUpdates(ruleStructure.leftTermRules ++ ruleStructure.rightTermRules ++ ruleStructure.bothTermRules),
      insideUnaryUpdates(ruleStructure.unaryRulesWithIndices)) ++ (0 until partitionsParent.length).map(i => ntBinaryPartition(partitionsParent(i), i)).mkString("\n")

  if(true) {val o = new FileWriter("inside.cl"); o.write(text); o.close()}

  def insideUnaryUpdates(rules: IndexedSeq[(UnaryRule[Int], Int)]): String = {
    val sb = new ArrayBuffer[String]
    sb += "float parent;"
    val rules2 = rules.sortBy(_._1.parent)
    var lastParent = -1
    for( (r, index) <- rules2) {
      if(r.parent != lastParent) {
        if(lastParent != -1) {
          sb += """top->syms[%d][gram] = parent;""".format(lastParent)
        }
        sb += """parent = rules->unaries[%d][gram] * bot->syms[%d][gram];""".format(index, r.child)
        lastParent = r.parent
      } else {
        sb += """parent = mad(rules->unaries[%d][gram], bot->syms[%d][gram], parent);""".format(index, r.child)
      }
    }
    if(lastParent != -1) {
      sb += """top->syms[%d][gram] = parent;""".format(lastParent)
    }
    sb.mkString("\n    ")
  }

  def insideRuleUpdates( _rules: IndexedSeq[(BinaryRule[Int], Int)]): String = {
    val byParent = _rules.sortBy(_._1.left).groupBy(_._1.parent)
    val sb = new ArrayBuffer[String]
    val lefts = _rules.map(_._1.left).toSet
    val rights = _rules.map(_._1.right).toSet
    sb += "float currentSum = 0.0f;"
    lefts.map(l => "const float left%d = left->syms[%d][gram];".format(l,l)).foreach(sb += _)
    rights.map(r => "const float r%d = right->syms[%d][gram];".format(r,r)).foreach(sb += _)
    for( (p, rules) <- byParent) {
      var lastLeft = -1
      for((r@BinaryRule(p, l, right), index) <- rules) {
        if(lastLeft != l) {
          if(lastLeft != -1) {
            sb += "}"
            sb += "p%d = mad(left%d, currentSum, p%d);".format(r.parent, lastLeft, r.parent)
          }
          sb += "currentSum = 0.0f;"
          sb += "if(left%d != 0.0f) { // %s".format(r.left, symbolName(r.left))
          lastLeft = l
        }
        sb += "  currentSum = mad(rules->binaries[%d][gram], r%d, currentSum); // %s".format(index, right, ruleString(index))
      }
      if(lastLeft != -1) {
        sb += "p%d = mad(left%d, currentSum, p%d);".format(p, lastLeft, p)
        sb += "}"
        sb += "currentSum = 0.0f;"
       }

    }

    sb.mkString("\n      ")
  }

  def symbolName(sym: Int): L = {
    ruleStructure.grammar.labelIndex.get(sym)
  }

  def ruleString(r: Int) = {
    ruleStructure.grammar.index.get(r) match {
      case BinaryRule(a, b, c) => "%s -> %s %s".format(a,b,c)
      case UnaryRule(a, b, c) => "%s -> %s (%s)".format(a,b,c)
    }
  }

  def insideTermRuleUpdates: String = {
    var lastLeft = -1
    val sb = new ArrayBuffer[String]
    sb += "float currentLeftScore, currentRightScore, currentSum = 0.0f;"
    // do A -> Term NonTerm
    for((r@BinaryRule(p, l, right), index) <- ruleStructure.leftTermRules.sortBy(_._1.left)) {
      if(lastLeft != l) {
        if(lastLeft != -1) {
          sb += "}"
        }
        sb += "currentLeftScore = leftTerm->syms[%d][gram]; // %s".format(l, symbolName(l))
        sb += "if(currentLeftScore != 0.0f) {"
        lastLeft = l
      }
      sb += """  out[%d] = mad(rules->binaries[%d][gram], currentLeftScore * right->syms[%d][gram], out[%d]); // %s""".format(r.parent, index, r.right, r.parent, ruleString(index))
    }
    if(lastLeft != -1)
      sb += "}"
    lastLeft = -1
    sb += "if (spanLength == 2) {"
    for((r@BinaryRule(p, left, right), index) <- ruleStructure.bothTermRules.sortBy(_._1.left)) {
      if(lastLeft != left) {
        if(lastLeft != -1)
          sb += "  }"
        sb += "  currentLeftScore = leftTerm->syms[%d][gram]; // %s".format(left, symbolName(left))
        sb += "  if(currentLeftScore != 0.0f) {"
        lastLeft = left
      }
      sb += """    out[%d] = mad(rules->binaries[%d][gram], currentLeftScore * rightTerm->syms[%d][gram], out[%d]); // %s """.format(r.parent, index, r.right, r.parent, ruleString(index))
    }
    if(lastLeft != -1)
      sb += "  }"
    sb += "}"
    var lastRight = -1
    for((r@BinaryRule(p, l, right), index) <- ruleStructure.rightTermRules.sortBy(_._1.right)) {
      if(lastRight != right) {
        if(lastRight != -1)
          sb += "}"
        sb += "currentRightScore = rightTerm->syms[%d][gram]; // right = %s".format(right, symbolName(right))
        sb += "if(currentRightScore != 0.0f) {"
        lastRight = right
      }
      sb += """  out[%d] = mad(rules->binaries[%d][gram], currentRightScore * left->syms[%d][gram], out[%d]); // %s""".format(r.parent, index, l, p, ruleString(index))
    }
    sb += "}"
    sb.mkString("\n    ")
  }

  val program = {
    val p = context.createProgram(text)
    p.setFastRelaxedMath()
    p.setUnsafeMathOptimizations()
    p.addBuildOption("-Werror")
    p.build()
    p
  }

  private def ntBinaryPartition(rules: IndexedSeq[(BinaryRule[Int], Int)], id: Int) = {
    """
__kernel void inside_binaries_%d(
              __global parse_cell * inside_bots,
              __global const parse_cell * inside_tops,
              __global const int* offsets,
              __global const int* lengths,
              const int spanLength,
              __global const rule_cell* rules) {
  const int sentence = get_global_id(0);
  const int begin = get_global_id(1);
  const int gram = get_global_id(2);
  const int end = begin + spanLength;
  const int length = lengths[sentence];
  if (end <= length) {
    __global const parse_cell* chart_top =  inside_tops + offsets[sentence];
    __global parse_cell* gout = CELL(inside_bots + offsets[sentence], begin, end);
    %s

    for(int split = begin + 1; split < end; ++split) {
      __global const parse_cell * left = CELL(chart_top, begin, split); // scale factor of (2 ^ SCALE_FACTOR)^((split - begin) - 1)
      __global const parse_cell * right = CELL(chart_top, split, end); // scale factor of (2^ SCALE_FACTOR)((end-split) - 1)
      %s
    }

    // out has a scale factor of (2^SCALE_FACTOR)^((end-split) + (split-begin) - 2) = (2^SCALE_FACTOR)^(end-begin-2)
    // multiply in a 2^SCALE_FACTOR to re-achieve balance.
    %s
  }
}
    """.format(id,
      rules.map(_._1.parent).toSet[Int].map("p" + _).mkString("float ", " = 0.0f,", " = 0.0f;"),
      insideRuleUpdates(rules),
      rules.map(_._1.parent).toSet[Int].map(p => "gout->syms[%d][gram] = ldexp(p%d, SCALE_FACTOR);".format(p,p)).mkString("\n   ")
    )
  }
}
