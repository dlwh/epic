package epic.parser.gpu

import epic.trees._
import com.nativelibs4java.opencl._
import collection.mutable.ArrayBuffer
import java.lang.{Float=>JFloat, Integer=>JInt}


class OutsideKernel[L](ruleStructure: RuleStructure[L], numGrammars: Int)(implicit context: CLContext) {
  def outsidePass(outsideTop: CLBuffer[JFloat],
                 outsideBot: CLBuffer[JFloat],
                 insideTop: CLBuffer[JFloat],
                 offsets: CLBuffer[JInt],
                 lengths: CLBuffer[JInt],
                 maxLength: Int,
                 rules: CLBuffer[JFloat],
                 events: CLEvent*)(implicit queue: CLQueue) = {
    println("start outside.")
    val ou, ob= new ArrayBuffer[CLEvent]()
    binaries.setArgs(outsideTop, outsideBot, insideTop, offsets, lengths, Integer.valueOf(maxLength), rules)
    unaries.setArgs(outsideTop, outsideBot, offsets, lengths, Integer.valueOf(maxLength), rules)
    var lastU = unaries.enqueueNDRange(queue, Array(lengths.getElementCount.toInt, 1, numGrammars), Array(1, 1, numGrammars), events:_*)
    ou += lastU

    for (len <- (maxLength - 1) to 1 by -1) {
      binaries.setArg(5, len)
      val b = binaries.enqueueNDRange(queue, Array(lengths.getElementCount.toInt, maxLength + 1 - len, numGrammars), Array(1, 1, numGrammars), lastU)
      ob += b
      unaries.setArg(4, len)
      lastU = unaries.enqueueNDRange(queue, Array(lengths.getElementCount.toInt, maxLength + 1 - len, numGrammars), Array(1, 1, numGrammars), b)
      ou += lastU
    }
    if(queue.getProperties.contains(CLDevice.QueueProperties.ProfilingEnable)) {
      queue.finish()
      val ouCount = ou.map(e => e.getProfilingCommandEnd - e.getProfilingCommandStart).sum / 1E9
      val obCount = ob.map(e => e.getProfilingCommandEnd - e.getProfilingCommandStart).sum / 1E9
      println("outside: " + ouCount + " " + obCount)
    }

    lastU
  }

  private lazy val binaries = program.createKernel("outside_binaries")
  private lazy val unaries = program.createKernel("outside_unaries")

  lazy val text = GrammarHeader.header(ruleStructure, numGrammars) +
"""
__kernel void outside_unaries(__global parse_cell * outside_tops,
              __global parse_cell * outside_bots,
              __global const int* offsets,
              __global const int* lengths,
              const int spanLength,
            __global const rule_cell* rules) {
  const int sentence = get_global_id(0);
  const int begin = get_global_id(1);
  const int end = begin + spanLength;
  const int gram = get_global_id(2);
  const int length = lengths[sentence];

  if(spanLength == length) {
    CELL(outside_tops + offsets[sentence], 0, length)->syms[ROOT][gram] = 1.0f;
  }

  if (end <= length) {
    __global const parse_cell* top = CELL(outside_tops + offsets[sentence], begin, end);
    __global parse_cell* bot = CELL(outside_bots + offsets[sentence], begin, end);
    %s
  }
}

__kernel void outside_binaries(__global parse_cell* outsides_top,
              __global const parse_cell* outsides_bot,
              __global const parse_cell * insides_top,
              __global const int* offsets,
              __global const int* lengths,
              const int spanLength,
            __global const rule_cell* rules) {
  const int sentence = get_global_id(0);
  const int begin = get_global_id(1);
  const int gram = get_global_id(2);
  const int end = begin + spanLength;
  const int length = lengths[sentence];
  float oparent[NUM_SYMS], otarget[NUM_SYMS];
  if (end <= length) {
    __global const parse_cell* inside = insides_top + offsets[sentence];
    __global parse_cell* obot = outsides_bot + offsets[sentence];
    for(int i = 0; i < NUM_SYMS; ++i) {
      otarget[i] = 0.0f;
    }
    // complete looking right
    for(int completion = end+1; completion <= length; ++completion) {
       __global const parse_cell * gparent = CELL(obot, begin, completion); // scale factor of (2 ^ SCALE_FACTOR)^(length-(completion - begin))
       __global const parse_cell * gright = CELL(inside, end, completion); // scale factor of (2 ^ SCALE_FACTOR)^((end - completion) - 1)
       // product of gparent and gright has scale (2^SCALE_FACTOR)^(length-(end-begin)-1), so need to scale by 1 to maintain invariant
       for(int i = 0; i < NUM_SYMS; ++i) {
         oparent[i] = gparent->syms[i][gram];
       }
       %s
    }

   // complete looking left
    for(int completion = 0; completion < begin; ++completion) {
       __global const parse_cell * gparent = CELL(obot, completion, end); // scale factor of (2 ^ SCALE_FACTOR)^(length-(end-completion))
       __global const parse_cell * gleft = CELL(inside, completion, begin); // scale factor of (2 ^ SCALE_FACTOR)^((begin - completion) - 1)
       // product of gparent and gleft has scale (2^SCALE_FACTOR)^(length-(end-begin)-1), so need to scale by 1 to maintain invariant
//         for(int i = 0; i < NUM_SYMS; ++i) {
//           icompl[i] = gleft[i];
//         }
       for(int i = 0; i < NUM_SYMS; ++i) {
         oparent[i] = gparent->syms[i][gram];
       }
       %s
    }

    // multiply in a 2^SCALE_FACTOR to re-achieve balance.
    __global parse_cell* gout = CELL(outsides_top + offsets[sentence], begin, end);
    for(int i = 0; i < NUM_SYMS; ++i) {
      gout->syms[i][gram] = ldexp(otarget[i], SCALE_FACTOR);
//        gout[i] = otarget[i];
    }
  }
}""".format(outsideUnaryUpdates(ruleStructure.unaryRulesWithIndices),
  outsideRightCompletionUpdates(ruleStructure.binaryRulesWithIndices),
  outsideLeftCompletionUpdates(ruleStructure.binaryRulesWithIndices))

def outsideUnaryUpdates(rules: IndexedSeq[(UnaryRule[Int], Int)]): String = {
    val sb = new ArrayBuffer[String]
    sb += "float child;"
    val rules2 = rules.sortBy(_._1.child)
    var lastChild = -1
    for( (r, index) <- rules2) {
      if(r.child != lastChild) {
        if(lastChild != -1) {
          sb += """bot->syms[%d][gram] = child;""".format(lastChild)
        }
        sb += """child = rules->unaries[%d][gram] * top->syms[%d][gram];""".format(index, r.parent)
        lastChild = r.child
      } else {
        sb += """child = mad(rules->unaries[%d][gram], top->syms[%d][gram], child);""".format(index, r.parent)
      }
    }
    if(lastChild != -1) {
      sb += """bot->syms[%d][gram] = child;""".format(lastChild)
    }
    sb.mkString("\n    ")
  }




  // otarget is the left child, completion on right.
  def outsideRightCompletionUpdates(rules: IndexedSeq[(BinaryRule[Int], Int)]): String = {
    // resort by right child, parent, left chidl
    val newrules = rules.sortBy(r => (r._1.right, r._1.parent, r._1.left))(Ordering.Tuple3)
    var lastRight = -1
    val sb = new ArrayBuffer[String]
    sb += "float currentCompl;"
    for((r@BinaryRule(p, l, right), index) <- newrules) {
      if(lastRight != right) {
        if(lastRight != -1)
          sb += "}"
        sb += "currentCompl = gright->syms[%d][gram];" format right
        sb += "if(currentCompl != 0.0) {"
        lastRight = right
      }
      sb += """otarget[%d] = mad(rules->binaries[%d][gram], currentCompl * oparent[%d], otarget[%d]);""".format(r.left, index, r.parent, r.left)
    }

    sb += "}"
    sb.mkString("\n    ")
  }

  // otarget is the right child, completion on left.
  private def outsideLeftCompletionUpdates(rules: IndexedSeq[(BinaryRule[Int], Int)]): String = {
    val newrules = rules.sortBy(r => (r._1.left, r._1.parent, r._1.right))(Ordering.Tuple3)
    var lastLeft = -1
    val sb = new ArrayBuffer[String]
    sb += "float currentCompl;"
    for((r@BinaryRule(p, l, right), index) <- newrules) {
      if(lastLeft != l) {
        if(lastLeft != -1)
          sb += "}"
        sb += "currentCompl = gleft->syms[%d][gram];" format l
        sb += "if(currentCompl != 0.0) {"
        lastLeft = l
      }
      sb += """otarget[%d] = mad(rules->binaries[%d][gram], currentCompl * oparent[%d], otarget[%d]);""".format(r.right, index, r.parent, r.right)
    }
    sb += "}"

    sb.mkString("\n    ")
  }

  lazy val program = {
    val p = context.createProgram(text)
    p.setFastRelaxedMath()
    p.setUnsafeMathOptimizations()
    p.build()
  }}
