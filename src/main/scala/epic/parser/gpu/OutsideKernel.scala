package epic.parser.gpu

import epic.trees._
import com.nativelibs4java.opencl._
import collection.mutable.ArrayBuffer
import java.lang.{Float=>JFloat, Integer=>JInt}
import java.io.FileWriter


class OutsideKernel[L](ruleStructure: RuleStructure[L], numGrammars: Int)(implicit context: CLContext) {
  def outsidePass(numSentences: Int,
                 outsideTop: CLBuffer[JFloat],
                 outsideBot: CLBuffer[JFloat],
                 insideTop: CLBuffer[JFloat],
                 posTags: CLBuffer[JFloat],
                 offsets: CLBuffer[JInt],
                 lengths: CLBuffer[JInt],
                 lengthOffsets: CLBuffer[JInt],
                 maxLength: Int,
                 rules: CLBuffer[JFloat],
                 events: CLEvent*)(implicit queue: CLQueue) = {
    val ou, ob, ot= new ArrayBuffer[CLEvent]()
    var lastU = null:CLEvent
    binaries.setArgs(outsideTop, outsideBot, insideTop, offsets, lengths, Integer.valueOf(maxLength), rules)
    unaries.setArgs(outsideTop, outsideBot, offsets, lengths, Integer.valueOf(maxLength), rules)
    tunaries.setArgs(outsideTop, outsideBot, offsets, lengths, rules)
    termbs.setArgs(outsideTop, outsideBot, insideTop, posTags, offsets, lengths, lengthOffsets, Integer.valueOf(maxLength), rules)
    bterms.setArgs(outsideTop, outsideBot, insideTop, posTags, offsets, lengths, lengthOffsets, rules)
    lastU = unaries.enqueueNDRange(queue, Array(numSentences, 1, numGrammars), Array(1, 1, numGrammars), events:_*)
    ou += lastU

    for (len <- (maxLength - 1) to 1 by -1) {
      binaries.setArg(5, len)
      lastU = binaries.enqueueNDRange(queue, Array(numSentences, maxLength + 1 - len, numGrammars), Array(1, 1, numGrammars), lastU)
      ob += lastU
      termbs.setArg(7, len)
      lastU = termbs.enqueueNDRange(queue, Array(numSentences, maxLength + 1 - len, numGrammars), Array(1, 1, numGrammars), lastU)
      ot += lastU
      if(len == 1) {
        lastU = bterms.enqueueNDRange(queue, Array(numSentences, maxLength, numGrammars), Array(1, 1, numGrammars), lastU)
        ot += lastU
        lastU = tunaries.enqueueNDRange(queue, Array(numSentences, maxLength, numGrammars), Array(1, 1, numGrammars), lastU)
      } else {
        unaries.setArg(4, len)
        lastU = unaries.enqueueNDRange(queue, Array(numSentences, maxLength + 1 - len, numGrammars), Array(1, 1, numGrammars), lastU)
      }
      ou += lastU
    }

    if(queue.getProperties.contains(CLDevice.QueueProperties.ProfilingEnable)) {
      queue.finish()
      val ouCount = ou.map(e => e.getProfilingCommandEnd - e.getProfilingCommandStart).sum / 1E9
      val obCount = ob.map(e => e.getProfilingCommandEnd - e.getProfilingCommandStart).sum / 1E9
      val otCount = ot.map(e => e.getProfilingCommandEnd - e.getProfilingCommandStart).sum / 1E9
      println("outside: " + ouCount + " " + obCount + " " + otCount)
    }

    lastU
  }

  private lazy val binaries = program.createKernel("outside_binaries")
  private lazy val termbs = program.createKernel("outside_term_binaries")
  private lazy val bterms = program.createKernel("outside_binary_terms")
  private lazy val unaries = program.createKernel("outside_unaries")
  private lazy val tunaries = program.createKernel("outside_term_unaries")

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

__kernel void outside_term_unaries(__global parse_cell * outside_tops,
              __global parse_cell * outside_bots,
              __global const int* offsets,
              __global const int* lengths,
            __global const rule_cell* rules) {
  const int sentence = get_global_id(0);
  const int begin = get_global_id(1);
  const int end = begin + 1;
  const int gram = get_global_id(2);
  const int length = lengths[sentence];

  if(1 == length) {
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
  float otarget[NUM_SYMS];
  if (end <= length) {
    __global const parse_cell* inside = insides_top + offsets[sentence];
    __global const parse_cell* obot = outsides_bot + offsets[sentence];
    for(int i = 0; i < NUM_SYMS; ++i) {
      otarget[i] = 0.0f;
    }
    // complete looking right
    for(int completion = end+1; completion <= length; ++completion) {
       __global const parse_cell * gparent = CELL(obot, begin, completion); // scale factor of (2 ^ SCALE_FACTOR)^(length-(completion - begin))
       __global const parse_cell * gright = CELL(inside, end, completion); // scale factor of (2 ^ SCALE_FACTOR)^((end - completion) - 1)
       // product of gparent and gright has scale (2^SCALE_FACTOR)^(length-(end-begin)-1), so need to scale by 1 to maintain invariant
       %s
    }

   // complete looking left
    for(int completion = 0; completion < begin; ++completion) {
       __global const parse_cell * gparent = CELL(obot, completion, end); // scale factor of (2 ^ SCALE_FACTOR)^(length-(end-completion))
       __global const parse_cell * gleft = CELL(inside, completion, begin); // scale factor of (2 ^ SCALE_FACTOR)^((begin - completion) - 1)
       // product of gparent and gleft has scale (2^SCALE_FACTOR)^(length-(end-begin)-1), so need to scale by 1 to maintain invariant
       %s
    }

    // multiply in a 2^SCALE_FACTOR to re-achieve balance.
    __global parse_cell* gout = CELL(outsides_top + offsets[sentence], begin, end);
    for(int i = 0; i < NUM_SYMS; ++i) {
      gout->syms[i][gram] = ldexp(otarget[i], SCALE_FACTOR);
    }
  }
}

__kernel void outside_term_binaries(__global parse_cell* outsides_top,
              __global const parse_cell* outsides_bot,
              __global const parse_cell * insides_top,
              __global const parse_cell * inside_tags,
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
  float otarget[NUM_SYMS];
   if (end <= length) {
    __global const parse_cell* inside = insides_top + offsets[sentence];
    __global const parse_cell* obot = outsides_bot + offsets[sentence];
    for(int i = 0; i < NUM_SYMS; ++i) {
      otarget[i] = 0.0f;
    }

    if (end < length) { // look right
      __global const parse_cell * gparent =  CELL(obot, begin, end+1);
      __global const parse_cell * gright = inside_tags + lengthOffsets[sentence] + (end);
      %s
    }

   // complete looking left
      if (begin > 0) { // look left
      __global const parse_cell * gparent =  CELL(obot, begin-1, end);
      __global const parse_cell * gleft = inside_tags + lengthOffsets[sentence] + (begin - 1);
      %s
     }

    // multiply in a 2^SCALE_FACTOR to re-achieve balance.
    __global parse_cell* gout = CELL(outsides_top + offsets[sentence], begin, end);
    for(int i = 0; i < NUM_SYMS; ++i) {
      gout->syms[i][gram] += ldexp(otarget[i], SCALE_FACTOR);
    }
  }
}

 __kernel void outside_binary_terms(__global parse_cell* outsides_top,
              __global parse_cell* outsides_bot,
              __global const parse_cell * insides_top,
              __global const parse_cell * inside_tags,
              __global const int* offsets,
              __global const int* lengths,
              __global const int* lengthOffsets,
            __global const rule_cell* rules) {
  const int sentence = get_global_id(0);
  const int begin = get_global_id(1);
  const int gram = get_global_id(2);
  const int length = lengths[sentence];
  const int end = begin + 1;
  float otarget[NUM_SYMS];
  if (end <= length) {
    __global const parse_cell* inside = insides_top + offsets[sentence];
    __global const parse_cell* obot = outsides_bot + offsets[sentence];
    for(int i = 0; i < NUM_SYMS; ++i) {
      otarget[i] = 0.0f;
    }
    // complete looking right
    for(int completion = end+1; completion <= length; ++completion) {
       __global const parse_cell * gparent = CELL(obot, begin, completion); // scale factor of (2 ^ SCALE_FACTOR)^(length-(completion - begin))
       __global const parse_cell * gright = CELL(inside, end, completion); // scale factor of (2 ^ SCALE_FACTOR)^((end - completion) - 1)
       // product of gparent and gright has scale (2^SCALE_FACTOR)^(length-(end-begin)-1), so need to scale by 1 to maintain invariant
       %s
    }

   // complete looking left
    for(int completion = 0; completion < begin; ++completion) {
       __global const parse_cell * gparent = CELL(obot, completion, end); // scale factor of (2 ^ SCALE_FACTOR)^(length-(end-completion))
       __global const parse_cell * gleft = CELL(inside, completion, begin); // scale factor of (2 ^ SCALE_FACTOR)^((begin - completion) - 1)
       // product of gparent and gleft has scale (2^SCALE_FACTOR)^(length-(end-begin)-1), so need to scale by 1 to maintain invariant
       %s
    }

    if (end < length) { // look right
      __global const parse_cell * gparent =  CELL(obot, begin, end+1);
      __global const parse_cell * gright = inside_tags + lengthOffsets[sentence] + (end);
      %s
    }

   // complete looking left
      if (begin > 0) { // look left
      __global const parse_cell * gparent =  CELL(obot, begin-1, end);
      __global const parse_cell * gleft = inside_tags + lengthOffsets[sentence] + (begin - 1);
      %s
     }
    // multiply in a 2^SCALE_FACTOR to re-achieve balance.
    __global parse_cell* gout = CELL(outsides_top + offsets[sentence], begin, end);
    __global parse_cell* gout2 = CELL(outsides_bot + offsets[sentence], begin, end);
    for(int i = 0; i < NUM_SYMS; ++i) {
//      if(otarget[i] != gout->syms[i][gram])
//        printf("%%d %%d %%e %%e %%e\n", begin, i, gout->syms[i][gram], ldexp(otarget[i], SCALE_FACTOR), gout->syms[i][gram]- ldexp(otarget[i], SCALE_FACTOR));
      gout->syms[i][gram] += ldexp(otarget[i], SCALE_FACTOR);
      gout2->syms[i][gram] += ldexp(otarget[i], SCALE_FACTOR);
    }
  }
}

    """.format(outsideUnaryUpdates(ruleStructure.ntermUnaries),
    outsideUnaryUpdates(ruleStructure.termUnaries),
    outsideRightCompletionUpdates(ruleStructure.ntRules),
    outsideLeftCompletionUpdates(ruleStructure.ntRules),
    outsideRightCompletionUpdates(ruleStructure.rightTermRules),
      outsideLeftCompletionUpdates(ruleStructure.leftTermRules),
    outsideRightCompletionUpdates(ruleStructure.leftTermRules),
    outsideLeftCompletionUpdates(ruleStructure.rightTermRules),
    outsideRightCompletionUpdates(ruleStructure.bothTermRules),
    outsideLeftCompletionUpdates(ruleStructure.bothTermRules)
  )

  if(true) {val o = new FileWriter("outside.cl"); o.write(text); o.close()}


  def outsideUnaryUpdates(rules: IndexedSeq[(UnaryRule[Int], Int)]): String = {
    val sb = new ArrayBuffer[String]
    sb += "float child;"
    val rules2 = rules.sortBy(_._1.child)
    var lastChild = -1
    for( (r, index) <- rules2) {
      if(r.child != lastChild) {
        if(lastChild != -1) {
          sb += """bot->syms[%d][gram] += child;""".format(lastChild)
        }
        sb += """child = rules->unaries[%d][gram] * top->syms[%d][gram];""".format(index, r.parent)
        lastChild = r.child
      } else {
        sb += """child = mad(rules->unaries[%d][gram], top->syms[%d][gram], child);""".format(index, r.parent)
      }
    }
    if(lastChild != -1) {
      sb += """bot->syms[%d][gram] += child;""".format(lastChild)
    }
    sb.mkString("\n    ")
  }


  // otarget is the left child, completion on right.
  def outsideRightCompletionUpdates(rules: IndexedSeq[(BinaryRule[Int], Int)]): String = {
    val newrules = rules.sortBy(r => (r._1.left, r._1.parent, r._1.right))(Ordering.Tuple3).groupBy(_._1.parent)
    val sb = new ArrayBuffer[String]
    sb += "float currentParent;"
    sb += "float currentSum = 0.0f; // sum for current left child"
    var lastLeft = -1
    for( (p, byParent) <- newrules) {
      val loaded = collection.mutable.BitSet.empty
      sb += "currentParent = gparent->syms[%d][gram]; // %s".format(p, symbolName(p))
      sb += "if (currentParent != 0.0f) {"
      for((r@BinaryRule(p, left, right), index) <- byParent) {
        if(lastLeft != left) {
          if(lastLeft != -1) {
            sb += "  otarget[%d] = mad(currentParent, currentSum, otarget[%d]);".format(lastLeft,lastLeft)
            sb += "  currentSum = 0.0f;"
          }
          sb += "  // left is %s".format(symbolName(left))
          lastLeft = left
        }
        if(!loaded(right)) {
          sb += "  float r%d = gright->syms[%d][gram]; // %s ".format(right, right, symbolName(right))
          loaded += right
        }
        sb += "  currentSum = mad(rules->binaries[%d][gram], r%d, currentSum);".format(index, right)
      }

      if(lastLeft != -1) {
        sb += "  otarget[%d] = mad(currentParent, currentSum, otarget[%d]);".format(lastLeft,lastLeft)
        sb += "  currentSum = 0.0f;"
      }
      sb += "}"
    }

    sb.mkString("\n      ")
  }


  def symbolName(sym: Int): L = {
    ruleStructure.grammar.labelIndex.get(sym)
  }

  // otarget is the right child, completion on left.
  private def outsideLeftCompletionUpdates(rules: IndexedSeq[(BinaryRule[Int], Int)]): String = {
    val newrules = rules.sortBy(r => (r._1.right, r._1.parent, r._1.left))(Ordering.Tuple3).groupBy(_._1.parent)
    val sb = new ArrayBuffer[String]
    sb += "float currentParent;"
    sb += "float currentSum = 0.0f; // sum for current right child"
    var lastRight = -1
    for( (p, byParent) <- newrules) {
      val loaded = collection.mutable.BitSet.empty
      sb += "currentParent = gparent->syms[%d][gram]; // %s".format(p, symbolName(p))
      sb += "if (currentParent != 0.0f) {"
      for((r@BinaryRule(p, left, right), index) <- byParent) {
        if(lastRight != right) {
          if(lastRight != -1) {
            sb += "  otarget[%d] = mad(currentParent, currentSum, otarget[%d]);".format(lastRight,lastRight)
            sb += "  currentSum = 0.0f;"
          }
          sb += "  // right is %s".format(symbolName(right))
          lastRight = right
        }
        if(!loaded(left)) {
          sb += "  float r%d = gleft->syms[%d][gram]; // %s ".format(left, left, symbolName(left))
          loaded += left
        }
        sb += "  currentSum = mad(rules->binaries[%d][gram], r%d, currentSum);".format(index, left)
      }

      if(lastRight != -1) {
        sb += "  otarget[%d] = mad(currentParent, currentSum, otarget[%d]);".format(lastRight,lastRight)
        sb += "  currentSum = 0.0f;"
      }
      sb += "}"
    }

    sb.mkString("\n      ")

  }

  val program = {
    val p = context.createProgram(text)
    p.setFastRelaxedMath()
    p.setUnsafeMathOptimizations()
    p.addBuildOption("-Werror")
    p.build()
  }
}
