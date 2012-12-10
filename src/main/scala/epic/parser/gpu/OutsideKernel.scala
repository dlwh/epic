package epic.parser.gpu

import epic.trees._
import com.nativelibs4java.opencl._
import collection.mutable.ArrayBuffer
import java.lang.{Float=>JFloat, Integer=>JInt, Long=>JLong}
import java.io.FileWriter


class OutsideKernel[C, L](ruleStructure: RuleStructure[C, L], numGrammars: Int)(implicit context: CLContext) {
  import ruleStructure._
  def outsidePass(numSentences: Int,
                 outsideTop: CLBuffer[JFloat],
                 outsideBot: CLBuffer[JFloat],
                 insideTop: CLBuffer[JFloat],
                 posTags: CLBuffer[JFloat],
                 offsets: CLBuffer[JInt],
                 lengths: CLBuffer[JInt],
                 lengthOffsets: CLBuffer[JInt],
                 masks: CLBuffer[JLong],
                 maxLength: Int,
                 rules: CLBuffer[JFloat],
                 events: CLEvent*)(implicit queue: CLQueue) = {
    val ou, ob, otb, ot = new ArrayBuffer[CLEvent]()
    var lastU = null:CLEvent
    lbinaries.foreach(_.setArgs(outsideTop, outsideBot, insideTop, offsets, lengths, masks, Integer.valueOf(maxLength), rules))
    rbinaries.foreach(_.setArgs(outsideTop, outsideBot, insideTop, offsets, lengths, masks, Integer.valueOf(maxLength), rules))
    unaries.setArgs(outsideTop, outsideBot, offsets, lengths, Integer.valueOf(maxLength), rules)
    tunaries.setArgs(outsideTop, outsideBot, offsets, lengths, rules)
    termbs.setArgs(outsideTop, outsideBot, insideTop, posTags, offsets, lengths, lengthOffsets, Integer.valueOf(maxLength), masks, rules)
    bterms.setArgs(outsideTop, outsideBot, insideTop, posTags, offsets, lengths, lengthOffsets, masks, rules)
    lastU = unaries.enqueueNDRange(queue, Array(numSentences, 1, numGrammars), Array(1, 1, numGrammars), events:_*)
    ou += lastU

    for (len <- (maxLength - 1) to 1 by -1) {
      lbinaries.foreach(_.setArg(6, len))
      rbinaries.foreach(_.setArg(6, len))
      val lastLB = lbinaries.map(_.enqueueNDRange(queue, Array(numSentences, maxLength + 1 - len, numGrammars), Array(1, 1, numGrammars), lastU))
      ob ++= lastLB
      val lastRB = for( (rb, block) <- rbinaries zip lastLB) yield rb.enqueueNDRange(queue, Array(numSentences, maxLength + 1 - len, numGrammars), Array(1, 1, numGrammars), block)
      ob ++= lastRB
      termbs.setArg(7, len)
      lastU = termbs.enqueueNDRange(queue, Array(numSentences, maxLength + 1 - len, numGrammars), Array(1, 1, numGrammars), lastRB:_*)
      otb += lastU
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
      val otbCount = otb.map(e => e.getProfilingCommandEnd - e.getProfilingCommandStart).sum / 1E9
      val otCount = ot.map(e => e.getProfilingCommandEnd - e.getProfilingCommandStart).sum / 1E9
      println("outside: " + ouCount + " " + obCount + " " + otCount + " " + otbCount)
    }

    lastU
  }

  private lazy val lbinaries = Array.tabulate(partitionsLeft.length)(i => program.createKernel("outside_binaries_left_" + i))
  private lazy val rbinaries = Array.tabulate(partitionsRight.length)(i => program.createKernel("outside_binaries_right_" + i))
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



__kernel void outside_term_binaries(__global parse_cell* outsides_top,
              __global const parse_cell* outsides_bot,
              __global const parse_cell * insides_top,
              __global const parse_cell * inside_tags,
              __global const int* offsets,
              __global const int* lengths,
              __global const int* lengthOffsets,
              const int spanLength,
              __global const pruning_mask* masks,
            __global const rule_cell* rules) {

  const int sentence = get_global_id(0);
  const int begin = get_global_id(1);
  const int gram = get_global_id(2);
  const int end = begin + spanLength;
  const int length = lengths[sentence];
  __global const pruning_mask* mask =  masks + offsets[sentence] + TRIANGULAR_INDEX(begin, end);
   if (end <= length) {
    if( IS_ANY_SET(*mask)) {
      float otarget[NUM_SYMS];
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
}

 __kernel void outside_binary_terms(__global parse_cell* outsides_top,
              __global parse_cell* outsides_bot,
              __global const parse_cell * insides_top,
              __global const parse_cell * inside_tags,
              __global const int* offsets,
              __global const int* lengths,
              __global const int* lengthOffsets,
              __global const pruning_mask* masks,
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
    outsideRightCompletionUpdates(ruleStructure.rightTermRules),
      outsideLeftCompletionUpdates(ruleStructure.leftTermRules),
    outsideRightCompletionUpdates(ruleStructure.leftTermRules),
    outsideLeftCompletionUpdates(ruleStructure.rightTermRules),
    outsideRightCompletionUpdates(ruleStructure.bothTermRules),
    outsideLeftCompletionUpdates(ruleStructure.bothTermRules)
  ) ++ (
    (0 until partitionsLeft.length).map(i => outside_binaries_left(partitionsLeft(i), i)).mkString("\n")
  ) ++ (
    (0 until partitionsRight.length).map(i => outside_binaries_right(partitionsRight(i), i)).mkString("\n")
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
          sb += "  float right%d = gright->syms[%d][gram]; // %s ".format(right, right, symbolName(right))
          loaded += right
        }
        sb += "  currentSum = mad(rules->binaries[%d][gram], right%d, currentSum);".format(index, right)
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
          sb += "  float right%d = gleft->syms[%d][gram]; // %s ".format(left, left, symbolName(left))
          loaded += left
        }
        sb += "  currentSum = mad(rules->binaries[%d][gram], right%d, currentSum);".format(index, left)
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

  private def outside_binaries_left(rules: IndexedSeq[(BinaryRule[Int], Int)], id: Int = 0) = {
   pruningCheckForSyms(rules.map(_._1.left).toSet, id+300) +    """
      __kernel void outside_binaries_left_%d(__global parse_cell* outsides_top,
              __global const parse_cell* outsides_bot,
              __global const parse_cell * insides_top,
              __global const int* offsets,
              __global const int* lengths,
              __global const pruning_mask* masks,
              const int spanLength,
            __global const rule_cell* rules) {
  const int sentence = get_global_id(0);
  const int begin = get_global_id(1);
  const int gram = get_global_id(2);
  const int end = begin + spanLength;
  const int length = lengths[sentence];
  __global const pruning_mask* lmask =  masks + offsets[sentence] + TRIANGULAR_INDEX(begin, end);
  if (end <= length && IS_ANY_IN_BLOCK_%d(*lmask)) {
    %s
    __global const parse_cell* inside = insides_top + offsets[sentence];
    __global const parse_cell* obot = outsides_bot + offsets[sentence];
    __global parse_cell* gout = CELL(outsides_top + offsets[sentence], begin, end);
    // complete looking right
    for(int completion = end+1; completion <= length; ++completion) {
       __global const parse_cell * gparent = CELL(obot, begin, completion); // scale factor of (2 ^ SCALE_FACTOR)^(length-(completion - begin))
       __global const parse_cell * gright = CELL(inside, end, completion); // scale factor of (2 ^ SCALE_FACTOR)^((end - completion) - 1)
       __global const pruning_mask* rmask =  masks + offsets[sentence] + TRIANGULAR_INDEX(end, completion);
       __global const pruning_mask* pmask =  masks + offsets[sentence] + TRIANGULAR_INDEX(begin, completion);
       if(IS_ANY_SET(*rmask) && IS_ANY_SET(*pmask)) {
       // product of gparent and gright has scale (2^SCALE_FACTOR)^(length-(end-begin)-1), so need to scale by 1 to maintain invariant
       %s
       }
    }

    // multiply in a 2^SCALE_FACTOR to re-achieve balance.
    %s
  }
}
    """.stripMargin.format(id,id+300,
      rules.map(_._1.left).toSet[Int].map("left" + _).mkString("float ", " = 0.0f,", " = 0.0f;"),
      outsideNTRightCompletionUpdates(rules),
      rules.map(_._1.left).toSet[Int].map(p => "gout->syms[%d][gram] += ldexp(left%d, SCALE_FACTOR);".format(p,p)).mkString("\n   ")
    )
  }


  private def outside_binaries_right(rules: IndexedSeq[(BinaryRule[Int], Int)], id: Int = 0) = {
    pruningCheckForSyms(rules.map(_._1.right).toSet, id) +  """
      __kernel void outside_binaries_right_%d(__global parse_cell* outsides_top,
              __global const parse_cell* outsides_bot,
              __global const parse_cell * insides_top,
              __global const int* offsets,
              __global const int* lengths,
              __global const pruning_mask* masks,
              const int spanLength,
            __global const rule_cell* rules) {
  const int sentence = get_global_id(0);
  const int begin = get_global_id(1);
  const int gram = get_global_id(2);
  const int end = begin + spanLength;
  const int length = lengths[sentence];
  __global const pruning_mask* rmask =  masks + offsets[sentence] + TRIANGULAR_INDEX(begin, end);
  if (end <= length && IS_ANY_IN_BLOCK_%d(*rmask)) {
    %s
    __global const parse_cell* inside = insides_top + offsets[sentence];
    __global const parse_cell* obot = outsides_bot + offsets[sentence];
    __global parse_cell* gout = CELL(outsides_top + offsets[sentence], begin, end);
   // complete looking left
    for(int completion = 0; completion < begin; ++completion) {
       __global const parse_cell * gparent = CELL(obot, completion, end); // scale factor of (2 ^ SCALE_FACTOR)^(length-(end-completion))
       __global const parse_cell * gleft = CELL(inside, completion, begin); // scale factor of (2 ^ SCALE_FACTOR)^((begin - completion) - 1)
       __global const pruning_mask* lmask =  masks + offsets[sentence] + TRIANGULAR_INDEX(completion, begin);
       __global const pruning_mask* pmask =  masks + offsets[sentence] + TRIANGULAR_INDEX(completion, end);
       if(IS_ANY_SET(*lmask) && IS_ANY_SET(*pmask)) {
         // product of gparent and gleft has scale (2^SCALE_FACTOR)^(length-(end-begin)-1), so need to scale by 1 to maintain invariant
         %s
       }
    }

    // multiply in a 2^SCALE_FACTOR to re-achieve balance.
   %s
  }
}
      """.stripMargin.format(id, id,
      rules.map(_._1.right).toSet[Int].map("right" + _).mkString("float ", " = 0.0f,", " = 0.0f;"),
      outsideNTLeftCompletionUpdates(rules),
      rules.map(_._1.right).toSet[Int].map(p => "gout->syms[%d][gram] += ldexp(right%d, SCALE_FACTOR);".format(p,p)).mkString("\n    ")
      )
  }


  // otarget is the left child, completion on right.
  def outsideNTRightCompletionUpdates(rules: IndexedSeq[(BinaryRule[Int], Int)]): String = {
    val newrules = rules.sortBy(r => (r._1.left, r._1.parent, r._1.right))(Ordering.Tuple3).groupBy(_._1.parent)
    val sb = new ArrayBuffer[String]
    val parents = rules.map(_._1.parent).toSet
    val rights = rules.map(_._1.right).toSet
    sb += "float currentSum = 0.0f; // sum for current left child"
    parents.map(p => "const float parent%d = gparent->syms[%d][gram];".format(p,p)).foreach(sb += _)
    rights.map(r => "const float right%d = gright->syms[%d][gram];".format(r,r)).foreach(sb += _)
    for( (p, byParent) <- newrules) {
      var lastLeft = -1
      sb += "if (COARSE_IS_SET(*pmask, %d)) {".format(ruleStructure.refinements.labels.project(p))
      sb += "if (parent%d != 0.0f) { // %s".format(p, symbolName(p))
      for((r@BinaryRule(p, left, right), index) <- byParent) {
        if(lastLeft != left) {
          if(lastLeft != -1) {
            sb += "  left%d = mad(parent%d, currentSum, left%d); // %s".format(lastLeft, p, lastLeft, symbolName(lastLeft))
            sb += "  currentSum = 0.0f;"
          }
          sb += "  // left is %s".format(symbolName(left))
          lastLeft = left
        }
        sb += "  currentSum = mad(rules->binaries[%d][gram], right%d, currentSum); // %s".format(index, right, ruleString(index))
      }

      if(lastLeft != -1) {
        sb += "  left%d = mad(parent%d, currentSum, left%d); // %s".format(lastLeft,p, lastLeft, symbolName(lastLeft))
        sb += "  currentSum = 0.0f;"
      }
      sb += "}"
      sb += "}"
    }

    sb.mkString("\n      ")
  }


  def ruleString(r: Int) = {
    ruleStructure.grammar.index.get(r) match {
      case BinaryRule(a, b, c) => "%s -> %s %s".format(a,b,c)
      case UnaryRule(a, b, c) => "%s -> %s (%s)".format(a,b,c)
    }
  }

  // otarget is the right child, completion on left.
  private def outsideNTLeftCompletionUpdates(rules: IndexedSeq[(BinaryRule[Int], Int)]): String = {
    val newrules = rules.sortBy(r => (r._1.right, r._1.parent, r._1.left))(Ordering.Tuple3).groupBy(_._1.parent)
    val sb = new ArrayBuffer[String]
    val parents = rules.map(_._1.parent).toSet
    val lefts = rules.map(_._1.left).toSet
    sb += "float currentSum = 0.0f; // sum for current right child"
    parents.map(p => "const float parent%d = gparent->syms[%d][gram];".format(p,p)).foreach(sb += _)
    lefts.map(l => "const float left%d = gleft->syms[%d][gram];".format(l,l)).foreach(sb += _)
    for( (p, byParent) <- newrules) {
      var lastRight = -1
      sb += "if (COARSE_IS_SET(*pmask, %d)) {".format(ruleStructure.refinements.labels.project(p))
      sb += "if (parent%d != 0.0f) { // %s".format(p, symbolName(p))
      for((r@BinaryRule(p, left, right), index) <- byParent) {
        if(lastRight != right) {
          if(lastRight != -1) {
            sb += "  right%d = mad(parent%d, currentSum, right%d); // %s".format(lastRight,p, lastRight, symbolName(lastRight))
            sb += "  currentSum = 0.0f;"
          }
          sb += "  // right is %s".format(symbolName(right))
          lastRight = right
        }
        sb += "  currentSum = mad(rules->binaries[%d][gram], left%d, currentSum); // %s".format(index, left, ruleString(index))
      }

      if(lastRight != -1) {
        sb += "  right%d = mad(parent%d, currentSum, right%d); // %s ".format(lastRight, p, lastRight, symbolName(lastRight))
        sb += "  currentSum = 0.0f;"
      }
      sb += "}"
      sb += "}"
    }

    sb.mkString("\n      ")

  }
}
