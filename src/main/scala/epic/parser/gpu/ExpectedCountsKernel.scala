package epic.parser.gpu

import epic.trees.{BinaryRule, UnaryRule}
import collection.mutable.ArrayBuffer
import com.nativelibs4java.opencl._
import java.lang.{Float=>JFloat, Integer=>JInt}
import breeze.util.Index
import collection.mutable

class ExpectedCountsKernel[L](ruleStructure: RuleStructure[L], numGrammars: Int)(implicit context: CLContext) {

  def expectedCounts(ecounts: CLBuffer[JFloat],
                     termECounts: CLBuffer[JFloat],
                     insideBot: CLBuffer[JFloat],
                     insideTop: CLBuffer[JFloat],
                     outsideBot: CLBuffer[JFloat],
                     outsideTop: CLBuffer[JFloat],
                     offsets: CLBuffer[JInt],
                     lengths: CLBuffer[JInt],
                     offLengths: CLBuffer[JInt],
                     maxLength: Int,
                     rules: CLBuffer[JFloat],
                     events: CLEvent*)(implicit queue: CLQueue)  = {

    val eu, eb = new ArrayBuffer[CLEvent]()
    binaries.setArgs(ecounts, insideTop, outsideBot, offsets, lengths, offLengths, Integer.valueOf(1), rules)
    unaries.setArgs(ecounts, insideTop, insideBot, outsideTop, offsets, lengths, offLengths, Integer.valueOf(1), rules)
    terms.setArgs(termECounts, insideTop, insideBot, outsideBot, offsets, lengths, offLengths, Integer.valueOf(1))

    val maxDim1Size = queue.getDevice.getMaxWorkItemSizes()(0)
    val nsyms = ruleStructure.numSyms
    if(maxDim1Size < nsyms * numGrammars) {
      terms.setArg(6, numGrammars / 8 + 1)
    }
    val gramMultiplier = if(maxDim1Size < nsyms * numGrammars) {
      8
    } else {
      numGrammars
    }

    queue.enqueueWaitForEvents(events:_*)

    val termFinished =  terms.enqueueNDRange(queue, Array(nsyms * gramMultiplier, lengths.getElementCount.toInt, maxLength), Array(nsyms * gramMultiplier, 1, 1))
    var lastBDep = termFinished
    var lastUDep = termFinished
    for (len <- 2 to maxLength) {
      unaries.setArg(7, len)
      binaries.setArg(6, len)
      lastBDep = binaries.enqueueNDRange(queue, Array(lengths.getElementCount.toInt, maxLength+1-len, numGrammars), Array(1, 1, numGrammars), lastBDep)
      eb += lastBDep
      lastUDep = unaries.enqueueNDRange(queue, Array(lengths.getElementCount.toInt, maxLength+1-len, numGrammars), Array(1, 1, numGrammars), lastUDep)
      eu += lastUDep
    }
    unaries.setArg(7, 1)
   lastUDep =  unaries.enqueueNDRange(queue, Array(lengths.getElementCount.toInt, maxLength, numGrammars), Array(1, 1, numGrammars), lastUDep, lastBDep)
    eu += lastUDep

    if(queue.getProperties.contains(CLDevice.QueueProperties.ProfilingEnable)) {
      queue.finish()
      val iuCount = eu.map(e => e.getProfilingCommandEnd - e.getProfilingCommandStart).sum / 1E9
      val ibCount = eb.map(e => e.getProfilingCommandEnd - e.getProfilingCommandStart).sum / 1E9
      println("ecounts: " + iuCount + " " + ibCount)
    }

    lastUDep
  }


  private lazy val binaries = program.createKernel("ecount_binaries")
  private lazy val unaries = program.createKernel("ecount_unaries")
  private lazy val terms = program.createKernel("ecount_terminals")


  lazy val text = {
    import ruleStructure._
    val byParent: Map[Int, IndexedSeq[(BinaryRule[Int], Int)]] = binaryRulesWithIndices.groupBy(_._1.parent)
    val uByParent: Map[Int, IndexedSeq[(UnaryRule[Int], Int)]] = unaryRulesWithIndices.groupBy(_._1.parent)
    GrammarHeader.header(ruleStructure, numGrammars) +"""
__kernel void ecount_binaries(__global rule_cell* ecounts,
   __global const parse_cell * insides_top,
   __global const parse_cell* outsides_bot,
   __global const int* offsets,
   __global const int* lengths,
  __global const int* lengthOffsets,
   const int span_length,
   __global const rule_cell* rules) {
  const int sentence = get_global_id(0);
  const int begin = get_global_id(1);
  const int gram = get_global_id(2);
  const int end = begin + span_length;
  const int length = lengths[sentence];
  __global rule_cell* ruleCounts = ecounts + (lengthOffsets[sentence] + begin);
  __global const parse_cell* obot = outsides_bot + offsets[sentence];
  __global const parse_cell* itop = insides_top + offsets[sentence];
  const float root_score = CELL(itop, 0, length)->syms[ROOT][gram]; // scale is 2^(SCALE_FACTOR)^(length-1)
  if(end <= length) {
    float oscore;
    __global const parse_cell* oparents = CELL(obot, begin, end);
    %s
  }
}

__kernel void ecount_unaries(
              __global rule_cell* ecounts,
              __global const parse_cell * insides_top,
              __global const parse_cell * insides_bot,
              __global const parse_cell * outsides_top,
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

  if (end <= length) {
    __global const parse_cell* itop = insides_top + offsets[sentence];
    __global const parse_cell* outside = outsides_top + offsets[sentence];
    __global const parse_cell* inside = insides_bot + offsets[sentence];
    const float root_score = CELL(itop, 0, length)->syms[ROOT][gram]; // scale is 2^(SCALE_FACTOR)^(length-1)
    __global rule_cell* ruleCounts = ecounts + (lengthOffsets[sentence] + begin);
    __global const parse_cell* in = CELL(inside, begin, end);
    __global const parse_cell* out = CELL(outside, begin, end);
    %s
  }
}

__kernel void ecount_terminals(
   __global parse_cell* term_ecounts,
   __global const parse_cell * insides_top,
   __global const parse_cell * insides_bot,
   __global const parse_cell * outsides_bot,
   __global const int* offsets,
   __global const int* lengths,
   __global const int* lengthOffsets,
   const int numGrammarsToDo) {
  const int sym = get_global_id(0)/ NUM_GRAMMARS;
  int grammar = get_global_id(0) %% NUM_GRAMMARS;
  const int sentence = get_global_id(1);
  const int begin = get_global_id(2);
  const int end = begin  + 1;
  const int length = lengths[sentence];
  if (begin < length) {
    __global const parse_cell* inside = insides_bot + offsets[sentence];
    __global const parse_cell* itop = insides_top + offsets[sentence];
    __global const parse_cell* in = CELL(inside, begin, end);
    __global const parse_cell* out = CELL(outsides_bot + offsets[sentence], begin, end);
    __global parse_cell* mybuf = term_ecounts + (lengthOffsets[sentence] + begin);
    // ibot has scale 0, obot has scale length - 1, root_score has scale length - 1. Woot.
    for(int i = 0; i < numGrammarsToDo && grammar < NUM_GRAMMARS; ++i) {
      const float root_score = CELL(itop, 0, length)->syms[ROOT][grammar]; // scale is 2^(SCALE_FACTOR)^(length-1)
      mybuf->syms[sym][grammar] = (in->syms[sym][grammar] * out->syms[sym][grammar])/root_score;
      grammar += (NUM_GRAMMARS / numGrammarsToDo);
    }
  }
}
    """.format(ecountBinaryRules(byParent), ecountUnaries(uByParent))
  }

  val registersToUse = 60

  private def ecountBinaryRules(byParent: Map[Int, IndexedSeq[(BinaryRule[Int], Int)]]):String = {
    val buf = new ArrayBuffer[String]()
    buf += (0 until registersToUse).map("r" + _).mkString("float ", ", ", ";")
    for((par, rx) <- byParent) {
      val rules = rx.sortBy(r => r._1.left -> r._1.right)(Ordering.Tuple2)

      // oparent has scale length + begin - end, root has scale length - 1
      // left * right has scale (end - begin-2)
      // left * right * oparent / root has scale -1
      buf += "oscore = ldexp(oparents->syms[%d][gram]/root_score, SCALE_FACTOR);".format(par)
      buf += "if (oscore != 0.0f) {"
      var r = 0
      while(r < rules.length) {
        val assignments = Index[(Symbol,Int)]()
        val setThisRound = mutable.BitSet.empty
        val ruleRegisters = ArrayBuffer[(Int, Int)]() // Register -> Rule
        val regInitializerPos = buf.size
        buf += "XXX"

        buf += "  for(int split = begin + 1; split < end; ++split) {"
        var lastLeft = -1
        assignments.index(('left -> 1))
        while(r < rules.length && assignments.size < registersToUse) {
          val (BinaryRule(_, l, right), ruleIndex) = rules(r)
          if(lastLeft != l) {
            buf += "    r0 = CELL(itop, begin, split)->syms[%d][gram];".format(l)
            lastLeft = l
          }
          val rightR = assignments.index(('right, right))
          val ruleR = assignments.index(('rule, ruleIndex))
          if(assignments.size < registersToUse) {
            ruleRegisters += (ruleR -> ruleIndex)
            if (!setThisRound(rightR)) {
              buf += "    r%d = CELL(itop, split, end)->syms[%d][gram];".format(rightR, right)
              setThisRound += rightR
            }
            buf += "    r%d = fma(rules->binaries[%d][gram], r0 * r%d * oscore, r%d);".format(ruleR, ruleIndex, rightR, ruleR)
            r += 1
          }
        }

        buf += "  }\n"

        // register flush time!
        buf += "  // flush time!"
        for( (reg, rule) <- ruleRegisters) {
          buf += "  ruleCounts->binaries[%d][gram] +=  r%d;".format(rule, reg)
        }
        buf(regInitializerPos) = ruleRegisters.map { case (reg, rule) => "r%d = 0.0f;".format(reg)}.mkString("  ", " ", "");
      }
      buf += "}\n"
    }
    buf.mkString("\n    ")
  }

  private def ecountUnaries(byParent: Map[Int,IndexedSeq[(UnaryRule[Int], Int)]]): String = {
    val buf = new ArrayBuffer[String]()
    buf += "    float oscore;"
    for( (par, rules) <- byParent) {
      // oparent has scale length + begin - end, root has scale length - 1
      // child has scale (end - begin-1)
      // child * oparent / root has scale 0 (yay!)
      buf += "oscore = out->syms[%d][gram]/root_score;".format(par)
      for( (r,index) <- rules) {
        buf += "ruleCounts->unaries[%d][gram] += rules->unaries[%d][gram] * oscore * in->syms[%d][gram];".format(index, index, r.child)
      }
    }

    buf.mkString("\n    ")

  }


  val program = {
    val p = context.createProgram(text)
    p.setFastRelaxedMath()
    p.setUnsafeMathOptimizations()
    p.build()
  }
}
