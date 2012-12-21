package epic.parser.gpu

import com.nativelibs4java.opencl.{CLQueue, CLEvent, CLBuffer, CLContext}
import java.lang.{Float=>JFloat, Integer=>JInt}

/**
 * 
 * @author dlwh
 */
class CopyPOSTagsKernel[L](rules: RuleStructure[L], numGrammars: Int)(implicit context: CLContext)  {

  def copyTags(numSentences: Int, maxLength: Int, tags: CLBuffer[JFloat], insideBot: CLBuffer[JFloat], offsets: CLBuffer[JInt], lens: CLBuffer[JInt], offLen: CLBuffer[JInt], events: CLEvent*)(implicit queue: CLQueue) = {
    kernel.setArgs(tags, insideBot, offsets, lens, offLen, Integer.valueOf(1))
    val maxDim1Size = queue.getDevice.getMaxWorkItemSizes()(0)
    if(maxDim1Size < rules.numSyms * numGrammars) {
      kernel.setArg(5, numGrammars / 8 + 1)
    }
    val gramMultiplier = if(maxDim1Size < rules.numSyms * numGrammars) {
      8
    } else {
      numGrammars
    }


    kernel.enqueueNDRange(queue,  Array(rules.numSyms * gramMultiplier, numSentences, maxLength), Array(rules.numSyms * gramMultiplier, 1, 1), events:_*)
  }


  private val copyPosToCharts =
    """
__kernel void copy_pos_to_charts(
  __global const parse_cell* pos_tags,
  __global parse_cell * insides_bot,
  __global const int* offsets,
  __global const int* lengths,
  __global const int* lengthOffsets,
  const int numGrammarsToDo) {
  const int sym = get_global_id(0)/ NUM_GRAMMARS;
  int grammar = get_global_id(0) % NUM_GRAMMARS;
  const int sentence = get_global_id(1);
  const int begin = get_global_id(2);
  const int end = begin  + 1;
  const int length = lengths[sentence];
  if (begin < length) {
    __global parse_cell* inside = insides_bot + offsets[sentence];
    __global parse_cell* in = CELL(inside, begin, end);
    __global const parse_cell* mybuf = pos_tags + (lengthOffsets[sentence] + begin);
    for(int i = 0; i < numGrammarsToDo && grammar < NUM_GRAMMARS; ++i) {
      in->syms[sym][grammar] = mybuf->syms[sym][grammar];
      grammar += (NUM_GRAMMARS / numGrammarsToDo);
    }
  }
}"""


  val program = {
    val p = context.createProgram(GrammarHeader.header(rules, numGrammars), copyPosToCharts)
    p.setUnsafeMathOptimizations()
    p.setFastRelaxedMath()
    p.build()
  }

  private val kernel = program.createKernel("copy_pos_to_charts")
}
