package epic.parser.gpu

import com.nativelibs4java.opencl._
import java.lang.{Float=>JFloat, Integer=>JInt}

/**
 * 
 * @author dlwh
 */
class PartitionCalculatorKernel[C, L](rules: RuleStructure[C, L], numGrammars: Int)(implicit context: CLContext) {

  def partitions(insideTop: CLBuffer[JFloat], offsets: CLBuffer[JInt], lengths: CLBuffer[JInt], numSentences: Int, events: CLEvent*)(implicit queue: CLQueue) = synchronized {
    val partitions = context.createFloatBuffer(CLMem.Usage.Output, numSentences * numGrammars)
    kernel.setArgs(partitions, insideTop, offsets, lengths)
    val toWait = kernel.enqueueNDRange(queue, Array(numSentences, numGrammars), Array(1, numGrammars), events:_*)

    val fPtr = partitions.read(queue, toWait)
    val floats = fPtr.getFloats
    partitions.release()
    fPtr.release()
    floats
  }

  val text =
    GrammarHeader.header(rules, numGrammars) + """
  #define LOG_2 %ff
  __kernel void compute_partitions(
     __global float* partitions,
     __global const parse_cell * insides_top,
     __global const int* offsets,
     __global const int* lengths) {
    const int sentence = get_global_id(0);
    const int grammar = get_global_id(1);
    const int length = lengths[sentence];
    __global const parse_cell* itop = insides_top + offsets[sentence];

    const float root_score = CELL(itop, 0, length)->syms[ROOT][grammar]; // scale is 2^(SCALE_FACTOR)^(length-1)
    partitions[sentence * NUM_GRAMMARS + grammar] = log(root_score) - SCALE_FACTOR * LOG_2 * (length-1);

  }""".format(math.log(2))

  val program = {
    val p = context.createProgram(text)
    p.setUnsafeMathOptimizations()
    p.setMadEnable()
    p.setFastRelaxedMath()
    p.build()
  }

  val kernel = program.createKernel("compute_partitions")

}
