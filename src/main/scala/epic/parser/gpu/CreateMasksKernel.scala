package epic.parser.gpu

import com.nativelibs4java.opencl._
import collection.mutable.ArrayBuffer
import java.lang.{Float=>JFloat, Integer=>JInt, Long=>JLong}

class CreateMasksKernel[L, L2](rules: RuleStructure[L, L2], numGrammars: Int)(implicit context: CLContext) {

  def projections = rules.refinements.labels

  def createMasks(numSentences: Int, numCells: Int,
                 masks: CLBuffer[JLong],
                 projected: GPUCharts,
                 offsets: CLBuffer[JInt],
                 events: CLEvent*)(implicit queue: CLQueue) = synchronized {
    set_coarse_masks.setArgs(masks, projected.top, projected.bot, offsets)
    val pn = new ArrayBuffer[CLEvent]()
    val b = set_coarse_masks.enqueueNDRange(queue, Array(numCells), events:_*)
    pn += b

    if(queue.getProperties.contains(CLDevice.QueueProperties.ProfilingEnable)) {
      queue.finish()
      val iuCount = pn.map(e => e.getProfilingCommandEnd - e.getProfilingCommandStart).sum / 1E9
      println("mask: " + iuCount)
    }

    b
  }

  private lazy val set_coarse_masks = program.createKernel("set_coarse_masks")
  private lazy val set_coarse_masks_term = program.createKernel("set_coarse_masks_term")

  val program = {
    val p = context.createProgram(text)
    p.setFastRelaxedMath()
    p.setUnsafeMathOptimizations()
    p.build()
  }

  lazy val text = GrammarHeader.header(rules, numGrammars) + """
#define PRUNING_THRESHOLD 3.3E-4f // approx exp(-8)

__kernel void set_coarse_masks(__global pruning_mask* masks,
                             __global projected_parse_cell* projected_top,
                             __global projected_parse_cell* projected_bot,
                             __global const int* offsets) {
  const int cell = get_global_id(0);

  for(int sym = 0; sym < NUM_PROJECTED_SYMS; ++sym) {
    if (projected_top[cell].syms[sym][0] > PRUNING_THRESHOLD || projected_bot[cell].syms[sym][0] > PRUNING_THRESHOLD) {
      SET_COARSE(masks[cell], sym);
    }
  }
}

     /*
__kernel void set_coarse_masks_term(__global pruning_mask* masks,
                               __global projected_parse_cell* projected_term,
                               __global const int* offsets,
                               __global const int* lengthOffsets,
                               __global const int* lengths) {

  const int sentence = get_global_id(0);
  const int begin = get_global_id(1);
  const int end = begin + 1;
  if(end < lengths[sentence]) {
    __global pruning_mask* mask = CELL(masks + offsets[sentence], begin, end);
    __global projected_parse_cell* cell = projected_term + lengthOffsets[sentence] + begin;

    for(int sym = 0; sym < NUM_PROJECTED_SYMS; ++sym) {
      if (cell->syms[sym][0] > PRUNING_THRESHOLD) {
        SET_COARSE(mask, sym);
      }
    }
  }
}
*/

                                                             """


}
