package epic.parser.gpu

import com.nativelibs4java.opencl._
import collection.mutable.ArrayBuffer
import java.lang.{Float=>JFloat, Integer=>JInt, Long=>JLong}

class CreateMasksKernel[L, L2](rules: RuleStructure[L, L2],numGrammars: Int, odds_ratio: Boolean=false)(implicit context: CLContext) {

  def projections = rules.refinements.labels

  def createMasks(numSentences: Int, numCells: Int,
                 masks: CLBuffer[JLong],
                 projectedTop: CLBuffer[JFloat],
                 projectedBot: CLBuffer[JFloat],
                 offsets: CLBuffer[JInt],
                 events: CLEvent*)(implicit queue: CLQueue) = synchronized {
    set_coarse_masks.setArgs(masks, projectedTop, projectedBot, offsets)
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
    if (projected_top[cell].syms[sym][0] > PRUNING_THRESHOLD
      || projected_bot[cell].syms[sym][0] > PRUNING_THRESHOLD) {
      SET_COARSE(masks[cell], sym);
    }
  }


}

                                                             """


}
