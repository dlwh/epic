package epic.parser.gpu

import epic.parser.projections.ProjectionIndexer
import com.nativelibs4java.opencl._
import collection.mutable.ArrayBuffer
import java.lang.{Float=>JFloat, Integer=>JInt}

class ProjectionKernel[L, L2](rules: RuleStructure[L2], projections: ProjectionIndexer[L, L2], numGrammars: Int, odds_ratio: Boolean=false)(implicit context: CLContext) {

  def projectCells(projectedTop: CLBuffer[JFloat],
                  projectedBot: CLBuffer[JFloat],
                 insideTop: CLBuffer[JFloat],
                 insideBot: CLBuffer[JFloat],
                 outsideTop: CLBuffer[JFloat],
                 outsideBot: CLBuffer[JFloat],
                 offsets: CLBuffer[JInt],
                 lengths: CLBuffer[JInt],
                 maxLength: Int,
                 events: CLEvent*)(implicit queue: CLQueue) = synchronized {
    project_nterms.setArgs(projectedBot, insideBot, outsideBot, insideTop, offsets, lengths, Integer.valueOf(1))
    val pn = new ArrayBuffer[CLEvent]()

    // TODO: retrofit inside/outside binaries and unaries to look at posTagsPointer....
    // TODO: also get ecounts...
    for (len <- 1 to maxLength) {
      project_nterms.setArg(0, projectedBot)
      project_nterms.setArg(1, insideBot)
      project_nterms.setArg(2, outsideBot)
      project_nterms.setArg(6, len)
      val b = project_nterms.enqueueNDRange(queue, Array(lengths.getElementCount.toInt, maxLength + 1 - len, numGrammars), Array(1, 1, numGrammars), events:_*)
      pn += b
    }


    for(len <- 1 to maxLength) {
      project_nterms.setArg(0, projectedTop)
      project_nterms.setArg(1, insideTop)
      project_nterms.setArg(2, outsideTop)
      project_nterms.setArg(6, len)
      val b2 = project_nterms.enqueueNDRange(queue, Array(lengths.getElementCount.toInt, maxLength + 1 - len, numGrammars), Array(1, 1, numGrammars), events:_*)
      pn += b2
    }

    if(queue.getProperties.contains(CLDevice.QueueProperties.ProfilingEnable)) {
      queue.finish()
      val iuCount = pn.map(e => e.getProfilingCommandEnd - e.getProfilingCommandStart).sum / 1E9
      println("project: " + iuCount)
    }

    pn.lastOption.getOrElse(null)
  }

  private lazy val project_nterms = program.createKernel("project_nterms")

  val program = {
    val p = context.createProgram(text)
    p.setFastRelaxedMath()
    p.setUnsafeMathOptimizations()
    p.build()
  }

  lazy val text = GrammarHeader.header(rules, numGrammars) + """
#define NUM_PROJECTED_SYMS  %d

typedef struct {
  float syms[NUM_PROJECTED_SYMS][NUM_GRAMMARS];
} projected_parse_cell;

__kernel void project_nterms(__global projected_parse_cell* projected,
                             __global const parse_cell* insides,
                             __global const parse_cell* outsides,
                             __global const parse_cell* insides_top,
                             __global const int* offsets,
                             __global const int* lengths,
                             const int spanLength) {
  const int sentence = get_global_id(0);
  const int begin = get_global_id(1);
  const int gram = get_global_id(2);
  const int end = begin + spanLength;
  const int length = lengths[sentence];

  if(end <= length) {
    const float root_score = CELL(insides_top + offsets[sentence], 0, length)->syms[ROOT][gram]; // scale is 2^(SCALE_FACTOR)^(length-1)
    __global const parse_cell* in = CELL(insides + offsets[sentence], begin, end);
    __global const parse_cell* out = CELL(outsides + offsets[sentence], begin, end);
    __global projected_parse_cell* target = CELL(projected + offsets[sentence], begin, end);

    %s
  }

}

                                                             """.format(projections.coarseIndex.size, projectNonterminalsInner)

  private def projectNonterminalsInner = {
    val buf = new ArrayBuffer[String]()
    if(odds_ratio)
      buf += "float sum = 0.0;"
    buf += "float cur;"
    for(coarse <- 0 until projections.coarseIndex.size) {
      buf += "cur = 0.0;"
      for(ref <- projections.refinementsOf(coarse)) {
        buf += "cur = mad(in->syms[%d][gram], out->syms[%d][gram], cur);".format(ref, ref)
      }
      if(odds_ratio) {
        buf += "sum += cur;"
        buf += "target->syms[%d][gram] = cur;".format(coarse)
      } else {
        buf += "target->syms[%d][gram] = cur/root_score;".format(coarse,coarse)
      }
    }

    if(odds_ratio) {
      buf += "const float norm = root_score - sum;"
      buf += "for (int i = 0; i < NUM_PROJECTED_SYMS; ++i) {"
      buf += "  target->syms[%d][gram] /= norm;"
      buf += "}"
    }
    buf.mkString("\n      ")
  }}
