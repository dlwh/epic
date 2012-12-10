package epic.parser.gpu

import com.nativelibs4java.opencl._
import collection.mutable.ArrayBuffer
import java.lang.{Float=>JFloat, Integer=>JInt}

class EPKernel[L, L2](rules: RuleStructure[L, L2],numGrammars: Int)(implicit context: CLContext) {

  def projections = rules.refinements.labels

  def updateCharts(numSentences: Int,
                   chart: CLBuffer[JFloat],
                   q: CLBuffer[JFloat],
                   msg: CLBuffer[JFloat],
                   offsets: CLBuffer[JInt],
                   lengths: CLBuffer[JInt],
                   spanLength: Int,
                   maxLength: Int,
                   events: CLEvent*)(implicit queue: CLQueue) = synchronized {
    update_chart.setArgs(chart, q, msg, offsets, lengths, Integer.valueOf(spanLength))
    val a = update_chart.enqueueNDRange(queue, Array(numSentences, maxLength + 1 - spanLength, numGrammars), Array(1, 1, numGrammars), events:_*)

    a
  }


  def updateQs(numSentences: Int,
               projectedTop: CLBuffer[JFloat],
               projectedBot: CLBuffer[JFloat],
               insideTop: CLBuffer[JFloat],
               insideBot: CLBuffer[JFloat],
               outsideTop: CLBuffer[JFloat],
               outsideBot: CLBuffer[JFloat],
               qTop: CLBuffer[JFloat],
               qBot: CLBuffer[JFloat],
               msgTop: CLBuffer[JFloat],
               msgBot: CLBuffer[JFloat],
               offsets: CLBuffer[JInt],
               lengths: CLBuffer[JInt],
               maxLength: Int,
               events: CLEvent*)(implicit queue: CLQueue) = synchronized {
    val pn = new ArrayBuffer[CLEvent]()
    val um = new ArrayBuffer[CLEvent]()
    val uq = new ArrayBuffer[CLEvent]()
    project_qnews.setArgs(projectedBot, insideBot, outsideBot, insideTop, qBot, msgBot, offsets, lengths, Integer.valueOf(1))
    update_msg.setArgs(qBot, projectedBot, msgBot, offsets, lengths, Integer.valueOf(1))
    update_q.setArgs(qBot, msgBot, offsets, lengths, Integer.valueOf(1))

    for (len <- 1 to maxLength) {
      project_qnews.setArg(8, len)
      val b = project_qnews.enqueueNDRange(queue, Array(numSentences, maxLength + 1 - len, numGrammars), Array(1, 1, numGrammars), events:_*)
      pn += b
      update_msg.setArg(5, len)
      um += update_msg.enqueueNDRange(queue, Array(numSentences, maxLength + 1 - len, numGrammars), Array(1, 1, numGrammars), b)
      update_msg.setArg(4, len)
      uq += update_q.enqueueNDRange(queue, Array(numSentences, maxLength + 1 - len, numGrammars), Array(1, 1, numGrammars), um.last)
    }

    project_qnews.setArgs(projectedTop, insideTop, outsideTop, insideTop, qTop, msgTop, offsets, lengths, Integer.valueOf(1))
    update_msg.setArgs(qTop, projectedTop, msgTop, offsets, lengths, Integer.valueOf(1))
    update_q.setArgs(qTop, msgTop, offsets, lengths, Integer.valueOf(1))
    for(len <- 1 to maxLength) {
      project_qnews.setArg(8, len)
      val b2 = project_qnews.enqueueNDRange(queue, Array(numSentences, maxLength + 1 - len, numGrammars), Array(1, 1, numGrammars), events:_*)
      pn += b2
      update_msg.setArg(5, len)
      um += update_msg.enqueueNDRange(queue, Array(numSentences, maxLength + 1 - len, numGrammars), Array(1, 1, numGrammars), b2)
      update_msg.setArg(4, len)
      uq += update_q.enqueueNDRange(queue, Array(numSentences, maxLength + 1 - len, numGrammars), Array(1, 1, numGrammars), um.last)
    }

    if(queue.getProperties.contains(CLDevice.QueueProperties.ProfilingEnable)) {
      queue.finish()
      val iuCount = pn.map(e => e.getProfilingCommandEnd - e.getProfilingCommandStart).sum / 1E9
      val umCount = um.map(e => e.getProfilingCommandEnd - e.getProfilingCommandStart).sum / 1E9
      val uqCount = uq.map(e => e.getProfilingCommandEnd - e.getProfilingCommandStart).sum / 1E9
      println("newQs: " + iuCount + " " +umCount + " " + uqCount)
    }

    queue.finish()
    uq.last
  }


  val program = {
    val p = context.createProgram(text)
    p.setFastRelaxedMath()
    p.setUnsafeMathOptimizations()
    p.addBuildOption("-Werror")
    p.build()
  }

  private val update_chart = program.createKernel("ep_update_chart")
  private val update_msg = program.createKernel("ep_update_msg")
  private val update_q = program.createKernel("ep_update_msg")
  private val project_qnews = program.createKernel("ep_project_charts")


  lazy val text = GrammarHeader.header(rules, numGrammars) + """

__kernel void ep_update_chart(__global parse_cell* charts,
                              __global const projected_q* qs,
                              __global const projected_parse_cell* msgs,
                              __global const int* offsets,
                              __global const int* lengths,
                             const int spanLength) {
  const int sentence = get_global_id(0);
  const int begin = get_global_id(1);
  const int gram = get_global_id(2);
  const int end = begin + spanLength;
  const int length = lengths[sentence];
  if(end <= length) {
    __global parse_cell* chart = CELL(charts + offsets[sentence], begin, end);
    __global const projected_q* q = CELL(qs + offsets[sentence], begin, end);
    __global const projected_parse_cell* msg = CELL(msgs + offsets[sentence], begin, end);
    if(q->off == 1.0f) return;
    float off = msg->off[gram] / q->off;
    if(q->off == 0.0f) off = 1.0f;
    %s
  }
}



__kernel void ep_update_msg(__global const projected_q* qs,
                            __global const projected_parse_cell* qnews,
                            __global projected_parse_cell* msgs,
                            __global const int* offsets,
                            __global const int* lengths,
                            const int spanLength) {
  const int sentence = get_global_id(0);
  const int begin = get_global_id(1);
  const int gram = get_global_id(2);
  const int end = begin + spanLength;
  const int length = lengths[sentence];
  if(end <= length) {
    __global const projected_q* q = CELL(qs + offsets[sentence], begin, end);
    __global projected_parse_cell* msg = CELL(msgs + offsets[sentence], begin, end);
    __global const projected_parse_cell* qnew = CELL(qnews + offsets[sentence], begin, end);
    float off = q->off * (msg->off[gram] / qnew->off[gram]);
    msg->off[gram] = (!isfinite(off) && off >= 0.0f) ? off : 1.0f;
    %s
  }
}

 __kernel void ep_update_q(__global projected_q* qs,
                           __global const projected_parse_cell* msgs,
                           __global const int* offsets,
                           __global const int* lengths,
                           const int spanLength) {
  const int sentence = get_global_id(0);
  const int begin = get_global_id(1);
  const int gram = get_global_id(2);
  const int end = begin + spanLength;
  const int length = lengths[sentence];
  if(end <= length) {
    __global projected_q* q = CELL(qs + offsets[sentence], begin, end);
    __global const projected_parse_cell* msg = CELL(msgs + offsets[sentence], begin, end);
    %s
  }
}

__kernel void ep_project_charts(__global projected_parse_cell* projected,
                             __global const parse_cell* insides,
                             __global const parse_cell* outsides,
                             __global const parse_cell* insides_top,
                             __global const projected_q* qs,
                             __global const projected_parse_cell* msgs,
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
    __global const projected_parse_cell* msg = CELL(msgs + offsets[sentence], begin, end);
    __global const projected_q* q = CELL(qs + offsets[sentence], begin, end);
    __global projected_parse_cell* target = CELL(projected + offsets[sentence], begin, end);
    float off = q->off / msg->off[gram];
    if(msg->off[gram] == 0.0 || q->off == 0.0f) off = 1.0f;

    %s
  }
}
                                                             """.format(updateChartsInner, updateMsgInner, updateQInner, projectNonterminalsInner)

  private def updateChartsInner = {
    val buf = new ArrayBuffer[String]()
    buf += "float cur;"
    for(coarse <- 0 until projections.coarseIndex.size) {
      buf += "cur = q->syms[%d] * off / msg->syms[%d][gram];".format(coarse, coarse)
      for(ref <- projections.refinementsOf(coarse)) {
        buf += "chart->syms[%d][gram] *= cur;".format(ref)
      }
    }
    buf.mkString("\n      ")
  }

  private def updateMsgInner = {
    val buf = new ArrayBuffer[String]()
    for(coarse <- 0 until projections.coarseIndex.size) {
      buf += "float r%d = q->syms[%d] * (msg->syms[%d][gram] / qnew->syms[%d][gram]);".format(coarse, coarse, coarse, coarse)
      buf += "msg->syms[%d][gram] = (isfinite(r%d) && r%d > 0.0f) ? r%d : 1.0f;".format(coarse, coarse, coarse, coarse)
    }
    buf.mkString("\n      ")
  }

  private def updateQInner = {
    val buf = new ArrayBuffer[String]()
    buf += "float sum = 0.0f;"
    buf += "float cur;"
    for(coarse <- 0 until projections.coarseIndex.size) {
      buf += "cur = 1.0f;"
      for(ref <- 0 until numGrammars) {
        buf += "cur *= msg->syms[%d][%d];".format(coarse, ref)
      }
      buf += "sum += cur;"
      buf += "q->syms[%d] = cur;".format(coarse)
    }

    buf += "cur = 1.0f;"
    for(ref <- 0 until numGrammars) {
      buf += "cur *= msg->off[%d];".format(ref)
    }
    buf += "sum += cur;"
    buf += "q->off = cur / sum;"
    buf += "if(q->off == 1.0f) return;"

    for(coarse <- 0 until projections.coarseIndex.size) {
      buf += "q->syms[%d] /= sum;".format(coarse)
    }

    buf.mkString("\n      ")
  }

  private def projectNonterminalsInner = {
    val buf = new ArrayBuffer[String]()
    buf += "float sum = 0.0f;"
    buf += "float cur, rescale;"
    for(coarse <- 0 until projections.coarseIndex.size) {
      buf += "// nb, inverse of update_charts. we have to remove one of these scales from either in or out to not overcount."
      buf += "rescale = msg->syms[%d][gram] / q->syms[%d] * off;".format(coarse, coarse)
      buf += "cur = 0.0;"
      for(ref <- projections.refinementsOf(coarse)) {
        buf += "cur = mad(in->syms[%d][gram], out->syms[%d][gram] * rescale, cur);".format(ref, ref)
      }
      buf += "sum += cur;"
      buf += "target->syms[%d][gram] = cur/root_score;".format(coarse)
    }

    buf += "const float norm = 1.0f - sum/root_score;"
    buf += "target->off[gram] = norm;"
    buf.mkString("\n      ")
  }

}
