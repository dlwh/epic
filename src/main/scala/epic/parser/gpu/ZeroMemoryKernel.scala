package epic.parser.gpu

import com.nativelibs4java.opencl._
import java.util

class ZeroMemoryKernel(implicit val context: CLContext) {
  val program = context.createProgram{
"""
__kernel void mem_zero(__global float* data, int len) {
  int trg = get_global_id(0);
  if(trg < len)
    data[trg] = 0.0f;
}
"""
  }

  val kernel = program.createKernel("mem_zero")


  def zeroMemory(data: CLBuffer[java.lang.Float], events: CLEvent*)(implicit queue: CLQueue): CLEvent = synchronized {
    kernel.setArgs(data, java.lang.Integer.valueOf(data.getElementCount.toInt))
    kernel.enqueueNDRange(queue, Array(data.getElementCount.toInt), events:_*)
  }
}


object ZeroMemoryKernel {
  // TODO: leak here.
  def apply()(implicit context: CLContext) = map.synchronized {
    import scala.collection.JavaConverters._
    map.asScala.getOrElseUpdate(context, new ZeroMemoryKernel)
  }

  private val map = new util.IdentityHashMap[CLContext, ZeroMemoryKernel]

}