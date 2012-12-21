package epic.parser.gpu

import com.nativelibs4java.opencl.{CLQueue, CLEvent, CLContext, CLBuffer}
import java.lang.{Float=>JFloat}
import com.nativelibs4java.opencl.CLMem.Usage

/**
 * 
 * @author dlwh
 */
case class GPUCharts(top: CLBuffer[JFloat],
                     bot: CLBuffer[JFloat],
                     tags: CLBuffer[JFloat]) {
  def release() {
    top.release()
    bot.release()
    tags.release()
  }

  def clear(events: CLEvent*)(implicit context: CLContext, queue: CLQueue): CLEvent = {
    val zmk = ZeroMemoryKernel()
    var a  = zmk.zeroMemory(top, events:_*)
    a = zmk.zeroMemory(bot,Seq(a) ++  events:_*)
    zmk.zeroMemory(tags,Seq(a) ++  events:_*)
  }

  override def finalize() {
    release()
    super.finalize()
  }
}

object GPUCharts {
  def forGrammar[L](structure: RuleStructure[L],
                    numGrammars: Int,
                    maxCells: Int,
                    maxTotalLength: Int)(implicit context: CLContext) = {
    val cellSize = structure.numSyms * numGrammars
    val insideTopDev, insideBotDev = context.createFloatBuffer(Usage.InputOutput, maxCells * cellSize)
    val posTagsDev = context.createFloatBuffer(Usage.Input, maxTotalLength * cellSize)

    new GPUCharts(insideTopDev, insideBotDev, posTagsDev)
  }

  def computeMaxSizes[L](totalBytes: Long, maxAllocSize: Long, structure: RuleStructure[L], numGrammars: Int) = {
    val cellSize = structure.numSyms * numGrammars
    val maxCells = ((totalBytes / cellSize).toInt / 4 / 2)  min 100000
    val maxTotalLength = (maxAllocSize / structure.numRules / 4).toInt min (maxCells * cellSize / structure.numRules) min 50000

    maxCells -> maxTotalLength
  }


}
