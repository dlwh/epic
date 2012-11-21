package epic.parser.gpu

import com.nativelibs4java.opencl.{CLBuffer, JavaCL}
import org.bridj.Pointer._
import math._
import com.nativelibs4java.opencl.CLMem.Usage
import org.bridj.Pointer
import java.lang
import com.nativelibs4java.util.IOUtils

object AddVectors extends App {

  val context = JavaCL.createBestContext()
  val queue = context.createDefaultQueue()
  val byteOrder = context.getByteOrder

  val n = 1024
  val aPtr = allocateFloats(n).order(byteOrder)
  val bPtr: Pointer[lang.Float] = allocateFloats(n).order(byteOrder)

  for (i <- 0 until n) {
    aPtr.set(i, cos(i).toFloat)
    bPtr.set(i, sin(i).toFloat)
  }

  // Create OpenCL input buffers (using the native memory pointers aPtr and bPtr) :
  val a = context.createBuffer(Usage.Input, aPtr)
  val b = context.createBuffer(Usage.Input, bPtr)

  // Create an OpenCL output buffer :
  val out = context.createBuffer(Usage.Output, lang.Float.TYPE, n)

  // Read the program sources and compile them :
  val src = IOUtils.readText(this.getClass.getResource("AddVectors.cl"))
  val program = context.createProgram(src)

  // Get and call the kernel :
  val addFloatsKernel = program.createKernel("add_floats")
  addFloatsKernel.setArgs(a, b, out, Integer.valueOf(n))
  val addEvt = addFloatsKernel.enqueueNDRange(queue, Array(n))

  val outPtr = out.read(queue, addEvt); // blocks until add_floats finished

  val arr = outPtr.toArray
  println(arr.mkString(", "))


}
