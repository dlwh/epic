package epic.models

import java.io.{BufferedInputStream, ObjectInputStream}
import java.util.zip.GZIPInputStream

/**
* TODO
*
* @author dlwh
**/
trait ModelLoader[+T] { outer =>
  def load(): T
  def capabilities: Array[String]
}

abstract class ClassPathModelLoader[+T](modelPath: String = "model.ser.gz") extends ModelLoader[T] {
  def load() = {
    val input = this.getClass.getResourceAsStream(modelPath)
    val gzipin = breeze.util.nonstupidObjectInputStream(new BufferedInputStream(new GZIPInputStream(input)), ignoreSerialVersionUID = true)
    try {
      gzipin.readObject().asInstanceOf[T]
    } finally {
      gzipin.close()
    }
  }
}

/* this class exists as a hack to get around limitations in service loader*/
class DelegatingLoader[+T](outer: ModelLoader[T]) extends ModelLoader[T] {
  def load() = outer.load()
  def capabilities = outer.capabilities
}
