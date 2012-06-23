package epic.framework

import java.util.zip.GZIPInputStream
import java.io.{File, FileInputStream, BufferedInputStream, ObjectInputStream}
import breeze.serialization.DataSerialization
import breeze.linalg.Counter

/**
 *
 * @author dlwh
 */
trait ModelFactory[Datum] {
  type MyModel <: Model[Datum]

  def make(train: IndexedSeq[Datum]): MyModel

  def readWeights(file: File):Counter[Feature, Double] = if(file != null && file.exists) {
    val in = new ObjectInputStream(new BufferedInputStream(new GZIPInputStream(new FileInputStream(file))))
    implicit val serFeature: DataSerialization.ReadWritable[Feature] = DataSerialization.naiveReadWritable
    val ctr = DataSerialization.read[Counter[Feature,Double]](in)
    in.close()
    ctr
  } else Counter()
}