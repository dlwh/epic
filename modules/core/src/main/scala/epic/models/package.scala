package epic

import java.io.{FileNotFoundException, IOException, File}
import java.util.zip.{GZIPInputStream, ZipFile}
import scala.collection.JavaConverters._
import scala.util.{Success, Try}

/**
 * TODO
 *
 * @author dlwh
 **/
package object models {

  def deserialize[T](model: String): T = deserialize[T](model, new File(System.getProperty("user.dir")))

  def readFromJar[T](model: String, file: File): T = {
    val zip = new ZipFile(file)
    val obj = zip.entries().asScala.collectFirst {
      case e if e.getName == model || e.getName.endsWith("model.ser.gz") =>
        breeze.util.nonstupidObjectInputStream(new GZIPInputStream(zip.getInputStream(e))).readObject().asInstanceOf[T]
    }
    obj.getOrElse(throw new RuntimeException(s"Could not find model $model in jar $file"))
  }

  /**
   * Deserializes a model by checking first, if path is a file, tries to either read the object
   * file named model or (something like it) exists in the directory
   * path.
   * @param model
   * @tparam T
   * @return
   */
  def deserialize[T](model: String, path: File): T = {
    if (!path.exists()) {
      throw new FileNotFoundException(path.toString)
    } else if (!path.isDirectory) {
      try {
        readFromJar(model, path)
      } catch {
        case ex: Exception =>
        breeze.util.readObject[T](path)
      }
    } else {
      // exists, is a directory
      val modelFile = Seq(model, s"$model.ser.gz", s"$model.gz", s"$model.ser", s"$model.zip", s"$model.jar").map(new File(path, _)).find(_.exists)
      modelFile match {
        case Some(f) if f.isDirectory =>
          deserialize(model, f)
        case Some(f) =>
          try {
            breeze.util.readObject[T](f)
          } catch {
            case ex: IOException =>
              try {
                readFromJar("", f)
              } catch {
                case ex: Exception =>
                  throw new RuntimeException(s"Could not find model $model in path $path", ex)
              }
          }
        case None =>
          // look for jar files, try to read from there
          path.listFiles().filter(f => f.getName.endsWith(".jar") || f.getName.endsWith(".zip")).iterator.map { f =>
            Try {
              readFromJar[T](model, f)
            }
          }.collectFirst { case Success(r) => r }.getOrElse {
            throw new RuntimeException(s"Could not find model $model in path $path")
          }
      }
    }
  }

}
