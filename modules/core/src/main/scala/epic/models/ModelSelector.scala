package epic.models

import scala.collection.JavaConverters._
import java.util.ServiceLoader
import scala.reflect.ClassTag

/**
 * TODO
 *
 * @author dlwh
 **/
trait ModelSelector[+T, Loader <: ModelLoader[T]] {

  protected def classLoader = this.getClass.getClassLoader
  protected def manifest: ClassTag[Loader]

  private lazy val serviceLoader = ServiceLoader.load(manifest.runtimeClass.asInstanceOf[Class[Loader]], classLoader)

  def findModel(features: String*): Option[Loader] = {
    findModel{ x => lazy val a = x.capabilities.toSet; features.forall(a) }
  }

  def findModel(filter: Loader => Boolean) = serviceLoader.synchronized {
    serviceLoader.asScala.find(filter)
  }

}


