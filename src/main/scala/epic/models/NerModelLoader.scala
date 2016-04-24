package epic.models

import scala.reflect.ClassTag
import epic.sequences.SemiCRF

trait NerModelLoader extends ModelLoader[SemiCRF[Any, String]]

object NerSelector extends ModelSelector[SemiCRF[Any, String], NerModelLoader] {
  override protected def manifest: ClassTag[NerModelLoader] = scala.reflect.classTag[NerModelLoader]
  def loadNer(language: String = "en"): Option[SemiCRF[Any, String]] = this.findModel(s"language:$language").map(_.load())
}

