package epic.models

import scala.reflect.ClassTag
import epic.sequences.CRF
import epic.trees.AnnotatedLabel


trait PosTagModelLoader extends ModelLoader[CRF[AnnotatedLabel, String]]

object PosTagSelector extends ModelSelector[CRF[AnnotatedLabel, String], PosTagModelLoader] {
  override protected def manifest: ClassTag[PosTagModelLoader] = scala.reflect.classTag[PosTagModelLoader]
  def loadTagger(language: String = "en"): Option[CRF[AnnotatedLabel, String]] = this.findModel(s"language:$language").map(_.load())
}

