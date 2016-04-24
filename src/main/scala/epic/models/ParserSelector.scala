package epic.models

import epic.trees.AnnotatedLabel
import epic.parser.Parser
import scala.reflect.ClassTag

/**
 * TODO
 *
 * @author dlwh
 **/
object ParserSelector extends ModelSelector[Parser[AnnotatedLabel, String], ParserModelLoader] {
  override protected def manifest: ClassTag[ParserModelLoader] = scala.reflect.classTag[ParserModelLoader]
  def loadParser(language: String = "en"): Option[Parser[AnnotatedLabel, String]] = this.findModel(s"language:$language").map(_.load())
}

trait ParserModelLoader extends ModelLoader[Parser[AnnotatedLabel,String]]
