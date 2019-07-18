package epic.models

trait LanguageSpecific { this: ModelLoader[_] =>
  def language: String
  def capabilities(): Array[String] = Array(s"language:$language")
}

trait EnglishModel extends LanguageSpecific { this: ModelLoader[_] =>
  def language = "en"
}