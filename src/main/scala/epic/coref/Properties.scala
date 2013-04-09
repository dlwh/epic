package epic.coref

import epic.ontonotes.DSpan
import epic.everything.Property

object Properties {
  def allExtractors = IndexedSeq(genderExtractor, numberExtractor, personExtractor)

  val Gender = Property("Gender")("Masc", "Fem", "Neuter")
  val Number = Property("Number")("Sing", "Plural")
  val Person = Property("Person")("First", "Second", "Third")

  val genderExtractor = new PropertyExtractor[String] {
    def property = Gender


    def extract(c: DSpan, context: CorefDocument) = {
      if(c.length == 1 && Phrases.isPronoun(c.getYield(context.words).head)) {
        c.getYield(context.words).head.toLowerCase match {
          case "he" | "him" | "his" => Gender.choices("Masc")
          case "she" | "her" | "hers" => Gender.choices("Fem")
          case "it" | "its" => Gender.choices("Neuter")
          case _ => -1
        }

      } else {
        -1
      }
    }
  }

  val numberExtractor = new PropertyExtractor[String] {
    def property = Number

    def extract(c: DSpan, context: CorefDocument) = {
      val cWords = c.getYield(context.words)
      val word = Phrases.headFor(cWords)
      if(Phrases.isPlural(word)) 1
      else 0
    }
  }

  val personExtractor = new PropertyExtractor[String] {
    def property = Person

    /**
     * Returns index of choice. -1 for unknown
     */
    def extract(c: DSpan, context: CorefDocument) = {
      val cWords = c.getYield(context.words)
      if(cWords.length == 1 && Phrases.isPronoun(cWords.head)) {
        cWords.head.toLowerCase match {
          case  "you" | "your" => Person.choices("Second")
          case  "I" | "me" | "we" | "us" | "my" | "our" => Person.choices("First")
          case _ => Person.choices("Third")
        }
      } else {
        Person.choices("Third")
      }
    }
  }

  def alwaysUnobservedExtractor[T](prop: Property[T]):PropertyExtractor[T] = new PropertyExtractor[T] {
    def property: Property[T] = prop

    def extract(c: DSpan, context: CorefDocument): Int = -1
  }
}
