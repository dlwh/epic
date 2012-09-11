package epic.coref

import epic.everything.models.Property

object Properties {
  def allExtractors = IndexedSeq(genderExtractor, numberExtractor, personExtractor)

  val Gender = Property("Gender")("Masc", "Fem", "Neuter")
  val Number = Property("Number")("Sing", "Plural")
  val Person = Property("Person")("First", "Second", "Third")

  val genderExtractor = new PropertyExtractor {
    def property = Gender

    def extract(c: MentionCandidate, context: CorefDocument) = {
      if(c.words.length == 1 && Phrases.isPronoun(c.words.head)) {
        c.words.head.toLowerCase match {
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

  val numberExtractor = new PropertyExtractor {
    def property = Number

    def extract(c: MentionCandidate, context: CorefDocument) = {
      val word = Phrases.headFor(c.words)
      if(Phrases.isPlural(word)) 1
      else 0
    }
  }

  val personExtractor = new PropertyExtractor {
    def property = Person

    /**
     * Returns index of choice. -1 for unknown
     */
    def extract(c: MentionCandidate, context: CorefDocument) = {
       if(c.words.length == 1 && Phrases.isPronoun(c.words.head)) {
        c.words.head.toLowerCase match {
          case  "you" | "your" => Person.choices("Second")
          case  "I" | "me" | "we" | "us" | "my" | "our" => Person.choices("First")
          case _ => Person.choices("Third")
        }
      } else {
        Person.choices("Third")
      }
    }
  }
}
