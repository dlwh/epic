package epic.preprocess

import org.scalatest.FunSuite
import epic.slab._
import epic.trees.Span
import epic.trees.SpanConvert._
import scalaz.std.list._

class TreebankTokenizerTest  extends FunSuite {

  private def isOneToken(w: String) =
    if(w === TreebankTokenizer(w).head.content) None else Some(w + " " +  TreebankTokenizer(w))

  test("simple words") {
    val words = List("Hi","there","pilgrim","happy","Thanksgiving","there")
    for(w <- words) {
      val parsed = TreebankTokenizer(w).head
      assert(w == parsed.content)
      assert(Token(Span(0, w.length)) == parsed)
    }
  }

  test("some symbols and abbreviations") {
    val words = List(".","...","$","-","/", "--", "v.", "vs.", "etc.")
    for(w <- words) {
      assert(isOneToken(w), w)
    }
    val special = Map(
      "(" -> "(",
      ")" -> ")",
      "[" -> "[",
      "]" -> "]",
      "{" -> "{",
      "}" -> "}"
    )

    for( (s,t) <- special) {
      assert(TreebankTokenizer(s).toList.map(_.content) === List(t))
    }
  }

  test("simple sentences") {
    val sents = Map( "Every good boy does fine." -> List("Every","good","boy","does","fine","."),
      "Hi there, pilgrim; happy Thanksgiving there, pilgrim?" -> List("Hi","there",",","pilgrim",";","happy","Thanksgiving","there",",","pilgrim","?"),
      "Hi there, pilgrim; happy Thanksgiving there, pilgrim!" -> List("Hi","there",",","pilgrim",";","happy","Thanksgiving","there",",","pilgrim","!"),
      "Hi there, (pilgrim); happy Thanksgiving there, pilgrim!" -> List("Hi","there",",","(", "pilgrim", ")", ";","happy","Thanksgiving","there",",","pilgrim","!")
    )
    for( (s,toks) <- sents) {
      assert(TreebankTokenizer(s).toList.map(_.content) === toks)
    }
  }

  test("quotes") {
    val sents = Map("\"Hi there\"" -> List("``","Hi","there","''"),
      "\"Hi there.\"" -> List("``","Hi","there",".","''"))
    for( (s,toks) <- sents) {
      assert(TreebankTokenizer(s).toList.map(_.content) === toks)
    }
  }

  test("contractions and possessives") {
    val sents = Map(//"didn't" -> List("did","n't"),
      "ya'll" -> List("ya","'ll"),
      "we're" -> List("we","'re"),
      "we've" -> List("we","'ve"),
      "YA'LL" -> List("YA","'LL"),
      "WE'RE" -> List("WE","'RE"),
      "WE'VE" -> List("WE","'VE"),
      "We've" -> List("We","'ve"),
      "cannot" -> List("can","not"),
      "can't" -> List("ca","n't"),
      "CAN'T" -> List("CA","N'T"),
      "I'm" -> List("I","'m"),
      "He's" -> List("He","'s"),
      "parents'" -> List("parents","'"),
    "America’s"->List("America","'s"),
    "O'Donnell’s"->List("O'Donnell","'s"),
    "o'clock"->List("o'clock")
    )
    for( (s,toks) <- sents) {
      assert(TreebankTokenizer(s).toList.map(_.content) === toks)
    }
  }

  test("moneys") {
    assert(TreebankTokenizer("99").toList.map(_.content) === List("99"))
    assert(TreebankTokenizer("$99").toList.map(_.content) === List("$","99"))
    assert(TreebankTokenizer("$99.33").toList.map(_.content) === List("$","99.33"))
  }

  test("dates + comma") {
    assert(TreebankTokenizer("13,").toList.map(_.content) === List("13", ","))
    assert(TreebankTokenizer("December 06,").toList.map(_.content) === List("December","06",","))
    assert(TreebankTokenizer("arunrob@gmail.com  October 13, 2010 at 12:39 PM").toList.map(_.content) === List("arunrob@gmail.com", "October","13",",", "2010", "at", "12:39", "PM"))
  }

  test("'sam i am'") {
    assert(TreebankTokenizer("'sam i am'").toList.map(_.content) === List("`", "sam", "i", "am", "'"))
  }


  test("special words") {
    val words = Map("cannot" -> List("can","not"),
      "d'ye"-> List("d'","ye"),
      "gimme"->List("gim","me"),
      "gonna"->List("gon","na"),
      "gotta"->List("got","ta"),
      "Lemme"->List("Lem","me"),
      "more'n"->List("more","'n"),
      "'tis"->List("'t","is"),
      "'Tis"->List("'T","is"),
      "wanna"->List("wan","na")
      //        "Whaddya"->List("Wha","dd","ya"),
      //        "Whatcha"->List("Wha","t","cha")
    )
    for( (s,toks) <- words) {
      assert(TreebankTokenizer(s).toList.map(_.content) === toks)
    }

  }

  test("acronyms") {
    val candidates = Seq("U.S.","u.s.","p.s.")
    for(s <- candidates) {
      assert(TreebankTokenizer(s).toList.map(_.content) === List(s,"."))
    }
  }

  test("URLs") {
    val text = "Go to http://google.com/ now!"
    assert(TreebankTokenizer(text).toList.map(_.content) === List("Go", "to", "http://google.com/", "now", "!"))
  }

  test("polish clitics") {
    val text = Seq("Osobiście radziłabym panu iść do domu", "Opłaciłam telefon", "własnym", "Załamała", "gdy wsiadałam do pociągu .", "temu moglibyśmy")
    val tok = Seq("Osobiście radziła by m panu iść do domu", "Opłaciła m telefon", "własnym", "Załamała", "gdy wsiadała m do pociągu .", "temu mogli by śmy")
    for( (txt,tk) <- text zip tok) {
      assert(TreebankTokenizer(txt).toList.map(_.content) === tk.split(" ").toList)
    }
  }

  test("polish shouldn't mess up english") {
    val text = Seq("slam", "Islam")
    text.foreach(w => assert(isOneToken(w)))
  }


  test("emails") {
    assert(TreebankTokenizer("Email asdf@asdf.com.").toList.map(_.content) === List("Email", "asdf@asdf.com", "."))
  }


  test("tweets") {
    for( (text, toks) <- tweets zip tweet_tokens) {
      assert(TreebankTokenizer(text).toList.map(_.content) === toks.toList)
    }
  }


  // test examples from https://github.com/brendano/ark-tweet-nlp/blob/master/examples/example_tweets.txt
  // Code is Apache License 2.0.0

  val tweets = """I predict I won't win a single game I bet on. Got Cliff Lee today, so if he loses its on me RT @e_one: Texas (cont) http://tl.gd/6meogh
    RT @DjBlack_Pearl: wat muhfuckaz wearin 4 the lingerie party?????
    Wednesday 27th october 2010. 》have a nice day :)
  RT @ddlovato: @joejonas oh, hey THANKS jerk!
  @thecamion I like monkeys, but I still hate COSTCO parking lots..
  @DDaimaru I may have to get minecraft after watching videos of it
    RT @eye_ee_duh_Esq: LMBO! This man filed an EMERGENCY Motion for Continuance on account of the Rangers game tonight! « Wow lmao
    RT @musicdenver: Lady Gaga - Bad Romance http://dld.bz/n6Xv
  RT @cheriexamor: When you have a good thing, hold it, squeeze it, never let it go.
  Texas Rangers are in the World Series!  Go Rangers!!!!!!!!! http://fb.me/D2LsXBJx
   @aliciakeys Put it in a love song :-))
   @hellocalyclops =))=))=)) Oh well
  hello (#hashtag)
  hello (@person)
  """.split("\n").map(_.trim)

  val tweet_tokens =
    """I
      |predict
      |I
      |wo
      |n't
      |win
      |a
      |single
      |game
      |I
      |bet
      |on
      |.
      |Got
      |Cliff
      |Lee
      |today
      |,
      |so
      |if
      |he
      |loses
      |its
      |on
      |me
      |RT
      |@e_one
      |:
      |Texas
      |(
      |cont
      |)
      |http://tl.gd/6meogh
      |QQQ
      |RT
      |@DjBlack_Pearl
      |:
      |wat
      |muhfuckaz
      |wearin
      |4
      |the
      |lingerie
      |party
      |?????
      |QQQ
      |Wednesday
      |27th
      |october
      |2010
      |.
      |》
      |have
      |a
      |nice
      |day
      |:)
      |QQQ
      |RT
      |@ddlovato
      |:
      |@joejonas
      |oh
      |,
      |hey
      |THANKS
      |jerk
      |!
      |QQQ
      |@thecamion
      |I
      |like
      |monkeys
      |,
      |but
      |I
      |still
      |hate
      |COSTCO
      |parking
      |lots
      |...
      |QQQ
      |@DDaimaru
      |I
      |may
      |have
      |to
      |get
      |minecraft
      |after
      |watching
      |videos
      |of
      |it
      |QQQ
      |RT
      |@eye_ee_duh_Esq
      |:
      |LMBO
      |!
      |This
      |man
      |filed
      |an
      |EMERGENCY
      |Motion
      |for
      |Continuance
      |on
      |account
      |of
      |the
      |Rangers
      |game
      |tonight
      |!
      |«
      |Wow
      |lmao
      |QQQ
      |RT
      |@musicdenver
      |:
      |Lady
      |Gaga
      |-
      |Bad
      |Romance
      |http://dld.bz/n6Xv
      |QQQ
      |RT
      |@cheriexamor
      |:
      |When
      |you
      |have
      |a
      |good
      |thing
      |,
      |hold
      |it
      |,
      |squeeze
      |it
      |,
      |never
      |let
      |it
      |go
      |.
      |QQQ
      |Texas
      |Rangers
      |are
      |in
      |the
      |World
      |Series
      |!
      |Go
      |Rangers
      |!!!!!!!!!
      |http://fb.me/D2LsXBJx
      |QQQ
      |@aliciakeys
      |Put
      |it
      |in
      |a
      |love
      |song
      |:-))
      |QQQ
      |@hellocalyclops
      |=))
      |=))
      |=))
      |Oh
      |well
      |QQQ
      |hello
      |(
      |#hashtag
      |)
      |QQQ
      |hello
      |(
      |@person
      |)
    """.stripMargin.split("QQQ").map(_.trim.split("\n").map(_.trim))


  test("Gettysburg address") {
    val text = """But, in a larger sense, we can not dedicate -- we can not consecrate -- we can not hallow -- this ground."""
    val words = TreebankTokenizer(text).toSeq.map(_.content)
    assert(words.length === 25, words)
    assert(words.startsWith(Seq("But", ",", "in", "a", "larger", "sense", ",", "we", "can", "not", "dedicate", "--")), words)
  }
}
