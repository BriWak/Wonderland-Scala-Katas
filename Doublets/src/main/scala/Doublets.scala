import scala.annotation.tailrec

object Doublets extends App {

  def doublets(word1: String, word2: String): List[String] = {

    @tailrec
    def helper(c : String, listOfWords : List[String]) : List[String] = {
      val nextWord = GetNextWord(c, word2, listOfWords)
      val acc = listOfWords

      (c, word2) match {
        case (w1, w2) if w1 == w2 =>
          acc
        case (c,_) if c == "word not found" =>
          List("")
        case _ =>
          val nextAdded = acc ::: List(nextWord)
          helper(nextWord, nextAdded)
      }
    }
    helper(word1, List(word1))
  }
  println(doublets("head", "tail"))
}

object CheckWord {
  def apply(word: String): Boolean = {
    val dict = scala.io.Source.fromFile("./Doublets/resources/words.edn").getLines.toSet
    val dictionary = dict.map(x => x.trim.dropRight(1).drop(1))
    dictionary.contains(word.toLowerCase)
  }
}

object GetNextWord {
  def apply(word: String, word2: String, previousWords: List[String]): String = {
    val alphabet = "abcdefghijklmnopqrstuvwxyz".toList
    val nextWord = {
      for {
        i <- 0 until word.length
        eachLetter <- alphabet
      } yield {
        word.patch(i, eachLetter.toString, 1)
      }
    }

    val returnWord = nextWord.toList.filter(x => x != word && !previousWords.contains(x) && CheckWord(x) )
    returnWord.length match {
      case 0 => "word not found"
      case 1 => returnWord.head
      case _ =>
        val closestMatch = returnWord.map(x => (x, distance(x,word2)))
        closestMatch.map(_.swap).min._2
    }
  }

  def distance(word1: String, word2: String): Int = {
    (word1, word2).zipped.map(_ == _).count(_ == false)
  }
}