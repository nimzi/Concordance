package common

class ConcreteWordDetector extends WordDetector[Char, Seq] {
  val sentenceTerminators: Set[Char] = ".?!".toSet
  val extraneousChars: Set[Char] = ",:()\"".toSet

  def computeWords(sentence: Sentence): Stream[Word] =
    splitIntoWords(stripPunctuationMarks(sentence))
      .filter(_.head.isLetter)
      .map(_.mkString.toLowerCase.toList)


  private def stripPunctuationMarks(sentence: Sentence): Sentence = sentence match {
    case Nil => Nil
    case c :: Nil if sentenceTerminators.contains(c) => Nil
    case c :: tail =>
      if (extraneousChars.contains(c))
        stripPunctuationMarks(tail)
      else
        Seq(c) ++ stripPunctuationMarks(tail)
  }

  def splitIntoWords(sentence:Sentence) : Stream[Word] = {
    def split(word: Word, sentence: Sentence): Stream[Word] = (word, sentence) match {
      case (Nil, Nil) => Stream.empty
      case (w, Nil) => Stream(w)
      case (Nil, c :: tail) => if (c.isWhitespace) split(Nil, tail) else split(word :+ c, tail)
      case (w, c :: tail) => if (c.isWhitespace) w #:: split(Nil, tail) else split(word :+ c, tail)
    }

    split(Nil, sentence)
  }




}