package common

class ConcreteSentenceDetector extends SentenceDetector[Char, Seq] {
  //
  // Parameters for tuning the computation
  //
  val sentenceTerminators: Set[Char] = ".?!".toSet

  def computeSentences(input: Input): Stream[Sentence] = splitIntoSentences(input)

  //
  // The following few calls implement an LL(k) paring approach.
  // RegEx could be used instead by LL(k) is more flexible
  //
  // The `production` calls can be thought of as state transitions
  // over Tape objects

  // Abstraction around input where input is broken up into who parts.
  // The point of the split can be thought of as a cursor
  case class Tape(prefix: Input, suffix: Input) {
    def lift = Option(this)
  }


  // Finds a sentence terminator if exists
  private def nextPeriod(in: Tape): Option[Tape] = in.suffix match {
    case Nil => None
    case c :: tail if sentenceTerminators.contains(c) => Tape(in.prefix :+ c, tail).lift
    case c :: tail => nextPeriod(Tape(in.prefix :+ c, tail))
  }

  // Skips zero or more empty spaces (always possible)
  private def skipZeroOrMoreEmpty(in: Tape): Tape = in.suffix match {
    case Nil => in
    case c :: tail => if (c.isWhitespace)
      skipZeroOrMoreEmpty(Tape(in.prefix :+ c, tail))
    else
      in
  }

  // Advance the "cursor" to next sentence if possible
  private def skipToNextSentence(in: Tape): Option[Tape] = nextPeriod(in) match {
    case Some(t) => skipZeroOrMoreEmpty(t) match {
      case Tape(p, rest@c :: tail) =>
        if (c.isUpper)
          Tape(t.prefix, rest).lift
        else
          skipToNextSentence(Tape(p, rest))
      case Tape(_, Nil) => Tape(t.prefix, Nil).lift
    }
    case None => None
  }

  // splits a list of chars into sentences
  private def splitIntoSentences(in: Input): Stream[Sentence] =
    skipToNextSentence(Tape(Nil, in)) match {
      case None => Stream.empty
      case Some(Tape(p, s)) => p #:: splitIntoSentences(s)
    }
}

