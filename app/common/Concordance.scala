package common

// Enabling higher-kinded types
import language.higherKinds

trait SentenceDetector[Element, Sequence[U <: Element] <: Traversable[U]] {
  type Input = Sequence[Element]
  type Sentence = Sequence[Element]

  def computeSentences(input: Input): Stream[Sentence]
}

trait WordDetector[Element, Sequence[U <: Element] <: Traversable[U]] {
  type Sentence = SentenceDetector[Element, Sequence]#Sentence
  type Word = Sequence[Element]

  def computeWords(sentence: Sentence): Stream[Word]
}

trait SentenceDetectorComponent[Element, Sequence[U <: Element] <: Traversable[U]] {
  type Sentence = SentenceDetector[Element, Sequence]#Sentence
  type Input = SentenceDetector[Element, Sequence]#Input
  val sentenceDetector: SentenceDetector[Element, Sequence]
}

trait WordDetectorComponent[Element, Sequence[U <: Element] <: Traversable[U]] {
  type Word = WordDetector[Element, Sequence]#Word
  val wordDetector: WordDetector[Element, Sequence]
}

case class WordStat(ocurrenceCount: Int, citationsBySentenceIndex: Seq[Int])

trait ConcordanceComputer[E, S[U <: E] <: Traversable[U]]
  extends SentenceDetectorComponent[E, S] with WordDetectorComponent[E, S] {
  type CumulativeStats = Map[Word, WordStat]

  def concordance(input: Input): CumulativeStats
}

// Concrete stuff...

class ConcreteConcordanceComputer extends ConcordanceComputer[Char, Seq] {

  // Implementing abstract vals defined in traits
  val sentenceDetector = new ConcreteSentenceDetector
  val wordDetector = new ConcreteWordDetector

  def concordance(input: Input): CumulativeStats = {
    val sentences = sentenceDetector.computeSentences(input)

    // At the moment there is no advantage to using streams over lists
    // but if we were to make results available incrementally or wanted to
    // distribute the computation then stream-based design carries advantages
    val wordsWithSentenceIndex: Stream[(Word, Int)] = sentences.zipWithIndex.flatMap {
      case (sentence, idx) =>
        wordDetector.computeWords(sentence).map(word => (word, idx))
    }

    val zero: CumulativeStats = Map()

    wordsWithSentenceIndex.foldLeft(zero)((statsMap, wordWithIndex) => {
      val (word, index) = wordWithIndex
      val updatedWordStat = {
        for (WordStat(count, citations) <- statsMap.get(word))
          yield WordStat(count + 1, citations :+ index)
      }
        .getOrElse(WordStat(1, Seq(index)))

      statsMap + (word -> updatedWordStat)
    })
  }

  def compute(input: String): Map[String, WordStat] = {
    val charSequence = input.stripMargin.replace("\n", " ").toList
    concordance(charSequence).map { case (w,s) => (w.mkString,s) }
  }


}
