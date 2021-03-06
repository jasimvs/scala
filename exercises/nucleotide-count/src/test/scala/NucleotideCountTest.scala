import org.scalatest.{Matchers, FunSuite}

/** @version 1.0.0 */
class NucleotideCountTest extends FunSuite with Matchers {


  test("empty strand") {
    new DNA("").nucleotideCounts should be (Right(Map('A' -> 0,
      'C' -> 0,
      'G' -> 0,
      'T' -> 0)))
  }

  test("strand with repeated nucleotide") {
    pending
    new DNA("GGGGGGG").nucleotideCounts should be (Right(Map('A' -> 0,
      'C' -> 0,
      'G' -> 7,
      'T' -> 0)))
  }

  test("strand with multiple nucleotides") {
    pending
    new DNA("AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC").nucleotideCounts should be (Right(Map('A' -> 20,
      'C' -> 12,
      'G' -> 17,
      'T' -> 21)))
  }

  test("strand with invalid nucleotides") {
    pending
    new DNA("AGXXACT").nucleotideCounts.isLeft should be (true)
  }
}