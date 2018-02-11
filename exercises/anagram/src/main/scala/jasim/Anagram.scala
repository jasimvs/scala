package jasim

object Anagram {

  def anagrams(word: String, words: List[String]): List[String] = {
      val lowerCaseWord = word.toLowerCase
      val sortedWord = lowerCaseWord.sorted

      words.filter(anagram => {
        val lowerCaseAnagram = anagram.toLowerCase
        lowerCaseAnagram != lowerCaseWord && lowerCaseAnagram.sorted == sortedWord
      })
    }
}
