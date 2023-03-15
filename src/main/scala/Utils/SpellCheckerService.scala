package Utils

trait SpellCheckerService:
  /**
    * This dictionary is a Map object that contains valid words as keys and their normalized equivalents as values (e.g.
    * we want to normalize the words "veux" and "aimerais" in one unique term: "vouloir").
    */
  val dictionary: Map[String, String]

  /**
    * Calculate the Levenstein distance between two words.
    * @param s1 the first word
    * @param s2 the second word
    * @return an integer value, which indicates the Levenstein distance between "s1" and "s2"
    */
  def stringDistance(s1: String, s2: String): Int

  /**
    * Get the syntactically closest word in the dictionary from the given misspelled word, using the "stringDistance"
    * function. If the word is a number or a pseudonym, this function just returns it.
    * @param misspelledWord the mispelled word to correct
    * @return the closest normalized word from "mispelledWord"
    */
  def getClosestWordInDictionary(misspelledWord: String): String
end SpellCheckerService

class SpellCheckerImpl(val dictionary: Map[String, String]) extends SpellCheckerService:
  def stringDistance(s1: String, s2: String): Int = {
    def distance(i: Int, j: Int): Int = {
      if i == s1.length then s2.length - j
      else if j == s2.length then s1.length - i
      else {
        val cost = if s1(i) == s2(j) then 0 else 1
        val d1 = distance(i + 1, j) + 1
        val d2 = distance(i, j + 1) + 1
        val d3 = distance(i + 1, j + 1) + cost
        d1 min d2 min d3
      }
    }

    distance(0, 0)
  }

  def getClosestWordInDictionary(misspelledWord: String): String = {
    if misspelledWord.forall(_.isDigit) 
    || misspelledWord.startsWith("_") then return misspelledWord
    
    if dictionary.contains(misspelledWord) then return dictionary(misspelledWord)

    val closestWord = dictionary.keys.minBy(stringDistance(_, misspelledWord))
    dictionary(closestWord)
  }
end SpellCheckerImpl
