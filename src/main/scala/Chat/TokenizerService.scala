package Chat

import Chat.Token.*
import Utils.SpellCheckerService

class TokenizerService(spellCheckerSvc: SpellCheckerService, products: List[String]):
  /**
    * Separate the user's input into tokens
    * @param input The user's input
    * @return A Tokenizer which allows iteration over the tokens of the input
    */
  def tokenize(input: String): Tokenized = {
    val tokens = input
      .replaceAll("[.,!?*']", " ")
      .replaceAll(" +", " ")
      .split(" ")
      .map(word => {
        val normalizedWord = spellCheckerSvc.getClosestWordInDictionary(word)
        normalizedWord match
          case normalizedWord if normalizedWord.forall(_.isDigit) => (normalizedWord, Token.NUM)
          case normalizedWord if normalizedWord.startsWith("_") => (normalizedWord, Token.PSEUDO)
          case normalizedWord if products.contains(normalizedWord) => (normalizedWord, Token.PRODUIT)
          case "bonjour" => ("bonjour", Token.BONJOUR)
          case "vouloir" => ("vouloir", Token.VOULOIR)
          case "je" => ("je", Token.JE)
          case "svp" => ("svp", Token.SVP)
          case "assoiffe" => ("assoiffe", Token.ASSOIFFE)
          case "affame" => ("affame", Token.AFFAME)
          case "etre" => ("etre", Token.ETRE)
          case "et" => ("et", Token.ET)
          case "ou" => ("ou", Token.OU)
          case _ => (normalizedWord, Token.UNKNOWN)
      })
    new TokenizedImpl(tokens)
  }
end TokenizerService
