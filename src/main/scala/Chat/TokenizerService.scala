package Chat

import Chat.Token.*
import Utils.SpellCheckerService

/**
 * Service that tokenize the user input.
 * @param spellCheckerSvc The service used to correct the user input
 * @param products The list of products that the user can order
 */
class TokenizerService(
  spellCheckerSvc: SpellCheckerService, 
  products: List[String],
  marques: List[String]
  ):
  /**
    * Separate the user's input into tokens
    * @param input The user's input
    * @return A Tokenizer which allows iteration over the tokens of the input
    */
  def tokenize(input: String): Tokenized = {
    val tokens = input
      .replaceAll("[.,!?*']", " ") // remove punctuation
      .replaceAll(" +", " ") // remove multiple spaces
      .split(" ")
      .map(word => {
        val normalizedWord = spellCheckerSvc.getClosestWordInDictionary(word)
        normalizedWord match
          case normalizedWord if normalizedWord.forall(_.isDigit) => (normalizedWord, Token.NUM)
          case normalizedWord if normalizedWord.startsWith("_") => (normalizedWord, Token.PSEUDO)
          case normalizedWord if products.contains(normalizedWord) => (normalizedWord, Token.PRODUIT)
          case normalizedWord if marques.contains(normalizedWord) => (normalizedWord, Token.MARQUE)
          case "bonjour" => ("bonjour", Token.BONJOUR)
          case "vouloir" => ("vouloir", Token.VOULOIR)
          case "je" => ("je", Token.JE)
          case "assoiffe" => ("assoiffe", Token.ASSOIFFE)
          case "affame" => ("affame", Token.AFFAME)
          case "etre" => ("etre", Token.ETRE)
          case "et" => ("et", Token.ET)
          case "ou" => ("ou", Token.OU)
          case "svp" => ("svp", Token.SVP)

          // Labo 2
          case "me" => ("me", Token.ME)
          case "appeler" => ("appeler", Token.APPELER)
          case "commander" => ("commander", Token.COMMANDER)
          case "connaitre" => ("connaitre", Token.CONNAITRE)
          case "mon" => ("mon", Token.MON)
          case "solde" => ("solde", Token.SOLDE)
          case "combien" => ("combien", Token.COMBIEN)
          case "couter" => ("couter", Token.COUTER)
          case "quel" => ("quel", Token.QUEL)
          case "le" => ("le", Token.LE)
          case "prix" => ("prix", Token.PRIX)
          case "de" => ("de", Token.DE)
          case _ => (normalizedWord, Token.UNKNOWN)
      })
    new TokenizedImpl(tokens)
  }
end TokenizerService
