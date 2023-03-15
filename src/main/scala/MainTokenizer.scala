import scala.io.StdIn
import Utils.*
import Chat.{TokenizerService, Token}

object MainTokenizer:
  /**
    * Convert the user input to lower case, then take an action depending on the value.
    *
    * @param tokenizerSvc The service used to tokenize the input
    * @param input The user input
    * @return whether the input loop should continue or not
    */
  def evaluateInput(tokenizerSvc: TokenizerService)(input: String): Boolean =
    input.toLowerCase match
      case "quitter" => 
        println("Adieu.")
        false  // close loop
      case "sante !" =>
        for i <- 2 to 6 do
          println(s"Nombre de *clinks* pour un santé de $i personnes : ${ClinksCalculator.calculateCombination(i, 2)}.")
        true  // continue loop
      case s =>
        // Tokenize the user input.
        val tokenizer = tokenizerSvc.tokenize(s)

        // Display every token.
        while
          val currentToken: (String, Token) = tokenizer.nextToken()
          println(currentToken)
          // Loop condition
          currentToken._2 != Token.EOL
        do ()

        println("============================================")
        true  // continue loop
  end evaluateInput

  def main(args: Array[String]): Unit =
    val spellCheckerSvc: SpellCheckerService = SpellCheckerImpl(Dictionary.dictionary)
    // We decided to set the list of products in the main function, 
    // because it is not the responsibility of the TokenizerService to know the list of products.
    // The TokenizerService should only know how to tokenize the user input.
    val tokenizerSvc: TokenizerService = TokenizerService(spellCheckerSvc, List("biere", "croissant"))

    println("Bienvenue au Chill-Out !")

    while 
      print("> ")
      evaluateInput(tokenizerSvc)(StdIn.readLine)
    do ()
  end main
end MainTokenizer
