package Chat

import Chat.Token.*
import Utils.SpellCheckerService

trait Tokenized:
  /**
    * Get the next token of the user input, or EOL if there is no more token.
    * @return a tuple that contains the string value of the current token, and the identifier of the token
    */
  def nextToken(): (String, Token)

class TokenizedImpl(var tokens: Array[(String, Token)]) extends Tokenized:
  def nextToken(): (String, Token) = {
    if (tokens.isEmpty) {
      ("EOL", Token.EOL)
    } else {
      val token = tokens.head
      tokens = tokens.tail
      token
    }
  }
end TokenizedImpl
