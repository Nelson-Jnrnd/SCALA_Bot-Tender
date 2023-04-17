package Chat

class UnexpectedTokenException(msg: String) extends Exception(msg){}

class Parser(tokenized: Tokenized):
  import ExprTree._
  import Chat.Token._

  // Start the process by reading the first token.
  var curTuple: (String, Token) = tokenized.nextToken()

  def curValue: String = curTuple._1
  def curToken: Token = curTuple._2

  /** Reads the next token and assigns it into the global variable curTuple */
  def readToken(): Unit = curTuple = tokenized.nextToken()

  /** "Eats" the expected token and returns it value, or terminates with an error. */
  private def eat(token: Token): String =
    if token == curToken then
      val tmp = curValue
      readToken()
      tmp
    else expected(token)

  /** Complains that what was found was not expected. The method accepts arbitrarily many arguments of type Token */
  private def expected(token: Token, more: Token*): Nothing =
    expected(more.prepended(token))
  private def expected(tokens: Seq[Token]): Nothing =
    val expectedTokens = tokens.mkString(" or ")
    throw new UnexpectedTokenException(s"Expected: $expectedTokens, found: $curToken")

  /** the root method of the parser: parses an entry phrase
   * @return the parsed expression tree
   * We divided the process for each branch of the grammar.
   */
  def parsePhrases() : ExprTree = {
    if curToken == BONJOUR then readToken() // BONJOUR is optional :'(
    curToken match {
      // Quel est le prix and Combien ça coûte are the same request so we group them together
      case QUEL | COMBIEN => return parsePriceAsk() 
      case JE => return parseJe()
      case _ => expected(BONJOUR, JE)
    }
  }

  /**
    * Parses the two branches of the grammar that wants to know the price of an order
    * then calls parseOrderExpr() to parse the order
    */
    private def parsePriceAsk() : ExprTree = {
    if curToken == COMBIEN then
      readToken()
      eat(COUTER)
    else if curToken == QUEL then
      readToken()
      eat(ETRE)
      eat(LE)
      eat(PRIX)
      eat(DE)
    else expected(COMBIEN, COUTER, QUEL, ETRE, PRIX)
    Price(parseOrderExpr())
  }

  /**
    * Parses the branches of the grammar that starts with "Je"
    * The sub-branches could reasonably be separated into different methods 
    * but we decided to keep them together as there are not that many of them and they are not that long
    * In the case of the command branch, we call parseOrderExpr() to parse the order
    */
    private def parseJe() : ExprTree = {
    if curToken != JE then expected(JE)
    eat(JE)
    curToken match {
      case ETRE => {
        eat(ETRE)
        curToken match {
          case ASSOIFFE => {
            readToken()
            Thirsty
          }
          case AFFAME => {
            readToken()
            Hungry
          }
          case PSEUDO => {
            Identification(eat(PSEUDO)) // capture the pseudo entered by the user
          }
          case _ => expected(ASSOIFFE, AFFAME, PSEUDO)
        }
      }
      case ME => {
        eat(ME)
        eat(APPELER)
        Identification(eat(PSEUDO))
      }
      case VOULOIR => {
        eat(VOULOIR)
        curToken match {
          case COMMANDER => {
            eat(COMMANDER)
            Order(parseOrderExpr()) // Parse the order
          }
          case CONNAITRE => {
            eat(CONNAITRE)
            eat(MON)
            eat(SOLDE)
            Solde
          }
          case _ => expected(COMMANDER, CONNAITRE)
        }
      }
      case _ => expected(ETRE, ME)
    }
  }

  /**
    * Parse an order, to fix the issue of left associativity 
    * it first parse the first product before checking if there is an and/or
    * if there is we parse the product following it
    * then we join the subtrees with the corresponding and/or node
    * 
    * We repeat this process as long as we need to
    */
  private def parseOrderExpr(): ExprTree = {
    var leftExpr = parseProductExpr()

    while (curToken == ET || curToken == OU) {
      if curToken == ET then {
        eat(ET)
        val rightExpr = parseProductExpr()
        leftExpr = And(leftExpr, rightExpr)
      } else if curToken == OU then {
        eat(OU)
        val rightExpr = parseProductExpr()
        leftExpr = Or(leftExpr, rightExpr)
      }
    }

    leftExpr
  }

  /**
    * Parse a product, if there is a singular determinant the number is 1
    * if there is no marque we set it to None
    */
  private def parseProductExpr(): ExprTree = {
    val num = curToken match {
      case NUM => eat(NUM).toInt
      case LE => {
        eat(LE)
        1
      }
      case _ => expected(NUM, LE)
    }

    val product = eat(PRODUIT)
  
    val optionMarque = curToken match {
      case MARQUE => Some(eat(MARQUE))
      case _ => None
    }
  
    Product(product, optionMarque, num)
  }