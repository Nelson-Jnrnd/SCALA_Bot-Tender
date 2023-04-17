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

  /** the root method of the parser: parses an entry phrase */
  // TODO - Part 2 Step 4
  def parsePhrases() : ExprTree = {
    if curToken == BONJOUR then readToken()
    curToken match {
      case QUEL | COMBIEN => return parsePriceAsk()
      case JE => return parseJe()
      case _ => expected(BONJOUR, JE)
    }
  }

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
    val marque = curToken match {
      case MARQUE => eat(MARQUE)
      case _ => ""
    }

    Product(product, marque, num)
  }

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

  private def parsePriceAsk() : ExprTree = {
    if curToken == COMBIEN then
      readToken()
      eat(COUTER)
      Price(parseOrderExpr())
    else if curToken == QUEL then
      readToken()
      eat(ETRE)
      eat(LE)
      eat(PRIX)
      eat(DE)
      Price(parseOrderExpr())
    else expected(COMBIEN, COUTER, QUEL, ETRE, PRIX)
  }

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
            Identification(eat(PSEUDO))
          }
          case _ => expected(ASSOIFFE, AFFAME)
        }
      }
      case ME => {
        eat(ME)
        eat(APPELER)
        Identification(eat(PSEUDO))
      }
      case VOULOIR => parseVouloir()
      case _ => expected(ETRE, ME)
    }
  }

  private def parseVouloir() : ExprTree = {
    if curToken != VOULOIR then expected(VOULOIR)
    eat(VOULOIR)
    curToken match {
      case COMMANDER => {
        eat(COMMANDER)
        Order(parseOrderExpr())
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