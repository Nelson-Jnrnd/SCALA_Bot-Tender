package Web

import Chat.{AnalyzerService, Parser, TokenizerService}
import Data.{MessageService, AccountService, SessionService, Session}
import cask.endpoints.websocket
import castor.Context.Simple.global

/**
  * Assembles the routes dealing with the message board:
  * - One route to display the home page
  * - One route to send the new messages as JSON
  * - One route to subscribe with websocket to new messages
  *
  * @param log
  */
class MessagesRoutes(tokenizerSvc: TokenizerService,
                     analyzerSvc: AnalyzerService,
                     msgSvc: MessageService,
                     accountSvc: AccountService,
                     sessionSvc: SessionService)(implicit val log: cask.Logger) extends cask.Routes:
    import Decorators.getSession

    val subscribers = new scala.collection.mutable.HashSet[cask.WsChannelActor]()

    def updateMessages() =
        val last20 = msgSvc.getLatestMessages(20)
        subscribers.foreach(_.send(cask.Ws.Text(
          Layouts.messageBoard(last20).toString
        )))

    @getSession(sessionSvc) // This decorator fills the `(session: Session)` part of the `index` method.
    @cask.get("/")
    def index()(session: Session) =
        // TODO - Part 3 Step 2: Display the home page (with the message board and the form to send new messages)
        Layouts.index(msgSvc.getLatestMessages(20), session.getCurrentUser.isDefined)
        //session.getCurrentUser.map(u => s"You are logged in as ${u} !")
        //       .getOrElse("You are not logged in !")
        
    // TODO - Part 3 Step 4b: Process the new messages sent as JSON object to `/send`. The JSON looks
    //      like this: `{ "msg" : "The content of the message" }`.
    //
    //      A JSON object is returned. If an error occurred, it looks like this:
    //      `{ "success" : false, "err" : "An error message that will be displayed" }`.
    //      Otherwise (no error), it looks like this:
    //      `{ "success" : true, "err" : "" }`
    //
    //      The following are treated as error:
    //      - No user is logged in
    //      - The message is empty
    //
    //      If no error occurred, every other user is notified with the last 20 messages
    //
    @getSession(sessionSvc)
    @cask.postJson("/send")
    def send(msg: ujson.Value)(session: Session) =
        if (session.getCurrentUser.isEmpty) {
            ujson.Obj("success" -> false, "err" -> "You are not logged in !")
        } else if (msg.str.isEmpty) {
            ujson.Obj("success" -> false, "err" -> "The message is empty !")
        } else if (msg.str.startsWith("@bot ")) {
          val msgContent = msg.str.substring(5)
            try {
                val tokenized = tokenizerSvc.tokenize(msgContent)
                val parser = new Parser(tokenized)
                val expr = parser.parsePhrases()
                val printResult = analyzerSvc.reply(session)(expr)
                val msgID = msgSvc.add(
                            sender = session.getCurrentUser.get,
                            msg = Layouts.message(msgContent),
                            mention = Option("bot")
                )
                val botMsgID = msgSvc.add(
                            sender = "bot",
                            msg = Layouts.message(printResult),
                            mention = Option(session.getCurrentUser.get),
                            replyToId = Option(msgID)
                )
                updateMessages()
                ujson.Obj("success" -> true, "err" -> "")
            } catch {
                case e: Exception => ujson.Obj("success" -> false, "err" -> e.getMessage)
            }
        } else {
            val msgID = msgSvc.add(
                            sender = session.getCurrentUser.get,
                            msg = Layouts.message(msg.str),
                            mention = None
            )
            updateMessages()
            ujson.Obj("success" -> true, "err" -> "")
        }
    // TODO - Part 3 Step 4c: Process and store the new websocket connection made to `/subscribe`
    //
    @cask.websocket("/subscribe")
    def subscribe() = cask.WsHandler { connection =>

        subscribers.add(connection)
        cask.WsActor {
          case cask.Ws.Close(_, _) => subscribers.remove(connection)
        }
    }
    // TODO - Part 3 Step 4d: Delete the message history when a GET is made to `/clearHistory`
    //
    @getSession(sessionSvc)
    @cask.get("/clearHistory")
    def clearHistory()(session: Session) = {
        msgSvc.deleteHistory()
        updateMessages()
    }
    // TODO - Part 3 Step 5: Modify the code of step 4b to process the messages sent to the bot (message
    //      starts with `@bot `). This message and its reply from the bot will be added to the message
    //      store together.
    //
    //      The exceptions raised by the `Parser` will be treated as an error (same as in step 4b)

    initialize()
end MessagesRoutes
