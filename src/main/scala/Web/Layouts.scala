package Web

import scalatags.Text.all._
import scalatags.Text.tags2._
import Data.MessageService.{MsgContent, Username}
import scala.compiletime.ops.boolean
/**
 * Assembles the method used to layout ScalaTags
 */
object Layouts:
    // You can use it to store your methods to generate ScalaTags.

    /**
     * Generates the HTML for the main page.
     * @param content The content to be displayed in the main page.
     * @return The HTML for the main page.
     */
    def main(nav: Frag, content: Frag*) = 
        html(
            head(
                script(src := "static/js/main.js"),
                link(rel:="stylesheet", href:= "/static/css/main.css")
            ),
            nav,
            body(
                div(cls := "content")(content)
            )
        )

    /**
      * Generates the HTML for the navbar.
      */
    def navbar(ref: String, text: String) =
        nav(
          a(cls := "nav-brand")("Bot-tender"),
          div(cls := "nav-item")(
            a(href := ref)(text),
          ),
        )

    def loginSuccess(username: String) =
        main(
            navbar("/", "Message board"),
            h1(s"Welcome $username !"),
            p("You are now logged in !")
        )

    def login(errorMsg: String) =
        main(
            navbar("/", "Message board"),
            errorDiv(errorMsg),
            loginForm("Login", "/login"),
            loginForm("Register", "/register")
        )

    def loginForm(title: String, actionPath: String) = 
        Seq(
            h1(title),
            form(id := "msgForm", action := actionPath, method := "post")(
                textboxForm("Username", "Enter your username")
        )   
        )
        
    def textboxForm(field: String, placeholderText: String) =
        Seq(
        label(`for` := "messageInput")(field),
        input(name := "username", id := "messageInput", `type` := "text", placeholder := placeholderText),
        input(`type` := "submit"),
        )
    
    def errorDiv(errorMsg: String) =
        div(id := "errorDiv", cls := "errorMsg")(errorMsg)
    /**
     * Generates the HTML for Message Board.
     * @param messages The messages to be displayed in the message board.
     * @return The HTML for the message board.
     */
    def messageBoard(messages: Seq[(Username, MsgContent)]) =
        div(id := "boardMessage")(
            if messages.isEmpty then
                "Please wait, the message are loading !"
            else
                for (author, msg) <- messages yield
                        div(cls := "msg")(
                            span(cls := "author")(author),
                            span(cls := "msg-content")(
                                //span(cls := "mention")(mention.getOrElse("")),
                                msg
                            )
                        )
        )

    def message(msgContent: String) =
            p(msgContent)

    /**
      * Generates the HTML for the welcome page.
      */
    def index(messages: Seq[(Username, MsgContent)], isLogged: Boolean) =
        main(
            if isLogged then navbar("/logout", "Logout") else navbar("/login", "Login"),
            messageBoard(messages),
            form(id := "msgForm", onsubmit := "submitMessageForm(); return false;")(
                div(id := "errorDiv", cls := "errorMsg"),
                label(`for` := "messageInput")("Your message:"),
                input(`type` := "text", id := "messageInput", placeholder := "Write your message"),
                input(`type` := "submit", value := "Envoyer")
            )
        )
end Layouts
