package Web

import scalatags.Text.all._
import scalatags.Text.tags2._
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
    def main(content: Frag*) = 
        html(
            head(
                script(src := "static/js/main.js"),
                link(rel:="stylesheet", href:= "/static/css/main.css")
            ),
            navbar(),
            body(
                div(cls := "content")(content)
            )
        )
    
    /**
      * Generates the HTML for the navbar.
      */
    def navbar() =
        nav(
          a(cls := "nav-brand")("Bot-tender"),
          div(cls := "nav-item")(
            a(href := "/")("Log in"),
          ),
        )
    
    /**
     * Generates the HTML for Message Board.
     * @param messages The messages to be displayed in the message board.
     * @return The HTML for the message board.
     */
    def messageBoard(messages: List[(String, Option[String], String)]) =
        div(id := "boardMessage")(
            if messages.isEmpty then
                "Please wait, the message are loading !"
            else
                for (author, mention, msg) <- messages yield
                        div(cls := "msg")(
                            span(cls := "author")(author),
                            span(cls := "msg-content")(
                                span(cls := "mention")(mention.getOrElse("")),
                                msg
                            )
                        )
        )

    /**
      * Generates the HTML for the welcome page.
      */
    def index() =
        main(
            messageBoard(List()),
            form(id := "msgForm", onsubmit := "submitMessageForm(); return false;")(
                div(id := "errorDiv", cls := "errorMsg"),
                label(`for` := "messageInput")("Your message:"),
                input(`type` := "text", id := "messageInput", placeholder := "Write your message"),
                input(`type` := "submit", value := "Envoyer")
            )
        )
end Layouts
