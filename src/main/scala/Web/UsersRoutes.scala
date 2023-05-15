package Web

import Data.{AccountService, SessionService, Session}
import Web.Decorators.getSession

/**
  * Assembles the routes dealing with the users:
  * - One route to display the login form and register form page
  * - One route to process the login form and display the login success page
  * - One route to process the register form and display the register success page
  * - One route to logout and display the logout success page
  * 
  * The username of the current session user is stored inside a cookie called `username`.
  */
class UsersRoutes(accountSvc: AccountService,
                  sessionSvc: SessionService)(implicit val log: cask.Logger) extends cask.Routes:
    // TODO - Part 3 Step 3a: Display a login form and register form page for the following URL: `/login`.
    @getSession(sessionSvc)
    @cask.get("/login")
    def login()(session: Session) = Layouts.login("")
    // TODO - Part 3 Step 3b: Process the login information sent by the form with POST to `/login`,
    //      set the user in the provided session (if the user exists) and display a successful or
    //      failed login page.
    //
    @getSession(sessionSvc)
    @cask.postForm("/login")
    def login(username: cask.FormValue)(session: Session) =
        if (accountSvc.isAccountExisting(username.value)) {
            session.setCurrentUser(username.value)
            Layouts.loginSuccess(username.value)
        } else {
            Layouts.login("The specified user does not exists")
        }
    // TODO - Part 3 Step 3c: Process the register information sent by the form with POST to `/register`,
    //      create the user, set the user in the provided session and display a successful
    //      register page.
    //
    @getSession(sessionSvc)
    @cask.postForm("/register")
    def register(username: cask.FormValue)(session: Session) =
        if (accountSvc.isAccountExisting(username.value)) {
            Layouts.login("The specified user already exists")
        } else {
            accountSvc.addAccount(username.value, 0)
            session.setCurrentUser(username.value)
            Layouts.loginSuccess(username.value)
        }
    // TODO - Part 3 Step 3d: Reset the current session and display a successful logout page.

    @getSession(sessionSvc)
    @cask.get("/logout")
    def logout()(session: Session) =
        session.reset()
        Layouts.login("You have been logged out")

    initialize()
end UsersRoutes
