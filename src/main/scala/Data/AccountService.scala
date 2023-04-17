package Data

import scala.collection.mutable

trait AccountService:
  /**
    * Retrieve the balance of a given account
    * @param user the name of the user whose account will be retrieve
    * @return the current balance of the user
    */
  def getAccountBalance(user: String): Double

  /**
    * Add an account to the existing accounts
    * @param user the name of the user
    * @param balance the initial balance value
    */
  def addAccount(user: String, balance: Double): Unit

  /**
    * Indicate is an account exist
    * @param user the name of the user whose account is checked to exist
    * @return whether the account exists or not
    */
  def isAccountExisting(user: String): Boolean

  /**
    * Update an account by decreasing its balance.
    * @param user the name of the user whose account will be updated
    * @param amount the amount to decrease
    * @return the new balance
    */
  def purchase(user: String, amount: Double): Double

class CouldNotFindAccountException(account: String) extends Exception(s"Could not find account $account")
class NotEnoughMoneyException(account: String, amount: Double) extends Exception(s"Not enough money in account $account to purchase $amount")
class AccountAlreadyExistsException(account: String) extends Exception(s"Account $account already exists")
class AccountImpl extends AccountService:

  val accounts: mutable.Map[String, Double] = mutable.Map.empty
  def getAccountBalance(user: String): Double = {
    accounts.getOrElse(user, throw new CouldNotFindAccountException(user))
  }
  def addAccount(user: String, balance: Double): Unit = {
    if (isAccountExisting(user)) throw new AccountAlreadyExistsException(user)
    accounts.put(user, balance)
  }
  def isAccountExisting(user: String): Boolean = {
    accounts.contains(user)
  }
  def purchase(user: String, amount: Double): Double = {
    if (!isAccountExisting(user)) throw new CouldNotFindAccountException(user)
    val balance = getAccountBalance(user)
    if (balance < amount) throw new NotEnoughMoneyException(user, amount)
    accounts.put(user, balance - amount)
    balance - amount
  }
end AccountImpl
