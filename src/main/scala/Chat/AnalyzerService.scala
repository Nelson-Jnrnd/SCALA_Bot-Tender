package Chat
import Data.{AccountService, ProductService, Session}
import scala.concurrent.Future
import Utils.FutureOps
import concurrent.duration.{DurationInt, DurationDouble}
import concurrent.ExecutionContext.Implicits.global
import scala.util.*


class AnalyzerService(productSvc: ProductService,
                      accountSvc: AccountService):
  import ExprTree._

  val startingSolde = 30.0
  var onReply: String => Unit = println(_)
  /**
    * Compute the price of the current node, then returns it. If the node is not a computational node, the method
    * returns 0.0.
    * For example if we had a "+" node, we would add the values of its two children, then return the result.
    * @return the result of the computation
    */
  def computePrice(t: ExprTree): Double = {
    t match {
      case Product(product, brand, quantity) => {
        brand match {
          case None => productSvc.getPrice(product, productSvc.getDefaultBrand(product)) * quantity
          case Some(brand) => productSvc.getPrice(product, brand) * quantity
        }
      }
      case Price(order) => computePrice(order)
      case Or(left, right) => math.min(computePrice(left), computePrice(right)) // We take the cheapest product
      case And(left, right) => computePrice(left) + computePrice(right)
      case _ => 0.0
      // In our implementation, Order is not a computational node it either has a Product child or an Or/And child
    }
  }

  def prepareProduct(product: String, brand: Option[String], quantity: Int): Future[Double] = {
    if (quantity == 0) {
      Future.successful(0.0)
    } else {
      FutureOps.randomSchedule(1.second, 0.5.second, 0.8).transformWith {
        case Success(_) => {
          println(s"Product ${product} prepared at ${java.time.LocalTime.now()}")
          val price = computePrice(Product(product, brand, 1))
          prepareProduct(product, brand, quantity - 1).map(_ + price)
        }
        case Failure(_) => {
          println(s"Product ${product} failed to prepare at ${java.time.LocalTime.now()}")
          prepareProduct(product, brand, quantity)
        }
      }
    }
  }

  def computeFuturePrice(t: ExprTree): List[Future[Double]] = {
    t match {
      case Product(product, brand, quantity) => List(prepareProduct(product, brand, quantity))
      case Price(order) => computeFuturePrice(order)
      case Or(left, right) => // We take the cheapest product
        if (computePrice(left) < computePrice(right)) {
          computeFuturePrice(left)
        } else {
          computeFuturePrice(right)
        }
      case And(left, right) => computeFuturePrice(left) ++ computeFuturePrice(right)
      case _ => List(Future.successful(0.0))
      // In our implementation, Order is not a computational node it either has a Product child or an Or/And child
    }
  }

  /**
    * Set the callback function that will be called when the bot has to reply to a message.
    * @param onReply the callback function
    */
  def setCallback(onReply: String => Unit) = this.onReply = onReply

  /**
    * Return the output text of the current node, in order to write it in console.
    * @return the output text of the current node
    */
  def reply(session: Session)(t: ExprTree): String =
    // you can use this to avoid having to pass the session when doing recursion
    val inner: ExprTree => String = reply(session)
    t match {
      // Example cases
      case Thirsty => "Eh bien, la chance est de votre côté, car nous offrons les meilleures bières de la région !"
      case Hungry => "Pas de soucis, nous pouvons notamment vous offrir des croissants faits maisons !"
      case Price(order) => s"Le prix total de votre commande est de ${computePrice(order)} CHF."
      case Identification(pseudo) => {
        session.getCurrentUser match
          case Some(user) => {
            if (user == pseudo.drop(1)) {
              s"Bien ou quoi, ${user} ?"
            } else {
              s"Oui, oui et moi je suis le pape."
            }
          }
          case None => s"C'est bien beau mais faudrait peut-être vous identifier avant, non ?"
      }
      case Solde => {
        session.getCurrentUser match {
          case Some(user) => s"Le montant actuel votre solde est de CHF ${accountSvc.getAccountBalance(user)}."
          case None => "Ptdr t'es qui ?"
        }
      }
      case Product(product, brand, quantity) =>{
        brand match {
          case None => s"$quantity $product ${productSvc.getDefaultBrand(product)}"
          case Some(brand) => s"$quantity $product $brand"
        }
      }
      case Order(order) => { // order is either a Product or an Or/And node
        session.getCurrentUser match {
          case Some(user) => {
            val price = computeFuturePrice(order)
            Future.foldLeft(price)(0.0)(_ + _).map(total => {
              onReply(
                try {
                  val completed = total == computePrice(order)
                  accountSvc.purchase(user, total)
                  s"La commande ${inner(order)} est ${if (!completed) then "partiellement" else ""} prête, vous pouvez venir la chercher au comptoir pour un total de ${total}! Votre nouveau solde est de ${accountSvc.getAccountBalance(user)} CHF."
                } catch {
                  case e: Data.NotEnoughMoneyException => s"Vous n'avez pas assez d'argent pour acheter ${inner(order)}."
                  case e: Data.CouldNotFindAccountException => s"Faut ouvrir un compte mon coco."
                })
              })
            
            s"Votre commande est en cours de préparation: ${inner(order)}."            
          }
          case None => "Ptdr t'es qui ?"
        }
      }
      case Or(left, right) => s"${inner(left)} ou ${inner(right)}"
      case And(left, right) => s"${inner(left)} et ${inner(right)}"
    }

end AnalyzerService
