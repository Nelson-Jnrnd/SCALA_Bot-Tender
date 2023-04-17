package Chat
import Data.{AccountService, ProductService, Session}


class AnalyzerService(productSvc: ProductService,
                      accountSvc: AccountService):
  import ExprTree._
  /**
    * Compute the price of the current node, then returns it. If the node is not a computational node, the method
    * returns 0.0.
    * For example if we had a "+" node, we would add the values of its two children, then return the result.
    * @return the result of the computation
    */
  // TODO - Part 2 Step 3
  def computePrice(t: ExprTree): Double = {
    t match {
      case Product(product, brand, quantity) => productSvc.getPrice(product, brand) * quantity
      case Price(order) => computePrice(order)
      case Or(left, right) => math.min(computePrice(left), computePrice(right))
      case And(left, right) => computePrice(left) + computePrice(right)
      case _ => 0.0
    }
  }

  /**
    * Return the output text of the current node, in order to write it in console.
    * @return the output text of the current node
    */
  def reply(session: Session)(t: ExprTree): String =
    // you can use this to avoid having to pass the session when doing recursion
    val inner: ExprTree => String = reply(session)
    t match {
      // TODO - Part 2 Step 3
      // Example cases
      case Thirsty => "Eh bien, la chance est de votre côté, car nous offrons les meilleures bières de la région !"
      case Hungry => "Pas de soucis, nous pouvons notamment vous offrir des croissants faits maisons !"
      case Price(order) => s"Le prix total de votre commande est de ${computePrice(order)} euros."
      case Identification(pseudo) => {
        if !accountSvc.isAccountExisting(pseudo) then
          accountSvc.addAccount(pseudo, 30.0)
        session.setCurrentUser(pseudo)
        s"Bonjour $pseudo !"
      }
      case Solde => {
        session.getCurrentUser match {
          case Some(user) => s"Votre solde est de ${accountSvc.getAccountBalance(user)} euros."
          case None => "Ptdr t'es qui ?"
        }
      }
      case Product(product, brand, quantity) => {
        session.getCurrentUser match {
          case Some(user) => {
            val price = computePrice(t)
            try {
              accountSvc.purchase(user, price)
              s"Vous avez acheté $quantity $product de la marque $brand pour un total de $price euros."
            } catch {
              case e: Data.NotEnoughMoneyException => s"Vous n'avez pas assez d'argent pour acheter $quantity $product de la marque $brand."
              case e: Data.CouldNotFindAccountException => s"Faut ouvrir un compte mon coco."
            }
          }
          case None => "Ptdr t'es qui ?"
        }
      }
      case Or(left, right) => s"${inner(left)} ${inner(right)}"
      case And(left, right) => s"${inner(left)} ${inner(right)}"
      case _ => "Je ne comprends pas votre demande."
    }
      
end AnalyzerService
