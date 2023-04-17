package Chat
import Data.{AccountService, ProductService, Session}


class AnalyzerService(productSvc: ProductService,
                      accountSvc: AccountService):
  import ExprTree._

  val startingSolde = 30.0
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
        if !accountSvc.isAccountExisting(pseudo) then // If the account doesn't exist, we create it
          accountSvc.addAccount(pseudo, startingSolde)
        session.setCurrentUser(pseudo)
        val trimmed = pseudo.drop(1)
        s"Bonjour $trimmed !"
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
            val price = computePrice(order)
            try {
              accountSvc.purchase(user, price)
              s"Vous avez acheté ${inner(order)} pour un total de $price CHF. Votre solde est maintenant de CHF ${accountSvc.getAccountBalance(user)}."
            } catch {
              case e: Data.NotEnoughMoneyException => s"Vous n'avez pas assez d'argent pour acheter ${inner(order)}."
              case e: Data.CouldNotFindAccountException => s"Faut ouvrir un compte mon coco."
            }
          }
          case None => "Ptdr t'es qui ?"
        }
      }
      case Or(left, right) => s"${inner(left)} ou ${inner(right)}"
      case And(left, right) => s"${inner(left)} et ${inner(right)}"
    }
      
end AnalyzerService
