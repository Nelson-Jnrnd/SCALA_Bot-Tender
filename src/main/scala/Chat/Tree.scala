package Chat

/**
  * This sealed trait represents a node of the tree.
  */
sealed trait ExprTree

/**
  * Declarations of the nodes' types.
  */
object ExprTree:
  // TODO - Part 2 Step 3
  // Example cases
  case object Thirsty extends ExprTree
  case object Hungry extends ExprTree
  case object Politeness extends ExprTree
  case object Solde extends ExprTree

  case class Or(left: ExprTree, right: ExprTree) extends ExprTree
  case class And(left: ExprTree, right: ExprTree) extends ExprTree
  case class Product(product: String, brand: String, quantity: Int) extends ExprTree  
  case class Identification(pseudo: String) extends ExprTree
  case class Price(order: ExprTree) extends ExprTree

