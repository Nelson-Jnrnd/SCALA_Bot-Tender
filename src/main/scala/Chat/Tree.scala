package Chat

/**
  * This sealed trait represents a node of the tree.
  */
sealed trait ExprTree

/**
  * Declarations of the nodes' types.
  * In our implementation, we have 4 types of nodes:
  * The first 3 are command nodes with respectively no parameters, "single" parameters and possibly recursive parameters.
  * - Leaf nodes: Thirsty, Hungry, Solde.
  * - Leaf nodes with parameters: Product, Identification 
  * - Inner nodes: Price, Order
  * The last one is an inner node with 2 possibly recursive parameters for the Or and And operators.
  */
object ExprTree:
  case object Thirsty extends ExprTree
  case object Hungry extends ExprTree
  case object Solde extends ExprTree

  case class Product(product: String, brand: Option[String], quantity: Int) extends ExprTree  
  case class Identification(pseudo: String) extends ExprTree

  case class Price(order: ExprTree) extends ExprTree
  case class Order(order: ExprTree) extends ExprTree


  case class Or(left: ExprTree, right: ExprTree) extends ExprTree
  case class And(left: ExprTree, right: ExprTree) extends ExprTree
  
  
