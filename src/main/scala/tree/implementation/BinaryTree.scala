package tree.implementation

import tree.traits.IntTree

object BinaryTree :

  def apply(xs: Int*): BinaryTree =

  def treeIntialization(tree:BinaryTree, xs: Int*):BinaryTree= xs match

  case Seq() => tree
  case _ => treeIntialization(tree.insert(xs.head).asInstanceOf[BinaryTree],xs.tail*)

  treeIntialization(Empty,xs*)

  abstract class BinaryTree extends IntTree :

  /* Helper function used by the delete-operation
  *  Function finds the node with the next higher value
  *  compared to the root node of the binary tree (see tests)
  * */
  def findSuccessor: BinaryTree =
  def findMin(node: IntTree): BinaryTree = node match
  case Empty => throw new Error("Empty subtree has no minimum")
  case n @ NonEmpty(_, Empty, _) => n
  case NonEmpty(_, left, _) => findMin(left)

  this match
  case Empty => throw new Error("Empty tree has no successor")
  case NonEmpty(_, _, Empty) => throw new Error("No right subtree, so no successor exists")
  case NonEmpty(_, _, right) => findMin(right)

  override def delete(i: Int): IntTree = this match
  case Empty => throw new Error(s"Element $i not found in tree")
  case NonEmpty(elem, left, right) =>
  if (i < elem) NonEmpty(elem, left.delete(i), right)
  else if (i > elem) NonEmpty(elem, left, right.delete(i))
  else (left, right) match
  case (Empty, r) => r
  case (l, Empty) => l
  case (l, r) =>
  val successor = this.findSuccessor
  NonEmpty(successor.root, l, r.delete(successor.root))

  override def map(mapFun: Int => Int): IntTree =
  def helper(tree: IntTree, acc: IntTree): IntTree = tree match
  case Empty => acc
  case NonEmpty(elem, left, right) =>
  val withRoot = acc.insert(mapFun(elem))
  val withLeft = helper(left, withRoot)
  helper(right, withLeft)
  helper(this, Empty)

  override def filter(filterFun: Int => Boolean): IntTree =
  def helper(tree: IntTree, acc: IntTree): IntTree = tree match
  case Empty => acc
  case NonEmpty(elem, left, right) =>
  val withRoot = if (filterFun(elem)) acc.insert(elem) else acc
  val withLeft = helper(left, withRoot)
  helper(right, withLeft)
  helper(this, Empty)

  override def tree2List: List[Int] = this match
  case Empty => Nil
  case NonEmpty(elem, left, right) => left.tree2List ++ (elem :: right.tree2List)

  override def isBinaryTree: Boolean =
  def check(tree: IntTree, min: Option[Int], max: Option[Int]): Boolean = tree match
  case Empty => true
  case NonEmpty(elem, left, right) =>
  val aboveMin = min.forall(m => elem > m)
  val belowMax = max.forall(m => elem < m)
  aboveMin && belowMax && check(left, min, Some(elem)) && check(right, Some(elem), max)
  check(this, None, None)