package tree.implementation

import tree.traits.IntTree

object BinaryTree:

  /**
    * Builds a binary search tree by inserting the values from left to right.
    * Duplicate values are ignored.
    *
    * @param xs the values to insert
    * @return the resulting binary search tree
    */
  def apply(xs: Int*): BinaryTree =
    xs.foldLeft[BinaryTree](Empty)((tree, elem) => tree.insert(elem))

abstract class BinaryTree extends IntTree:

  override def insert(elem: Int): BinaryTree

  /**
    * Finds the smallest element in this tree's right subtree.
    *
    * @return the successor node of this tree's root
    * @throws Error if this tree is empty or has no right subtree
    */
  def findSuccessor: BinaryTree =
    def findMin(node: IntTree): BinaryTree = node match
      case Empty => throw new Error("Empty subtree has no minimum")
      case n @ NonEmpty(_, Empty, _) => n
      case NonEmpty(_, left, _) => findMin(left)

    this match
      case Empty => throw new Error("Empty tree has no successor")
      case NonEmpty(_, _, Empty) =>
        throw new Error("No right subtree, so no successor exists")
      case NonEmpty(_, _, right) => findMin(right)

  override def delete(i: Int): IntTree = this match
    case Empty => throw new Error(s"Element $i not found in tree")
    case NonEmpty(elem, left, right) =>
      if (i < elem) NonEmpty(elem, left.delete(i), right)
      else if (i > elem) NonEmpty(elem, left, right.delete(i))
      else
        (left, right) match
          case (Empty, r) => r
          case (l, Empty) => l
          case (l, r) =>
            val successor = findSuccessor
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
        val withRoot = if filterFun(elem) then acc.insert(elem) else acc
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
