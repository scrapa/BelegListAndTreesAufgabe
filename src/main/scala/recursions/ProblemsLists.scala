package recursions

import list.traits.IntList
import list.implementation._

object ProblemsLists :

  /**
    * Creates a list containing the given number repeated the requested number of times.
    *
    * @param i the value to repeat
    * @param times the number of repetitions; non-positive values produce an empty list
    * @return the list of repeated values
    */
  def duplicateNum(i: Int, times: Int): IntList =
    if (times <= 0) Empty
    else Cons(i, duplicateNum(i, times - 1))

  /**
    * Duplicates each input element for which the predicate is true.
    *
    * @param predicate determines which values to duplicate
    * @param l the input list
    * @return a list with matching values appearing twice
    */
  def duplicateNumbersFulfillingPredicate(predicate: Int => Boolean, l: IntList): IntList = l match
    case Empty => Empty
    case Cons(h, t) =>
      if predicate(h) then Cons(h, Cons(h, duplicateNumbersFulfillingPredicate(predicate, t)))
      else Cons(h, duplicateNumbersFulfillingPredicate(predicate, t))

  /**
    * Returns all subsets of the input list, including the empty list.
    *
    * @param l the input list
    * @return the list of all subsets, each represented as an integer list
    */
  def combinations(l: IntList): List[IntList] = l match
    case Empty => List(Empty)
    case Cons(h, t) =>
      val withoutHead = combinations(t)
      val withHead = prependAll(h, withoutHead)
      withoutHead ++ withHead

  private def prependAll(elem: Int, lists: List[IntList]): List[IntList] = lists match
    case Nil => Nil
    case headList :: tailLists => Cons(elem, headList) :: prependAll(elem, tailLists)