package recursions

import list.traits.IntList
import list.implementation._

object ProblemsLists :

  def duplicateNum(i: Int, times: Int): IntList =
    if (times <= 0) Empty
    else Cons(i, duplicateNum(i, times - 1))

  def duplicateNumbersFulfillingPredicate(predicate: Int => Boolean, l: IntList): IntList = l match
  case Empty => Empty
  case Cons(h, t) =>
  if (predicate(h)) Cons(h, Cons(h, duplicateNumbersFulfillingPredicate(predicate, t)))
  else Cons(h, duplicateNumbersFulfillingPredicate(predicate, t))

  def combinations(l: IntList): List[IntList] = l match
  case Empty => List(Empty)
  case Cons(h, t) =>
  val withoutHead = combinations(t)
  val withHead = prependAll(h, withoutHead)
  withoutHead ++ withHead

  private def prependAll(elem: Int, lists: List[IntList]): List[IntList] = lists match
  case Nil => Nil
  case headList :: tailLists => Cons(elem, headList) :: prependAll(elem, tailLists)