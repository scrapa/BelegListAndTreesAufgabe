package list.implementation

import list.traits.IntList

object SinglyLinkedIntList:

  /**
    * Creates a singly linked list containing the given values in order.
    *
    * @param xs the values to put in the list
    * @return the resulting list
    */
  def apply(xs: Int*): SinglyLinkedIntList = xs match
    case Seq() => Empty
    case _ => Cons(xs.head, SinglyLinkedIntList(xs.tail*))

abstract class SinglyLinkedIntList extends IntList:

  override def prefix(other: IntList): IntList = other match
    case Empty => this
    case Cons(h, t) => Cons(h, prefix(t))

  override def size: Int = this match
    case Empty => 0
    case Cons(_, t) => 1 + t.size

  override def map(mapFunc: Int => Int): IntList = this match
    case Empty => Empty
    case Cons(h, t) => Cons(mapFunc(h), t.map(mapFunc))

  override def filter(filterFunc: Int => Boolean): IntList = this match
    case Empty => Empty
    case Cons(h, t) =>
      if filterFunc(h) then Cons(h, t.filter(filterFunc))
      else t.filter(filterFunc)

  override def foldRight(initial: Int)(reduceFunc: (Int, Int) => Int): Int = this match
    case Empty => initial
    case Cons(h, t) => reduceFunc(h, t.foldRight(initial)(reduceFunc))

  override def reduceRight(reduceFunc: (Int, Int) => Int): Int = this match
    case Empty => throw new IllegalArgumentException("reduceRight on empty list")
    case Cons(h, Empty) => h
    case Cons(h, t) => reduceFunc(h, t.reduceRight(reduceFunc))

  override def forAll(predicateFunc: Int => Boolean): Boolean = this match
    case Empty => true
    case Cons(h, t) => predicateFunc(h) && t.forAll(predicateFunc)

  override def foldLeft(initial: Int)(reduceFunc: (Int, Int) => Int): Int = this match
    case Empty => initial
    case Cons(h, t) => t.foldLeft(reduceFunc(initial, h))(reduceFunc)

  override def reduceLeft(reduceFunc: (Int, Int) => Int): Int = this match
    case Empty => throw new IllegalArgumentException("reduceLeft on empty list")
    case Cons(h, Empty) => h
    case Cons(h1, Cons(h2, t)) => Cons(reduceFunc(h1, h2), t).reduceLeft(reduceFunc)

  override def insertSorted(elem: Int): IntList = this match
    case Empty => Cons(elem, Empty)
    case Cons(h, t) =>
      if elem <= h then Cons(elem, this)
      else Cons(h, t.insertSorted(elem))

  override def insertionSort: IntList = this match
    case Empty => Empty
    case Cons(h, t) => t.insertionSort.insertSorted(h)

  override def flip: IntList =
    def loop(remaining: IntList, acc: IntList): IntList = remaining match
      case Empty => acc
      case Cons(h, t) => loop(t, Cons(h, acc))

    loop(this, Empty)
