package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_)             => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def max(t: Tree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(left, right) => max(left) max max(right)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(x)             => 1
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }

  def map[A, B](t: Tree[A])(f: (A) => B): Tree[B] = t match {
    case Leaf(x)             => Leaf(f(x))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(x)             => f(x)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  def size2[A](t: Tree[A]): Int = fold(t)(_ => 1)(1 + _ + _)

  def max2(t: Tree[Int]): Int = fold(t)(identity)(_ max _)

  def depth2[A](t: Tree[A]): Int = fold(t)(_ => 1)((x, y) => 1 + (x max y))

  def map2[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold[A, Tree[B]](t)(x => Leaf(f(x)))(Branch(_, _))

}