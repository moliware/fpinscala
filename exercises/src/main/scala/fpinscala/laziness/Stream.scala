package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = this match {
    case Empty       => Nil
    case Cons(h, t)  => h() :: t().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => Cons(h, () => t().take(n - 1))
    case _                   => Empty
  }

  def takeViaUnfold(n: Int): Stream[A] = unfold((this, n)) {
    case (Cons(h, t), n) if n > 0 => Some((h(), (t(), n - 1)))
    case _                        => None
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case s                   => s
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) =>
      lazy val head = h()
      if (p(head)) Cons(() => head, () => t().takeWhile(p)) else Empty
    case _ => Empty
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) =>
      lazy val head = h()
      if (p(head)) Some((head, t())) else None
    case _ => None
  }

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else Empty)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] = foldRight(Option.empty[A])((a, b) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a, z) => cons(f(a), z))

  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case _          => None
  }

  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, z) => if (p(a)) cons(a, z) else z)

  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)(cons(_, _))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a, z) => f(a) append z )

  def zipWith[B](sb: Stream[B]): Stream[(A, B)] = unfold((this, sb)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(((h1(), h2()), (t1(), t2())))
    case _                            => None
  }

  def zipAll[B](sb: Stream[B]): Stream[(Option[A],Option[B])] = unfold((this, sb)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
    case (_, Cons(h2, t2))            => Some((None, Some(h2())), (empty, t2()))
    case (Cons(h1, t1), _)            => Some(((Some(h1()), None), (t1(), empty)))
    case _                            => None
  }

  def startsWith[B](s: Stream[B]): Boolean = zipAll(s).takeWhile(_._2.isDefined).forAll(x => x._1 == x._2)

  def tails: Stream[Stream[A]] = unfold(this) {
    case Empty        => None
    case s: Cons[A]   => Some((s, s.t()))
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def fibs(): Stream[Int] = {
    def fibs2(v0: Int, v1: Int): Stream[Int] = {
      Stream.cons(v0, fibs2(v1, v0 + v1))
    }
    fibs2(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => Stream.cons(a, unfold(s)(f))
    case None         => Stream.empty
  }

  def onesViaUnfold: Stream[Int] = unfold(1)(_ => Some((1, 1)))

  def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(_ => Some((a, a)))

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(v =>  Some(v, v + 1))

  def fibsViaUnfold(): Stream[Int] = unfold((0, 1)) {
    case (v0, v1) => Some(v0, (v1, v0 + v1 ))
  }

}