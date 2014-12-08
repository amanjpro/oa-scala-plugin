package ch.usi.inf.l3.oa.library


import ch.usi.inf.l3.oa.quals._

trait TLifter[A, B] {
  def lift(x: A, y: B): A with B
}

// Generic Algebra
trait Algebra[F[_]] {
  // basic combinators
  def merge[A, B](mix: TLifter[A, B], a1: F[A], a2: F[B]): F[A with B]
  def empty: F[Any]

  // Derived combinator
  def decorator[A](parent: F[A], action: A => A): F[A] = {
    merge[A, Any](new LiftDecorate(action), parent, empty)
  }
}

class LiftDecorate[A](action: A => A) extends TLifter[A, Any] {
  def lift(x: A, y: Any) = action(x)
}


