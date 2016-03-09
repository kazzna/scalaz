package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Nondeterminism` */
final class NondeterminismOps[F[_], A] private[syntax](s: => F[A])(implicit val F: Nondeterminism[F]) extends Ops[F[A]] {
  ////

  lazy val self: F[A] = s
  final def <~*~>[B](f: F[A => B]): F[B] = F.ap(self)(f)
  final def ntuple[B](f: => F[B]): F[(A, B)] = F.tuple2(self, f)

  /**
   * DSL for constructing Nondeterminism expressions.
   *
   * `(f1 |~@~| f2 |~@~| ... |~@~| fn)((v1, v2, ... vn) => ...)` is an alternative to `Nondeterminism[F].applyN(f1, f2, ..., fn)((v1, v2, ... vn) => ...)`
   *
   * `(f1 |~@~| f2 |~@~| ... |~@~| fn).tupled` is an alternative to `Nondeterminism[F].applyN(f1, f2, ..., fn)(TupleN.apply _)`
   *
   * Warning: each call to `|~@~|` leads to an allocation of wrapper object. For performance sensitive code, consider using
   *          [[scalaz.Nondeterminism]]`#applyN` directly.
   */
  final def |~@~|[B](fb: => F[B]) = new NondeterminismBuilder[F, A, B] {
    lazy val a: F[A] = self
    lazy val b: F[B] = fb
  }
  /** Alias for `|~@~|` */
  final def ~⊛~[B](fb: => F[B]) = |~@~|(fb)

  // Do not remove this comment; used as delimiter by `genTypeClasses` sbt task.
  ////
}

sealed trait ToNondeterminismOps0 {
  implicit def ToNondeterminismOpsUnapply[FA](v: FA)(implicit F0: Unapply[Nondeterminism, FA]) =
    new NondeterminismOps[F0.M, F0.A](F0(v))(F0.TC)

}

trait ToNondeterminismOps extends ToNondeterminismOps0 with ToMonadOps {
  implicit def ToNondeterminismOps[F[_], A](v: => F[A])(implicit F0: Nondeterminism[F]) =
    new NondeterminismOps[F, A](v)

  ////

  def ^~^[F[_], A, B, C](fa: => F[A], fb: => F[B])(
      f: (A, B) => C)(implicit F: Nondeterminism[F]): F[C] =
    F.apply2(fa, fb)(f)

  def ^~~^[F[_], A, B, C, D](fa: => F[A], fb: => F[B], fc: => F[C])(
      f: (A, B, C) => D)(implicit F: Nondeterminism[F]): F[D] =
    F.apply3(fa, fb, fc)(f)

  def ^~~~^[F[_], A, B, C, D, E](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D])(
      f: (A, B, C, D) => E)(implicit F: Nondeterminism[F]): F[E] =
    F.apply4(fa, fb, fc, fd)(f)

  def ^~~~~^[F[_], A, B, C, D, E, I](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E])(
      f: (A, B, C, D, E) => I)(implicit F: Nondeterminism[F]): F[I] =
    F.apply5(fa, fb, fc, fd, fe)(f)

  def ^~~~~~^[F[_], A, B, C, D, E, I, J](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], fi: => F[I])(
      f: (A, B, C, D, E, I) => J)(implicit F: Nondeterminism[F]): F[J] =
    F.apply6(fa, fb, fc, fd, fe, fi)(f)

  def ^~~~~~~^[F[_], A, B, C, D, E, I, J, K](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], fi: => F[I], fj: => F[J])(
      f: (A, B, C, D, E, I, J) => K)(implicit F: Nondeterminism[F]): F[K] =
    F.apply7(fa, fb, fc, fd, fe, fi, fj)(f)

  ////
}

trait NondeterminismSyntax[F[_]] extends MonadSyntax[F] {
  implicit def ToNondeterminismOps[A](v: F[A]): NondeterminismOps[F, A] =
    new NondeterminismOps[F, A](v)(NondeterminismSyntax.this.F)

  def F: Nondeterminism[F]
  ////
  def ^~^[A, B, C](fa: => F[A], fb: => F[B])(
      f: (A, B) => C): F[C] =
    F.apply2(fa, fb)(f)

  def ^~~^[A, B, C, D](fa: => F[A], fb: => F[B], fc: => F[C])(
      f: (A, B, C) => D): F[D] =
    F.apply3(fa, fb, fc)(f)

  def ^~~~^[A, B, C, D, E](fa: => F[A],  fb: => F[B], fc: => F[C], fd: => F[D])(
      f: (A, B, C, D) => E): F[E] =
    F.apply4(fa, fb, fc, fd)(f)

  def ^~~~~^[A, B, C, D, E, I](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E])(
      f: (A, B, C, D, E) => I): F[I] =
    F.apply5(fa, fb, fc, fd, fe)(f)

  def ^~~~~~^[A, B, C, D, E, I, J](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], fi: => F[I])(
      f: (A, B, C, D, E, I) => J): F[J] =
    F.apply6(fa, fb, fc, fd, fe, fi)(f)

  def ^~~~~~~^[A, B, C, D, E, I, J, K](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], fi: => F[I], fj: => F[J])(
      f: (A, B, C, D, E, I, J) => K): F[K] =
    F.apply7(fa, fb, fc, fd, fe, fi, fj)(f)

  ////
}
