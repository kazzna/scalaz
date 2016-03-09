package scalaz
package syntax

/** @see [[scalaz.syntax.NondeterminismOps]]`#|@~|` */
private[scalaz] trait NondeterminismBuilder[M[_], A, B] {
  val a: M[A]
  val b: M[B]

  def apply[C](f: (A, B) => C)(implicit nd: Nondeterminism[M]): M[C] = nd.apply2(a, b)(f)

  def tupled(implicit nd: Nondeterminism[M]): M[(A, B)] = apply(Tuple2.apply)

  def ~⊛~[C](cc: => M[C]) = |~@~|(cc)

  def |~@~|[C](cc: => M[C]) = new NondeterminismBuilder3[C] {
    lazy val c = cc
  }

  sealed abstract class NondeterminismBuilder3[C] {
    val c: M[C]

    def apply[D](f: (A, B, C) => D)(implicit nd: Nondeterminism[M]): M[D] =
      nd.apply3(a, b, c)(f)

    def tupled(implicit nd: Nondeterminism[M]): M[(A, B, C)] = apply(Tuple3.apply)

    def ~⊛~[D](dd: => M[D]) = |~@~|(dd)

    def |~@~|[D](dd: => M[D]) = new NondeterminismBuilder4[D] {
      lazy val d = dd
    }


    sealed abstract class NondeterminismBuilder4[D] {
      val d: M[D]

      def apply[E](f: (A, B, C, D) => E)(implicit nd: Nondeterminism[M]): M[E] =
        nd.apply4(a, b, c, d)(f)

      def tupled(implicit nd: Nondeterminism[M]): M[(A, B, C, D)] = apply(Tuple4.apply)

      def ~⊛~[E](ee: => M[E]) = |~@~|(ee)

      def |~@~|[E](ee: => M[E]) = new NondeterminismBuilder5[E] {
        lazy val e = ee
      }


      sealed abstract class NondeterminismBuilder5[E] {
        val e: M[E]

        def apply[F](f: (A, B, C, D, E) => F)(implicit nd: Nondeterminism[M]): M[F] =
          nd.apply5(a, b, c, d, e)(f)

        def tupled(implicit nd: Nondeterminism[M]): M[(A, B, C, D, E)] = apply(Tuple5.apply)

        def ~⊛~[F](f: => M[F]) = |~@~|(f)

        def |~@~|[F](f: => M[F]) = new NondeterminismBuilder6[F] {
          lazy val ff = f
        }


        sealed abstract class NondeterminismBuilder6[F] {
          val ff: M[F]

          def apply[G](f: (A, B, C, D, E, F) => G)(implicit nd: Nondeterminism[M]): M[G] =
            nd.apply6(a, b, c, d, e, ff)(f)

          def tupled(implicit nd: Nondeterminism[M]): M[(A, B, C, D, E, F)] = apply(Tuple6.apply)

          def ~⊛~[G](gg: => M[G]) = |~@~|(gg)

          def |~@~|[G](gg: => M[G]) = new NondeterminismBuilder7[G] {
            lazy val g = gg
          }

          sealed abstract class NondeterminismBuilder7[G] {
            val g: M[G]

            def apply[H](f: (A, B, C, D, E, F, G) => H)(implicit nd: Nondeterminism[M]): M[H] =
              nd.apply7(a, b, c, d, e, ff, g)(f)

            def tupled(implicit nd: Nondeterminism[M]): M[(A, B, C, D, E, F, G)] = apply(Tuple7.apply)

            def ~⊛~[H](hh: => M[H]) = |~@~|(hh)

            def |~@~|[H](hh: => M[H]) = new NondeterminismBuilder8[H] {
              lazy val h = hh
            }


            sealed abstract class NondeterminismBuilder8[H] {
              val h: M[H]

              def apply[I](f: (A, B, C, D, E, F, G, H) => I)(implicit nd: Nondeterminism[M]): M[I] =
                nd.apply8(a, b, c, d, e, ff, g, h)(f)

              def tupled(implicit nd: Nondeterminism[M]): M[(A, B, C, D, E, F, G, H)] = apply(Tuple8.apply)

              def ~⊛~[I](ii: => M[I]) = |~@~|(ii)

              def |~@~|[I](ii: => M[I]) = new NondeterminismBuilder9[I] {
                lazy val i = ii
              }


              sealed abstract class NondeterminismBuilder9[I] {
                val i: M[I]

                def apply[J](f: (A, B, C, D, E, F, G, H, I) => J)(implicit nd: Nondeterminism[M]): M[J] =
                  nd.apply9(a, b, c, d, e, ff, g, h, i)(f)

                def tupled(implicit nd: Nondeterminism[M]): M[(A, B, C, D, E, F, G, H, I)] = apply(Tuple9.apply)

                def ~⊛~[J](jj: => M[J]) = |~@~|(jj)

                def |~@~|[J](jj: => M[J]) = new NondeterminismBuilder10[J] {
                  lazy val j = jj
                }

                sealed abstract class NondeterminismBuilder10[J] {
                  val j: M[J]

                  def apply[K](f: (A, B, C, D, E, F, G, H, I, J) => K)(implicit nd: Nondeterminism[M]): M[K] =
                    nd.apply10(a, b, c, d, e, ff, g, h, i, j)(f)

                  def tupled(implicit nd: Nondeterminism[M]): M[(A, B, C, D, E, F, G, H, I, J)] = apply(Tuple10.apply)

                  def ~⊛~[K](kk: => M[K]) = |~@~|(kk)

                  def |~@~|[K](kk: => M[K]) = new NondeterminismBuilder11[K] {
                    lazy val k = kk
                  }

                  sealed abstract class NondeterminismBuilder11[K] {
                    val k: M[K]

                    def apply[L](f: (A, B, C, D, E, F, G, H, I, J, K) => L)(implicit nd: Nondeterminism[M]): M[L] =
                      nd.apply11(a, b, c, d, e, ff, g, h, i, j, k)(f)

                    def tupled(implicit nd: Nondeterminism[M]): M[(A, B, C, D, E, F, G, H, I, J, K)] = apply(Tuple11.apply)

                    def ~⊛~[L](ll: => M[L]) = |~@~|(ll)

                    def |~@~|[L](ll: => M[L]) = new NondeterminismBuilder12[L] {
                      lazy val l = ll
                    }

                    sealed abstract class NondeterminismBuilder12[L] {
                      val l: M[L]

                      def apply[MM](f: (A, B, C, D, E, F, G, H, I, J, K, L) => MM)(implicit nd: Nondeterminism[M]): M[MM] =
                        nd.apply12(a, b, c, d, e, ff, g, h, i, j, k, l)(f)

                      def tupled(implicit nd: Nondeterminism[M]): M[(A, B, C, D, E, F, G, H, I, J, K, L)] = apply(Tuple12.apply)
                    }

                  }

                }

              }

            }

          }

        }

      }

    }

  }

}
