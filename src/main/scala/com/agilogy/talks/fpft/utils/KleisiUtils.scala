package com.agilogy.talks.fpft.utils

import scalaz._
import Scalaz._

import scala.language.higherKinds

object KleisiUtils {

  def kleisliBifunctor[F[_, _] : Bifunctor, R]: Bifunctor[({type l[E, A] = Kleisli[F[E, ?], R, A]})#l] =
    new Bifunctor[({type l[E, A] = Kleisli[F[E, ?], R, A]})#l] {
      override def bimap[A, B, C, D](fab: Kleisli[F[A,?], R, B])(f: A => C, g: B => D): Kleisli[F[C,?], R, D] = {
        Kleisli[F[C,?], R, D](fab.run.andThen(_.bimap(f,g)))
      }
    }


}
