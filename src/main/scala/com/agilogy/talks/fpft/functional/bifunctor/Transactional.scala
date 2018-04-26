package com.agilogy.talks.fpft.functional.bifunctor

import com.agilogy.talks.fpft.utils.KleisiUtils
import scalaz.{Bifunctor, Bitraverse, Kleisli}

import scala.language.higherKinds

trait Transactional[F[+ _, + _]] {

  implicit def B: Bitraverse[F]

  type Transaction

  type TxAction[E, A] = Kleisli[F[E, ?], Transaction, A] // ~== Transaction => F[E,A]

  object TxAction {
    def apply[E, A](f: Transaction => F[E, A]): TxAction[E, A] = Kleisli[F[E, ?], Transaction, A](f)
  }

  protected implicit val BT: Bifunctor[TxAction] = KleisiUtils.kleisliBifunctor[F, Transaction]

}