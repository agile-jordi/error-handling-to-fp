package com.agilogy.talks.fpft.utils

import scala.util.control.NonFatal

object EitherUtils {

  def left[L](l: L): Either[L, Nothing] = Left[L, Nothing](l)

  def right[R](r: R): Either[Nothing, R] = Right[Nothing, R](r)

  def fromTry[R](f: => R): Either[Throwable, R] = {
    try {
      right(f)
    } catch {
      case NonFatal(e) => left(e)
    }
  }

}
