package com.agilogy.talks.fpft.utils

object EitherUtils {

  def left[L](l: L): Either[L, Nothing] = Left[L, Nothing](l)

  def right[R](r: R): Either[Nothing, R] = Right[Nothing, R](r)

}
