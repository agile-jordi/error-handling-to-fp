package com.agilogy.talks.fpft

object TestEitherUtils {

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  implicit class TestEitherOps[E, R](self: Either[E, R]) {
    def getOrFail: R = self match {
      case Right(r) => r
      case Left(l) => throw new RuntimeException(s"Unexpected Left $l")
    }
  }

}
