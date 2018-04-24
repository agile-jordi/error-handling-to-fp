package com.agilogy.talks.fpft.functional.bifunctor

import com.agilogy.talks.fpft.domain.{Document, DocumentId}
import com.agilogy.talks.fpft.infrastructure.SideEffects.SqlException
import scalaz.Scalaz._
import scalaz._

import scala.language.higherKinds

sealed trait UpdateDocumentError

sealed trait InsertDocumentError

final case class ConnectionError(sqlException: SqlException) extends InsertDocumentError with UpdateDocumentError

final case class DocumentAlreadyExists(id: DocumentId) extends InsertDocumentError

final case class DocumentNotFound(id: DocumentId) extends UpdateDocumentError

object Documents {

  type Transaction

  type TxAction[F[_, _], E, A] = Kleisli[F[E, ?], Transaction, A] // ~== Transaction => F[E,A]

  object TxAction {
    def apply[F[_, _], E, A](f: Transaction => F[E, A]): TxAction[F, E, A] = Kleisli[F[E, ?], Transaction, A](f)
  }

  implicit def kleisliBifunctor[F[_, _] : Bifunctor, R]: Bifunctor[({type l[E, A] = Kleisli[F[E, ?], R, A]})#l] =
    new Bifunctor[({type l[E, A] = Kleisli[F[E, ?], R, A]})#l] {
      override def bimap[A, B, C, D](fab: Kleisli[F[A,?], R, B])(f: A => C, g: B => D): Kleisli[F[C,?], R, D] = {
        Kleisli[F[C,?], R, D](fab.run.andThen(_.bimap(f,g)))
      }
    }

  implicit class TxActionOps[F[_, _] : Bifunctor, E, A](self: TxAction[F, E, A]) {

    private val B: Bifunctor[({type l[E, A] = Kleisli[F[E, ?], Transaction, A]})#l] = kleisliBifunctor[F,Transaction]

    def leftMap[E2](f: E => E2): TxAction[F, E2, A] = B.leftMap(self)(f)

    //TODO: Add all bifunctor ops here ;_(
  }

  class DocumentService[F[+ _, + _] : Bifunctor](documentRepository: DocumentRepository[F], transactionController: TransactionController[F])
  (implicit M: Monad[F[_, ?]]) {

    @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
    implicit def monad[E]: Monad[F[E, ?]] = new Monad[F[E, ?]] {
      override def bind[A, B](fa: F[E, A])(f: A => F[E, B]): F[E, B] = M.bind(fa.asInstanceOf[F[_, A]])(f).asInstanceOf[F[E, B]]

      override def point[A](a: => A): F[E, A] = M.point(a).asInstanceOf[F[E, A]]
    }

    @SuppressWarnings(Array("org.wartremover.warts.Throw"))
    def saveDocumentIfNotFound(document: Document): F[ConnectionError, Unit] = {

      transactionController.inTransaction {
        for {
          doc <- documentRepository.getDocument(document.id)
          _ <- if (doc.nonEmpty) {
            documentRepository.updateDocument(document).leftMap {
              case e: ConnectionError => e
              case _ => throw new IllegalStateException("Unreachable code")
            }
          } else {
            documentRepository.insertDocument(document).leftMap {
              case e: ConnectionError => e
              case _ => throw new IllegalStateException("Unreachable code")
            }
          }
        } yield ()
      }
    }


    def insertDocument(document: Document): F[InsertDocumentError, Unit] = {
      transactionController.inTransaction {
        documentRepository.insertDocument(document)
      }
    }

  }


  abstract class TransactionController[F[+ _, _]] {

    def begin(): F[ConnectionError, Transaction]

    def commit(tx: Transaction): F[ConnectionError, Unit]

    def rollback(tx: Transaction): F[ConnectionError, Unit]


    @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
    def inTransaction[E >: ConnectionError, T](f: TxAction[F, E, T])(implicit m: Monad[F[E, ?]]): F[E, T] = {
      for {
        tx <- begin().asInstanceOf[F[E, Transaction]]
        res <- f.run(tx)
        _ <- commit(tx).asInstanceOf[F[E, Transaction]]
      } yield {
        res
      }
    }

  }


  trait DocumentRepository[F[_, _]] {

    def getDocument(documentId: DocumentId): TxAction[F, ConnectionError, Option[Document]]

    def insertDocument(document: Document): TxAction[F, InsertDocumentError, Unit]

    def updateDocument(document: Document): TxAction[F, UpdateDocumentError, Unit]

  }

  object DocumentRepository {
    def apply[F[_, _] : DocumentRepository]: DocumentRepository[F] = implicitly[DocumentRepository[F]]
  }

}
