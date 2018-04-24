package com.agilogy.talks.fpft.functional.eithert

import com.agilogy.talks.fpft.domain.{Document, DocumentId}
import com.agilogy.talks.fpft.infrastructure.SideEffects.SqlException
import scalaz._
import Scalaz._

import scala.language.higherKinds

sealed trait InsertDocumentError

sealed trait UpdateDocumentError

final case class ConnectionError(sqlException: SqlException) extends InsertDocumentError with UpdateDocumentError

final case class DocumentAlreadyExists(id: DocumentId) extends InsertDocumentError

final case class DocumentNotFound(id: DocumentId) extends UpdateDocumentError

object Documents {

  type Transaction

  type Action[F[_], E, A] = EitherT[F, E, A]

  object Action {
    def apply[F[_] : Functor, E, A](s: F[Either[E, A]]): EitherT[F, E, A] = EitherT.fromEither(s)
  }

  type TxAction[F[_], E, A] = Kleisli[Action[F, E, ?], Transaction, A]

  object TxAction {
    def apply[F[_], E, A](f: Transaction => EitherT[F, E, A]): TxAction[F, E, A] = Kleisli[Action[F, E, ?], Transaction, A](f)
  }

  implicit def txActionBifunctor[F[_]:Functor]: Bifunctor[TxAction[F, ?, ?]] = new Bifunctor[TxAction[F,?,?]] {
    override def bimap[A, B, C, D](fab: TxAction[F, A, B])(f: A => C, g: B => D): TxAction[F, C, D] = TxAction{
      tx =>
        fab.run(tx).bimap(f,g)
    }
  }


  class DocumentService[F[+ _] : Monad](documentRepository: DocumentRepository[F], transactionController: TransactionController[F]) {

    @SuppressWarnings(Array("org.wartremover.warts.Throw"))
    def saveDocumentIfNotFound(document: Document): Action[F, ConnectionError, Unit] = {

//      import TxAction.txActionBifunctor

      transactionController.inTransaction {
        for {
          doc <- documentRepository.getDocument(document.id)
          _ <- if (doc.nonEmpty) {
            documentRepository.updateDocument(document).leftMap{
              case e: ConnectionError => e
              case _ => throw new IllegalStateException("Unreachable code")
            }
          } else {
            documentRepository.insertDocument(document).leftMap{
              case e: ConnectionError => e
              case _ => throw new IllegalStateException("Unreachable code")
            }
          }
        } yield ()
      }
    }


    def insertDocument(document: Document): Action[F, InsertDocumentError, Unit] = {
      transactionController.inTransaction {
        documentRepository.insertDocument(document)
      }
    }

  }


  abstract class TransactionController[F[+ _]] {

    def begin(): Action[F, ConnectionError, Transaction]

    def commit(tx: Transaction): Action[F, ConnectionError, Unit]

    def rollback(tx: Transaction): Action[F, ConnectionError, Unit]


    @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
    def inTransaction[E >: ConnectionError, T](f: TxAction[F, E, T])(implicit m: Monad[F]): Action[F, E, T] = {
      for {
        tx <- begin().asInstanceOf[Action[F, E, Transaction]]
        res <- f.run(tx)
        _ <- commit(tx).asInstanceOf[Action[F, E, Transaction]]
      } yield {
        res
      }
    }

  }


  trait DocumentRepository[F[_]] {

    def getDocument(documentId: DocumentId): TxAction[F, ConnectionError, Option[Document]]

    def insertDocument(document: Document): TxAction[F, InsertDocumentError, Unit]

    def updateDocument(document: Document): TxAction[F, UpdateDocumentError, Unit]

  }

  object DocumentRepository {
    def apply[F[_] : DocumentRepository]: DocumentRepository[F] = implicitly[DocumentRepository[F]]
  }

}
