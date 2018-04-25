package com.agilogy.talks.fpft.functional.eithert

import com.agilogy.talks.fpft.domain._
import scalaz._
import Scalaz._

import scala.language.higherKinds

trait Documents[F[_]] {

  implicit def M:Monad[F]

  type Transaction

  type Action[E, A] = EitherT[F, E, A] // ~= F[Either[E,A]]

  object Action {
    def apply[E, A](s: F[Either[E, A]]): EitherT[F, E, A] = EitherT.fromEither(s)
  }

  type TxAction[E, A] = Kleisli[Action[E, ?], Transaction, A] // ~= Transaction => Action[E,A] ~= Transaction => F[Either[E,A]]

  object TxAction {
    def apply[E, A](f: Transaction => Action[E, A]): TxAction[E, A] = Kleisli[Action[E, ?], Transaction, A](f)
  }

  implicit def txActionBifunctor: Bifunctor[TxAction] = new Bifunctor[TxAction] {
    override def bimap[A, B, C, D](fab: TxAction[A, B])(f: A => C, g: B => D): TxAction[C, D] = TxAction{
      tx =>
        fab.run(tx).bimap(f,g)
    }
  }


  class DocumentService(documentRepository: DocumentRepository, transactionController: TransactionController) {

    @SuppressWarnings(Array("org.wartremover.warts.Throw"))
    def saveDocumentIfNotFound(document: Document): Action[ConnectionError, Unit] = {

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


    def insertDocument(document: Document): Action[InsertDocumentError, Unit] = {
      transactionController.inTransaction {
        documentRepository.insertDocument(document)
      }
    }

  }


  trait TransactionController {

    def begin(): Action[ConnectionError, Transaction]

    def commit(tx: Transaction): Action[ConnectionError, Unit]

    def rollback(tx: Transaction): Action[ConnectionError, Unit]

    @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
    def inTransaction[E >: ConnectionError, T](f: TxAction[E, T]): Action[E, T] = {
      begin().asInstanceOf[Action[E, Transaction]].flatMap { tx =>
        Action{
          f.run(tx).toEither.map {
            case Left(e) =>
              rollback(tx)
              Left[E,T](e)
            case Right(r) =>
              commit(tx)
              Right[E,T](r)
          }
        }
      }
    }
  }


  trait DocumentRepository {

    def getDocument(documentId: DocumentId): TxAction[ConnectionError, Option[Document]]

    def insertDocument(document: Document): TxAction[InsertDocumentError, Unit]

    def updateDocument(document: Document): TxAction[UpdateDocumentError, Unit]

  }

}
