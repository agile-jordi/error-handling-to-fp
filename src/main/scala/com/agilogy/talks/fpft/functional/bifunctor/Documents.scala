package com.agilogy.talks.fpft.functional.bifunctor

import com.agilogy.talks.fpft.domain._
import scalaz.Scalaz._
import scalaz._

import scala.language.higherKinds


trait Documents[F[+ _, + _]] extends Transactional[F] {

  implicit def M[E]: Monad[F[E, ?]]

  class DocumentService(documentRepository: DocumentRepository, transactionController: TransactionController) {

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


    def insertDocument(document: Document): F[InsertDocumentError, Unit] = transactionController.inTransaction {
      documentRepository.insertDocument(document)
    }

  }


  trait TransactionController {

    def begin(): F[ConnectionError, Transaction]

    def commit(tx: Transaction): F[ConnectionError, Unit]

    def rollback(tx: Transaction): F[ConnectionError, Unit]

    @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
    def inTransaction[E >: ConnectionError, T](f: TxAction[E, T]): F[E, T] = {
      val b: F[E, Transaction] = begin()
      b.flatMap { tx =>
        val res = f.run(tx)
        val isLeft = res.bifoldLeft(false)((_, _) => true)((_, _) => false)
        if (isLeft) {
          rollback(tx)
        } else {
          commit(tx)
        }
        //TODO: Proper error handling of commit / rollback
        res
      }

    }

  }


  trait DocumentRepository {

    def getDocument(documentId: DocumentId): TxAction[ConnectionError, Option[Document]]

    def insertDocument(document: Document): TxAction[InsertDocumentError, Unit]

    def updateDocument(document: Document): TxAction[UpdateDocumentError, Unit]

  }

}
