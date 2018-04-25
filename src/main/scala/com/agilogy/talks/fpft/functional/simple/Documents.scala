package com.agilogy.talks.fpft.functional.simple

import com.agilogy.talks.fpft.domain.{Document, DocumentId}
import scalaz._
import Scalaz._

import scala.language.higherKinds

trait Documents[F[_]] {

  implicit def M:Monad[F]

  type Transaction

  type TxAction[A] = Kleisli[F, Transaction, A]

  object TxAction{
    def apply[A](f: Transaction => F[A]): TxAction[A] = Kleisli(f)
  }

  class DocumentService(documentRepository: DocumentRepository, transactionController: TransactionController) {

    def saveDocumentIfNotFound(document: Document): F[Unit] = {
      transactionController.inTransaction {
        for {
          doc <- documentRepository.getDocument(document.id)
          _ <- if (doc.nonEmpty) {
            documentRepository.updateDocument(document)
          } else {
            documentRepository.insertDocument(document)
          }
        } yield ()
      }
    }

    def insertDocument(document: Document): Unit = {
      transactionController.inTransaction {
          documentRepository.insertDocument(document)
      }
    }

  }

  trait TransactionController {

    def begin(): F[Transaction]

    def commit(tx: Transaction): F[Unit]

    def rollback(tx: Transaction): F[Unit]


    def inTransaction[T](f: TxAction[T]): F[T] = {
      for {
        tx <- begin()
        res <- f.run(tx)
        _ <- commit(tx)
      } yield res
    }
  }

  final case class DocumentNotFound(id: DocumentId) extends RuntimeException {
    override def getMessage: String = s"Document $id not found"
  }

  trait DocumentRepository {
    def getDocument(documentId: DocumentId): TxAction[Option[Document]]

    def insertDocument(document: Document): TxAction[Unit]

    def updateDocument(document: Document): TxAction[Unit]
  }

}
