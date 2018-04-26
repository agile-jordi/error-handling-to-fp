package com.agilogy.talks.fpft.functional.simplest

import com.agilogy.talks.fpft.domain.{Document, DocumentId}
import scalaz.Scalaz._
import scalaz._

import scala.language.higherKinds

class DocumentService[F[_] : Monad : DocumentRepository] {

  private val documentRepository = implicitly[DocumentRepository[F]]

  def saveDocumentIfNotFound(document: Document): F[Unit] = {
    for {
      doc <- documentRepository.getDocument(document.id)
      _ <- if (doc.nonEmpty) {
        documentRepository.updateDocument(document)
      } else {
        documentRepository.insertDocument(document)
      }
    } yield ()
  }

  def insertDocument(document: Document): F[Unit] = {
    documentRepository.insertDocument(document)
  }

}


trait DocumentRepository[F[_]] {

  def getDocument(documentId: DocumentId): F[Option[Document]]

  def insertDocument(document: Document): F[Unit]

  def updateDocument(document: Document): F[Unit]
}


