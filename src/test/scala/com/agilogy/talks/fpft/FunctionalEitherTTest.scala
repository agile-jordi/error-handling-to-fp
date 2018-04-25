package com.agilogy.talks.fpft

import com.agilogy.talks.fpft.domain.{Document, DocumentAlreadyExists, DocumentId, InsertDocumentError}
import com.agilogy.talks.fpft.infrastructure.SideEffects

class FunctionalEitherTTest extends TestTemplate {

  import com.agilogy.talks.fpft.functional.eithert.DocumentsSynchEngine._

  private val service = new DocumentService(documentRepository, transactionController(new SideEffects.DataSource))

  override def saveDocument(docId: DocumentId, body: String): Unit = {
    service.saveDocumentIfNotFound(Document(docId, body))
  }

  override def insertDocument(docId: DocumentId, body: String): Unit = {
    val res = service.insertDocument(Document(docId, body))
    assert(res.toEither === Left[InsertDocumentError, Unit](DocumentAlreadyExists(docId)))
  }
}
