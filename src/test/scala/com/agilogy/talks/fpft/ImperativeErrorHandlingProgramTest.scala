package com.agilogy.talks.fpft

import com.agilogy.talks.fpft.domain.{Document, DocumentAlreadyExists, DocumentId, InsertDocumentError}
import com.agilogy.talks.fpft.imperative.erorhandling._
import com.agilogy.talks.fpft.infrastructure.SideEffects
import TestEitherUtils._

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
class ImperativeErrorHandlingProgramTest extends TestTemplate {

  private val service = new DocumentService(new DocumentRepository(), new TransactionController(new SideEffects.DataSource))

  override def saveDocument(docId: DocumentId, body: String): Unit = {
    service.saveDocumentIfNotFound(Document(docId, body)).getOrFail
  }

  override def insertDocument(docId: DocumentId, body: String): Unit = {
    val res = service.insertDocument(Document(docId, body))
    assert(res === Left[InsertDocumentError, Unit](DocumentAlreadyExists(docId)))
  }

}
