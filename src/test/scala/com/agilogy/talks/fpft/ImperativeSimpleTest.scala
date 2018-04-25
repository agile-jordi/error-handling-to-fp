package com.agilogy.talks.fpft

import com.agilogy.talks.fpft.domain.{Document, DocumentId}
import com.agilogy.talks.fpft.imperative.simple._
import com.agilogy.talks.fpft.infrastructure.SideEffects
import com.agilogy.talks.fpft.infrastructure.SideEffects.SqlException


@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
class ImperativeSimpleTest extends TestTemplate {

  private val service = new DocumentService(new DocumentRepository(), new TransactionController(new SideEffects.DataSource))

  override def saveDocument(docId: DocumentId, body: String): Unit = {
    service.saveDocumentIfNotFound(Document(docId, body))
  }

  override def insertDocument(docId: DocumentId, body: String): Unit = {
    val res = intercept[SqlException]{
      service.insertDocument(Document(DocumentId("agile-manifesto"), "am.org"))
    }
    assert(res === SqlException("Duplicate key 'agile-manifesto' in documents"))
  }
}
