package com.agilogy.talks.fpft

import com.agilogy.talks.fpft.domain.{Document, DocumentId}
import com.agilogy.talks.fpft.infrastructure.SideEffects
import com.agilogy.talks.fpft.infrastructure.SideEffects.SqlException

class FunctionalSimpleTest extends TestTemplate {

  import com.agilogy.talks.fpft.functional.simple.DocumentsSynchEngine._

  private val service = new DocumentService(documentRepository, transactionController(new SideEffects.DataSource))

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
