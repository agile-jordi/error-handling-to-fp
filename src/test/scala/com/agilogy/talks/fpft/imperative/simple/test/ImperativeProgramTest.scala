package com.agilogy.talks.fpft.imperative.simple.test

import com.agilogy.talks.fpft.domain.{Document, DocumentId}
import com.agilogy.talks.fpft.imperative.simple._
import com.agilogy.talks.fpft.infrastructure.SideEffects
import com.agilogy.talks.fpft.infrastructure.SideEffects.SqlException
import org.scalatest.FunSpec


@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
class ImperativeProgramTest extends FunSpec {

  describe("An imperative program") {

    it("should run with side effects") {
      SideEffects.reset()
      val repository = new DocumentRepository()
      val dataSource = new SideEffects.DataSource
      val transactionController = new TransactionController(dataSource)
      val service = new DocumentService(repository, transactionController)
      service.saveDocumentIfNotFound(Document(DocumentId("agile-manifesto"), "www.agilemanifesto.org"))
      service.saveDocumentIfNotFound(Document(DocumentId("agile-manifesto"), "https://www.agilemanifesto.org"))
      val expectedFirstLogs = List(
        "[Conn-1] - GetConnection",
        "[Conn-1] - Begin",
        "[Conn-1] - select * from documents where id = 'agile-manifesto'",
        "[Conn-1] - insert into documents(id, content) values ('agile-manifesto', 'www.agilemanifesto.org')",
        "[Conn-1] - Commit",
        "[Conn-2] - GetConnection",
        "[Conn-2] - Begin",
        "[Conn-2] - select * from documents where id = 'agile-manifesto'",
        "[Conn-2] - update documents set content = 'https://www.agilemanifesto.org' where id = 'agile-manifesto'",
        "[Conn-2] - Commit"
      )
      assert(SideEffects.getLogs() === expectedFirstLogs)

      val res = intercept[SqlException]{
        service.insertDocument(Document(DocumentId("agile-manifesto"), "am.org"))
      }
      assert(res === SqlException("Duplicate key 'agile-manifesto' in documents"))
      val newLogs = SideEffects.getLogs(expectedFirstLogs.size)
      assert(newLogs === List(
        "[Conn-3] - GetConnection",
        "[Conn-3] - Begin",
        "[Conn-3] - insert into documents(id, content) values ('agile-manifesto', 'am.org')",
        "[Conn-3] - Rollback"
      ))
    }
  }

}
