package com.agilogy.talks.fpft.imperative.erorhandling.test

import com.agilogy.talks.fpft.domain.{Document, DocumentId}
import com.agilogy.talks.fpft.imperative.erorhandling._
import com.agilogy.talks.fpft.infrastructure.SideEffects
import org.scalatest.FunSpec


@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
class ImperativeErrorHandlingProgramTest extends FunSpec {


  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  implicit class TestEitherOps[E, R](self: Either[E, R]) {
    def getOrFail: R = self match {
      case Right(r) => r
      case Left(l) => throw new RuntimeException(s"Unexpected Left $l")
    }
  }

  describe("An imperative program") {

    it("should run with side effects") {
      SideEffects.reset()
      val repository = new DocumentRepository()
      val dataSource = new SideEffects.DataSource
      val transactionController = new TransactionController(dataSource)
      val service = new DocumentService(repository, transactionController)
      val docId = DocumentId("agile-manifesto")
      service.saveDocumentIfNotFound(Document(docId, "www.agilemanifesto.org")).getOrFail
      service.saveDocumentIfNotFound(Document(docId, "https://www.agilemanifesto.org")).getOrFail
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

      val res = service.insertDocument(Document(docId, "am.org"))
      assert(res === Left[InsertDocumentError, Unit](DocumentAlreadyExists(docId)))
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
