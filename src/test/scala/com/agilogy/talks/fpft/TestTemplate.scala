package com.agilogy.talks.fpft

import com.agilogy.talks.fpft.domain.DocumentId
import com.agilogy.talks.fpft.infrastructure.SideEffects
import org.scalatest.FunSpec

trait TestTemplate extends FunSpec {

  def saveDocument(docId: DocumentId, body: String): Unit

  def insertDocument(docId: DocumentId, body: String): Unit

  it("should run with side effects") {
    SideEffects.reset()
    val docId = DocumentId("agile-manifesto")
    saveDocument(docId, "www.agilemanifesto.org")
    saveDocument(docId, "https://www.agilemanifesto.org")
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

    insertDocument(docId, "am.org")
    val newLogs = SideEffects.getLogs(expectedFirstLogs.size)
    assert(newLogs === List(
      "[Conn-3] - GetConnection",
      "[Conn-3] - Begin",
      "[Conn-3] - insert into documents(id, content) values ('agile-manifesto', 'am.org')",
      "[Conn-3] - Rollback"
    ))
  }


}


