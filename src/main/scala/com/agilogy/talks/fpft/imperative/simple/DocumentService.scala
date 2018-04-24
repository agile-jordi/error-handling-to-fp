package com.agilogy.talks.fpft.imperative.simple

import com.agilogy.talks.fpft.domain.{Document, DocumentId}
import com.agilogy.talks.fpft.infrastructure.SideEffects.{Connection, DataSource}

import scala.util.control.NonFatal

class DocumentService(documentRepository: DocumentRepository, transactionController: TransactionController) {


  def saveDocumentIfNotFound(document: Document): Unit = {
    transactionController.inTransaction {
      conn =>
        val documentExists = documentRepository.getDocument(document.id)(conn).nonEmpty
        if (documentExists) {
          documentRepository.updateDocument(document)(conn)
        } else {
          documentRepository.insertDocument(document)(conn)
        }
    }
  }

  def insertDocument(document: Document): Unit = {
    transactionController.inTransaction {
      conn =>
        documentRepository.insertDocument(document)(conn)
    }
  }

}

@SuppressWarnings(Array("org.wartremover.warts.Throw"))
class TransactionController(dataSource: DataSource) {

  def inTransaction[T](f: Connection => T): T = {
    val conn = dataSource.getConnection()
    try {
      conn.beginTransaction()
      val res = f(conn)
      conn.commit()
      res
    } catch {
      case NonFatal(t) =>
        //TODO: Proper error handling of rollback
        conn.rollback()
        throw t
    }
  }

}

final case class DocumentNotFound(id:DocumentId) extends RuntimeException{
  override def getMessage: String = s"Document $id not found"
}

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements", "org.wartremover.warts.Throw"))
class DocumentRepository {

  def getDocument(documentId: DocumentId)(conn: Connection): Option[Document] = {
    conn.executeSelect(s"select * from documents where id = '${documentId.id}'").headOption
  }

  def insertDocument(document: Document)(conn: Connection): Unit = {
    conn.executeUpdate(s"insert into documents(id, content) values ('${document.id.id}', '${document.content}')")
  }

  def updateDocument(document: Document)(conn: Connection): Unit = {
    val updated = conn.executeUpdate(s"update documents set content = '${document.content}' where id = '${document.id.id}'")
    if(updated == 0) throw DocumentNotFound(document.id)
  }

}
