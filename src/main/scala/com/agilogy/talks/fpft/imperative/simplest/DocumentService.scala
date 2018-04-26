package com.agilogy.talks.fpft.imperative.simplest

import com.agilogy.talks.fpft.domain.{Document, DocumentId}
import com.agilogy.talks.fpft.imperative.DocumentNotFoundException
import com.agilogy.talks.fpft.infrastructure.SideEffects.{Connection, DataSource}

import scala.util.control.NonFatal

class DocumentService(documentRepository: DocumentRepository) {

  def saveDocumentIfNotFound(document: Document): Unit = {
    val documentExists = documentRepository.getDocument(document.id).nonEmpty
    if (documentExists) {
      documentRepository.updateDocument(document)
    } else {
      documentRepository.insertDocument(document)
    }
  }

  def insertDocument(document: Document): Unit = {
    documentRepository.insertDocument(document)
  }

}

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements", "org.wartremover.warts.Throw"))
class DocumentRepository(dataSource: DataSource) {

  private def inTransaction[T](f: Connection => T): T = {
    val conn = dataSource.getConnection()
    try {
      val res = f(conn)
      conn.commit()
      res
    } catch {
      case NonFatal(t) =>
        conn.rollback()
        throw t
    }
  }

  def getDocument(documentId: DocumentId): Option[Document] = {
    inTransaction(_.executeSelect(s"select * from documents where id = '${documentId.id}'").headOption)
  }

  def insertDocument(document: Document): Unit = {
    inTransaction(_.executeUpdate(s"insert into documents(id, content) values ('${document.id.id}', '${document.content}')"))
  }

  def updateDocument(document: Document): Unit = {
    val updated = inTransaction(_.executeUpdate(s"update documents set content = '${document.content}' where id = '${document.id.id}'"))
    if (updated == 0) throw DocumentNotFoundException(document.id)
  }

}
