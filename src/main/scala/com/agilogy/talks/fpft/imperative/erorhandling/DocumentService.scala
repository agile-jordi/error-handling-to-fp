package com.agilogy.talks.fpft.imperative.erorhandling

import com.agilogy.talks.fpft.domain._
import com.agilogy.talks.fpft.infrastructure.SideEffects.{Connection, DataSource, SqlException}
import com.agilogy.talks.fpft.utils.EitherUtils._

class DocumentService(documentRepository: DocumentRepository, transactionController: TransactionController) {

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def saveDocumentIfNotFound(document: Document): Either[ConnectionError, Unit] = {
    transactionController.inTransaction {
      conn =>
        for {
          doc <- documentRepository.getDocument(document.id)(conn)
          _ <- if (doc.nonEmpty) {
            documentRepository.updateDocument(document)(conn).left.map {
              case e: ConnectionError => e
              case _ => throw new IllegalStateException("Unreachable code")
            }
          } else {
            documentRepository.insertDocument(document)(conn).left.map {
              case e: ConnectionError => e
              case _ => throw new IllegalStateException("Unreachable code")
            }
          }
        } yield ()
    }
  }


  def insertDocument(document: Document): Either[InsertDocumentError, Unit] = {
    transactionController.inTransaction {
      conn =>
        documentRepository.insertDocument(document)(conn)
    }
  }

}

class TransactionController(dataSource: DataSource) {

  private def handleErrors[T](f: => T): Either[ConnectionError, T] = {
    try {
      right(f)
    } catch {
      case e: SqlException => left(ConnectionError(e))
    }
  }

  def inTransaction[E >: ConnectionError, T](f: Connection => Either[E, T]): Either[E, T] = {
    val conn = dataSource.getConnection()
    val txResult = for {
      _ <- handleErrors(conn.beginTransaction())
      res <- f(conn)
      _ <- handleErrors(conn.commit())
    } yield res
    if(txResult.isLeft){
      //TODO: Proper error handling of rollback
      conn.rollback()
    }
    txResult
  }

}


class DocumentRepository {

  def getDocument(documentId: DocumentId)(conn: Connection): Either[ConnectionError, Option[Document]] = {
    try {
      right(conn.executeSelect(s"select * from documents where id = '${documentId.id}'").headOption)
    } catch {
      case e: SqlException => left(ConnectionError(e))
    }
  }

  def insertDocument(document: Document)(conn: Connection): Either[InsertDocumentError, Unit] = {
    try {
      right(conn.executeUpdate(s"insert into documents(id, content) values ('${document.id.id}', '${document.content}')")).right.map(_ => ())
    } catch {
      case SqlException(msg) if msg.startsWith("Duplicate key") => left(DocumentAlreadyExists(document.id))
      case e: SqlException => left(ConnectionError(e))
    }
  }

  def updateDocument(document: Document)(conn: Connection): Either[UpdateDocumentError, Unit] = {
    try {
      val updatedRows = conn.executeUpdate(s"update documents set content = '${document.content}' where id = '${document.id.id}'")
      if (updatedRows == 0) left(DocumentNotFound(document.id))
      else right(())
    } catch {
      case e: SqlException => left(ConnectionError(e))
    }
  }

}
