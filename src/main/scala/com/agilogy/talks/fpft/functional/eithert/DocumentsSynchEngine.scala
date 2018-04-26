package com.agilogy.talks.fpft.functional.eithert

import com.agilogy.talks.fpft.domain._
import com.agilogy.talks.fpft.infrastructure.SideEffects.{Connection, DataSource, SqlException}
import com.agilogy.talks.fpft.utils.EitherUtils._
import scalaz._
import Scalaz._

import scala.language.higherKinds

object DocumentsSynchEngine extends Documents[Id] {

  override type Transaction = Connection

  override implicit def M: Monad[Id] = new Monad[Id] {
    override def point[A](a: => A): Id[A] = a

    override def bind[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)
  }


  def transactionController(ds: DataSource): DocumentsSynchEngine.TransactionController = new TransactionController {

    override def begin(): Action[ConnectionError, Transaction] = {
      try {
        val conn = ds.getConnection()
        conn.beginTransaction()
        Action(right(conn))
      } catch {
        case s: SqlException => Action(left(ConnectionError(s)))
      }
    }

    override def commit(tx: Connection): Action[ConnectionError, Unit] = {
      try {
        Action(right(tx.commit()))
      } catch {
        case e: SqlException => Action(left(ConnectionError(e)))
      }
    }

    override def rollback(tx: Connection): Action[ConnectionError, Unit] = {
      try {
        Action(right(tx.rollback()))
      } catch {
        case e: SqlException => Action(left(ConnectionError(e)))
      }
    }
  }

  val documentRepository: DocumentsSynchEngine.DocumentRepository = new DocumentRepository {

    def getDocument(documentId: DocumentId): TxAction[ConnectionError, Option[Document]] = TxAction {
      conn =>
        try {
          Action(right(conn.executeSelect(s"select * from documents where id = '${documentId.id}'").headOption))
        } catch {
          case e: SqlException => Action(left(ConnectionError(e)))
        }
    }

    def insertDocument(document: Document): TxAction[InsertDocumentError, Unit] = TxAction {
      conn =>
        try {
          Action(right(conn.executeUpdate(s"insert into documents(id, content) values ('${document.id.id}', '${document.content}')")).right.map(_ => ()))
        } catch {
          case SqlException(msg) if msg.startsWith("Duplicate key") => Action(left(DocumentAlreadyExists(document.id)))
          case e: SqlException => Action(left(ConnectionError(e)))
        }
    }

    def updateDocument(document: Document): TxAction[UpdateDocumentError, Unit] = TxAction {
      conn =>
        try {
          val updatedRows = conn.executeUpdate(s"update documents set content = '${document.content}' where id = '${document.id.id}'")
          if (updatedRows == 0) Action(left(DocumentNotFound(document.id)))
          else Action(right(()))
        } catch {
          case e: SqlException => Action(left(ConnectionError(e)))
        }
    }
  }

}
