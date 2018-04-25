package com.agilogy.talks.fpft.functional.simple

import com.agilogy.talks.fpft.domain._
import com.agilogy.talks.fpft.infrastructure.SideEffects.{Connection, DataSource}
import com.agilogy.talks.fpft.utils.EitherUtils._
import scalaz.Monad
import scalaz.Scalaz._

import scala.language.higherKinds

object DocumentsSynchEngine extends Documents[Id] {

  override type Transaction = Connection

  override implicit def M: Monad[Id] = new Monad[Id] {
    override def point[A](a: => A): Id[A] = a

    override def bind[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)
  }


  def transactionController(ds: DataSource): TransactionController = new TransactionController {

    override def begin(): Transaction = {
      val conn = ds.getConnection()
      conn.beginTransaction()
      conn
    }

    override def commit(tx: Connection): Unit = {
      right(tx.commit())
    }

    override def rollback(tx: Connection): Unit = {
      right(tx.rollback())
    }
  }

  val documentRepository: DocumentRepository = new DocumentRepository {

    def getDocument(documentId: DocumentId): TxAction[Option[Document]] = TxAction {
      _.executeSelect(s"select * from documents where id = '${documentId.id}'").headOption
    }

    def insertDocument(document: Document): TxAction[Unit] = TxAction {
      _.executeUpdate(s"insert into documents(id, content) values ('${document.id.id}', '${document.content}')")
    }

    def updateDocument(document: Document): TxAction[Unit] = TxAction {
      conn =>
          conn.executeUpdate(s"update documents set content = '${document.content}' where id = '${document.id.id}'")
    }
  }

}
