package com.agilogy.talks.fpft.functional.bifunctor

import com.agilogy.talks.fpft.domain._
import com.agilogy.talks.fpft.infrastructure.SideEffects.{Connection, DataSource, SqlException}
import scalaz.{Applicative, Bifunctor, Bitraverse, Monad}
import com.agilogy.talks.fpft.utils.EitherUtils._
import scalaz._
import Scalaz._

import scala.language.higherKinds

object DocumentsSynchEngine extends Documents[Either] {

  override def M[E]: Monad[Either[E, ?]] = new Monad[Either[E,?]] {
    override def bind[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = fa.flatMap(f)

    override def point[A](a: => A): Either[E, A] = right(a)
  }

  override implicit def B: Bitraverse[Either] = new Bitraverse[Either] {

    override def bitraverseImpl[G[_], A, B, C, D](fab: Either[A, B])(f: A => G[C], g: B => G[D])(implicit evidence$1: Applicative[G]): G[Either[C, D]] = {
      fab.fold(a => f(a).map(Left.apply[C,D]), b => g(b).map(Right.apply[C,D]))
    }
  }

  override type Transaction = Connection

 def transactionController(ds: DataSource): DocumentsSynchEngine.TransactionController = new TransactionController {

    override def begin(): Either[ConnectionError, Connection] = {
      try {
        val conn = ds.getConnection()
        conn.beginTransaction()
        right(conn)
      } catch {
        case s: SqlException => left(ConnectionError(s))
      }
    }

    override def commit(tx: Connection): Either[ConnectionError, Unit] = {
      try {
        right(tx.commit())
      } catch {
        case e: SqlException => left(ConnectionError(e))
      }
    }

    override def rollback(tx: Connection): Either[ConnectionError, Unit] = {
      try {
        right(tx.rollback())
      } catch {
        case e: SqlException => left(ConnectionError(e))
      }
    }
  }

  val documentRepository: DocumentsSynchEngine.DocumentRepository = new DocumentRepository {

    def getDocument(documentId: DocumentId): TxAction[ConnectionError, Option[Document]] = TxAction {
      conn =>
        try {
          right(conn.executeSelect(s"select * from documents where id = '${documentId.id}'").headOption)
        } catch {
          case e: SqlException => left(ConnectionError(e))
        }
    }

    def insertDocument(document: Document): TxAction[InsertDocumentError, Unit] = TxAction {
      conn =>
        try {
          right(conn.executeUpdate(s"insert into documents(id, content) values ('${document.id.id}', '${document.content}')")).right.map(_ => ())
        } catch {
          case SqlException(msg) if msg.startsWith("Duplicate key") => left(DocumentAlreadyExists(document.id))
          case e: SqlException => left(ConnectionError(e))
        }
    }

    def updateDocument(document: Document): TxAction[UpdateDocumentError, Unit] = TxAction {
      conn =>
        try {
          val updatedRows = conn.executeUpdate(s"update documents set content = '${document.content}' where id = '${document.id.id}'")
          if (updatedRows == 0) left(DocumentNotFound(document.id))
          else right(())
        } catch {
          case e: SqlException => left(ConnectionError(e))
        }
    }
  }


}
