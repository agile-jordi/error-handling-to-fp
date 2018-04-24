package com.agilogy.talks.fpft.infrastructure

import com.agilogy.talks.fpft.domain.{Document, DocumentId}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

@SuppressWarnings(Array("org.wartremover.warts.Var", "org.wartremover.warts.NonUnitStatements", "org.wartremover.warts.Throw", "org.wartremover.warts.MutableDataStructures"))
object SideEffects {

  private val documents: mutable.HashMap[String, String] = mutable.HashMap.empty
  private val effects: ListBuffer[String] = ListBuffer.empty
  private var lastConnectionId: Int = 0

  def getLogs(since: Int = 0): List[String] = {
    effects.toList.slice(since, effects.size)
  }

  def reset(initialDocuments: String*): Unit = {
    effects.clear()
    documents.clear()
    lastConnectionId = 0
    initialDocuments.zipWithIndex.foreach {
      case (doc, idx) => documents.put(idx.toString, doc)
    }
  }

  def sideEffect(description: String): Unit = {
    effects += description
    println(description)
  }

  final case class SqlException(message: String) extends RuntimeException {
    override def getMessage: String = message
  }

  final case class Connection(connectionId: Int) {

    def beginTransaction(): Unit = {
      sideEffect(s"[$this] - Begin")
    }

    def commit(): Unit = sideEffect(s"[$this] - Commit")

    def rollback(): Unit = {
      sideEffect(s"[$this] - Rollback")
    }

    private val SelectDocumentById = """select \* from documents where id = '(.+)'""".r

    def executeSelect(sql: String): Seq[Document] = {
      val res = sql match {
        case SelectDocumentById(id) => documents.get(id).toList.map(Document(DocumentId(id), _))
        case _ => throw SqlException(s"Illegal query $sql")
      }
      sideEffect(s"[$this] - $sql")
      res
    }

    private val UpdateDocument = "update documents set content = '([^']*)' where id = '(.+)'".r
    private val InsertDocument = """insert into documents\(id, content\) values \('(.+)', '([^']*)'\)""".r


    def executeUpdate(sql: String): Int = {
      sideEffect(s"[$this] - $sql")
      sql match {
        case UpdateDocument(content, id) =>
          if (documents.contains(id)) {
            documents.put(id, content)
            1
          } else {
            0
          }
        case InsertDocument(id, content) =>
          if (documents.contains(id)) throw SqlException(s"Duplicate key '$id' in documents")
          documents.put(id, content)
          1
        case _ => throw SqlException(s"Illegal query $sql")
      }
    }

    override def toString: String = s"Conn-$connectionId"
  }

  class DataSource {

    def getConnection(): Connection = {
      lastConnectionId += 1
      val res = Connection(lastConnectionId)
      sideEffect(s"[$res] - GetConnection")
      res
    }

  }

}