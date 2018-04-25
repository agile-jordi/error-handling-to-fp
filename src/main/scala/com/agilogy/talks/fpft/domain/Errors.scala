package com.agilogy.talks.fpft.domain

import com.agilogy.talks.fpft.infrastructure.SideEffects.SqlException

sealed trait InsertDocumentError

sealed trait UpdateDocumentError

final case class ConnectionError(sqlException: SqlException) extends InsertDocumentError with UpdateDocumentError

final case class DocumentAlreadyExists(id: DocumentId) extends InsertDocumentError

final case class DocumentNotFound(id: DocumentId) extends UpdateDocumentError

