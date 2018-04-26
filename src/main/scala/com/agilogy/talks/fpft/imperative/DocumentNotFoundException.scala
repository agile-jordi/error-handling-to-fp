package com.agilogy.talks.fpft.imperative

import com.agilogy.talks.fpft.domain.DocumentId

final case class DocumentNotFoundException(id:DocumentId) extends RuntimeException{
  override def getMessage: String = s"Document $id not found"
}

