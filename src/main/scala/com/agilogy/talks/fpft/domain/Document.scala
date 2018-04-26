package com.agilogy.talks.fpft.domain

final case class DocumentId(id: String)

final case class Document(id: DocumentId, content: String)
