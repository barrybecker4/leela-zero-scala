package leelazero.uct

object SearchResult {
  val DEFAULT = SearchResult(valid = false)

  def fromScore(score: Float): SearchResult = {
    if (score > 0.0f) SearchResult(1.0f)
    else if (score < 0.0f) SearchResult()
    else SearchResult(0.5f)
  }
}

case class SearchResult(eval: Float = 0.0f, valid: Boolean = true )
