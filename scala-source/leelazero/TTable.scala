package leelazero

/** Transposition entry */
class TTEntry() {
  var hash: Long = 0
  var visits: Int = 0
  var evalSum: Double = 0.0
}


object TTable {
  val TRANS_TABLE: TTable = new TTable()
}

/** Transposition table */
class TTable(size: Int = 500000) {
  var komi: Float = 0
  var buckets: Array[TTEntry] = Array.ofDim[TTEntry](size)

  def update(hash: Long, komi: Float, node: UctNode): Unit = this.synchronized {
    var index: Int = hash.toInt
    index %= buckets.length

    // update TT
    buckets(index).hash = hash
    buckets(index).visits = node.visits    //getVisits()
    buckets(index).evalSum = node.blackEvals  // getBlackEvals()

    if (this.komi != komi) {
      buckets = Array.ofDim[TTEntry](buckets.length)
      this.komi = komi
    }
  }

  def sync(hash: Long, komi: Float, node: UctNode): Unit = this.synchronized {
    var index: Int = hash.toInt
    index %= buckets.length

    // check for hash fail
    if (buckets(index).hash != hash || this.komi != komi) {
      return
    }

    // valid entry in TT should have more info than tree
    if (buckets(index).visits > node.visits) { // getVisits()) {
      // entry in TT has more info (new node)
      node.visits = buckets(index).visits
      node.blackEvals = buckets(index).evalSum
    }
  }
}