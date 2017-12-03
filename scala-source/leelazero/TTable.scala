package leelazero

/** Transposition entry */
class TTEntry {
  var hash: Long = 0
  var visits: Int = 0
  var evalSum: Double = 0.0
}

//object TTable {
//  val TRANS_TABLE: TTable = new TTable(size)
//}

/** Transposition table */
class TTable(size: Int = 500000) {
  val komi: Float = 0
  var buckets: Seq[TTEntry] = Seq()

  //def update(has: Long, komi: Float, node: UctNode) = {
  //}
}

/**
void TTable::update(uint64 hash, const float komi, const UCTNode * node) {
    LOCK(m_mutex, lock);

    unsigned int index = (unsigned int)hash;
    index %= m_buckets.size();

  /* update TT  */
    m_buckets[index].m_hash       = hash;
    m_buckets[index].m_visits     = node->get_visits();
    m_buckets[index].m_eval_sum   = node->get_blackevals();

    if (m_komi != komi) {
        std::fill(begin(m_buckets), end(m_buckets), TTEntry());
        m_komi = komi;
    }
}

void TTable::sync(uint64 hash, const float komi, UCTNode * node) {
    LOCK(m_mutex, lock);

    unsigned int index = (unsigned int)hash;
    index %= m_buckets.size();

    /* check for hash fail  */
    if (m_buckets[index].m_hash != hash || m_komi != komi) {
        return;
    }

    /*
        valid entry in TT should have more info than tree
  */
    if (m_buckets[index].m_visits > node->get_visits()) {
        /*
            entry in TT has more info (new node)
  */
        node->set_visits(m_buckets[index].m_visits);
        node->set_blackevals(m_buckets[index].m_eval_sum);
    }
}
*/