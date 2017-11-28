package leelazero

import FastBoard._

class FullBoard(size: Short = MAX_BOARD_SIZE) extends FastBoard(size) {

  var hash: Long = _
  var ko_hash: Long = _
  val zobrist: Zobrist = new Zobrist(boardSize)


  def removeString(i: Short): Short = {
    var pos: Short = i
    var removed: Short = 0
    val color: Short = square(i)

    do {
      hash ^= zobrist.zobrist(square(pos))(pos)
      ko_hash ^= zobrist.zobrist(square(pos))(pos)

      square(pos) = EMPTY
      parentString(pos) = maxSq
      totalStones(color) -= 1

      removeNeighbor(pos, color)

      emptyIdx(pos) = emptyCnt
      emptySquare(emptyCnt) = pos
      emptyCnt += 1

      hash ^= zobrist.zobrist(square(pos))(pos)
      ko_hash ^= zobrist.zobrist(square(pos))(pos)

      removed += 1
      pos = next(pos)
    } while (pos != i)
    removed
  }

}

/*


uint64 FullBoard::calc_ko_hash(void) {
    uint64 res;

#ifdef _WIN32
    res = 0x1234567887654321UI64;
#else
    res = 0x1234567887654321ULL;
#endif

    for (int i = 0; i < m_maxsq; i++) {
        if (m_square[i] != INVAL) {
            res ^= Zobrist::zobrist[m_square[i]][i];
        }
    }

    ko_hash = res;

    /* Tromp-Taylor has positional superko */
    return res;
}

uint64 FullBoard::calc_hash(void) {
    uint64 res;

#ifdef _WIN32
    res = 0x1234567887654321UI64;
#else
    res = 0x1234567887654321ULL;
#endif

    for (int i = 0; i < m_maxsq; i++) {
        if (m_square[i] != INVAL) {
            res ^= Zobrist::zobrist[m_square[i]][i];
        }
    }

    /* prisoner hashing is rule set dependent */
    res ^= Zobrist::zobrist_pris[0][m_prisoners[0]];
    res ^= Zobrist::zobrist_pris[1][m_prisoners[1]];

    if (m_tomove == BLACK)
#ifdef _WIN32
        res ^= 0xABCDABCDABCDABCDUI64;
#else
        res ^= 0xABCDABCDABCDABCDULL;
#endif

    hash = res;

    return res;
}

std::array<uint64, 8> FullBoard::get_rotated_hashes(void) {
    std::array<uint64, 8> result;

    for (int sym = 0; sym < 8; sym++) {
        uint64 res = 0x1234567887654321ULL;

        for (int i = 0; i < m_maxsq; i++) {
            if (m_square[i] != INVAL) {
                int newi = rotate_vertex(i, sym);
                res ^= Zobrist::zobrist[m_square[i]][newi];
            }
        }
        /* prisoner hashing is rule set dependent */
        res ^= Zobrist::zobrist_pris[0][m_prisoners[0]];
        res ^= Zobrist::zobrist_pris[1][m_prisoners[1]];
        if (m_tomove == BLACK)
           res ^= 0xABCDABCDABCDABCDULL;
        result[sym] = res;
    }

    return result;
}

uint64 FullBoard::get_canonical_hash(void) {
    auto hashes = get_rotated_hashes();
    return *std::min_element(hashes.cbegin(), hashes.cend());
}

uint64 FullBoard::get_hash(void) {
    return hash;
}

uint64 FullBoard::get_ko_hash(void) {
    return ko_hash;
}

int FullBoard::update_board(const int color, const int i, bool &capture) {
    assert(m_square[i] == EMPTY);

    hash ^= Zobrist::zobrist[m_square[i]][i];
    ko_hash ^= Zobrist::zobrist[m_square[i]][i];

    m_square[i] = (square_t)color;
    m_next[i] = i;
    m_parent[i] = i;
    m_libs[i] = count_pliberties(i);
    m_stones[i] = 1;
    m_totalstones[color]++;

    hash ^= Zobrist::zobrist[m_square[i]][i];
    ko_hash ^= Zobrist::zobrist[m_square[i]][i];

    /* update neighbor liberties (they all lose 1) */
    add_neighbour(i, color);

    /* did we play into an opponent eye? */
    int eyeplay = (m_neighbours[i] & s_eyemask[!color]);

    int captured_sq;
    int captured_stones = 0;

    for (int k = 0; k < 4; k++) {
        int ai = i + m_dirs[k];

        if (m_square[ai] == !color) {
            if (m_libs[m_parent[ai]] <= 0) {
                int this_captured = remove_string(ai);
                captured_sq = ai;
                captured_stones += this_captured;
            }
        } else if (m_square[ai] == color) {
            int ip = m_parent[i];
            int aip = m_parent[ai];

            if (ip != aip) {
                if (m_stones[ip] >= m_stones[aip]) {
                    merge_strings(ip, aip);
                } else {
                    merge_strings(aip, ip);
                }
            }
        }
    }

    hash ^= Zobrist::zobrist_pris[color][m_prisoners[color]];
    m_prisoners[color] += captured_stones;
    hash ^= Zobrist::zobrist_pris[color][m_prisoners[color]];

    /* move last vertex in list to our position */
    int lastvertex = m_empty[--m_empty_cnt];
    m_empty_idx[lastvertex] = m_empty_idx[i];
    m_empty[m_empty_idx[i]] = lastvertex;

    /* check whether we still live (i.e. detect suicide) */
    if (m_libs[m_parent[i]] == 0) {
        assert(captured_stones == 0);
        remove_string_fast(i);
    }

    if (captured_stones) {
        capture = true;
        /* check for possible simple ko */
        if (captured_stones == 1 && eyeplay) {
            return captured_sq;
        }
    }

    return -1;
}

void FullBoard::display_board(int lastmove) {
    FastBoard::display_board(lastmove);

    myprintf("Hash: %llX Ko-Hash: %llX\n\n", hash, ko_hash);
}

void FullBoard::reset_board(int size) {
    FastBoard::reset_board(size);

    calc_hash();
    calc_ko_hash();
}

 */