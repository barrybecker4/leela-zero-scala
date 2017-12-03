package leelazero

import SgfTree._
import FastBoard.EMPTY
import FastBoardSerializer._


object SgfTree {
  /** End-Of-Tree marker */
  val EOT: Short = 0
}

class SgfTree {

  private var initialized = false
  private var koState: KoState = _
  private var winner: Byte = FastBoard.INVALID // no winner initially
  private var children: Seq[SgfTree] = Seq()
  private var properties: Map[String, String] = _

  /** Initialize defaults. The SGF might be missing boardsize or komi which means we'll never initialize properly */
  def initState(): Unit = {
    koState = new GameState(19, 7.5f)
    initialized = true
  }

  def getState: KoState = {
    assert(initialized)
    koState
  }

  def getChild(count: Short): SgfTree = {
    if (count < children.length) {
      assert(initialized)
      children(count)
    } else null
  }

  /**
    * This follows the entire line, and doesn't really need the intermediate states,
    * just the moves. As a consequence, states that contain more than just moves won't have any effect.
    */
  def followMainlineState(movenum: Int): GameState = {
    var link: SgfTree = this
    // This initializes a starting state from a KoState and sets up the game history.
    val result: GameState = getState.asInstanceOf[GameState]

    var i = 0
    while (i <= movenum && link != null) {
      // root position has no associated move
      if (i != 0) {
        val move: Short = link.getMove(result.getToMove)
        if (move != EOT) {
          if (move != PASS && move != EMPTY
            && result.getBoard.getSquare(move) != EMPTY) {
            // Fail loading
            return result
          }
          result.playMove(move)
        }
      }
      link = link.getChild(0)
      i += 1
    }

    result
  }

  def getStateFromMainline(movenum: Int): KoState = {
    var link = this
    var last = this
    var i = 0
    while (i <= movenum && link != null) {
      link = link.getChild(0)
      if (link == null) {
        return last.getState
      } else {
        last = link
      }
      i += 1
    }
    link.getState
  }

  /** @return the number of states is one more than the number of moves */
  def countMainlineMoves(): Int = {
    var link = this
    var count = -1
    while (link != null) {
      link = link.getChild(0)
      count += 1
    }
    count
  }

  def loadFromString(gameBuff: String): SgfTree = {
    //std::istringstream pstream(gameBuff)

    //SGFParser.parse(pstream, this)
    SgfParser.parse(gameBuff, this) // loads properties with moves

    initState() // Set up the root state to defaults

    // populates the states from the moves; split this up in root node, achor (handicap), other nodes
    populateStates()
  }

  def loadFromFile(fileName: String, index: Int): SgfTree = {
    val gameBuff: String = SgfParser.chopFromFile(fileName, index)
    loadFromString(gameBuff)
  }

  def populateStates(): SgfTree = {
    null // long method
  }

  def getMove(toMove: Byte): Short = EOT   // FIXME
}

/**

void SGFTree::populate_states(void) {
    PropertyMap::iterator it;
    bool valid_size = false;
    bool has_handicap = false;

    // first check for go game setup in properties
    it = m_properties.find("GM");
    if (it != m_properties.end()) {
        if (it->second != "1") {
            throw std::runtime_error("SGF Game is not a Go game");
        } else {
            if (!m_properties.count("SZ")) {
                // No size, but SGF spec defines default size for Go
                m_properties.insert(std::make_pair("SZ", "19"));
                valid_size = true;
            }
        }
    }

    // board size
    it = m_properties.find("SZ");
    if (it != m_properties.end()) {
        std::string size = it->second;
        std::istringstream strm(size);
        int bsize;
        strm >> bsize;
        if (bsize <= FastBoard::MAXBOARDSIZE) {
            // Assume 7.5 komi if not specified
            m_state.init_game(bsize, 7.5f);
            valid_size = true;
        } else {
            throw std::runtime_error("Board size not supported.");
        }
    }

    // komi
    it = m_properties.find("KM");
    if (it != m_properties.end()) {
        std::string foo = it->second;
        std::istringstream strm(foo);
        float komi;
        strm >> komi;
        int handicap = m_state.get_handicap();
        // last ditch effort: if no GM or SZ, assume 19x19 Go here
        int bsize = 19;
        if (valid_size) {
            bsize = m_state.board.get_boardsize();
        }
        m_state.init_game(bsize, komi);
        m_state.set_handicap(handicap);
    }

    // handicap
    it = m_properties.find("HA");
    if (it != m_properties.end()) {
        std::string size = it->second;
        std::istringstream strm(size);
        float handicap;
        strm >> handicap;
        has_handicap = (handicap > 0.0f);
        m_state.set_handicap((int)handicap);
    }

    // result
    it = m_properties.find("RE");
    if (it != m_properties.end()) {
        std::string result = it->second;
        if (boost::algorithm::find_first(result, "Time")) {
            // std::cerr << "Skipping: " << result << std::endl;
            m_winner = FastBoard::EMPTY;
        } else {
            if (boost::algorithm::starts_with(result, "W+")) {
                m_winner = FastBoard::WHITE;
            } else if (boost::algorithm::starts_with(result, "B+")) {
                m_winner = FastBoard::BLACK;
            } else {
                m_winner = FastBoard::INVAL;
                // std::cerr << "Could not parse game result: " << result << std::endl;
            }
        }
    } else {
        m_winner = FastBoard::EMPTY;
    }

    // handicap stones
    auto prop_pair_ab = m_properties.equal_range("AB");
    // Do we have a handicap specified but no handicap stones placed in
    // the same node? Then the SGF file is corrupt. Let's see if we can find
    // them in the next node, which is a common bug in some Go apps.
    if (has_handicap && prop_pair_ab.first == prop_pair_ab.second) {
        if (!m_children.empty()) {
            auto& successor = m_children[0];
            prop_pair_ab = successor.m_properties.equal_range("AB");
        }
    }
    // Loop through the stone list and apply
    for (auto it = prop_pair_ab.first; it != prop_pair_ab.second; ++it) {
        auto move = it->second;
        int vtx = string_to_vertex(move);
        apply_move(FastBoard::BLACK, vtx);
    }

    // XXX: count handicap stones
    const auto& prop_pair_aw = m_properties.equal_range("AW");
    for (auto it = prop_pair_aw.first; it != prop_pair_aw.second; ++it) {
        auto move = it->second;
        int vtx = string_to_vertex(move);
        apply_move(FastBoard::WHITE, vtx);
    }

    it = m_properties.find("PL");
    if (it != m_properties.end()) {
        std::string who = it->second;
        if (who == "W") {
            m_state.set_to_move(FastBoard::WHITE);
        } else if (who == "B") {
            m_state.set_to_move(FastBoard::BLACK);
        }
    }

    // now for all children play out the moves
    for (auto& child_state : m_children) {
        // propagate state
        child_state.copy_state(*this);

        // XXX: maybe move this to the recursive call
        // get move for side to move
        int move = child_state.get_move(m_state.get_to_move());
        if (move != EOT) {
            child_state.apply_move(move);
        }

        child_state.populate_states();
    }
}

void SGFTree::copy_state(const SGFTree& tree) {
    m_initialized = tree.m_initialized;
    m_state = tree.m_state;
}

void SGFTree::apply_move(int color, int move) {
    if (move != FastBoard::PASS && move != FastBoard::RESIGN) {
        int curr_sq = m_state.board.get_square(move);
        if (curr_sq == !color || curr_sq == FastBoard::INVAL) {
            throw std::runtime_error("Illegal move");
        }
        // Playing on an occupied square is legal in SGF setup,
        // but we can't really handle it. So just ignore and hope that works.
        if (curr_sq == color) {
            return;
        }
        assert(curr_sq == FastBoard::EMPTY);
    }
    m_state.play_move(color, move);
}

void SGFTree::apply_move(int move) {
    int color = m_state.get_to_move();
    apply_move(color, move);
}

void SGFTree::add_property(std::string property, std::string value) {
    m_properties.emplace(property, value);
}

SGFTree * SGFTree::add_child() {
    // first allocation is better small
    if (m_children.size() == 0) {
        m_children.reserve(1);
    }
    m_children.emplace_back();
    return &(m_children.back());
}

int SGFTree::string_to_vertex(const std::string& movestring) const {
    if (movestring.size() == 0) {
        return FastBoard::PASS;
    }

    if (m_state.board.get_boardsize() <= 19) {
        if (movestring == "tt") {
            return FastBoard::PASS;
        }
    }

    int bsize = m_state.board.get_boardsize();
    if (bsize == 0) {
        throw std::runtime_error("Node has 0 sized board");
    }

    char c1 = movestring[0];
    char c2 = movestring[1];

    int cc1;
    int cc2;

    if (c1 >= 'A' && c1 <= 'Z') {
        cc1 = 26 + c1 - 'A';
    } else {
        cc1 = c1 - 'a';
    }
    if (c2 >= 'A' && c2 <= 'Z') {
        cc2 = bsize - 26 - (c2 - 'A') - 1;
    } else {
        cc2 = bsize - (c2 - 'a') - 1;
    }

    // catch illegal SGF
    if (cc1 < 0 || cc1 >= bsize
        || cc2 < 0 || cc2 >= bsize) {
        throw std::runtime_error("Illegal SGF move");
    }

    int vtx = m_state.board.get_vertex(cc1, cc2);

    return vtx;
}

int SGFTree::get_move(int tomove) {
    std::string movestring;

    if (tomove == FastBoard::BLACK) {
        movestring = "B";
    } else {
        movestring = "W";
    }

    PropertyMap::iterator it;
    it = m_properties.find(movestring);

    if (it != m_properties.end()) {
        std::string movestring = it->second;
        return string_to_vertex(movestring);
    }

    return SGFTree::EOT;
}

FastBoard::square_t SGFTree::get_winner() {
    return m_winner;
}

std::vector<int> SGFTree::get_mainline() {
    std::vector<int> moves;

    SGFTree * link = this;
    int tomove = link->m_state.get_to_move();
    link = link->get_child(0);

    while (link != nullptr && link->is_initialized()) {
        int move = link->get_move(tomove);
        if (move != SGFTree::EOT) {
            moves.push_back(move);
        }
        tomove = !tomove;
        link = link->get_child(0);
    }

    return moves;
}

std::string SGFTree::state_to_string(GameState& pstate, int compcolor) {
    auto state = std::make_unique<GameState>();

    // make a working copy
  *state = pstate;

    std::string header;
    std::string moves;

    float komi = state->get_komi();
    int size = state->board.get_boardsize();
    time_t now;
    time(&now);
    char timestr[sizeof "2017-10-16"];
    strftime(timestr, sizeof timestr, "%F", localtime(&now));

    header.append("(;GM[1]FF[4]RU[Chinese]");
    header.append("DT[" + std::string(timestr) + "]");
    header.append("SZ[" + std::to_string(size) + "]");
    header.append("KM[" + str(boost::format("%.1f") % komi) + "]");

    auto leela_name = std::string{PROGRAM_NAME};
    leela_name.append(" " + std::string(PROGRAM_VERSION));
    if (!cfg_weightsfile.empty()) {
        leela_name.append(" " + cfg_weightsfile.substr(0, 8));
    }

    if (compcolor == FastBoard::WHITE) {
        header.append("PW[" + leela_name + "]");
        header.append("PB[Human]");
    } else {
        header.append("PB[" + leela_name + "]");
        header.append("PW[Human]");
    }

    state->rewind();

    // check handicap here (anchor point)
    int handicap = 0;
    std::string handicapstr;

    for (int i = 0; i < size; i++) {
        for (int j = 0; j < size; j++) {
            int vertex = state->board.get_vertex(i, j);
            int square = state->board.get_square(vertex);

            if (square == FastBoard::BLACK) {
                handicap++;
                handicapstr.append("[" + state->board.move_to_text_sgf(vertex) + "]");
            }
        }
    }

    if (handicap > 0) {
        header.append("HA[" + std::to_string(handicap) + "]");
        moves.append("AB" + handicapstr);
    }

    moves.append("\n");

    int counter = 0;

    while (state->forward_move()) {
        int move = state->get_last_move();
        if (move == FastBoard::RESIGN) {
            break;
        }
        std::string movestr = state->board.move_to_text_sgf(move);
        if (state->get_to_move() == FastBoard::BLACK) {
            moves.append(";W[" + movestr + "]");
        } else {
            moves.append(";B[" + movestr + "]");
        }
        if (++counter % 10 == 0) {
            moves.append("\n");
        }
    }

    if (state->get_last_move() != FastBoard::RESIGN) {
        float score = state->final_score();

        if (score > 0.0f) {
            header.append("RE[B+" + str(boost::format("%.1f") % score) + "]");
        } else {
            header.append("RE[W+" + str(boost::format("%.1f") % -score) + "]");
        }
    } else {
        // Last move was resign, so side to move won
        if (state->get_to_move() == FastBoard::BLACK) {
            header.append("RE[B+Resign]");
        } else {
            header.append("RE[W+Resign]");
        }
    }

    std::string result(header);
    result.append("\n");
    result.append(moves);
    result.append(")\n");

    return result;
}
  */