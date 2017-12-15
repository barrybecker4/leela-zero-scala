package leelazero

import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}

import FastBoardSerializer.PASS
import TTable.TRANS_TABLE
import UctSearch._
import SearchResult._


object UctSearch {

  // Depending on rule set and state of the game, we might prefer to pass, or we might prefer not to pass
  // unless it's the last resort. Same for resigning.
  val NORMAL: Int = 0
  val NO_PASS: Int = 1 << 0
  val NO_RESIGN: Int = 1 << 1

  /** Maximum size of the tree in memory. Nodes are about 40 bytes, so limit to ~1.6G. */
  val MAX_TREE_SIZE: Int = 40000000
}

/** Upper Confidence Tree search */
class UctSearch(rootState: KoState) {

  private var maxPlayouts: Int = 0
  private var nodes: AtomicInteger = new AtomicInteger(0)  // Atomic
  private var run = new AtomicBoolean(false)
  private var playouts = new AtomicInteger(0)


  setPlayoutLimit(Config.cfg_max_playouts)

  def playSimulation(history: GameHistory, node: UctNode): SearchResult = {
    val currState: KoState = history.getCurrentState
    val color = currState.getToMove
    val hash = currState.getHash
    val komi = currState.komi

    var result = SearchResult.DEFAULT

    TRANS_TABLE.sync(hash, komi, node)
    node.doVirtualLoss()

    if (!node.hasChildren && nodes.get() < MAX_TREE_SIZE) {

      val (eval, success) = node.createChildren(nodes.get, history)
      if (success) {
        result = new SearchResult(eval)
      } else if (currState.getPasses >= 2) {
        val score = currState.finalScore
        result = SearchResult.fromScore(score)
      }
    }

    if (node.hasChildren && !result.valid) {
      val next = node.uctSelectChild(color)

      if (next != null) {
        val move = next.getMove

        if (move != PASS) {
          currState.playMove(move)

          if (!currState.superKo()) {
            result = playSimulation(history, next)
          } else {
            next.invalidate()
          }
        } else {
          currState.playPass()
          result = playSimulation(history, next)
        }
      }
    }

    if (result.valid) {
      node.update(result.eval)
    }
    node.undoVirtualLoss()
    TRANS_TABLE.update(hash, komi, node)

    result
  }

  def think(color: Byte, vertex: Short): Short = { 1 }

  def setPlayoutLimit(playouts: Int): Unit = {
    assert(playouts.getClass.isAssignableFrom(maxPlayouts.getClass),
      "Inconsistent types for playout amount.") // is_convertible<decltype(playouts), decltype(m_maxplayouts)>::value,
    if (playouts == 0) {
      maxPlayouts = Int.MaxValue /* std::numeric_limits<decltype(m_maxplayouts)>::max()*/
    } else {
      maxPlayouts = playouts
    }
  }

  def isRunning: Boolean = { run.get }
  def playoutLimitReached(): Boolean = playouts.get > maxPlayouts
  def incrementPlayouts(): Unit = { playouts.incrementAndGet() }
}

/*

void UCTSearch::dump_stats(KoState & state, UCTNode & parent) {
    if (cfg_quiet || !parent.has_children()) {
        return;
    }

    const int color = state.get_to_move();

    // sort children, put best move on top
    m_root.sort_root_children(color);

    UCTNode * bestnode = parent.get_first_child();

    if (bestnode->first_visit()) {
        return;
    }

    int movecount = 0;
    UCTNode * node = bestnode;

    while (node != nullptr) {
        if (++movecount > 2 && !node->get_visits()) break;

        std::string tmp = state.move_to_text(node->get_move());
        std::string pvstring(tmp);

        myprintf("%4s -> %7d (V: %5.2f%%) (N: %4.1f%%) PV: ",
            tmp.c_str(),
            node->get_visits(),
            node->get_visits() > 0 ? node->get_eval(color)*100.0f : 0.0f,
            node->get_score() * 100.0f);

        KoState tmpstate = state;

        tmpstate.play_move(node->get_move());
        pvstring += " " + get_pv(tmpstate, *node);

        myprintf("%s\n", pvstring.c_str());

        node = node->get_sibling();
    }
}

int UCTSearch::get_best_move(passflag_t passflag) {
    int color = m_rootstate.board.get_to_move();

    // Make sure best is first
    m_root.sort_root_children(color);

    // Check whether to randomize the best move proportional
    // to the playout counts, early game only.
    auto movenum = int(m_rootstate.get_movenum());
    if (movenum < cfg_random_cnt) {
        m_root.randomize_first_proportionally();
    }

    int bestmove = m_root.get_first_child()->get_move();

    // do we have statistics on the moves?
    if (m_root.get_first_child() != nullptr) {
        if (m_root.get_first_child()->first_visit()) {
            return bestmove;
        }
    }

    float bestscore = m_root.get_first_child()->get_eval(color);

    // do we want to fiddle with the best move because of the rule set?
    if (passflag & UCTSearch::NOPASS) {
        // were we going to pass?
        if (bestmove == FastBoard::PASS) {
            UCTNode * nopass = m_root.get_nopass_child(m_rootstate);

            if (nopass != nullptr) {
                myprintf("Preferring not to pass.\n");
                bestmove = nopass->get_move();
                if (nopass->first_visit()) {
                    bestscore = 1.0f;
                } else {
                    bestscore = nopass->get_eval(color);
                }
            } else {
                myprintf("Pass is the only acceptable move.\n");
            }
        }
    } else {
        if (!cfg_dumbpass && bestmove == FastBoard::PASS) {
            // Either by forcing or coincidence passing is
            // on top...check whether passing loses instantly
            // do full count including dead stones.
            // In a reinforcement learning setup, it is possible for the
            // network to learn that, after passing in the tree, the two last
            // positions are identical, and this means the position is only won
            // if there are no dead stones in our own territory (because we use
            // Trump-Taylor scoring there). So strictly speaking, the next
            // heuristic isn't required for a pure RL network, and we have
            // a commandline option to disable the behavior during learning.
            // On the other hand, with a supervised learning setup, we fully
            // expect that the engine will pass out anything that looks like
            // a finished game even with dead stones on the board (because the
            // training games were using scoring with dead stone removal).
            // So in order to play games with a SL network, we need this
            // heuristic so the engine can "clean up" the board. It will still
            // only clean up the bare necessity to win. For full dead stone
            // removal, kgs-genmove_cleanup and the NOPASS mode must be used.
            float score = m_rootstate.final_score();
            // Do we lose by passing?
            if ((score > 0.0f && color == FastBoard::WHITE)
                ||
                (score < 0.0f && color == FastBoard::BLACK)) {
                myprintf("Passing loses :-(\n");
                // Find a valid non-pass move.
                UCTNode * nopass = m_root.get_nopass_child(m_rootstate);
                if (nopass != nullptr) {
                    myprintf("Avoiding pass because it loses.\n");
                    bestmove = nopass->get_move();
                    if (nopass->first_visit()) {
                        bestscore = 1.0f;
                    } else {
                        bestscore = nopass->get_eval(color);
                    }
                } else {
                    myprintf("No alternative to passing.\n");
                }
            } else {
                myprintf("Passing wins :-)\n");
            }
        } else if (!cfg_dumbpass
                   && m_rootstate.get_last_move() == FastBoard::PASS) {
            // Opponents last move was passing.
            // We didn't consider passing. Should we have and
            // end the game immediately?
            float score = m_rootstate.final_score();
            // do we lose by passing?
            if ((score > 0.0f && color == FastBoard::WHITE)
                ||
                (score < 0.0f && color == FastBoard::BLACK)) {
                myprintf("Passing loses, I'll play on.\n");
            } else {
                myprintf("Passing wins, I'll pass out.\n");
                bestmove = FastBoard::PASS;
            }
        }
    }

    int visits = m_root.get_first_child()->get_visits();

    // if we aren't passing, should we consider resigning?
    if (bestmove != FastBoard::PASS) {
        // resigning allowed
        if ((passflag & UCTSearch::NORESIGN) == 0) {
            size_t movetresh = (m_rootstate.board.get_boardsize()
                                * m_rootstate.board.get_boardsize()) / 4;
            // bad score and visited enough
            if (bestscore < ((float)cfg_resignpct / 100.0f)
                && visits > 100
                && m_rootstate.m_movenum > movetresh) {
                myprintf("Score looks bad. Resigning.\n");
                bestmove = FastBoard::RESIGN;
            }
        }
    }

    return bestmove;
}

std::string UCTSearch::get_pv(KoState & state, UCTNode & parent) {
    if (!parent.has_children()) {
        return std::string();
    }

    auto best_child = parent.get_best_root_child(state.get_to_move());
    auto best_move = best_child->get_move();
    auto res = state.move_to_text(best_move);

    state.play_move(best_move);

    auto next = get_pv(state, *best_child);
    if (!next.empty()) {
        res.append(" ").append(next);
    }
    return res;
}

void UCTSearch::dump_analysis(int playouts) {
    if (cfg_quiet) {
        return;
    }

    GameState tempstate = m_rootstate;
    int color = tempstate.board.get_to_move();

    std::string pvstring = get_pv(tempstate, m_root);
    float winrate = 100.0f * m_root.get_eval(color);
    myprintf("Playouts: %d, Win: %5.2f%%, PV: %s\n",
             playouts, winrate, pvstring.c_str());
}

void UCTWorker::operator()() {
    do {
        auto currstate = std::make_unique<GameState>(m_rootstate);
        auto result = m_search->play_simulation(*currstate, m_root);
        if (result.valid()) {
            m_search->increment_playouts();
        }
    } while(m_search->is_running() && !m_search->playout_limit_reached());
}

int UCTSearch::think(int color, passflag_t passflag) {
    assert(m_playouts == 0);
    assert(m_nodes == 0);

    // Start counting time for us
    m_rootstate.start_clock(color);

    // set side to move
    m_rootstate.board.set_to_move(color);

    // set up timing info
    Time start;

    m_rootstate.get_timecontrol().set_boardsize(m_rootstate.board.get_boardsize());
    auto time_for_move = m_rootstate.get_timecontrol().max_time_for_move(color);

    myprintf("Thinking at most %.1f seconds...\n", time_for_move/100.0f);

    // create a sorted list off legal moves (make sure we
    // play something legal and decent even in time trouble)
    float root_eval;
    m_root.create_children(m_nodes, m_rootstate, root_eval);
    m_root.kill_superkos(m_rootstate);
    if (cfg_noise) {
        m_root.dirichlet_noise(0.25f, 0.03f);
    }

    myprintf("NN eval=%f\n",
             (color == FastBoard::BLACK ? root_eval : 1.0f - root_eval));

    m_run = true;
    int cpus = cfg_num_threads;
    ThreadGroup tg(thread_pool);
    for (int i = 1; i < cpus; i++) {
        tg.add_task(UCTWorker(m_rootstate, this, &m_root));
    }

    bool keeprunning = true;
    int last_update = 0;
    do {
        auto currstate = std::make_unique<GameState>(m_rootstate);

        auto result = play_simulation(*currstate, &m_root);
        if (result.valid()) {
            increment_playouts();
        }

        Time elapsed;
        int centiseconds_elapsed = Time::timediff(start, elapsed);

        // output some stats every few seconds
        // check if we should still search
        if (centiseconds_elapsed - last_update > 250) {
            last_update = centiseconds_elapsed;
            dump_analysis(static_cast<int>(m_playouts));
        }
        keeprunning  = is_running();
        keeprunning &= (centiseconds_elapsed < time_for_move);
        keeprunning &= !playout_limit_reached();
    } while(keeprunning);

    // stop the search
    m_run = false;
    tg.wait_all();
    m_rootstate.stop_clock(color);
    if (!m_root.has_children()) {
        return FastBoard::PASS;
    }

    // display search info
    myprintf("\n");

    dump_stats(m_rootstate, m_root);
    Training::record(m_rootstate, m_root);

    Time elapsed;
    int centiseconds_elapsed = Time::timediff(start, elapsed);
    if (centiseconds_elapsed > 0) {
        myprintf("%d visits, %d nodes, %d playouts, %d n/s\n\n",
                 m_root.get_visits(),
                 static_cast<int>(m_nodes),
                 static_cast<int>(m_playouts),
                 (m_playouts * 100) / (centiseconds_elapsed+1));
    }
    int bestmove = get_best_move(passflag);
    return bestmove;
}

void UCTSearch::ponder() {
    assert(m_playouts == 0);
    assert(m_nodes == 0);

    m_run = true;
    int cpus = cfg_num_threads;
    ThreadGroup tg(thread_pool);
    for (int i = 1; i < cpus; i++) {
        tg.add_task(UCTWorker(m_rootstate, this, &m_root));
    }
    do {
        auto currstate = std::make_unique<GameState>(m_rootstate);
        auto result = play_simulation(*currstate, &m_root);
        if (result.valid()) {
            increment_playouts();
        }
    } while(!Utils::input_pending() && is_running());

    // stop the search
    m_run = false;
    tg.wait_all();
    // display search info
    myprintf("\n");
    dump_stats(m_rootstate, m_root);

    myprintf("\n%d visits, %d nodes\n\n", m_root.get_visits(), (int)m_nodes);
}

void UCTSearch::set_playout_limit(int playouts) {
    static_assert(std::is_convertible<decltype(playouts),
                                      decltype(m_maxplayouts)>::value,
                  "Inconsistent types for playout amount.");
    if (playouts == 0) {
        m_maxplayouts = std::numeric_limits<decltype(m_maxplayouts)>::max();
    } else {
        m_maxplayouts = playouts;
    }
}


private:
    void dump_stats(KoState & state, UCTNode & parent);
    std::string get_pv(KoState & state, UCTNode & parent);
    void dump_analysis(int playouts);
    int get_best_move(passflag_t passflag);

    GameState & m_rootstate;
    UCTNode m_root{FastBoard::PASS, 0.0f};
    std::atomic<int> m_nodes{0};
    std::atomic<int> m_playouts{0};
    std::atomic<bool> m_run{false};
    int m_maxplayouts;
};
 */
