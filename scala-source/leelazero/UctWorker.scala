package leelazero

class UctWorker(history: GameHistory, search: UctSearch, root: UctNode) {
  operator()

  def operator(): Unit = {
    do {
      val historyCopy = history.copy()
      val result = search.playSimulation(historyCopy, root)
      if (result.valid) {
        search.incrementPlayouts()
      }
    } while (search.isRunning && !search.playoutLimitReached())
  }

}
/*
class UCTWorker {
public:
    UCTWorker(GameState & state, UCTSearch * search, UCTNode * root)
      : m_rootstate(state), m_search(search), m_root(root) {};
    void operator()();
private:
    GameState & m_rootstate;
    UCTSearch * m_search;
    UCTNode * m_root;
};

void UCTWorker::operator()() {
    do {
        auto currstate = std::make_unique<GameState>(m_rootstate);
        auto result = m_search->play_simulation(*currstate, m_root);
        if (result.valid()) {
            m_search->increment_playouts();
        }
    } while(m_search->is_running() && !m_search->playout_limit_reached());
}

 */