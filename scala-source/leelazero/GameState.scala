package leelazero

// placeholder for now
class GameState(size: Short, komi: Float) extends KoState(size, komi) {

  private var gameHistory: KoState = _
  private var timeControl: TimeControl = _
}

/*
    std::vector<std::shared_ptr<KoState>> game_history;
    TimeControl m_timecontrol;


void GameState::init_game(int size, float komi) {
    KoState::init_game(size, komi);

    game_history.clear();
    game_history.emplace_back(std::make_shared<KoState>(*this));

    m_timecontrol.set_boardsize(board.get_boardsize());
    m_timecontrol.reset_clocks();

    return;
};

void GameState::reset_game() {
    KoState::reset_game();

    game_history.clear();
    game_history.emplace_back(std::make_shared<KoState>(*this));

    m_timecontrol.reset_clocks();
}

bool GameState::forward_move(void) {
    if (game_history.size() > m_movenum + 1) {
        m_movenum++;
        *(static_cast<KoState*>(this)) = *game_history[m_movenum];
        return true;
    } else {
        return false;
    }
}

bool GameState::undo_move(void) {
    if (m_movenum > 0) {
        m_movenum--;

        // don't actually delete it!
        //game_history.pop_back();

        // this is not so nice, but it should work
        *(static_cast<KoState*>(this)) = *game_history[m_movenum];

        // This also restores hashes as they're part of state
        return true;
    } else {
        return false;
    }
}
 */