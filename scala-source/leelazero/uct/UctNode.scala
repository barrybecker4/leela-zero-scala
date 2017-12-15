package leelazero.uct

import leelazero.network.Network
import leelazero.network.Network.ScoredNode
import leelazero.uct.UctNode._
import leelazero.board.GameHistory
import leelazero.board.FastBoard._
import leelazero.board.{FastBoard, GameHistory}


object UctNode {

  /**
    * When we visit a node, add this amount of virtual losses to it to encourage other CPUs to
    * explore other parts of the search tree.
    */
  val VIRTUAL_LOSS_COUNT: Int = 3
}

/**
  * A node in the UCT search tree
  * @param vertex move position represented
  * @param score from the UCT evaluation
  */
class UctNode(var vertex: Short, var score: Float) {

  var hasChildren: Boolean = false // atomic?
  var firstChild: UctNode = _
  var nextSibling: UctNode = _
  private var move: Short = _
  var visits: Int = 0
  private var virtualLoss: Int = 0
  var blackEvals: Double = 0
  private var valid = true // node alive (not superko)
  // Is someone adding scores to this node? We don't need to unset this.
  var isExpanding: Boolean = false

  def firstVisit(): Boolean = visits == 0

  def linkChild(newChild: UctNode): Unit = {
    newChild.nextSibling = firstChild
    firstChild = newChild
  }

  //SMP::Mutex & UCTNode::get_mutex() {return m_nodemutex;} // ???

  /** @return (eval, success)  */
  def createChildren(nodeCount: Int, history: GameHistory): (Float, Boolean) = {
    // check whether somebody beat us to it (atomic)
    if (hasChildren) return (0, false)
    val state = history.getCurrentState

    this.synchronized {

      // no successors in final state
      if (state.getPasses >= 2) return (0, false)

      // check whether somebody beat us to it (after taking the lock) or someone else is running the expansion
      if (hasChildren || isExpanding) return (0, false)

      // We'll be the one queueing this node for expansion, stop others
      isExpanding = true
    }

    val rawNetList = Network.getScoredMoves(history, Network.ENSEMBLE_RANDOM_ROTATION)

    // DCNN returns winrate as side to move
    var netEval: Float = rawNetList._2
    val toMove = state.board.getToMove
    // our search functions evaluate from black's point of view
    if (toMove == WHITE) {
      netEval = 1.0f - netEval
    }
    val eval: Float = netEval

    val board: FastBoard = state.board
    var nodeList: Seq[ScoredNode] = null

    for (node <- rawNetList._1) {
      val vertex: Short = node._2
      if (vertex != PASS) {
        if (vertex != state.getKoMove && !board.isSuicide(vertex, board.getToMove)) {
          nodeList :+= node
        }
      } else {
        nodeList :+= node
      }
    }
    linkNodeList(nodeCount, nodeList)

    (eval, true)
  }


  def linkNodeList(nodeCount: Int, nodeList: Seq[ScoredNode]): Unit = {
  }

  def uctSelectChild(color: Byte): UctNode = {
    null
  }

  def invalidate(): Unit = { valid = false }
  def isValid: Boolean = valid

  def getMove: Short = move
  def doVirtualLoss(): Unit = { virtualLoss += VIRTUAL_LOSS_COUNT }
  def undoVirtualLoss(): Unit = { virtualLoss -= VIRTUAL_LOSS_COUNT }

  def update(eval: Float): Unit = {
    visits += 1
    accumulateEval(eval)
  }

  def accumulateEval(eval: Float): Unit = {
    // atomicAdd(blackEvals, eval) // imple
    // atomic_add(m_blackevals, (double)eval);
  }
}
/*

void UCTNode::link_nodelist(std::atomic<int> & nodecount,
                            std::vector<Network::scored_node> & nodelist)
{
    size_t totalchildren = nodelist.size();
    if (!totalchildren)
        return;

    // sort (this will reverse scores, but linking is backwards too)
    std::sort(begin(nodelist), end(nodelist));

    // link the nodes together, we only really link the last few
    size_t maxchilds = 362;
    int childrenadded = 0;
    size_t childrenseen = 0;

    LOCK(get_mutex(), lock);

    for (const auto& node : nodelist) {
        if (totalchildren - childrenseen <= maxchilds) {
            auto vtx = new UCTNode(node.second, node.first);
            link_child(vtx);
            childrenadded++;
        }
        childrenseen++;
    }

    nodecount += childrenadded;
    m_has_children = true;
}

void UCTNode::kill_superkos(KoState & state) {
    UCTNode * child = m_firstchild;

    while (child != nullptr) {
        int move = child->get_move();

        if (move != FastBoard::PASS) {
            KoState mystate = state;
            mystate.play_move(move);

            if (mystate.superko()) {
                UCTNode * tmp = child->m_nextsibling;
                delete_child(child);
                child = tmp;
                continue;
            }
        }
        child = child->m_nextsibling;
    }
}

void UCTNode::dirichlet_noise(float epsilon, float alpha) {
    auto child = m_firstchild;
    auto child_cnt = size_t{0};

    while (child != nullptr) {
        child_cnt++;
        child = child->m_nextsibling;
    }

    auto dirichlet_vector = std::vector<float>{};

    std::gamma_distribution<float> gamma(alpha, 1.0f);
    for (size_t i = 0; i < child_cnt; i++) {
        dirichlet_vector.emplace_back(gamma(*Random::get_Rng()));
    }

    auto sample_sum = std::accumulate(begin(dirichlet_vector),
                                      end(dirichlet_vector), 0.0f);

    // If the noise vector sums to 0 or a denormal, then don't try to
    // normalize.
    if (sample_sum < std::numeric_limits<float>::min()) {
        return;
    }

    for (auto& v: dirichlet_vector) {
        v /= sample_sum;
    }

    child = m_firstchild;
    child_cnt = 0;
    while (child != nullptr) {
        auto score = child->get_score();
        auto eta_a = dirichlet_vector[child_cnt];
        score = score * (1 - epsilon) + epsilon * eta_a;
        child->set_score(score);
        child = child->m_nextsibling;
    }
}

void UCTNode::randomize_first_proportionally() {
    auto accum_vector = std::vector<uint32>{};

    auto child = m_firstchild;
    auto accum = uint32{0};
    while (child != nullptr) {
        accum += child->get_visits();
        accum_vector.emplace_back(accum);
        child = child->m_nextsibling;
    }

    auto pick = Random::get_Rng()->randuint32(accum);
    auto index = size_t{0};
    for (size_t i = 0; i < accum_vector.size(); i++) {
        if (pick < accum_vector[i]) {
            index = i;
            break;
        }
    }

    // Take the early out
    if (index == 0) {
        return;
    }

    // Now swap the child at index with the first child
    child = m_firstchild;
    auto child_cnt = size_t{0};
    while (child != nullptr) {
        // Because of the early out we can't be swapping the first
        // child. Stop at the predecessor, so we can put the nextsibling
        // pointer.
        if (index == child_cnt + 1) {
            // We stopped one early, so we should have a successor
            assert(child->m_nextsibling != nullptr);
            auto old_first = m_firstchild;
            auto old_next = child->m_nextsibling->m_nextsibling;
            // Set up links for the new first node
            m_firstchild = child->m_nextsibling;
            m_firstchild->m_nextsibling = old_first;
            // Point through our nextsibling ptr
            child->m_nextsibling = old_next;
            return;
        }
        child_cnt++;
        child = child->m_nextsibling;
    }
}

float UCTNode::get_eval(int tomove) const {
    // Due to the use of atomic updates and virtual losses, it is
    // possible for the visit count to change underneath us. Make sure
    // to return a consistent result to the caller by caching the values.
    auto visits = get_visits() + m_virtual_loss;
    auto blackeval = get_blackevals();
    if (visits > 0) {
        auto score = static_cast<float>(blackeval / (double)visits);
        if (tomove == FastBoard::WHITE) {
            score = 1.0f - score;
        }
        return score;
    } else {
        // If a node has not been visited yet,
        // the eval is the first-play-urgency.
        return 1.1f;
    }
}

double UCTNode::get_blackevals() const {
    return m_blackevals;
}

void UCTNode::set_blackevals(double blackevals) {
    m_blackevals = blackevals;
}


UCTNode* UCTNode::uct_select_child(int color) {
    UCTNode * best = nullptr;
    float best_value = -1000.0f;

    LOCK(get_mutex(), lock);
    // Progressive widening
    // int childbound = std::max(2, (int)(((log((double)get_visits()) - 3.0) * 3.0) + 2.0));
    int childbound = 362;
    int childcount = 0;
    UCTNode * child = m_firstchild;

    // Count parentvisits.
    // We do this manually to avoid issues with transpositions.
    int parentvisits = 0;
    // Make sure we are at a valid successor.
    while (child != nullptr && !child->valid()) {
        child = child->m_nextsibling;
    }
    while (child != nullptr  && childcount < childbound) {
        parentvisits      += child->get_visits();
        child = child->m_nextsibling;
        // Make sure we are at a valid successor.
        while (child != nullptr && !child->valid()) {
            child = child->m_nextsibling;
        }
        childcount++;
    }
    float numerator = std::sqrt((double)parentvisits);

    childcount = 0;
    child = m_firstchild;
    // Make sure we are at a valid successor.
    while (child != nullptr && !child->valid()) {
        child = child->m_nextsibling;
    }
    if (child == nullptr) {
        return nullptr;
    }

    // Prune bad probabilities
    // auto parent_log = std::log((float)parentvisits);
    // auto cutoff_ratio = cfg_cutoff_offset + cfg_cutoff_ratio * parent_log;
    // auto best_probability = child->get_score();
    // assert(best_probability > 0.001f);

    while (child != nullptr && childcount < childbound) {
        // Prune bad probabilities
        // if (child->get_score() * cutoff_ratio < best_probability) {
        //     break;
        // }

        // get_eval() will automatically set first-play-urgency
        float winrate = child->get_eval(color);
        float psa = child->get_score();
        float denom = 1.0f + child->get_visits();
        float puct = cfg_puct * psa * (numerator / denom);
        float value = winrate + puct;
        assert(value > -1000.0f);

        if (value > best_value) {
            best_value = value;
            best = child;
        }

        child = child->m_nextsibling;
        // Make sure we are at a valid successor.
        while (child != nullptr && !child->valid()) {
            child = child->m_nextsibling;
        }
        childcount++;
    }

    assert(best != nullptr);

    return best;
}

class NodeComp : public std::binary_function<UCTNode::sortnode_t,
                                             UCTNode::sortnode_t, bool> {
public:
    NodeComp() = default;
    // winrate, visits, score, child
    //        0,     1,     2,     3

    bool operator()(const UCTNode::sortnode_t a, const UCTNode::sortnode_t b) {
        // One node has visits, the other does not
        if (!std::get<1>(a) && std::get<1>(b)) {
            return false;
        }

        if (!std::get<1>(b) && std::get<1>(a)) {
            return true;
        }

        // Neither has visits, sort on prior score
        if (!std::get<1>(a) && !std::get<1>(b)) {
            return std::get<2>(a) > std::get<2>(b);
        }

        // Both have visits, but the same amount, prefer winrate
        if (std::get<1>(a) == std::get<1>(b)) {
            return std::get<0>(a) > std::get<0>(b);
        }

        // Both have different visits, prefer greater visits
        return std::get<1>(a) > std::get<1>(b);
    }
};

/*
    sort children by converting linked list to vector,
    sorting the vector, and reconstructing to linked list again
    Requires node mutex to be held.
*/
void UCTNode::sort_children() {
    assert(get_mutex().is_held());
    std::vector<std::tuple<float, UCTNode*>> tmp;

    UCTNode * child = m_firstchild;

    while (child != nullptr) {
        tmp.emplace_back(child->get_score(), child);
        child = child->m_nextsibling;
    }

    std::sort(begin(tmp), end(tmp));

    m_firstchild = nullptr;

    for (auto& sortnode : tmp) {
        link_child(std::get<1>(sortnode));
    }
}

void UCTNode::sort_root_children(int color) {
    LOCK(get_mutex(), lock);
    auto tmp = std::vector<sortnode_t>{};

    auto child = m_firstchild;
    while (child != nullptr) {
        auto visits = child->get_visits();
        auto score = child->get_score();
        if (visits) {
            auto winrate = child->get_eval(color);
            tmp.emplace_back(winrate, visits, score, child);
        } else {
            tmp.emplace_back(0.0f, 0, score, child);
        }
        child = child->m_nextsibling;
    }

    // reverse sort, because list reconstruction is backwards
    std::stable_sort(rbegin(tmp), rend(tmp), NodeComp());

    m_firstchild = nullptr;

    for (auto& sortnode : tmp) {
        link_child(std::get<3>(sortnode));
    }
}

/**
 * Helper function to get a sortnode_t
 * eval is set to 0 if no visits instead of first-play-urgency
 */
UCTNode::sortnode_t get_sortnode(int color, UCTNode* child) {
    auto visits = child->get_visits();
    return UCTNode::sortnode_t(
        visits == 0 ? 0.0f : child->get_eval(color),
        visits,
        child->get_score(),
        child);
}

UCTNode* UCTNode::get_best_root_child(int color) {
    LOCK(get_mutex(), lock);
    assert(m_firstchild != nullptr);

    NodeComp compare;
    auto child = m_firstchild;
    auto best_child = get_sortnode(color, child);
    while (child != nullptr) {
        auto test = get_sortnode(color, child);
        if (compare(test, best_child)) {
            best_child = test;
        }
        child = child->m_nextsibling;
    }
    return std::get<3>(best_child);
}

UCTNode* UCTNode::get_first_child() const {
    return m_firstchild;
}

UCTNode* UCTNode::get_sibling() const {
    return m_nextsibling;
}

UCTNode* UCTNode::get_pass_child() const {
    UCTNode * child = m_firstchild;

    while (child != nullptr) {
        if (child->m_move == FastBoard::PASS) {
            return child;
        }
        child = child->m_nextsibling;
    }

    return nullptr;
}

UCTNode* UCTNode::get_nopass_child(FastState& state) const {
    UCTNode * child = m_firstchild;

    while (child != nullptr) {
        /* If we prevent the engine from passing, we must bail out when
           we only have unreasonable moves to pick, like filling eyes.
           Note that this isn't knowledge isn't required by the engine,
           we require it because we're overruling its moves. */
        if (child->m_move != FastBoard::PASS
            && !state.board.is_eye(state.get_to_move(), child->m_move)) {
            return child;
        }
        child = child->m_nextsibling;
    }

    return nullptr;
}

// unsafe in SMP, we don't know if people hold pointers to the
// child which they might dereference
void UCTNode::delete_child(UCTNode * del_child) {
    LOCK(get_mutex(), lock);
    assert(del_child != nullptr);

    if (del_child == m_firstchild) {
        m_firstchild = m_firstchild->m_nextsibling;
        delete del_child;
        return;
    } else {
        UCTNode * child = m_firstchild;
        UCTNode * prev  = nullptr;

        do {
            prev  = child;
            child = child->m_nextsibling;

            if (child == del_child) {
                prev->m_nextsibling = child->m_nextsibling;
                delete del_child;
                return;
            }
        } while (child != nullptr);
    }

    assert(false && "Child to delete not found");
}
 */