package leelazero.sgf

import org.scalatest.FunSuite
import leelazero.board.FastBoard._
import leelazero.TestUtil._


class SgfTreeSuite extends FunSuite {

  test("constructor without init") {
    val tree: SgfTree = new SgfTree()

    assertResult(INVALID) {tree.getWinner}
    assertThrows[AssertionError] {
      tree.getState
    }
  }

  test("constructor with init") {
    val tree: SgfTree = new SgfTree()
    tree.initState()

    assertResult(INVALID) {tree.getWinner}
    assertResult(true) {tree.isInitialized}
    assertResult(
      clean("""
       |Passes: 0
       |Black (X) Prisoners: 0
       |White (O) Prisoners: 0
       |Black (X) to move
       |   a b c d e f g h j k l m n o p q r s t
       |19 . . . . . . . . . . . . . . . . . . . 19
       |18 . . . . . . . . . . . . . . . . . . . 18
       |17 . . . . . . . . . . . . . . . . . . . 17
       |16 . . . + . . . . . + . . . . . + . . . 16
       |15 . . . . . . . . . . . . . . . . . . . 15
       |14 . . . . . . . . . . . . . . . . . . . 14
       |13 . . . . . . . . . . . . . . . . . . . 13
       |12 . . . . . . . . . . . . . . . . . . . 12
       |11 . . . . . . . . . . . . . . . . . . . 11
       |10 . . . + . . . . . + . . . . . + . . . 10
       | 9 . . . . . . . . . . . . . . . . . . .  9
       | 8 . . . . . . . . . . . . . . . . . . .  8
       | 7 . . . . . . . . . . . . . . . . . . .  7
       | 6 . . . . . . . . . . . . . . . . . . .  6
       | 5 . . . . . . . . . . . . . . . . . . .  5
       | 4 . . . + . . . . . + . . . . . + . . .  4
       | 3 . . . . . . . . . . . . . . . . . . .  3
       | 2 . . . . . . . . . . . . . . . . . . .  2
       | 1 . . . . . . . . . . . . . . . . . . .  1
       |   a b c d e f g h j k l m n o p q r s t
       |
       |Hash: 26f03c7022b1f5c5 Ko-Hash: c09064475382a152""")) {tree.getState.toString}
  }

  test("add child") {
    val tree: SgfTree = new SgfTree()
    tree.initState()

    assertResult(0) {tree.countMainlineMoves()}
    val child = new SgfTree()
    tree.addChild(child)

    assertResult(child) {tree.getChild(0)}
    assertResult(null) {tree.getChild(1)}
  }

  test("state to string for 2002-01-10-7 (120)") {
    val tree: SgfTree = SgfParser.loadFromFile("games/2002-01-10-7.sgf")

    assertResult(clean(
      """(;GM[1]FF[4]RU[Chinese]DT[2002-01-10]SZ[19]KM[5.5]PB[Leela-Zero-Scala 0.6]PW[Human]RE[W+1.5]
        |
        |;B[pd];W[dp];B[pp];W[dd];B[fq];W[nc];B[qf];W[jd];B[dn];W[fp]
        |;B[gp];W[fo];B[eq];W[do];B[dq];W[go];B[hp];W[dj];B[bp];W[bo]
        |;B[cp];W[co];B[cc];W[dc];B[cd];W[ce];B[be];W[bf];B[db];W[eb]
        |;B[ec];W[ed];B[cb];W[df];B[fc];W[fb];B[bd];W[gc];B[ek];W[gk]
        |;B[ej];W[di];B[ei];W[gi];B[cg];W[cf];B[dh];W[eh];B[ci];W[cj]
        |;B[bj];W[bi];B[bh];W[ch];B[ep];W[eo];B[ci];W[dg];B[ai];W[ch]
        |;B[hj];W[gj];B[dh];W[ck];B[ch];W[bk];B[ag];W[af];B[fh];W[eg]
        |;B[bg];W[fi];B[ob];W[nb];B[oc];W[ne];B[nd];W[md];B[od];W[me]
        |;B[qn];W[jo];B[jq];W[kp];B[ho];W[hn];B[in];W[io];B[hm];W[gn]
        |;B[jm];W[il];B[im];W[jk];B[lm];W[kq];B[jr];W[op];B[oq];W[nq]
        |;B[nr];W[nm];B[np];W[no];B[mq];W[mo];B[kk];W[kj];B[lk];W[nk]
        |;B[lj];W[ki];B[li];W[kh];B[ni];W[qm];B[pn];W[pm];B[on];W[mm]
        |;B[pk];W[oj];B[oi];W[pj];B[pi];W[qk];B[qi];W[rn];B[ro];W[rm]
        |;B[lh];W[lg];B[mg];W[mf];B[hl];W[hk];B[fl];W[gl];B[gm];W[fm]
        |;B[em];W[fn];B[bn];W[cn];B[cm];W[bm];B[bl];W[an];B[cl];W[al]
        |;B[dk];W[ak];B[aj];W[am];B[ap];W[dl];B[dm])
        |""".stripMargin)) {
      tree.toString
    }
  }

  test("state to string for variation_matches (120)") {
    val tree: SgfTree = SgfParser.loadFromFile("games/variation_matches.sgf")

    assertResult(clean(
      """(;GM[1]FF[4]RU[Chinese]DT[2017-12-25]SZ[19]KM[7.5]PB[Leela-Zero-Scala 0.6]PW[Human]RE[W+4.5]
        |
        |;B[pp];W[qq];B[dd];W[qp];B[pd];W[qo];B[dp];W[cc];B[cd];W[dc]
        |;B[ec];W[ed];B[bc];W[bb];B[bd];W[fc];B[eb];W[fb];B[cb];W[oe]
        |;B[fd];W[ee];B[db];W[gd];B[fe];W[ef];B[ge];W[he];B[gf];W[hd]
        |;B[hf];W[if];B[ig];W[jg];B[ih];W[jf];B[jh];W[kh];B[ki];W[lh]
        |;B[pe];W[li];B[kj];W[lj];B[kk];W[lk];B[kl];W[pf];B[of];W[qf]
        |;B[od];W[ne];B[nd];W[me];B[md];W[ld];B[le];W[og];B[nf];W[mf]
        |;B[lc];W[kd];B[kc];W[eh];B[gh];W[ej];B[ch];W[ci];B[bi];W[cj]
        |;B[dh];W[di];B[bj];W[ck];B[bk];W[cl];B[eg];W[fg];B[dg];W[fh]
        |;B[gi];W[fp];B[hp];W[ff];B[gg];W[fi];B[gj];W[fr];B[cq];W[iq]
        |;B[np];W[ip];B[ho];W[io];B[hn];W[hq];B[fj];W[fk];B[no];W[gk]
        |;B[hk];W[hl];B[gl];W[fl];B[gm];W[fm];B[fn];W[en];B[fo];W[eo]
        |;B[ep];W[hm];B[fq];W[gn];B[gp];W[in];B[gr];W[gq];B[er];W[jc]
        |;B[jb];W[ic];B[pl];W[rd];B[qc];W[rc];B[rb];W[qb];B[pb];W[qm]
        |;B[ll];W[ql];B[lq];W[ml];B[mm];W[nl];B[pm];W[pn];B[on];W[om]
        |;B[pk];W[nm];B[mn];W[qk];B[fp];W[hr];B[fs];W[ik];B[hj];W[ij]
        |;B[ji];W[il];B[ng];W[oh];B[nh];W[ni];B[oi];W[pi];B[df];W[bl]
        |;B[cn];W[bn];B[bo];W[ii];B[hi];W[kn];B[pq];W[lm];B[qr];W[km]
        |;B[jl];W[jm];B[de];W[rr];B[ei];W[pr];B[or];W[qs];B[po];W[qn]
        |;B[re];W[qe];B[qd];W[rf];B[se];W[sf];B[bm];W[cm];B[an];W[sd]
        |;B[al];W[ib];B[kq];W[jr];B[dm];W[em];B[dl];W[dk];B[kr];W[dn]
        |;B[nn];W[kb];B[lb];W[ja];B[la];W[sb];B[qa];W[co];B[bn];W[do]
        |;B[mr];W[cp];B[bp];W[bq];B[cr];W[fa];B[ko];W[jo];B[ka];W[jb]
        |;B[mk];W[nk];B[os];W[kp];B[lo];W[lp];B[mp];W[jp];B[ps];W[qr]
        |;B[jq];W[js];B[hs];W[ir];B[ks];W[el];B[go];W[gm];B[sa];W[ra]
        |;B[qb];W[ea];B[sa];W[sc];B[gc];W[gb];B[jj];W[mg];B[da];W[mh]
        |;B[ln];W[im];B[jk];W[mj];B[gs];W[ra];B[oq];W[sa];B[is];W[bh]
        |;B[bg];W[ah];B[ag];W[ak];B[aj];W[hc];B[ai];W[qj];B[bh];W[fi]
        |;B[fh];W[mc];B[mb];W[nc];B[nb];W[oc];B[ob];W[ke];B[br];W[lf]
        |;B[aq];W[oj];B[na];W[kg];B[ed];W[ie];B[ar];W[hg];B[hh];W[jd]
        |;B[eh];W[ha];B[ak];W[dm];B[am];W[ol];B[sj];W[ok];B[sk];W[pj]
        |;B[se];W[re];B[sm];W[ri];B[si];W[sh];B[rh];W[sg];B[rj];W[qi]
        |;B[ba];W[rk];B[sl];W[sn];B[rn];W[ro];B[so];W[sp];B[sn];W[rm]
        |)
        |""".stripMargin)) {
      tree.toString
    }
  }
}
