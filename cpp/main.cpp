// vim: shiftwidth=2 expandtab foldmethod=marker

// include {{{
#include <algorithm>
#include <cassert>
#include <chrono>
#include <cmath>
#include <cstdlib>
#include <iomanip>
#include <iostream>
#include <limits>
#include <map>
#include <queue>
#include <random>
#include <set>
#include <stack>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>
// }}}

// debug(...) {{{
#ifdef LOCAL_DEBUG
#include "./debug.h"
#else
bool debug_enabled = false;
#define debug(...)
#define debugn(...)
#define debug_reset()
#endif

// }}}

// misc {{{
using namespace std;

typedef long long ll;

#define rep(i, n) for (int i = 0; i < (int)(n); i++)
#define rep2(i, s, n) for (int i = (s); i < (int)(n); i++)

template <class A>
ostream& out_vec(const vector<A>& x, const string& sep) {  // {{{
  rep(i, x.size()) {
    cout << x[i];
    if (i < x.size() - 1) cout << sep;
  }
  return cout;
}  // }}}

// }}}

// main {{{
void solve();
int main() {
  cout << setprecision(20);
  solve();
}
// }}}

void solve() {
  int N, L;
  cin >> N >> L;
  int K;
  cin >> K;
  vector<int> A(N);
  for(int i = 0; i < N; i++) cin >> A[i];

  int lo = 1;
  int hi = L + 1;
  while(lo + 1 < hi) {
    int mid = (lo + hi) / 2;

    int start = 0;
    int cuts = 0;
    for(int i = 0; i < N; i++) {
      if(A[i] - start >= mid) {
        start = A[i];
        cuts++;
      }
    }

    if(L - start < mid) cuts--;

    if(cuts < K) {
      hi = mid;
    } else {
      lo = mid;
    }
  }
  cout << lo << endl;
}
