// vim: shiftwidth=2 expandtab foldmethod=marker

// include {{{
#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <set>
#include <map>
#include <queue>
#include <stack>
#include <cmath>
#include <utility>
#include <cstdlib>
// }}}

// debug(...) {{{
#ifdef LOCAL_DEBUG
#include "./debug.h"
#else
#define debug(...)
#endif
// }}}

// misc {{{
using namespace std;
typedef long long ll;
#define rep(i, n) for (int i = 0; i < (int)(n); i++)
#define rep2(i, s, n) for (int i = (s); i < (int)(n); i++)

template<class T>
void sort(T& arr) {
  std::sort(arr.begin(), arr.end());
}
template<class T, class By>
void sort_by(T& arr, By by) {
  std::sort(arr.begin(), arr.end(), [by](auto l, auto r){return  by(l) < by(r); });
}

template<class T>
void reverse(T& arr) {
  std::reverse(arr.begin(), arr.end());
}

// }}}

// main {{{
void solve();
int main() {
  solve();
}
// }}}

void solve() {
  int N;
  cin >> N;
  vector<int> H(N);
  rep(i, N) {
    cin >> H[i];
  }

  int sum = 0;
  while(true) {
    int i = 0;
    while(i < N && H[i] == 0) i++;
    if(i == N) break;
    while(i < N && H[i] > 0) {
      H[i]--;
      i++;
    }
    sum++;
  }
  cout << sum << endl;
}
