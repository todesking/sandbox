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
#define debugn(...)
#define debug_reset()
#endif

#include <cstdio>

template<class T>
std::string bin(int bits, const T& x) {
  char buf[bits + 1];
  buf[bits] = '\0';
  for(int i = 0; i < bits; i++) {
    if((x >> i) & 1) {
      buf[bits - i - 1] = '1';
    } else {
      buf[bits - i - 1] = '0';
    }
  }
  return std::string(buf);
}
std::string bin(int x) {
  if(x < 0) return bin(32, x);
  for(int i = 4; i < 31; i++) {
    int y = 1 << i;
    if(x < y) return bin(i, x);
  }
  return bin(32, x);
}
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
