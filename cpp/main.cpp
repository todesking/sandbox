// vim: shiftwidth=2 expandtab foldmethod=marker

// include {{{
#include <algorithm>
#include <iostream>
#include <iomanip>
#include <string>
#include <vector>
#include <map>
#include <set>
#include <unordered_set>
#include <map>
#include <unordered_map>
#include <queue>
#include <stack>
#include <cmath>
#include <utility>
#include <cstdlib>
#include <random>
#include <chrono>
#include <limits>
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

// bin {{{
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
template<class T, class U>

using umap = unordered_map<T, U>;
template<class T>
using uset = unordered_set<T>;

typedef long long ll;

#define rep(i, n) for (int i = 0; i < (int)(n); i++)
#define rep2(i, s, n) for (int i = (s); i < (int)(n); i++)

template<class T>
void sort(T& arr) { // {{{
  std::sort(arr.begin(), arr.end());
} // }}}
template<class T, class By>
void sort_by(T& arr, By by) { // {{{
  std::sort(arr.begin(), arr.end(), [by](auto l, auto r){return  by(l) < by(r); });
} // }}}

template<class T>
void reverse(T& arr) { // {{{
  std::reverse(arr.begin(), arr.end());
} // }}}

class Random { // {{{
  public:
  std::uniform_int_distribution<> uniform_int;
  std::mt19937 rnd;
  std::bernoulli_distribution bern_dist;
  int uniform(int l, int r) {
    return uniform_int(rnd, std::uniform_int_distribution<>::param_type(l, r));
  }
  int next(int N) {
    return uniform(0, N - 1);
  }
  int bern(double p) {
    return bern_dist(rnd, std::bernoulli_distribution::param_type(p));
  }
}; // }}}

template <class A>
ostream& out_vec(const vector<A>& x, const string& sep) { // {{{
  rep(i, x.size()) {
    cout << x[i];
    if(i < x.size() - 1)
      cout << sep;
  }
  return cout;
} // }}}
// }}}

// modulo {{{
ll modpow(ll a, ll n, ll mod) {
    ll res = 1;
    while (n > 0) {
        if (n & 1) res = res * a % mod;
        a = a * a % mod;
        n >>= 1;
    }
    return res;
}

ll modplus(ll a, ll b, ll mod) {
  ll x = ((a % mod) + (b % mod)) % mod;
  if(x < 0) x += mod;
  return x;
}

ll modminus(ll a, ll b, ll mod) {
  return modplus(a, -b, mod);
}

// }}}

class UnionFind { // {{{
  vector<int> parent;

  public:

  UnionFind(int N): parent(N) {
    rep(i, N) parent[i] = i;
  }

  int root(int x) {
    if(parent[x] == x) return x;
    else return parent[x] = root(parent[x]);
  }

  void unite(int x, int y) {
    int rx = root(x);
    int ry = root(y);
    if(rx != ry) {
      parent[rx] = ry;
    }
  }
}; // }}}

// main {{{
void solve();
int main() {
  cout << setprecision(20);
  solve();
}
// }}}

void solve() {
}
