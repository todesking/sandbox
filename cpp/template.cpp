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

// bin {{{
template <class T>
std::string bin(int bits, const T& x) {
  char buf[bits + 1];
  buf[bits] = '\0';
  for (int i = 0; i < bits; i++) {
    if ((x >> i) & 1) {
      buf[bits - i - 1] = '1';
    } else {
      buf[bits - i - 1] = '0';
    }
  }
  return std::string(buf);
}
std::string bin(int x) {
  if (x < 0) return bin(32, x);
  for (int i = 4; i < 31; i++) {
    int y = 1 << i;
    if (x < y) return bin(i, x);
  }
  return bin(32, x);
}
// }}}

// misc {{{
using namespace std;
template <class T, class U>

using umap = unordered_map<T, U>;
template <class T>
using uset = unordered_set<T>;

typedef long long ll;

#define rep(i, n) for (int i = 0; i < (int)(n); i++)
#define rep2(i, s, n) for (int i = (s); i < (int)(n); i++)

template <class T>
void sort(T& arr) {  // {{{
  std::sort(arr.begin(), arr.end());
}  // }}}
template <class T, class By>
void sort_by(T& arr, By by) {  // {{{
  std::sort(arr.begin(), arr.end(), [by](auto l, auto r) { return by(l) < by(r); });
}  // }}}

template <class T>
void reverse(T& arr) {  // {{{
  std::reverse(arr.begin(), arr.end());
}  // }}}

class Random {  // {{{
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
};  // }}}

template <class A>
ostream& out_vec(const vector<A>& x, const string& sep) {  // {{{
  rep(i, x.size()) {
    cout << x[i];
    if (i < x.size() - 1) cout << sep;
  }
  return cout;
}  // }}}

bool ends_with(const string& s, const string& x) {
  if(s.length() < x.length()) return false;
  for(int i = 0; i < x.size(); i++) {
    if(s[s.size() - 1 - i] != x[x.size() - 1 - i]) return false;
  }
  return true;
}
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
  if (x < 0) x += mod;
  return x;
}

ll modminus(ll a, ll b, ll mod) {
  return modplus(a, -b, mod);
}

// }}}

class UnionFind {  // {{{
  vector<int> parent;

 public:
  UnionFind(int N) : parent(N) {
    rep(i, N) parent[i] = i;
  }

  int root(int x) {
    if (parent[x] == x)
      return x;
    else
      return parent[x] = root(parent[x]);
  }

  void unite(int x, int y) {
    int rx = root(x);
    int ry = root(y);
    if (rx != ry) { parent[rx] = ry; }
  }
};  // }}}

// modulo {{{
// https://qiita.com/drken/items/3b4fdf0a78e7a138cd9a
long long modinv(long long a, long long m) {
  long long b = m, u = 1, v = 0;
  while (b) {
    long long t = a / b;
    a -= t * b;
    swap(a, b);
    u -= t * v;
    swap(u, v);
  }
  u %= m;
  if (u < 0) u += m;
  return u;
}

ll modfact(ll a, ll m) {
  ll x = 1;
  for (ll i = 2; i <= a; i++) {
    x *= i;
    x %= m;
  }
  return x;
}

ll mod_P(ll n, ll r, ll m) {
  ll x = 1;
  for (ll i = n - r + 1; i <= n; i++) {
    x *= i;
    x %= m;
  }
  return x;
}

ll mod_C(ll n, ll r, ll m) {
  return mod_P(n, r, m) * modinv(modfact(r, m), m) % m;
}

// }}}

