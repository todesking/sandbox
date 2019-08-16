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

// misc {{{
using namespace std;
typedef long long ll;
#define rep(i, n) for (int i = 0; i < (int)(n); i++)
#define rep2(i, s, n) for (int i = (s); i < (int)(n); i++)
// }}}

// debug(...) {{{
static bool debug_enabled = false;
static int debug_max_elms = 10;

static int debug_max_count = 50;
static int debug_current_count = 0;

void debug1(char const * x) { cout << x; }
template<class T> void debug1(const T& x) { cout << x; }
template<class T1, class T2> void debug1(const pair<T1, T2>& x) { cout << "(" << x.first << ", " << x.second << ")"; }
template<class T> void debug1(const vector<T>& x) {
  cout << "[";
  auto i = x.begin();
  while(i != x.end()) {
    debug1(*i);
	++i;
	if(i != x.end()) cout << ", ";
    if(std::distance(x.begin(), i) > debug_max_elms) {
      cout << "...";
      break;
    }
  }
  cout << "]";
}
template<class K, class V> void debug1(const map<K, V>& x) {
  cout << "{";
  auto i = x.begin();
  while(i != x.end()) {
    debug1(get<0>(*i));
    cout << ": ";
    debug1(get<1>(*i));
    ++i;
    if(i != x.end()) cout << ", ";
  }
  cout << "}";
}

void debug2() { cout << endl; }
template<class T, class... Ts> void debug2(const T& x, const Ts&... xs) {
  cout << ' ';
  debug1(x);
  debug2(xs...);
}

template<class T, class... Ts> void debug(const T& x, const Ts&... xs) {
  if(debug_enabled) {
    ++debug_current_count;
    if(debug_current_count > debug_max_count) debug_enabled = false;
	cout << "[D] ";
	debug1(x);
	debug2(xs...);
  }
}
// }}}

// main {{{
void solve();
int main() {
  auto v = getenv("DEBUG");
  if(v && string(v) == "1") debug_enabled = true;
  solve();
}
// }}}

void solve() {
}
