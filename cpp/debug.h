// vim: shiftwidth=2 expandtab foldmethod=marker

#include <iostream>
#include <map>
#include <vector>

// debug {{{
bool debug_enabled = true;
int debug_max_elms = 10;

int debug_max_count = 50;
int debug_current_count = 0;

void debug1(char const * x) { std::cout << x; }
template<class T> void debug1(const T& x) { std::cout << x; }
template<class T1, class T2> void debug1(const std::pair<T1, T2>& x) { std::cout << "(" << x.first << ", " << x.second << ")"; }
template<class T> void debug1(const std::vector<T>& x) {
  std::cout << "[";
  auto i = x.begin();
  while(i != x.end()) {
    debug1(*i);
	++i;
	if(i != x.end()) std::cout << ", ";
    if(std::distance(x.begin(), i) > debug_max_elms) {
      std::cout << "...";
      break;
    }
  }
  std::cout << "]";
}
template<class K, class V> void debug1(const std::map<K, V>& x) {
  std::cout << "{";
  auto i = x.begin();
  while(i != x.end()) {
    debug1(std::get<0>(*i));
    std::cout << ": ";
    debug1(std::get<1>(*i));
    ++i;
    if(i != x.end()) std::cout << ", ";
  }
  std::cout << "}";
}

void debug_impl2() { std::cout << std::endl; }
template<class T, class... Ts> void debug_impl2(const T& x, const Ts&... xs) {
  std::cout << ' ';
  debug1(x);
  debug_impl2(xs...);
}

void debug_check() {
  if(debug_enabled) {
    ++debug_current_count;
    if(debug_current_count > debug_max_count) {
      std::cout << "... (debug log disabled)" << std::endl;
      debug_enabled = false;
    }
  }
}

void debug_reset() {
  debug_enabled = true;
  debug_current_count = 0;
}

template<class T, class... Ts> void debug(const T& x, const Ts&... xs) {
  debug_check();
  if(debug_enabled) {
	std::cout << "[D] ";
	debug1(x);
	debug_impl2(xs...);
  }
}
// }}}

// debugn {{{

#define debugn1(VAR) debugn1_impl(#VAR, VAR)
#define debugn_dispatch9(A, ...) { debugn1(A); }
#define debugn_dispatch8(A, ...) { debugn1(A); debugn_dispatch9(__VA_ARGS__, debugn_end()); }
#define debugn_dispatch7(A, ...) { debugn1(A); debugn_dispatch8(__VA_ARGS__, debugn_end()); }
#define debugn_dispatch6(A, ...) { debugn1(A); debugn_dispatch7(__VA_ARGS__, debugn_end()); }
#define debugn_dispatch5(A, ...) { debugn1(A); debugn_dispatch6(__VA_ARGS__, debugn_end()); }
#define debugn_dispatch4(A, ...) { debugn1(A); debugn_dispatch5(__VA_ARGS__, debugn_end()); }
#define debugn_dispatch3(A, ...) { debugn1(A); debugn_dispatch4(__VA_ARGS__, debugn_end()); }
#define debugn_dispatch2(A, ...) { debugn1(A); debugn_dispatch3(__VA_ARGS__, debugn_end()); }
#define debugn_dispatch1(A, ...) { debugn1(A); debugn_dispatch2(__VA_ARGS__, debugn_end()); }
#define debugn(...) { debug_check(); if(debug_enabled) { debugn_begin(); debugn_dispatch1(__VA_ARGS__, debugn_end()); } }


bool debugn_started = false;
struct debugn_end{};

void debugn1_impl(const char* name, debugn_end end) {
  if(debugn_started) std::cout << std::endl;
  debugn_started = false;
}
template<class T>
void debugn1_impl(const char* name, const T& value) {
  std::cout << name << ": " << value << ", ";
}
void debugn_begin() {
  debugn_started = true;
  std::cout << "[D] ";
}

// }}}
