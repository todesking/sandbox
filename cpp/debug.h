#include <iostream>
#include <map>
#include <vector>

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

template<class T, class... Ts> void debug(const T& x, const Ts&... xs) {
  if(debug_enabled) {
    ++debug_current_count;
    if(debug_current_count > debug_max_count) {
      std::cout << "... (debug log disabled)" << std::endl;
      debug_enabled = false;
    }
	std::cout << "[D] ";
	debug1(x);
	debug_impl2(xs...);
  }
}

