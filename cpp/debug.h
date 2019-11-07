// vim: shiftwidth=2 expandtab foldmethod=marker

#include <iostream>
#include <map>
#include <set>
#include <tuple>
#include <vector>

// debug {{{
bool debug_enabled = true;
int debug_max_elms = 40;

int debug_max_count = 100;
int debug_current_count = 0;

void debug1(char const* x) { std::cerr << x; }
template <class T>
void debug1(const T& x) {
  std::cerr << x;
}
template <class T1, class T2>
void debug1(const std::pair<T1, T2>& x) {
  std::cerr << "(" << x.first << ", " << x.second << ")";
}
// https://stackoverflow.com/questions/6245735/pretty-print-stdtuple
namespace pp_tuple {  // {{{
template <std::size_t...>
struct seq {};

template <std::size_t N, std::size_t... Is>
struct gen_seq : gen_seq<N - 1, N - 1, Is...> {};

template <std::size_t... Is>
struct gen_seq<0, Is...> : seq<Is...> {};

template <class Ch, class Tr, class Tuple, std::size_t... Is>
void print_tuple(std::basic_ostream<Ch, Tr>& os, Tuple const& t, seq<Is...>) {
  using swallow = int[];
  (void)swallow{0, (void(os << (Is == 0 ? "" : ", ") << std::get<Is>(t)), 0)...};
}
}  // namespace pp_tuple

template <class... T>
void debug1(const std::tuple<T...>& x) {
  std::cerr << "(";
  pp_tuple::print_tuple(std::cerr, x, pp_tuple::gen_seq<sizeof...(T)>());
  std::cerr << ")";
}

template <class T>
void debug1_coll(const T& x, const char* left, const char* right) {
  std::cerr << left;
  auto i = x.begin();
  while (i != x.end()) {
    debug1(*i);
    ++i;
    if (i != x.end()) std::cerr << ", ";
    if (std::distance(x.begin(), i) > debug_max_elms) {
      std::cerr << "...";
      break;
    }
  }
  std::cerr << right;
}
template <class T>
void debug1(const std::set<T>& x) {
  debug1_coll(x, "{", "}");
}
template <class T>
void debug1(const std::vector<T>& x) {
  debug1_coll(x, "[", "]");
}
template <class K, class V>
void debug1(const std::map<K, V>& x) {
  std::cerr << "{";
  auto i = x.begin();
  while (i != x.end()) {
    debug1(std::get<0>(*i));
    std::cerr << ": ";
    debug1(std::get<1>(*i));
    ++i;
    if (i != x.end()) std::cerr << ", ";
  }
  std::cerr << "}";
}

void debug_impl2() { std::cerr << std::endl; }
template <class T, class... Ts>
void debug_impl2(const T& x, const Ts&... xs) {
  std::cerr << ' ';
  debug1(x);
  debug_impl2(xs...);
}

void debug_check() {
  if (debug_enabled) {
    ++debug_current_count;
    if (debug_current_count > debug_max_count) {
      std::cerr << "... (debug log disabled)" << std::endl;
      debug_enabled = false;
    }
  }
}

void debug_reset() {
  debug_enabled = true;
  debug_current_count = 0;
}

template <class T, class... Ts>
void debug(const T& x, const Ts&... xs) {
  debug_check();
  if (debug_enabled) {
    std::cerr << "[D] ";
    debug1(x);
    debug_impl2(xs...);
  }
}
// }}}

// debugn {{{

#define debugn1(VAR) debugn1_impl(#VAR, VAR)
#define debugn_dispatch9(A, ...) \
  { debugn1(A); }
#define debugn_dispatch8(A, ...)                 \
  {                                              \
    debugn1(A);                                  \
    debugn_dispatch9(__VA_ARGS__, debugn_end()); \
  }
#define debugn_dispatch7(A, ...)                 \
  {                                              \
    debugn1(A);                                  \
    debugn_dispatch8(__VA_ARGS__, debugn_end()); \
  }
#define debugn_dispatch6(A, ...)                 \
  {                                              \
    debugn1(A);                                  \
    debugn_dispatch7(__VA_ARGS__, debugn_end()); \
  }
#define debugn_dispatch5(A, ...)                 \
  {                                              \
    debugn1(A);                                  \
    debugn_dispatch6(__VA_ARGS__, debugn_end()); \
  }
#define debugn_dispatch4(A, ...)                 \
  {                                              \
    debugn1(A);                                  \
    debugn_dispatch5(__VA_ARGS__, debugn_end()); \
  }
#define debugn_dispatch3(A, ...)                 \
  {                                              \
    debugn1(A);                                  \
    debugn_dispatch4(__VA_ARGS__, debugn_end()); \
  }
#define debugn_dispatch2(A, ...)                 \
  {                                              \
    debugn1(A);                                  \
    debugn_dispatch3(__VA_ARGS__, debugn_end()); \
  }
#define debugn_dispatch1(A, ...)                 \
  {                                              \
    debugn1(A);                                  \
    debugn_dispatch2(__VA_ARGS__, debugn_end()); \
  }
#define debugn(...)                                \
  {                                                \
    debug_check();                                 \
    if (debug_enabled) {                           \
      debugn_begin();                              \
      debugn_dispatch1(__VA_ARGS__, debugn_end()); \
    }                                              \
  }

bool debugn_started = false;
struct debugn_end {};

void debugn1_impl(const char* name, debugn_end end) {
  if (debugn_started) std::cerr << std::endl;
  debugn_started = false;
}
template <class T>
void debugn1_impl(const char* name, const T& value) {
  std::cerr << name << ": ";
  debug1(value);
  std::cerr << ", ";
}
void debugn_begin() {
  debugn_started = true;
  std::cerr << "[D] ";
}

// }}}
