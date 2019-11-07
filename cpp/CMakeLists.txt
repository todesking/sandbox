cmake_minimum_required(VERSION 3.15)

project(my_cpp_project CXX)

list(APPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake)

set(CMAKE_CXX_FLAGS "-Wall -std=c++14 -fsanitize=address -Wall -Wshorten-64-to-32 -Wunused-value")

if(DEFINED LOCAL_DEBUG AND LOCAL_DEBUG)
    add_definitions(-DLOCAL_DEBUG)
endif()

add_executable(a.out
  main.cpp
  debug.h
)
add_executable(my_cpp_project::a.out ALIAS a.out)

include(ClangFormat)
target_clangformat_setup(my_cpp_project::a.out)

