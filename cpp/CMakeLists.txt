cmake_minimum_required(VERSION 3.15)

project(my_cpp_project CXX)

set(CMAKE_CXX_FLAGS "-Wall -std=c++14")

if(DEFINED LOCAL_DEBUG AND LOCAL_DEBUG)
    add_definitions(-DLOCAL_DEBUG)
endif()

add_executable(a.out main.cpp)

