cmake_minimum_required(VERSION 2.6)
project(libao)
set(CMAKE_BUILD_TYPE DEBUG)

################################################################################

set(CMAKE_C_FLAGS "-Wall -g -fPIC -pedantic -std=c99")
set(CMAKE_C_FLAGS_RELEASE  "-O3")
set(CMAKE_C_FLAGS_DEBUG    "-O0")

set(CMAKE_CXX_FLAGS "-Wall -g -fPIC -pedantic -std=c++11")
set(CMAKE_CXX_FLAGS_RELEASE  "-O3")
set(CMAKE_CXX_FLAGS_DEBUG    "-O0")

################################################################################

add_subdirectory(src)
add_subdirectory(test)
