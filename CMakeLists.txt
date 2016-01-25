cmake_minimum_required(VERSION 2.6)
project(ao)
set(CMAKE_BUILD_TYPE RELEASE)

################################################################################

set(CMAKE_CXX_FLAGS "-Wall -g -fPIC -pedantic -std=c++11 -mavx")
set(CMAKE_CXX_FLAGS_RELEASE  "-O3 -DRELEASE")
set(CMAKE_CXX_FLAGS_DEBUG    "-O0")

include_directories(vendor)
include_directories(lib/inc)

################################################################################

# Build the ao library and test suite
add_subdirectory(lib)

# Build the ao-guile building
add_subdirectory(bind)

# Build efsw as a separate library
add_subdirectory(vendor/efsw)
