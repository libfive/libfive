cmake_minimum_required(VERSION 2.6)
project(libao)
set(CMAKE_BUILD_TYPE DEBUG)

################################################################################

find_path(GUILE_INCLUDE_DIR libguile.h /usr/local/include/guile/2.0)
find_library(GUILE_LIBRARY NAMES guile-2.0 PATH /usr/local/lib)

find_package(Boost REQUIRED)

################################################################################

set(CMAKE_C_FLAGS "-Wall -g -fPIC -pedantic -std=c99")
set(CMAKE_C_FLAGS_RELEASE  "-O3")
set(CMAKE_C_FLAGS_DEBUG    "-O0")

set(CMAKE_CXX_FLAGS "-Wall -g -fPIC -pedantic -std=c++11")
set(CMAKE_CXX_FLAGS_RELEASE  "-O3")
set(CMAKE_CXX_FLAGS_DEBUG    "-O0")

################################################################################

include_directories(.)
include_directories(${GUILE_INCLUDE_DIR})
include_directories(${BOOST_INCLUDE_DIR})

################################################################################

add_library(ao SHARED store.cpp token.cpp scm.cpp atom.cpp tree.cpp)

target_link_libraries(ao ${GUILE_LIBRARY})
#install(TARGETS fab DESTINATION ${PROJECT_SOURCE_DIR})

