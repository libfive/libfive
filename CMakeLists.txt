cmake_minimum_required(VERSION 2.6)
project(Ao)

set(CMAKE_BUILD_TYPE RELEASE)
set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake)

################################################################################

set(CMAKE_CXX_FLAGS "-Wall -Wextra -Wno-comment -g -fPIC -pedantic -std=c++11 -Werror=switch -march=native")
set(CMAKE_CXX_FLAGS_RELEASE  "-O3 -DRELEASE")
set(CMAKE_CXX_FLAGS_DEBUG    "-O0")

################################################################################

if (APPLE)
    set(CMAKE_MACOSX_RPATH ON)
endif()

################################################################################

add_subdirectory(ao)
