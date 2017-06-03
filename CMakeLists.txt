cmake_minimum_required(VERSION 2.6)
project(Ao)

set(CMAKE_BUILD_TYPE RELEASE)
set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake)

################################################################################

set(CMAKE_CXX_FLAGS "-Wall -Wextra -g -fPIC -pedantic -Werror=switch -march=native")
set(CMAKE_CXX_FLAGS_RELEASE  "-O3 -DRELEASE -DEIGEN_NO_DEBUG")
set(CMAKE_CXX_FLAGS_DEBUG    "-O0")

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_C_STANDARD 99)
set(CMAKE_C_STANDARD_REQUIRED ON)

if(APPLE)
    set(CMAKE_MACOSX_RPATH ON)
endif()

################################################################################

execute_process(COMMAND git log --pretty=format:'%h' -n 1
                OUTPUT_VARIABLE GITREV)
execute_process(COMMAND bash -c "git diff --quiet --exit-code || echo +"
                OUTPUT_VARIABLE GITDIFF)
execute_process(COMMAND git describe --exact-match --tags
                OUTPUT_VARIABLE GITTAG
                ERROR_QUIET)
execute_process(COMMAND git rev-parse --abbrev-ref HEAD
                OUTPUT_VARIABLE GITBRANCH)

add_definitions(-D'GITREV="${GITREV}${GITDIFF}"'
                -D'GITTAG="${GITTAG}"'
                -D'GITBRANCH="${GITBRANCH}"')

################################################################################

find_package(Guile 2.2 QUIET)

# Always build the kernel and test suite
add_subdirectory(ao)

if(GUILE_FOUND)
    add_subdirectory(gui)
endif(GUILE_FOUND)
