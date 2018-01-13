cmake_minimum_required(VERSION 3.3)
project(libfive)

# Properly distinguish between Apple and upstream Clang
cmake_policy(SET CMP0025 NEW)

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
# Find all packages here at the top level so we can print debugging info
find_package(Qt5Core)
find_package(Boost REQUIRED)
find_package(PNG REQUIRED)

find_package(PkgConfig REQUIRED)
pkg_check_modules(GUILE guile-2.2)
pkg_check_modules(EIGEN REQUIRED eigen3)

if (UNIX AND NOT(APPLE))
    find_package(Threads REQUIRED)
endif(UNIX AND NOT(APPLE))

################################################################################
# Inform the user which subsystems will be built and any missing deps
message("Checking dependencies:")
message("  libfive: ✓")

if (GUILE_FOUND)
    message("  guile-libfive:     ✓")

    # Sadly, this is a global setting (there's no target_link_directories)
    link_directories(${GUILE_LIBRARY_DIRS})
else ()
    message("  guile-libfive:     ✘   (needs Guile 2.2 or later)")
endif()

if (Qt5Core_FOUND AND GUILE_FOUND)
    message("  Studio:       ✓")
else ()
    if (Qt5Core_FOUND)
        message("  Studio:       ✘   (needs Guile 2.2 or later)")
    elseif (GUILE_FOUND)
        message("  Studio:       ✘   (Qt 5.7 or later)")
    else()
        message("  Studio:       ✘   (needs Guile 2.2 or later and Qt 5.7 or later)")
    endif()
endif()

################################################################################

# Always build the kernel and test suite
add_subdirectory(libfive)

if(GUILE_FOUND AND Qt5Core_FOUND)
    add_subdirectory(studio)
endif(GUILE_FOUND AND Qt5Core_FOUND)
