cmake_minimum_required(VERSION 3.3)
project(libfive)

# Properly distinguish between Apple and upstream Clang
cmake_policy(SET CMP0025 NEW)

option(BUILD_STUDIO_APP "Build Studio application" ON)
option(BUILD_GUILE_BINDINGS "Build Guile bindings" ON)

set(CMAKE_BUILD_TYPE RELEASE)
set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake)

################################################################################

if(NOT MSVC)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -g -fPIC -pedantic -Werror=switch -march=native")
    if (APPLE)
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -stdlib=libc++")
    else()
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -lstdc++")
    endif()
    set(CMAKE_CXX_FLAGS_DEBUG "-O0")
else()
    set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_DEBUG ${CMAKE_BINARY_DIR}/Output/Debug)
    set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_RELEASE ${CMAKE_BINARY_DIR}/Output/Release)
    set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS ON)
    set(CMAKE_CXX_FLAGS "/EHsc /WX /D_USE_MATH_DEFINES /D_SCL_SECURE_NO_WARNINGS")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /wd4267 /wd4244 /wd4305")
    set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} /MD")
    set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} /MDd")
endif()

set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -O3 -DRELEASE -DEIGEN_NO_DEBUG")

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_C_STANDARD 99)
set(CMAKE_C_STANDARD_REQUIRED ON)

if(APPLE)
    set(CMAKE_MACOSX_RPATH ON)
endif()

# Work around an issue with Boost::Interval on OpenBSD and MinGW on Windows
if ("${CMAKE_SYSTEM_NAME}" STREQUAL "OpenBSD" OR MINGW)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -D__USE_ISOC99")
endif()

################################################################################
# Find all packages here at the top level so we can print debugging info
if(NOT MSVC)
    find_package(Qt5Core)
    find_package(Boost REQUIRED)
    find_package(PkgConfig REQUIRED)
    pkg_check_modules(GUILE guile-2.2>=2.2.1)
    pkg_check_modules(EIGEN REQUIRED eigen3>=3.2.92)
else()
    find_package(eigen3 REQUIRED)
    find_package(boost REQUIRED 1.65.0)
endif()

find_package(PNG REQUIRED)

if (UNIX AND NOT(APPLE))
    find_package(Threads REQUIRED)
    set(CMAKE_CXX_LINK_FLAGS "${CMAKE_CXX_LINK_FLAGS} -latomic")
endif(UNIX AND NOT(APPLE))

################################################################################
# Inform the user which subsystems will be built and any missing deps
message("Checking dependencies:")
message("  libfive:		✓")

if (BUILD_GUILE_BINDINGS)
  if (GUILE_FOUND)
    message("  libfive-guile:	✓")

    # Sadly, this is a global setting (there's no target_link_directories)
    link_directories(${GUILE_LIBRARY_DIRS})
  else ()
    message("  libfive-guile:	✘   (needs Guile 2.2 or later)")
  endif()
else()
    message("  libfive-guile:	✘   (skipping)")
endif()

if (BUILD_STUDIO_APP)
  if (Qt5Core_FOUND AND GUILE_FOUND)
    message("  Studio:		✓")
  else ()
    if (Qt5Core_FOUND)
      message("  Studio:		✘   (needs Guile 2.2 or later)")
    elseif (GUILE_FOUND)
      message("  Studio:		✘   (Qt 5.7 or later)")
    else()
      message("  Studio:		✘   (needs Guile 2.2 or later and Qt 5.7 or later)")
    endif()
  endif()
else()
    message("  Studio:		✘   (skipping)")
endif()

################################################################################

# Set a flag to detect the case where users run CMake in the wrong directory
set(LIBFIVE_BUILD_FROM_ROOT true)

# Always build the kernel and test suite
add_subdirectory(libfive)

if(BUILD_STUDIO_APP AND GUILE_FOUND AND Qt5Core_FOUND)
    add_subdirectory(studio)
endif(BUILD_STUDIO_APP AND GUILE_FOUND AND Qt5Core_FOUND)
