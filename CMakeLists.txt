cmake_minimum_required(VERSION 3.14)
project(libfive
  LANGUAGES C CXX
)

set(FIVE_MAJOR_VERSION 0)
set(FIVE_MINOR_VERSION 1)
set(FIVE_PATCH_VERSION 1)
set(FIVE_VERSION
  ${FIVE_MAJOR_VERSION}.${FIVE_MINOR_VERSION}.${FIVE_PATCH_VERSION})


# Properly distinguish between Apple and upstream Clang
cmake_policy(SET CMP0025 NEW)

option(BUILD_STUDIO_APP "Build Studio application" ON)
option(BUILD_GUILE_BINDINGS "Build Guile bindings" ON)
option(LIBFIVE_PACKED_OPCODES
    "Tightly pack opcodes (breaks compatibility with older saved f-reps)"
    OFF)

set(FIVE_CXX_STANDARD 14 CACHE STRING "C++ standard for libfive")

set(FIVE_INSTALL_LIB_DIR lib CACHE PATH "Installation path for libfive files")
set(FIVE_INSTALL_BIN_DIR bin CACHE PATH "Installation path for libfive binary files")
set(FIVE_INSTALL_INCLUDE_DIR include CACHE PATH "Installation path for header files")
set(FIVE_CMAKE_INSTALL_DIR lib/cmake CACHE PATH "Installation path for cmake files")


# set(CMAKE_BUILD_TYPE RELEASE)
# set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_BINARY_DIR}/cmake)

if (LIBFIVE_PACKED_OPCODES)
    message("Using packed opcodes, which breaks compatibility with saved f-reps!")
    add_definitions(-DLIBFIVE_PACKED_OPCODES)
endif()

# Build libfive with ccache if the package is present
set(LIBFIVE_CCACHE_BUILD OFF CACHE BOOL "Set to ON for a ccache enabled build")
if(LIBFIVE_CCACHE_BUILD)
  find_program(CCACHE_PROGRAM ccache)
  if(CCACHE_PROGRAM)
      set(LIBFIVE_CCACHE_MAXSIZE "" CACHE STRING "Size of ccache")
      set(LIBFIVE_CCACHE_DIR "" CACHE STRING "Directory to keep ccached data")
      set(LIBFIVE_CCACHE_PARAMS "CCACHE_CPP2=yes CCACHE_HASHDIR=yes"
          CACHE STRING "Parameters to pass through to ccache")

      set(CCACHE_PROGRAM "${LIBFIVE_CCACHE_PARAMS} ${CCACHE_PROGRAM}")
      if (LIBFIVE_CCACHE_MAXSIZE)
        set(CCACHE_PROGRAM "CCACHE_MAXSIZE=${LIBFIVE_CCACHE_MAXSIZE} ${CCACHE_PROGRAM}")
      endif()
      if (LIBFIVE_CCACHE_DIR)
        set(CCACHE_PROGRAM "CCACHE_DIR=${LIBFIVE_CCACHE_DIR} ${CCACHE_PROGRAM}")
      endif()
      set_property(GLOBAL PROPERTY RULE_LAUNCH_COMPILE ${CCACHE_PROGRAM})
  else()
    message(FATAL_ERROR "Unable to find the program ccache. Set LIBFIVE_CCACHE_BUILD to OFF")
  endif()
endif()

################################################################################

if(NOT MSVC)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -g -fPIC -pedantic -Werror=switch -march=native")
    if (${CMAKE_CXX_COMPILER_ID} STREQUAL "AppleClang")
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

set(CMAKE_CXX_STANDARD ${FIVE_CXX_STANDARD})
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
    if (BUILD_STUDIO_APP)
        find_package(Qt5Core)
    endif ()
    find_package(Boost REQUIRED)
    # find_package(Boost REQUIRED COMPONENTS system thread)
    find_package(PkgConfig REQUIRED)
    pkg_check_modules(GUILE guile-2.2>=2.2.1)
    find_package(Eigen3 REQUIRED)
    # pkg_check_modules(EIGEN REQUIRED eigen3>=3.2.92)
else()
    find_package(Eigen3 REQUIRED)
    find_package(Boost REQUIRED 1.65.0)
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


include(CMakePackageConfigHelpers)
# write_basic_package_version_file(
#   LibFiveConfigVersion.cmake
#   VERSION ${FIVE_VERSION}
#   COMPATIBILITY AnyNewerVersion
# )

install(EXPORT LibFiveTargets
        DESTINATION ${FIVE_CMAKE_INSTALL_DIR}
)

configure_package_config_file(
	"cmake/LibFiveConfig.cmake.in" "${CMAKE_CURRENT_BINARY_DIR}/LibFiveConfig.cmake"
	INSTALL_DESTINATION ${FIVE_CMAKE_INSTALL_DIR}
)

install(FILES "${CMAKE_CURRENT_BINARY_DIR}/LibFiveConfig.cmake"
        DESTINATION ${FIVE_CMAKE_INSTALL_DIR}
)

# Create/Install LibFiveConfig.cmake and related files.
#
# The below section is modified from KitWare CMake wiki on "How to create a ProjectConfig.cmake file".
# See: https://gitlab.kitware.com/cmake/community/wikis/doc/tutorials/How-to-create-a-ProjectConfig.cmake-file
# export(TARGETS five five-guile Studio FILE ${libfive_BINARY_DIR}/LibFiveConfig.cmake)
# export(PACKAGE libfive)
# # Add LibFiveConfigVersion.cmake
# configure_file(cmake/LibFiveConfigVersion.cmake.in
#   "${libfive_BINARY_DIR}/LibFiveConfigVersion.cmake" @ONLY)
# Add LibFiveConfig.cmake for build tree and for install tree.
# -- Note: CONFIG_INCLUDE_DIRS is the location of the include directories.
# -- Note: libfive_CMAKE_DIR is set to the path of LibFiveConfig.cmake when it is called.
# set(CONFIG_INCLUDE_DIRS "${libfive_SOURCE_DIR}/src" )
# configure_file(cmake/LibFiveConfig.cmake.in
#   "${libfive_BINARY_DIR}/LibFiveConfig.cmake" @ONLY)
# file(RELATIVE_PATH RELATIVE_INCLUDE_DIR "/${libfive_INSTALL_CMAKE_DIR}"
#   "/${libfive_INSTALL_INCLUDE_DIR}")
# set(CONFIG_INCLUDE_DIRS "\${libfive_CMAKE_DIR}/${RELATIVE_INCLUDE_DIR}")
# configure_file(cmake/LibFiveConfig.cmake.in
#   "${PROJECT_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/LibFiveConfig.cmake" @ONLY)
# # Install the LibFiveConfig.cmake and LibFiveConfigVersion.cmake
# install(FILES
#   "${libfive_BINARY_DIR}/LibFiveConfigVersion.cmake"
#   "${libfive_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/LibFiveConfig.cmake"
#   DESTINATION "${libfive_INSTALL_CMAKE_DIR}" COMPONENT dev)
# # Install the export set for use with the install-tree
# install(EXPORT LibFiveTargets DESTINATION
#   "${libfive_INSTALL_CMAKE_DIR}" COMPONENT dev)
# # Configure and install pkg-config file.
# configure_file(cmake/libfive.pc.in
#   "${libfive_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/libfive.pc" @ONLY)
# install(FILES
#   "${libfive_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/libfive.pc"
#   DESTINATION "${libfive_INSTALL_LIB_DIR}/pkgconfig"
# )




# export(TARGETS
#         CoreTargets
#         FiveGuileTargets
#       NAMESPACE five::
#       FILE LibFiveTargets.cmake
# )

# include(CMakePackageConfigHelpers)
# write_basic_package_version_file(
#   LibFiveConfigVersion.cmake
#   VERSION ${FIVE_VERSION}
#   COMPATIBILITY AnyNewerVersion
# )


# install(EXPORT MyLibTargets
#         FILE MyLibTargets.cmake
#         NAMESPACE MyLib::
#         DESTINATION lib/cmake/MyLib
#          )
# install(EXPORT FiveGuile DESTINATION lib/cmake)

# export(TARGETS five-guile FILE FiveGuileConfig.cmake)



# export(EXPORT fiveTargets
#   FILE "${CMAKE_CURRENT_BINARY_DIR}/five/fiveTargets.cmake"
#   NAMESPACE Upstream::
# )
# configure_file(cmake/fiveConfig.cmake
#   "${CMAKE_CURRENT_BINARY_DIR}/five/fiveConfig.cmake"
#   COPYONLY
# )

# set(ConfigPackageLocation lib/cmake/five)
# install(EXPORT fiveTargets
#   FILE
#     fiveTargets.cmake
#   NAMESPACE
#     Upstream::
#   DESTINATION
#     ${ConfigPackageLocation}
# )
# install(
#   FILES
#     cmake/fiveConfig.cmake
#     "${CMAKE_CURRENT_BINARY_DIR}/five/fiveConfigVersion.cmake"
#   DESTINATION
#     ${ConfigPackageLocation}
#   COMPONENT
#     Devel
# )
