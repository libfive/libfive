# Sanity-checking to make sure CMake was run against the correct directory
if (NOT DEFINED LIBFIVE_BUILD_FROM_ROOT)
    message(FATAL_ERROR "CMake must be called against top-level directory")
endif()

option(BUILD_TESTS "Build test suite" ON)

ADD_CUSTOM_TARGET(version-directory
    COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_CURRENT_BINARY_DIR}/src)
# Add a custom command that produces version.c, plus
# a dummy output that's not actually produced, in order
# to force version.cmake to always be re-run before the build
ADD_CUSTOM_TARGET(five-version
    BYPRODUCTS ${CMAKE_CURRENT_BINARY_DIR}/src/version.c
    DEPENDS version-directory
    # Pass the current source dir to version.cmake, so that it can run git
    # commands there instead of in the binary dir. This enables it to find
    # the right info when the build dir is outside the libfive repo.
    COMMAND ${CMAKE_COMMAND}
        -DLIBFIVE_CURRENT_SOURCE_DIR=${CMAKE_CURRENT_SOURCE_DIR}
        -P ${CMAKE_CURRENT_SOURCE_DIR}/src/version.cmake
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/src
)

if(MSVC)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /bigobj /DFIVE_DLL")
endif()

set(five_sources
    src/eval/base.cpp
    src/eval/deck.cpp
    src/eval/eval_interval.cpp
    src/eval/eval_jacobian.cpp
    src/eval/eval_array.cpp
    src/eval/eval_deriv_array.cpp
    src/eval/eval_feature.cpp
    src/eval/tape.cpp
    src/eval/feature.cpp

    src/render/discrete/heightmap.cpp
    src/render/discrete/voxels.cpp

    src/render/brep/contours.cpp
    src/render/brep/edge_tables.cpp
    src/render/brep/manifold_tables.cpp
    src/render/brep/mesh.cpp
    src/render/brep/neighbor_tables.cpp
    src/render/brep/progress.cpp

    src/render/brep/dc/marching.cpp
    src/render/brep/dc/dc_contourer.cpp
    src/render/brep/dc/dc_mesher.cpp
    src/render/brep/dc/dc_neighbors2.cpp
    src/render/brep/dc/dc_neighbors3.cpp
    src/render/brep/dc/dc_worker_pool2.cpp
    src/render/brep/dc/dc_worker_pool3.cpp
    src/render/brep/dc/dc_tree2.cpp
    src/render/brep/dc/dc_tree3.cpp
    src/render/brep/dc/dc_xtree2.cpp
    src/render/brep/dc/dc_xtree3.cpp
    src/render/brep/dc/dc_object_pool2.cpp
    src/render/brep/dc/dc_object_pool3.cpp

    src/render/brep/hybrid/hybrid_debug.cpp
    src/render/brep/hybrid/hybrid_worker_pool2.cpp
    src/render/brep/hybrid/hybrid_worker_pool3.cpp
    src/render/brep/hybrid/hybrid_neighbors2.cpp
    src/render/brep/hybrid/hybrid_neighbors3.cpp
    src/render/brep/hybrid/hybrid_tree2.cpp
    src/render/brep/hybrid/hybrid_tree3.cpp
    src/render/brep/hybrid/hybrid_xtree2.cpp
    src/render/brep/hybrid/hybrid_xtree3.cpp
    src/render/brep/hybrid/hybrid_object_pool2.cpp
    src/render/brep/hybrid/hybrid_object_pool3.cpp
    src/render/brep/hybrid/hybrid_mesher.cpp

    src/render/brep/simplex/simplex_debug.cpp
    src/render/brep/simplex/simplex_neighbors2.cpp
    src/render/brep/simplex/simplex_neighbors3.cpp
    src/render/brep/simplex/simplex_worker_pool2.cpp
    src/render/brep/simplex/simplex_worker_pool3.cpp
    src/render/brep/simplex/simplex_tree2.cpp
    src/render/brep/simplex/simplex_tree3.cpp
    src/render/brep/simplex/simplex_xtree2.cpp
    src/render/brep/simplex/simplex_xtree3.cpp
    src/render/brep/simplex/simplex_object_pool2.cpp
    src/render/brep/simplex/simplex_object_pool3.cpp
    src/render/brep/simplex/simplex_mesher.cpp

    src/render/brep/vol/vol_neighbors.cpp
    src/render/brep/vol/vol_object_pool.cpp
    src/render/brep/vol/vol_tree.cpp
    src/render/brep/vol/vol_worker_pool.cpp

    src/solve/solver.cpp

    src/tree/cache.cpp
    src/tree/opcode.cpp
    src/tree/archive.cpp
    src/tree/deserializer.cpp
    src/tree/serializer.cpp
    src/tree/tree.cpp

    src/oracle/oracle_clause.cpp
    src/oracle/transformed_oracle.cpp
    src/oracle/transformed_oracle_clause.cpp

    src/libfive.cpp
)

set(five_version_src
    "${CMAKE_CURRENT_BINARY_DIR}/src/version.c"
)


add_library(five SHARED
    ${five_sources}
    ${five_version_src}
)

# So that the same target can be used in the source tree and
# in other projects.
add_library(LibFive::core ALIAS five)

set_property(TARGET five PROPERTY VERSION ${FIVE_VERSION})

add_dependencies(five five-version)


target_include_directories(five
  PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
         $<INSTALL_INTERFACE:${CMAKE_INSTALL_PREFIX}/include>
         ${Boost_INCLUDE_DIRS}
         ${PNG_INCLUDE_DIR}
)


set(LIBS ${PNG_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} )
target_link_libraries(five PUBLIC Eigen3::Eigen PRIVATE ${LIBS})

set_target_properties(five
   PROPERTIES
   CXX_STANDARD "${FIVE_CXX_STANDARD}"
   CXX_STANDARD_REQUIRED "YES"
   CXX_EXTENSION "NO"
   INSTALL_RPATH ${CMAKE_INSTALL_PREFIX}/lib
        EXPORT_NAME core
)

install(
  TARGETS five
  EXPORT LibFiveTargets
  RUNTIME DESTINATION ${FIVE_INSTALL_BIN_DIR}
  LIBRARY DESTINATION ${FIVE_INSTALL_LIB_DIR}
  ARCHIVE DESTINATION ${FIVE_INSTALL_LIB_DIR}
)


install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/include/
        DESTINATION ${FIVE_INSTALL_INCLUDE_DIR}
        FILES_MATCHING
          PATTERN "*.h"
          PATTERN "*.hpp"
)

if (BUILD_TESTS)
  add_subdirectory(test)
endif(BUILD_TESTS)

if (BUILD_GUILE_BINDINGS AND GUILE_FOUND)
    add_subdirectory(bind)
endif(BUILD_GUILE_BINDINGS AND GUILE_FOUND)

