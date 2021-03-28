if(MSVC)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /bigobj")
endif()

add_library(libfive SHARED
    eval/base.cpp
    eval/deck.cpp
    eval/eval_interval.cpp
    eval/eval_jacobian.cpp
    eval/eval_array.cpp
    eval/eval_deriv_array.cpp
    eval/eval_feature.cpp
    eval/tape.cpp
    eval/feature.cpp

    render/discrete/heightmap.cpp
    render/discrete/voxels.cpp

    render/brep/contours.cpp
    render/brep/edge_tables.cpp
    render/brep/manifold_tables.cpp
    render/brep/mesh.cpp
    render/brep/neighbor_tables.cpp
    render/brep/progress.cpp

    render/brep/dc/marching.cpp
    render/brep/dc/dc_contourer.cpp
    render/brep/dc/dc_mesher.cpp
    render/brep/dc/dc_neighbors2.cpp
    render/brep/dc/dc_neighbors3.cpp
    render/brep/dc/dc_worker_pool2.cpp
    render/brep/dc/dc_worker_pool3.cpp
    render/brep/dc/dc_tree2.cpp
    render/brep/dc/dc_tree3.cpp
    render/brep/dc/dc_xtree2.cpp
    render/brep/dc/dc_xtree3.cpp
    render/brep/dc/dc_object_pool2.cpp
    render/brep/dc/dc_object_pool3.cpp

    render/brep/hybrid/hybrid_debug.cpp
    render/brep/hybrid/hybrid_worker_pool2.cpp
    render/brep/hybrid/hybrid_worker_pool3.cpp
    render/brep/hybrid/hybrid_neighbors2.cpp
    render/brep/hybrid/hybrid_neighbors3.cpp
    render/brep/hybrid/hybrid_tree2.cpp
    render/brep/hybrid/hybrid_tree3.cpp
    render/brep/hybrid/hybrid_xtree2.cpp
    render/brep/hybrid/hybrid_xtree3.cpp
    render/brep/hybrid/hybrid_object_pool2.cpp
    render/brep/hybrid/hybrid_object_pool3.cpp
    render/brep/hybrid/hybrid_mesher.cpp

    render/brep/simplex/simplex_debug.cpp
    render/brep/simplex/simplex_neighbors2.cpp
    render/brep/simplex/simplex_neighbors3.cpp
    render/brep/simplex/simplex_worker_pool2.cpp
    render/brep/simplex/simplex_worker_pool3.cpp
    render/brep/simplex/simplex_tree2.cpp
    render/brep/simplex/simplex_tree3.cpp
    render/brep/simplex/simplex_xtree2.cpp
    render/brep/simplex/simplex_xtree3.cpp
    render/brep/simplex/simplex_object_pool2.cpp
    render/brep/simplex/simplex_object_pool3.cpp
    render/brep/simplex/simplex_mesher.cpp

    render/brep/vol/vol_neighbors.cpp
    render/brep/vol/vol_object_pool.cpp
    render/brep/vol/vol_tree.cpp
    render/brep/vol/vol_worker_pool.cpp

    solve/solver.cpp

    tree/opcode.cpp
    tree/archive.cpp
    tree/data.cpp
    tree/deserializer.cpp
    tree/serializer.cpp
    tree/tree.cpp
    tree/operations.cpp

    oracle/oracle_clause.cpp
    oracle/transformed_oracle.cpp
    oracle/transformed_oracle_clause.cpp

    libfive.cpp
)

################################################################################
# Attach the Git revision to libfive.cpp
execute_process(COMMAND git log --pretty=format:'%h' -n 1
                OUTPUT_VARIABLE GIT_REV
                WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
                ERROR_QUIET)

# Check whether we got any revision (which isn't always the case, e.g.
# when someone downloaded a zip file from Github instead of a checkout)
if ("${GIT_REV}" STREQUAL "")
    set(GIT_REV "N/A")
    set(GIT_DIFF "")
    set(GIT_TAG "N/A")
    set(GIT_BRANCH "N/A")
else()
    execute_process(COMMAND bash -c "git diff --quiet --exit-code || echo +"
                    OUTPUT_VARIABLE GIT_DIFF
                    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
    execute_process(COMMAND git describe --exact-match --tags
                    OUTPUT_VARIABLE GIT_TAG
                    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
                    ERROR_QUIET)
    execute_process(COMMAND git rev-parse --abbrev-ref HEAD
                    OUTPUT_VARIABLE GIT_BRANCH
                    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

    string(STRIP "${GIT_REV}" GIT_REV)
    string(SUBSTRING "${GIT_REV}" 1 7 GIT_REV)
    string(STRIP "${GIT_DIFF}" GIT_DIFF)
    string(STRIP "${GIT_TAG}" GIT_TAG)
    string(STRIP "${GIT_BRANCH}" GIT_BRANCH)
endif()

set_property(
    SOURCE libfive.cpp
    APPEND PROPERTY COMPILE_DEFINITIONS
    GIT_TAG="${GIT_TAG}"
    GIT_REV="${GIT_REV}${GIT_DIFF}"
    GIT_BRANCH="${GIT_BRANCH}"
)

################################################################################
# Produce a library named 'libfive' on all platforms
set_target_properties(libfive PROPERTIES PREFIX "")

target_include_directories(libfive SYSTEM PUBLIC
    ${Boost_INCLUDE_DIRS}
    ${PNG_INCLUDE_DIR}
    ${EIGEN_INCLUDE_DIRS}
)
target_include_directories(libfive PUBLIC
    ../include
)

set(LIBS ${PNG_LIBRARIES})

if(UNIX AND NOT(APPLE))
    set(LIBS ${LIBS} ${CMAKE_THREAD_LIBS_INIT})
endif(UNIX AND NOT(APPLE))

if (UNIX)
    install(TARGETS libfive DESTINATION lib)
    install(FILES ../include/libfive.h DESTINATION include)
    install(DIRECTORY ../include/libfive DESTINATION include
            FILES_MATCHING PATTERN "*.hpp")
endif(UNIX)

target_link_libraries(libfive ${LIBS})
