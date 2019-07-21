# Add a custom command that produces version.c, plus
# a dummy output that's not actually produced, in order
# to force version.cmake to always be re-run before the build
ADD_CUSTOM_TARGET(five-version
    BYPRODUCTS ${CMAKE_CURRENT_BINARY_DIR}/version.c
    # Pass the current source dir to version.cmake, so that it can run git
    # commands there instead of in the binary dir. This enables it to find
    # the right info when the build dir is outside the libfive repo.
    COMMAND ${CMAKE_COMMAND}
        -DLIBFIVE_CURRENT_SOURCE_DIR=${CMAKE_CURRENT_SOURCE_DIR}
        -P ${CMAKE_CURRENT_SOURCE_DIR}/version.cmake
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)

if(MSVC)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /bigobj /DFIVE_DLL")
endif()

add_library(five SHARED
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

    render/brep/dc/intersection_aligner.cpp
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

    solve/bounds.cpp
    solve/solver.cpp

    tree/cache.cpp
    tree/opcode.cpp
    tree/archive.cpp
    tree/deserializer.cpp
    tree/serializer.cpp
    tree/tree.cpp

    oracle/oracle_clause.cpp
    oracle/transformed_oracle.cpp
    oracle/transformed_oracle_clause.cpp

    libfive.cpp

    ${CMAKE_CURRENT_BINARY_DIR}/version.c
)
add_dependencies(five five-version)

target_include_directories(five SYSTEM PUBLIC
    ${Boost_INCLUDE_DIRS}
    ${PNG_INCLUDE_DIR}
    ${EIGEN_INCLUDE_DIRS}
)
target_include_directories(five PUBLIC
    ../include
)

set(LIBS ${PNG_LIBRARIES})

if(UNIX AND NOT(APPLE))
    set(LIBS ${LIBS} ${CMAKE_THREAD_LIBS_INIT})
endif(UNIX AND NOT(APPLE))

if (UNIX)
    install(TARGETS five DESTINATION lib)
    install(FILES ../include/libfive.h DESTINATION include)
    install(DIRECTORY ../include/libfive DESTINATION include)
endif(UNIX)

target_link_libraries(five ${LIBS})
