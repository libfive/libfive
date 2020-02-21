
set(five_sources
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
)

if(LIBFIVE_BUILD_STATIC_LIB)
    add_library(five STATIC ${five_sources})
    target_compile_definitions(five PUBLIC
        FIVE_STATIC)
else()
    add_library(five SHARED ${five_sources})
    target_compile_definitions(five PUBLIC
        FIVE_DLL)
endif()

if(LIBFIVE_UNNORMALIZED_DERIVS)
    target_compile_definitions(five PUBLIC
        LIBFIVE_UNNORMALIZED_DERIVS=1
    )
endif()

if(MSVC)
    target_compile_definitions(five PUBLIC
        _USE_MATH_DEFINES
        NOMINMAX
        _SCL_SECURE_NO_WARNINGS
    )
endif()

target_include_directories(five PUBLIC
    ../include
)

set(LIBS Eigen3::Eigen PNG::PNG Boost::boost)

if(UNIX AND NOT(APPLE))
    set(LIBS ${LIBS} ${CMAKE_THREAD_LIBS_INIT})
endif(UNIX AND NOT(APPLE))

if (UNIX)
    install(TARGETS five DESTINATION lib)
    install(FILES ../include/libfive.h DESTINATION include)
    install(DIRECTORY ../include/libfive DESTINATION include)
endif(UNIX)

target_link_libraries(five ${LIBS})

add_library(five::five ALIAS five)

if(LIBFIVE_GENERATE_GIT_VERSION)
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
    add_dependencies(five five-version)
    target_compile_definitions(five PUBLIC
        LIBFIVE_GENERATE_GIT_VERSION=1
    )
    target_sources(five PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/version.c)
endif()