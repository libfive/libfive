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

set(five_sources
    ############### .cpp files ###############################

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
    render/brep/dc/dc_root2.cpp
    render/brep/dc/dc_root3.cpp
    render/brep/dc/dc_tree2.cpp
    render/brep/dc/dc_tree3.cpp
    render/brep/dc/dc_xtree2.cpp
    render/brep/dc/dc_xtree3.cpp
    render/brep/dc/dc_object_pool2.cpp
    render/brep/dc/dc_object_pool3.cpp

    render/brep/hybrid/hybrid_debug.cpp
    render/brep/hybrid/hybrid_neighbors2.cpp
    render/brep/hybrid/hybrid_neighbors3.cpp
    render/brep/hybrid/hybrid_root2.cpp
    render/brep/hybrid/hybrid_root3.cpp
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
    render/brep/simplex/simplex_root2.cpp
    render/brep/simplex/simplex_root3.cpp
    render/brep/simplex/simplex_tree2.cpp
    render/brep/simplex/simplex_tree3.cpp
    render/brep/simplex/simplex_xtree2.cpp
    render/brep/simplex/simplex_xtree3.cpp
    render/brep/simplex/simplex_object_pool2.cpp
    render/brep/simplex/simplex_object_pool3.cpp
    render/brep/simplex/simplex_mesher.cpp

    render/brep/vol/vol_neighbors.cpp
    render/brep/vol/vol_object_pool.cpp
    render/brep/vol/vol_root.cpp
    render/brep/vol/vol_tree.cpp

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

    ############### .inl files ###############################

    render/brep/neighbors.inl
    render/brep/object_pool.inl
    render/brep/root.inl
    render/brep/xtree.inl

    render/brep/dc/dc_neighbors.inl
    render/brep/dc/dc_tree.inl

    render/brep/hybrid/hybrid_tree.inl

    render/brep/simplex/simplex_neighbors.inl
    render/brep/simplex/simplex_tree.inl

    ############### .h and .hpp files ########################

    ../include/libfive/eval/base.hpp
    ../include/libfive/eval/clause.hpp
    ../include/libfive/eval/deck.hpp
    ../include/libfive/eval/eval_array.hpp
    ../include/libfive/eval/eval_array_size.hpp
    ../include/libfive/eval/eval_deriv_array.hpp
    ../include/libfive/eval/eval_feature.hpp
    ../include/libfive/eval/eval_interval.hpp
    ../include/libfive/eval/eval_jacobian.hpp
    ../include/libfive/eval/evaluator.hpp
    ../include/libfive/eval/feature.hpp
    ../include/libfive/eval/interval.hpp
    ../include/libfive/eval/tape.hpp

    ../include/libfive/render/axes.hpp

    ../include/libfive/render/discrete/heightmap.hpp
    ../include/libfive/render/discrete/voxels.hpp

    ../include/libfive/render/brep/brep.hpp
    ../include/libfive/render/brep/contours.hpp
    ../include/libfive/render/brep/default_new_delete.hpp
    ../include/libfive/render/brep/dual.hpp
    ../include/libfive/render/brep/edge_tables.hpp
    ../include/libfive/render/brep/free_thread_handler.hpp
    ../include/libfive/render/brep/indexes.hpp
    ../include/libfive/render/brep/manifold_tables.hpp
    ../include/libfive/render/brep/mesh.hpp
    ../include/libfive/render/brep/multithread_recursive.hpp
    ../include/libfive/render/brep/neighbor_tables.hpp
    ../include/libfive/render/brep/neighbors.hpp
    ../include/libfive/render/brep/object_pool.hpp
    ../include/libfive/render/brep/progress.hpp
    ../include/libfive/render/brep/region.hpp
    ../include/libfive/render/brep/root.hpp
    ../include/libfive/render/brep/settings.hpp
    ../include/libfive/render/brep/util.hpp
    ../include/libfive/render/brep/xtree.hpp

    ../include/libfive/render/brep/dc/dc_contourer.hpp
    ../include/libfive/render/brep/dc/dc_flags.hpp
    ../include/libfive/render/brep/dc/dc_mesher.hpp
    ../include/libfive/render/brep/dc/dc_neighbors.hpp
    ../include/libfive/render/brep/dc/dc_tree.hpp
    ../include/libfive/render/brep/dc/intersection.hpp
    ../include/libfive/render/brep/dc/marching.hpp

    ../include/libfive/render/brep/hybrid/hybrid_debug.hpp
    ../include/libfive/render/brep/hybrid/hybrid_mesher.hpp
    ../include/libfive/render/brep/hybrid/hybrid_neighbors.hpp
    ../include/libfive/render/brep/hybrid/hybrid_tree.hpp

    ../include/libfive/render/brep/simplex/qef.hpp
    ../include/libfive/render/brep/simplex/simplex_debug.hpp
    ../include/libfive/render/brep/simplex/simplex_mesher.hpp
    ../include/libfive/render/brep/simplex/simplex_neighbors.hpp
    ../include/libfive/render/brep/simplex/simplex_tree.hpp
    ../include/libfive/render/brep/simplex/surface_edge_map.hpp

    ../include/libfive/render/brep/vol/vol_neighbors.hpp
    ../include/libfive/render/brep/vol/vol_tree.hpp

    ../include/libfive/solve/solver.hpp

    ../include/libfive/tree/archive.hpp
    ../include/libfive/tree/cache.hpp
    ../include/libfive/tree/deserializer.hpp
    ../include/libfive/tree/opcode.hpp
    ../include/libfive/tree/serializer.hpp
    ../include/libfive/tree/tree.hpp

    ../include/libfive/oracle/oracle.hpp
    ../include/libfive/oracle/oracle_clause.hpp
    ../include/libfive/oracle/oracle_context.hpp
    ../include/libfive/oracle/oracle_storage.hpp
    ../include/libfive/oracle/transformed_oracle.hpp
    ../include/libfive/oracle/transformed_oracle_clause.hpp

    ../include/libfive.h
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

target_compile_features(five PUBLIC cxx_std_17)

set(LIBS Eigen3::Eigen PNG::PNG Boost::boost tbb::tbb)

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