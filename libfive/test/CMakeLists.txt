set(SRCS main.cpp
    api.cpp
    archive.cpp
    contours.cpp
    deck.cpp
    dual.cpp
    eval_interval.cpp
    eval_jacobian.cpp
    eval_array.cpp
    eval_deriv_array.cpp
    eval_feature.cpp
    feature.cpp
    heightmap.cpp
    hybrid_meshing.cpp
    indexes.cpp
    marching.cpp
    manifold_tables.cpp
    mesh.cpp
    neighbors.cpp
    object_pool.cpp
    oracle.cpp
    oracle_context.cpp
    progress.cpp
    qef.cpp
    region.cpp
    simplex.cpp
    solver.cpp
    surface_edge_map.cpp
    transformed_oracle.cpp
    tree.cpp
    voxels.cpp
    vol_tree.cpp
    xtree.cpp
    util/mesh_checks.cpp
    util/shapes.cpp)
set(LIBS five)

add_executable(libfive-test ${SRCS})
target_link_libraries(libfive-test ${LIBS})
target_include_directories(libfive-test PRIVATE .)

if (BUILD_GUILE_BINDINGS AND GUILE_FOUND)
    target_sources(libfive-test PRIVATE guile.cpp)
    target_link_libraries(libfive-test ${GUILE_LIBRARIES})
    target_include_directories(libfive-test PUBLIC . ${GUILE_INCLUDE_DIRS})
endif(BUILD_GUILE_BINDINGS AND GUILE_FOUND)
