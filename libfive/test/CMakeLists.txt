set(SRCS main.cpp
    api.cpp
    archive.cpp
    bounds.cpp
    cache.cpp
    contours.cpp
    deck.cpp
    dual.cpp
    eval_interval.cpp
    eval_jacobian.cpp
    eval_array.cpp
    eval_deriv.cpp
    eval_deriv_array.cpp
    eval_feature.cpp
    eval_point.cpp
    feature.cpp
    heightmap.cpp
    hybrid_meshing.cpp
    indexes.cpp
    marching.cpp
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
    xtree.cpp
    util/mesh_checks.cpp
    util/shapes.cpp)
set(LIBS five)

if (GUILE_FOUND)
    set(SRCS ${SRCS} guile.cpp)
    set(LIBS ${LIBS} five-guile)
endif(GUILE_FOUND)

add_executable(libfive-test ${SRCS})
target_link_libraries(libfive-test ${LIBS})
target_include_directories(libfive-test PRIVATE .)
