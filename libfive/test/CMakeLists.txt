set(SRCS main.cpp
    api.cpp
    archive.cpp
    bounds.cpp
    cache.cpp
    contours.cpp
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
    marching.cpp
    mesh.cpp
    neighbors.cpp
    oracle.cpp
    region.cpp
    solver.cpp
    tape.cpp
    ternary.cpp
    transformed_oracle.cpp
    tree.cpp
    voxels.cpp
    xtree.cpp
    util/shapes.cpp)
set(LIBS five)

if (GUILE_FOUND)
    set(SRCS ${SRCS} guile.cpp)
    set(LIBS ${LIBS} five-guile)
endif(GUILE_FOUND)

add_executable(libfive-test ${SRCS})
target_link_libraries(libfive-test ${LIBS})
