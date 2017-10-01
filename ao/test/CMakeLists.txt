set(SRCS main.cpp
    api.cpp
    bounds.cpp
    cache.cpp
    contours.cpp
    dual.cpp
    eval.cpp
    eval_i.cpp
    eval_a.cpp
    feature.cpp
    heightmap.cpp
    marching.cpp
    mesh.cpp
    region.cpp
    solver.cpp
    tape.cpp
    template.cpp
    tree.cpp
    voxels.cpp
    xtree.cpp
    util/shapes.cpp)
set(LIBS ao-kernel)

if (GUILE_FOUND)
    set(SRCS ${SRCS} guile.cpp)
    set(LIBS ${LIBS} guile-ao)
endif(GUILE_FOUND)

add_executable(ao-test ${SRCS})
target_link_libraries(ao-test ${LIBS})
