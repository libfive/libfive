set(SRCS main.cpp
    avx.cpp
    cache.cpp
    contours.cpp
    dual.cpp
    eval.cpp
    heightmap.cpp
    #lib.cpp
    mesh.cpp
    feature.cpp
    solver.cpp
    region.cpp
    template.cpp
    tree.cpp
    voxels.cpp
    xtree.cpp
    util/shapes.cpp)
set(LIBS ao-kernel) # ao-kernel-c)

if (GUILE_FOUND)
    #    set(SRCS ${SRCS} guile.cpp)
    #set(LIBS ${LIBS} guile-ao-kernel)
endif(GUILE_FOUND)

add_executable(ao-test ${SRCS})
target_link_libraries(ao-test ${LIBS})
