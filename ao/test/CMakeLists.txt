add_executable(ao-test main.cpp
    avx.cpp
    cache.cpp
    dc.cpp
    eval.cpp
    heightmap.cpp
    mesh.cpp
    octree.cpp
    region.cpp
    contours.cpp
    feature.cpp
    solver.cpp
    subregion.cpp
    tree.cpp
    util/shapes.cpp)
target_link_libraries(ao-test ao-kernel ao-kernel-c)
