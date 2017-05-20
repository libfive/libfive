find_package(Boost QUIET REQUIRED)
find_package(PNG REQUIRED)
find_package(Eigen3 REQUIRED)
find_package(GLM REQUIRED)

################################################################################

add_library(ao-kernel STATIC
    eval/evaluator_avx.cpp
    eval/evaluator_base.cpp
    eval/result.cpp
    eval/feature.cpp
    format/contours.cpp
    format/image.cpp
    format/mesh.cpp
    render/dc.cpp
    render/dc2d.cpp
    render/heightmap.cpp
    render/octree.cpp
    render/quadtree.cpp
    render/region.cpp
    render/subregion.cpp
    solve/solver.cpp
    tree/cache.cpp
    tree/opcode.cpp
    tree/tree.cpp
)

target_include_directories(ao-kernel SYSTEM PUBLIC
    ${BOOST_INCLUDE_DIR}
    ${PNG_INCLUDE_DIR}
    ${EIGEN3_INCLUDE_DIR}
    ${GLM_INCLUDE_DIR})
target_include_directories(ao-kernel PUBLIC ../include)

target_link_libraries(ao-kernel ${PNG_LIBRARIES})
