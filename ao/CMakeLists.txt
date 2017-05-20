find_package(Boost QUIET REQUIRED)
find_package(PNG REQUIRED)

################################################################################

add_library(straylight-kernel STATIC
    src/bind/bind_s7.cpp
    src/eval/evaluator_avx.cpp
    src/eval/evaluator_base.cpp
    src/eval/result.cpp
    src/eval/feature.cpp
    src/format/contours.cpp
    src/format/image.cpp
    src/format/mesh.cpp
    src/render/dc.cpp
    src/render/dc2d.cpp
    src/render/heightmap.cpp
    src/render/octree.cpp
    src/render/quadtree.cpp
    src/render/region.cpp
    src/render/subregion.cpp
    src/solve/solver.cpp
    src/tree/cache.cpp
    src/tree/opcode.cpp
    src/tree/tree.cpp
)

target_include_directories(straylight-kernel SYSTEM PRIVATE
    ${BOOST_INCLUDE_DIR}
    ${PNG_INCLUDE_DIR})
target_include_directories(straylight-kernel PUBLIC include)

target_link_libraries(straylight-kernel ${PNG_LIBRARIES})
