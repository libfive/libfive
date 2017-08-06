find_package(Boost QUIET REQUIRED)
find_package(PNG REQUIRED)
find_package(Eigen3 REQUIRED)

################################################################################

add_library(ao-kernel-core STATIC
    eval/evaluator_avx.cpp
    eval/evaluator_base.cpp
    eval/result.cpp
    eval/feature.cpp
    render/discrete/heightmap.cpp
    render/discrete/voxels.cpp
    render/brep/xtree.cpp
    render/brep/contours.cpp
    render/brep/mesh.cpp
    render/brep/marching.cpp
    solve/solver.cpp
    tree/cache.cpp
    tree/opcode.cpp
    tree/template.cpp
    tree/tree.cpp
)

target_include_directories(ao-kernel-core SYSTEM PRIVATE
    ${BOOST_INCLUDE_DIR}
    ${PNG_INCLUDE_DIR}
)
target_include_directories(ao-kernel-core PUBLIC
    ../include
    ${EIGEN3_INCLUDE_DIR} # Eigen::Vector3f is used in APIs
)

target_link_libraries(ao-kernel-core ${PNG_LIBRARIES})
