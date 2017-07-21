find_package(Boost QUIET REQUIRED)
find_package(PNG REQUIRED)
find_package(Eigen3 REQUIRED)

################################################################################

add_library(ao-kernel STATIC
    eval/evaluator_avx.cpp
    eval/evaluator_base.cpp
    eval/result.cpp
    eval/feature.cpp
    #format/contours.cpp
    #format/mesh.cpp
    #render/dc.cpp
    #render/dc2d.cpp
    render/discrete/heightmap.cpp
    #render/octree.cpp
    #render/quadtree.cpp
    render/discrete/voxels.cpp
    render/brep/xtree.cpp
    render/brep/interpolator.cpp
    render/brep/scaffold.cpp
    render/brep/ms.cpp
    render/brep/mt.cpp
    render/brep/contours.cpp
    render/brep/mesh.cpp
    #    render/subregion.cpp
    solve/solver.cpp
    tree/cache.cpp
    tree/opcode.cpp
    tree/template.cpp
    tree/tree.cpp
)

target_include_directories(ao-kernel SYSTEM PRIVATE
    ${BOOST_INCLUDE_DIR}
    ${PNG_INCLUDE_DIR}
)
target_include_directories(ao-kernel PUBLIC
    ../include
    ${EIGEN3_INCLUDE_DIR} # Eigen::Vector3f is used in APIs
)

target_link_libraries(ao-kernel ${PNG_LIBRARIES})
