add_library(ao-kernel SHARED
    eval/evaluator.cpp
    eval/eval_interval.cpp
    eval/eval_array.cpp
    eval/eval_deriv_array.cpp
    eval/eval_deriv.cpp
    eval/eval_feature.cpp
    eval/eval_point.cpp
    eval/result.cpp
    eval/tape.cpp
    eval/feature.cpp
    render/discrete/heightmap.cpp
    render/discrete/voxels.cpp
    render/brep/xtree.cpp
    render/brep/contours.cpp
    render/brep/mesh.cpp
    render/brep/marching.cpp
    solve/bounds.cpp
    solve/solver.cpp
    tree/cache.cpp
    tree/opcode.cpp
    tree/template.cpp
    tree/tree.cpp
    ao.cpp
)

target_include_directories(ao-kernel SYSTEM PRIVATE
    ${BOOST_INCLUDE_DIR}
    ${PNG_INCLUDE_DIR}
)
target_include_directories(ao-kernel PUBLIC
    ../include
    ${EIGEN3_INCLUDE_DIR} # Eigen::Vector3f is used in APIs
)

set(LIBS ${PNG_LIBRARIES})

if(UNIX)
    set(LIBS ${LIBS} ${CMAKE_THREAD_LIBS_INIT})
endif(UNIX)

target_link_libraries(ao-kernel ${LIBS})
