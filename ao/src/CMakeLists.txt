add_library(ao-kernel SHARED
    eval/base.cpp
    eval/eval_affine.cpp
    eval/eval_interval.cpp
    eval/eval_jacobian.cpp
    eval/eval_array.cpp
    eval/eval_deriv_array.cpp
    eval/eval_deriv.cpp
    eval/eval_feature.cpp
    eval/eval_point.cpp
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

target_include_directories(ao-kernel SYSTEM PUBLIC
    ${Boost_INCLUDE_DIRS}
    ${PNG_INCLUDE_DIR}
    ${EIGEN3_INCLUDE_DIR}
)
target_include_directories(ao-kernel PUBLIC
    ../include
)

set(LIBS ${PNG_LIBRARIES})

if(UNIX AND NOT(APPLE))
    set(LIBS ${LIBS} ${CMAKE_THREAD_LIBS_INIT})
endif(UNIX AND NOT(APPLE))

target_link_libraries(ao-kernel ${LIBS})
