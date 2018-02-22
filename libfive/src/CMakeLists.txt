# Add a custom command that produces version.c, plus
# a dummy output that's not actually produced, in order
# to force version.cmake to always be re-run before the build
ADD_CUSTOM_COMMAND(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/version.c
           ${CMAKE_CURRENT_BINARY_DIR}/_version.c
    COMMAND ${CMAKE_COMMAND} -P ${CMAKE_CURRENT_SOURCE_DIR}/version.cmake)

if(MSVC)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /DFIVE_DLL")
endif()

add_library(five SHARED
    eval/base.cpp
    eval/eval_interval.cpp
    eval/eval_interval_array.cpp
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
    render/brep/xtree2.cpp
    render/brep/xtree3.cpp
    render/brep/contours.cpp
    render/brep/mesh.cpp
    render/brep/marching.cpp
    solve/bounds.cpp
    solve/solver.cpp
    tree/cache.cpp
    tree/opcode.cpp
    tree/oracle.cpp
    tree/template.cpp
    tree/transformed_oracle.cpp
    tree/tree.cpp
    libfive.cpp

    ${CMAKE_CURRENT_BINARY_DIR}/version.c
)

target_include_directories(five SYSTEM PUBLIC
    ${Boost_INCLUDE_DIRS}
    ${PNG_INCLUDE_DIR}
    ${EIGEN_INCLUDE_DIRS}
)
target_include_directories(five PUBLIC
    ../include
)

set(LIBS ${PNG_LIBRARIES})

if(UNIX AND NOT(APPLE))
    set(LIBS ${LIBS} ${CMAKE_THREAD_LIBS_INIT})
endif(UNIX AND NOT(APPLE))

if (UNIX)
    install(TARGETS five DESTINATION lib)
    install(FILES ../include/libfive.h DESTINATION include)
    install(DIRECTORY ../include/libfive DESTINATION include)
endif(UNIX)

target_link_libraries(five ${LIBS})
