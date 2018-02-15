ADD_CUSTOM_COMMAND(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/bundle.cpp
    COMMAND ${CMAKE_COMMAND} -P ${CMAKE_CURRENT_SOURCE_DIR}/bundle.cmake
    DEPENDS libfive-guile.cpp shapes.scm csg.scm transforms.scm text.scm util.scm vec.scm
            sandbox.scm ${CMAKE_CURRENT_SOURCE_DIR}/bundle.cmake)

# five-guile library
add_library(five-guile SHARED ${CMAKE_CURRENT_BINARY_DIR}/bundle.cpp)
target_link_libraries(five-guile five ${GUILE_LIBRARIES})
target_include_directories(five-guile PUBLIC . ${GUILE_INCLUDE_DIRS})

# five-guile-console utility
if(UNIX AND NOT(APPLE))
    add_executable(five-guile-console five-guile-console.cpp)
    target_link_libraries(five-guile-console five five-guile ${GUILE_LIBRARIES})
    target_include_directories(five-guile-console PUBLIC . ${GUILE_INCLUDE_DIRS})
    install(TARGETS five-guile-console DESTINATION bin)
endif(UNIX AND NOT(APPLE))

if(UNIX)
    install(TARGETS five-guile DESTINATION lib)
endif(UNIX)
