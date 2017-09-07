if (GUILE_FOUND)
    ADD_CUSTOM_COMMAND(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/bundle.cpp
        COMMAND ${CMAKE_COMMAND} -P ${CMAKE_CURRENT_SOURCE_DIR}/bundle.cmake
        DEPENDS shapes.scm csg.scm transforms.scm)

    add_library(guile-ao-kernel SHARED ${CMAKE_CURRENT_BINARY_DIR}/bundle.cpp)
    target_link_libraries(guile-ao-kernel ao-kernel ${GUILE_LIBRARIES})
    target_include_directories(guile-ao-kernel PUBLIC . ${GUILE_INCLUDE_DIRS})
endif(GUILE_FOUND)
