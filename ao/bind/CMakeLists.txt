if (GUILE_FOUND)
    ADD_CUSTOM_COMMAND(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/bundle.cpp
        COMMAND ${CMAKE_COMMAND} -P ${CMAKE_CURRENT_SOURCE_DIR}/bundle.cmake
        DEPENDS ao-guile.cpp shapes.scm csg.scm transforms.scm text.scm vec.scm
                sandbox.scm ${CMAKE_CURRENT_SOURCE_DIR}/bundle.cmake)

    add_library(guile-ao SHARED ${CMAKE_CURRENT_BINARY_DIR}/bundle.cpp)
    target_link_libraries(guile-ao ao-kernel ${GUILE_LIBRARIES})
    target_include_directories(guile-ao PUBLIC . ${GUILE_INCLUDE_DIRS})
endif(GUILE_FOUND)
