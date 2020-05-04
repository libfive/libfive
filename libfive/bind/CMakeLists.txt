ADD_CUSTOM_COMMAND(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/bundle.cpp
    COMMAND ${CMAKE_COMMAND} -P ${CMAKE_CURRENT_SOURCE_DIR}/bundle.cmake
    DEPENDS libfive-guile.cpp shapes.scm csg.scm transforms.scm text.scm util.scm vec.scm
            sandbox.scm ${CMAKE_CURRENT_SOURCE_DIR}/bundle.cmake)

add_library(five-guile SHARED ${CMAKE_CURRENT_BINARY_DIR}/bundle.cpp)
target_link_libraries(five-guile five ${GUILE_LIBRARIES})
target_include_directories(five-guile PUBLIC . ${GUILE_INCLUDE_DIRS})

if(UNIX)
    install(TARGETS five-guile DESTINATION lib)
    file(READ ${CMAKE_CURRENT_SOURCE_DIR}/libfive.scm libfive-scm-data)
    string(REPLACE "@libfive-guile-lib-location@" "${CMAKE_INSTALL_PREFIX}/lib"
      libfive-scm-out "${libfive-scm-data}")
    file(WRITE "${CMAKE_INSTALL_PREFIX}/share/guile/site/libfive.scm" "${libfive-scm-out}")
endif(UNIX)
