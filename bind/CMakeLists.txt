find_path(GUILE_INCLUDE_DIR libguile.h /usr/local/include/guile/2.0)
find_library(GUILE_LIBRARY NAMES guile-2.0)

add_library(ao SHARED api.cpp guile/guile.cpp)
target_link_libraries(ao ao-kernel ao-ui ${GUILE_LIBRARY})
target_include_directories(ao PRIVATE ${GUILE_INCLUDE_DIR} .)

add_custom_command(
    TARGET ao POST_BUILD
    COMMAND mkdir -p ${PROJECT_SOURCE_DIR}/lib
    COMMAND cp ${CMAKE_CURRENT_BINARY_DIR}/libao*
               ${PROJECT_SOURCE_DIR}/lib
)
