find_path(GUILE_INCLUDE_DIR libguile.h /usr/local/include/guile/2.0)
find_library(GUILE_LIBRARY NAMES guile-2.0)

add_executable(ao-guile guile.cpp)
target_link_libraries(ao-guile ao efsw ${GUILE_LIBRARY})
target_include_directories(ao-guile PUBLIC ${GUILE_INCLUDE_DIR})

add_custom_command(
    TARGET ao-guile POST_BUILD
    COMMAND ln -sf ${CMAKE_CURRENT_BINARY_DIR}/ao-guile
                   ${PROJECT_SOURCE_DIR}/ao-guile
)
