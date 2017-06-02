add_library(ao-kernel-c SHARED lib.cpp)
target_link_libraries(ao-kernel-c ao-kernel)
target_include_directories(ao-kernel-c PUBLIC .)

if (GUILE_FOUND)
    add_library(guile-ao-kernel SHARED lib.cpp guile.cpp)
    target_link_libraries(guile-ao-kernel ao-kernel ${GUILE_LIBRARIES})
    target_include_directories(guile-ao-kernel PUBLIC . ${GUILE_INCLUDE_DIRS})
endif(GUILE_FOUND)
