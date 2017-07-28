add_library(ao-kernel SHARED lib.cpp)
target_link_libraries(ao-kernel ao-kernel-core)
target_include_directories(ao-kernel PUBLIC .)

if (GUILE_FOUND)
    add_library(guile-ao-kernel SHARED lib.cpp guile.cpp)
    target_link_libraries(guile-ao-kernel ao-kernel ${GUILE_LIBRARIES})
    target_include_directories(guile-ao-kernel PUBLIC . ${GUILE_INCLUDE_DIRS})
endif(GUILE_FOUND)
