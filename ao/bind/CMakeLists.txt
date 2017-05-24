add_library(ao-kernel-c SHARED lib.cpp)
target_link_libraries(ao-kernel-c ao-kernel)
target_include_directories(ao-kernel-c PUBLIC .)
