file(GLOB SRC_FILES *.cpp)
add_executable(ao-kernel-test ${SRC_FILES})
target_link_libraries(ao-kernel-test ao-kernel)
