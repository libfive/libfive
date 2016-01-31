file(GLOB SRC_FILES *.cpp)
add_executable(ao-ui-test ${SRC_FILES})
target_link_libraries(ao-ui-test ao-ui)
