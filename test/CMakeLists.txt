file(GLOB SRC_FILES ../ui/test/*.cpp ../kernel/test/*.cpp)

add_executable(ao-test ${SRC_FILES} main.cpp)
target_link_libraries(ao-test ao-kernel ao-ui)
