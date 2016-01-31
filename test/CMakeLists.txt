file(GLOB SRC_FILES ../ui/test/*.cpp ../kernel/test/*.cpp)
include_directories(../ui/include)
include_directories(../kernel/include)

add_executable(ao-test ${SRC_FILES} main.cpp)
target_link_libraries(ao-test ao-kernel ao-ui)
