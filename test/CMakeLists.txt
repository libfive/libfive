file(GLOB SRC_FILES ../ui/test/*.cpp ../kernel/test/*.cpp)

add_executable(ao-test ${SRC_FILES} main.cpp api.cpp)
target_include_directories(ao-test PUBLIC ../bind)
target_link_libraries(ao-test ao)
