file(GLOB_RECURSE SRC_FILES
     ../kernel/test/*.cpp
     ../ui/test/*.cpp
     ../bind/test/*.cpp)

add_executable(ao-test ${SRC_FILES} main.cpp)
target_include_directories(ao-test PUBLIC ../bind)
target_link_libraries(ao-test ao)
