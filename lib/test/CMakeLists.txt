include_directories(../../vendor)
include_directories(../src)

file(GLOB SRC_FILES *.cpp)
add_executable(test-ao ${SRC_FILES})
target_link_libraries(test-ao ao)
