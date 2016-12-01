find_package(Boost QUIET REQUIRED)
find_package(PNG REQUIRED)

################################################################################

file(GLOB SRC_FILES src/tree/*.cpp src/format/*.cpp
                    src/render/*.cpp src/eval/*.cpp src/solve/*.cpp)
add_library(ao-kernel STATIC ${SRC_FILES})

target_include_directories(ao-kernel SYSTEM PRIVATE
    ${BOOST_INCLUDE_DIR}
    ${PNG_INCLUDE_DIR})
target_include_directories(ao-kernel PUBLIC include)

target_link_libraries(ao-kernel ${PNG_LIBRARIES})
