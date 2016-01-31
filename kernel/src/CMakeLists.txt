find_package(Boost REQUIRED)

find_package(png REQUIRED)

################################################################################

file(GLOB SRC_FILES tree/*.cpp format/*.cpp ui/*.cpp
                    render/*.cpp eval/*.cpp)
add_library(ao-kernel STATIC ${SRC_FILES})

target_include_directories(ao-kernel PUBLIC
    ${BOOST_INCLUDE_DIR}
    ${PNG_INCLUDE_DIR})
target_link_libraries(ao-kernel
    ${PNG_LIBRARIES})
