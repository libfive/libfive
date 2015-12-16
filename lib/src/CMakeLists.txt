find_path(GUILE_INCLUDE_DIR libguile.h /usr/local/include/guile/2.0)
find_library(GUILE_LIBRARY NAMES guile-2.0 PATH /usr/local/lib)

find_package(Boost REQUIRED)

find_package(png REQUIRED)

################################################################################

include_directories(${GUILE_INCLUDE_DIR})
include_directories(${BOOST_INCLUDE_DIR})
include_directories(${PNG_INCLUDE_DIR})

################################################################################

file(GLOB SRC_FILES core/*.cpp format/*.cpp bind/*.cpp ui/*.cpp render/*.cpp)
add_library(ao SHARED ${SRC_FILES})

target_link_libraries(ao ${GUILE_LIBRARY})
target_link_libraries(ao ${PNG_LIBRARIES})
