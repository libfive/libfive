find_path(GUILE_INCLUDE_DIR libguile.h /usr/local/include/guile/2.0)
find_library(GUILE_LIBRARY NAMES guile-2.0)

find_path(EPOXY_INCLUDE_DIR epoxy/gl.h)
find_library(EPOXY_LIBRARY NAMES epoxy)

find_package(Boost REQUIRED)

find_package(png REQUIRED)

find_package(PkgConfig REQUIRED)
pkg_search_module(GLFW REQUIRED glfw3)

find_library(OPENGL_LIBRARY OpenGL)

################################################################################

add_definitions(-DGLFW_INCLUDE_GLCOREARB)

################################################################################

file(GLOB SRC_FILES core/*.cpp format/*.cpp ui/*.cpp
                    render/*.cpp gl/*.cpp)

add_library(ao STATIC ${SRC_FILES})
target_include_directories(ao PUBLIC
    ${BOOST_INCLUDE_DIR}
    ${PNG_INCLUDE_DIR}
    ${GLFW_INCLUDE_DIRS}
    ${EPOXY_INCLUDE_DIR})
target_link_libraries(ao
    ${PNG_LIBRARIES}
    ${GLFW_LIBRARIES}
    ${OPENGL_LIBRARY}
    ${EPOXY_LIBRARY})

################################################################################

add_executable(ao-guile bind/guile.cpp)
target_link_libraries(ao-guile ao ${GUILE_LIBRARY})
target_include_directories(ao-guile PUBLIC ${GUILE_INCLUDE_DIR})

add_custom_command(
    TARGET ao-guile POST_BUILD
    COMMAND ln -sf ${CMAKE_CURRENT_BINARY_DIR}/ao-guile
                   ${PROJECT_SOURCE_DIR}/../ao-guile
)

################################################################################

# Mask error message that warns about including gl.h and gl3.h
# (since it's actually a false positive caused by epoxy/gl.h)
if (APPLE)
    add_definitions("-DGL_DO_NOT_WARN_IF_MULTI_GL_VERSION_HEADERS_INCLUDED")
endif (APPLE)
