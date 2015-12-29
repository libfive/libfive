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

# Mask error message that warns about including gl.h and gl3.h
# (since it's actually a false positive caused by epoxy/gl.h)
if (APPLE)
    add_definitions("-DGL_DO_NOT_WARN_IF_MULTI_GL_VERSION_HEADERS_INCLUDED")
endif (APPLE)
