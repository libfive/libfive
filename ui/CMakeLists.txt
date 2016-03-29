find_path(EPOXY_INCLUDE_DIR epoxy/gl.h)
find_library(EPOXY_LIBRARY NAMES epoxy)

find_package(PkgConfig REQUIRED)
pkg_search_module(GLFW REQUIRED glfw3)

find_library(OPENGL_LIBRARIES OpenGL)

################################################################################

add_definitions(-DGLFW_INCLUDE_GLCOREARB)

################################################################################

file(GLOB SRC_FILES src/*.cpp src/gl/*.cpp)
add_library(ao-ui STATIC ${SRC_FILES})

target_include_directories(ao-ui SYSTEM PRIVATE
    ${GLFW_INCLUDE_DIRS}
    ${EPOXY_INCLUDE_DIR})
target_include_directories(ao-ui PUBLIC include)
target_include_directories(ao-ui SYSTEM PUBLIC vendor)


target_link_libraries(ao-ui
    ${GLFW_LIBRARIES}
    ${OPENGL_LIBRARY}
    ${EPOXY_LIBRARY} efsw ao-kernel)

# Build efsw as a separate library
add_subdirectory(vendor/efsw)

################################################################################

# Mask error message that warns about including gl.h and gl3.h
# (since it's actually a false positive caused by epoxy/gl.h)
if (APPLE)
    add_definitions("-DGL_DO_NOT_WARN_IF_MULTI_GL_VERSION_HEADERS_INCLUDED")
endif (APPLE)
