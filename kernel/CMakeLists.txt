find_package(Boost QUIET REQUIRED)
find_package(png REQUIRED)

################################################################################

file(GLOB_RECURSE SRC_FILES src/*.cpp)

################################################################################

if (CUDA_FOUND)
    set(CUDA_NVCC_FLAGS ${CUDA_NVCC_FLAGS}
        -O3 -Xcompiler -Wno-language-extension-token
        -gencode arch=compute_30,code=sm_30)

    # Need to add this so that nested CMake calls can find
    # include/ao/kernel/.../accelerator.hpp header
    include_directories(include)

    file(GLOB_RECURSE CUDA_FILES src/*.cu)
    cuda_add_library(ao-kernel STATIC ${SRC_FILES} ${CUDA_FILES})
else()
    add_library(ao-kernel STATIC ${SRC_FILES})
endif()

################################################################################

target_include_directories(ao-kernel SYSTEM PRIVATE
    ${BOOST_INCLUDE_DIR}
    ${PNG_INCLUDE_DIR})
target_include_directories(ao-kernel PUBLIC include)

target_link_libraries(ao-kernel ${PNG_LIBRARIES})
