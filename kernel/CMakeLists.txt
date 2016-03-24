find_package(Boost QUIET REQUIRED)
find_package(png REQUIRED)

file(GLOB SRC_FILES src/tree/*.cpp src/format/*.cpp
                    src/render/*.cpp src/eval/*.cpp)

################################################################################

if (CUDA_FOUND)
    set(CUDA_NVCC_FLAGS ${CUDA_NVCC_FLAGS}
        -O3 -Xcompiler -Wno-language-extension-token
        -gencode arch=compute_35,code=sm_35)

    # Need to add this so that nested CMake calls can find
    # include/ao/kernel/.../accelerator.hpp header
    include_directories(include)

    cuda_add_library(ao-kernel STATIC ${SRC_FILES} src/eval/accelerator.cu)
else()
    add_library(ao-kernel STATIC ${SRC_FILES})
endif()

target_include_directories(ao-kernel SYSTEM PRIVATE
    ${BOOST_INCLUDE_DIR}
    ${PNG_INCLUDE_DIR})
target_include_directories(ao-kernel PUBLIC include)

target_link_libraries(ao-kernel ${PNG_LIBRARIES})
