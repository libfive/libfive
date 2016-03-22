find_package(Boost REQUIRED)
find_package(png REQUIRED)

file(GLOB SRC_FILES src/tree/*.cpp src/format/*.cpp
                    src/render/*.cpp src/eval/*.cpp)

################################################################################

find_package(CUDA)
if (CUDA_FOUND)
    set(CUDA_NVCC_FLAGS
      ${CUDA_NVCC_FLAGS} -Xcompiler -Wno-language-extension-token)
    cuda_add_library(ao-kernel STATIC ${SRC_FILES} src/render/accelerator.cu)
else()
    add_library(ao-kernel STATIC ${SRC_FILES})
endif()

target_include_directories(ao-kernel SYSTEM PRIVATE
    ${BOOST_INCLUDE_DIR}
    ${PNG_INCLUDE_DIR})
target_include_directories(ao-kernel PUBLIC include)

target_link_libraries(ao-kernel ${PNG_LIBRARIES})
