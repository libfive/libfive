add_library(libfive-stdlib SHARED stdlib.cpp stdlib_impl.cpp)

# Produce a library named 'libfive-stdlib' on all platforms
set_target_properties(libfive-stdlib PROPERTIES PREFIX "")
target_link_libraries(libfive-stdlib libfive)

if (UNIX)
    install(TARGETS libfive-stdlib DESTINATION ${CMAKE_INSTALL_LIBDIR})
endif(UNIX)
