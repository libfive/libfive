add_library(five-stdlib SHARED stdlib.cpp stdlib_impl.cpp)
target_link_libraries(five-stdlib five)

if (UNIX)
    install(TARGETS five-stdlib DESTINATION lib)
endif(UNIX)
