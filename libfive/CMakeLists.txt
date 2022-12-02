# Sanity-checking to make sure CMake was run against the correct directory
if (NOT DEFINED LIBFIVE_BUILD_FROM_ROOT)
    message(FATAL_ERROR "CMake must be called against top-level directory")
endif()

option(BUILD_TESTS "Build test suite" ON)

add_subdirectory(src)
add_subdirectory(stdlib)

if (BUILD_TESTS)
  add_subdirectory(test)
endif(BUILD_TESTS)

if (BUILD_GUILE_BINDINGS AND GUILE_FOUND)
  add_subdirectory(bind/guile)
endif(BUILD_GUILE_BINDINGS AND GUILE_FOUND)

if (BUILD_PYTHON_BINDINGS AND Python3_FOUND)
  add_subdirectory(bind/python)
endif()

if(UNIX AND NOT(APPLE))
    set(DEST_DIR "${CMAKE_INSTALL_PREFIX}")
    CONFIGURE_FILE("libfive.pc.in" "libfive.pc" @ONLY)
    install(FILES ${CMAKE_BINARY_DIR}/libfive/libfive.pc DESTINATION ${CMAKE_INSTALL_LIBDIR}/pkgconfig)
endif(UNIX AND NOT(APPLE))
