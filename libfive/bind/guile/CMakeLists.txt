# To improve application startup time, we precompile all of the Guile
# source files here, then use that search path when running unit tests
set(SRCS
    libfive/kernel.scm
    libfive/lib.scm
    libfive/sandbox.scm
    libfive/util.scm
    libfive/vec.scm

    libfive/stdlib/csg.scm
    libfive/stdlib/shapes.scm
    libfive/stdlib/text.scm
    libfive/stdlib/transforms.scm)

set(OUTS "")
foreach(SRC ${SRCS})
    set(FULL_SRC ${CMAKE_CURRENT_SOURCE_DIR}/${SRC})
    string(REPLACE ".scm" ".go" OUT ${SRC})
    list(APPEND OUTS ${OUT})

    # We compile each Guile file, but not to the build directory - instead, we
    # run the compiler without an output flag set, so that the file is saved
    # to the default cache.  This means that compilation for modules that refer
    # to each other will only happen once.
    #
    # Then, copy the file from Guile's compilation cache to the build directory
    execute_process(
        COMMAND guile -c "(use-modules (system base compile))(format #t \"~A\" (compiled-file-name \"${FULL_SRC}\"))"
        OUTPUT_VARIABLE CCACHE_FILE)
    add_custom_command(OUTPUT ${OUT}
        COMMAND guild compile -L${CMAKE_CURRENT_SOURCE_DIR} ${FULL_SRC}
        COMMAND cp ${CCACHE_FILE} ${OUT}
        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${SRC})
endforeach(SRC)

add_custom_target(libfive-guile ALL DEPENDS ${OUTS})

################################################################################

# Find the installation directory for compiled files
execute_process(
    COMMAND guile -c "(format #t \"~A\" (%site-ccache-dir))"
    OUTPUT_VARIABLE GUILE_CCACHE_DIR)

# Install pre-compiled libfive modules
install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/libfive/
        DESTINATION ${GUILE_CCACHE_DIR}/libfive
        FILES_MATCHING PATTERN "*.go")
