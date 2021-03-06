set(SRCS
    libfive/__init__.py
    libfive/ffi.py
    libfive/shape.py
    libfive/stdlib/csg.py
    libfive/stdlib/shapes.py
    libfive/stdlib/transforms.py
    libfive/stdlib/text.py
)

set(OUTS "")
foreach(SRC ${SRCS})
    set(FULL_SRC ${CMAKE_CURRENT_SOURCE_DIR}/${SRC})
    list(APPEND OUTS ${SRC})

    # We copy each file from the source directory to the build dir
    add_custom_command(OUTPUT ${SRC}
        COMMAND ${CMAKE_COMMAND} -E copy ${FULL_SRC} ${CMAKE_CURRENT_BINARY_DIR}/${SRC}
        WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
        DEPENDS ${FULL_SRC})
endforeach(SRC)

add_custom_target(libfive-python ALL DEPENDS ${OUTS})

################################################################################

# TODO: make installation work
