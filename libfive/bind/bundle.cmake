# Read in the main C++ file
file(READ ${CMAKE_CURRENT_LIST_DIR}/libfive-guile.cpp LIBFIVE_GUILE)

# LOAD is a macro that automatically drops a SCM file into the LIBFIVE_GUILE source
macro(LOAD TARGET)
    # Read in the target Scheme file
    string(TOLOWER "${TARGET}.scm" FILENAME)
    file(READ "${CMAKE_CURRENT_LIST_DIR}/${FILENAME}" SCM)

    # Modify any substrings that look like a termination sequence )"
    string(REPLACE ")\"" ") \"" SCM "${SCM}")

    # Perform string substitution to splice the Scheme text into the C++ source
    string(REPLACE "LIBFIVE_GUILE_${TARGET}" "${SCM}" LIBFIVE_GUILE "${LIBFIVE_GUILE}")
endmacro(LOAD)

LOAD(SHAPES)
LOAD(CSG)
LOAD(TRANSFORMS)
LOAD(TEXT)
LOAD(VEC)
LOAD(SANDBOX)
LOAD(UTIL)

file(WRITE ${CMAKE_CURRENT_SOURCE_DIR}/bundle.cpp "${LIBFIVE_GUILE}")
