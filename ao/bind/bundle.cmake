# Read in the main C++ file
file(READ ${CMAKE_CURRENT_LIST_DIR}/ao-guile.cpp AO_GUILE)

# LOAD is a macro that automatically drops a SCM file into the AO_GUILE source
macro(LOAD TARGET)
    # Read in the target Scheme file
    string(TOLOWER "${CMAKE_CURRENT_LIST_DIR}/${TARGET}.scm" FILENAME)
    file(READ ${FILENAME} SCM)

    # Modify any substrings that look like a termination sequence )"
    string(REPLACE ")\"" ") \"" SCM "${SCM}")

    # Perform string substitution to splice the Scheme text into the C++ source
    string(REPLACE "AO_GUILE_${TARGET}" "${SCM}" AO_GUILE "${AO_GUILE}")
endmacro(LOAD)

LOAD(SHAPES)
LOAD(CSG)
LOAD(TRANSFORMS)
LOAD(TEXT)
LOAD(VEC)
LOAD(SANDBOX)

file(WRITE ${CMAKE_CURRENT_SOURCE_DIR}/bundle.cpp "${AO_GUILE}")
