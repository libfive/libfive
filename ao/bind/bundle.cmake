# Read in all of the Scheme files
file(READ ${CMAKE_CURRENT_LIST_DIR}/shapes.scm SHAPES)
file(READ ${CMAKE_CURRENT_LIST_DIR}/csg.scm CSG)
file(READ ${CMAKE_CURRENT_LIST_DIR}/transforms.scm TRANSFORMS)
file(READ ${CMAKE_CURRENT_LIST_DIR}/vec.scm VEC)
file(READ ${CMAKE_CURRENT_LIST_DIR}/sandbox.scm SANDBOX)

# Escape any strings that look like a termination sequence )"
string(REPLACE ")\"" ")\\\"" SHAPES "${SHAPES}")
string(REPLACE ")\"" ")\\\"" CSG "${CSG}")
string(REPLACE ")\"" ")\\\"" TRANSFORMS "${TRANSFORMS}")
string(REPLACE ")\"" ")\\\"" VEC "${VEC}")
string(REPLACE ")\"" ")\\\"" SANDBOX "${SANDBOX}")

# Read in the main C++ file
file(READ ${CMAKE_CURRENT_LIST_DIR}/ao-guile.cpp AO_GUILE)

# Perform string substitution to splice the Scheme text into the C++ source
string(REPLACE "AO_GUILE_SHAPES" "${SHAPES}" AO_GUILE "${AO_GUILE}")
string(REPLACE "AO_GUILE_CSG" "${CSG}" AO_GUILE "${AO_GUILE}")
string(REPLACE "AO_GUILE_TRANSFORMS" "${TRANSFORMS}" AO_GUILE "${AO_GUILE}")
string(REPLACE "AO_GUILE_VEC" "${VEC}" AO_GUILE "${AO_GUILE}")
string(REPLACE "AO_GUILE_SANDBOX" "${SANDBOX}" AO_GUILE "${AO_GUILE}")

file(WRITE ${CMAKE_CURRENT_SOURCE_DIR}/bundle.cpp "${AO_GUILE}")
