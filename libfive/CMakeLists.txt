add_subdirectory(src)
add_subdirectory(test)

if (GUILE_FOUND)
    add_subdirectory(bind)
endif(GUILE_FOUND)
