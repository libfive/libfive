# Install libfive's Scheme bindings to site package directory
execute_process(
    COMMAND guile -c "(format #t \"~A\" (%site-dir))"
    OUTPUT_VARIABLE GUILE_SITEDIR)
install(DIRECTORY libfive/ DESTINATION ${GUILE_SITEDIR}/libfive
        FILES_MATCHING PATTERN "*.scm")
