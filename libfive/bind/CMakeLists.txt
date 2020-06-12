ADD_CUSTOM_COMMAND(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/bundle.cpp
    COMMAND ${CMAKE_COMMAND} -P ${CMAKE_CURRENT_SOURCE_DIR}/bundle.cmake
    DEPENDS libfive-guile.cpp shapes.scm csg.scm transforms.scm text.scm util.scm vec.scm
            sandbox.scm ${CMAKE_CURRENT_SOURCE_DIR}/bundle.cmake)

add_library(five-guile SHARED ${CMAKE_CURRENT_BINARY_DIR}/bundle.cpp)

add_library(five::guile ALIAS five-guile)

#add_dependencies(five-guile five)
target_link_libraries(five-guile five ${GUILE_LIBRARIES})

# set(FIVE_GUILE_INCLUDE_DIRS ${CMAKE_CURRENT_SOURCE_DIR})

target_include_directories(five-guile
  PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
         $<INSTALL_INTERFACE:${CMAKE_INSTALL_PREFIX}/include>
         ${GUILE_INCLUDE_DIRS}
)



set_target_properties(five-guile
   PROPERTIES
   CXX_STANDARD "${FIVE_CXX_STANDARD}"
   CXX_STANDARD_REQUIRED "YES"
   CXX_EXTENSION "NO"
   INSTALL_RPATH ${CMAKE_INSTALL_PREFIX}/lib
)



install(
  TARGETS five-guile 
  EXPORT LibFiveTargets
  RUNTIME DESTINATION ${FIVE_INSTALL_BIN_DIR}
  LIBRARY DESTINATION ${FIVE_INSTALL_LIB_DIR}
  ARCHIVE DESTINATION ${FIVE_INSTALL_LIB_DIR}
)


install(
  FILES
    libfive-guile.h
  DESTINATION ${FIVE_INSTALL_INCLUDE_DIR}
)

