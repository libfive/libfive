# Sanity-checking to make sure CMake was run against the correct directory
if (NOT DEFINED LIBFIVE_BUILD_FROM_ROOT)
    message(FATAL_ERROR "CMake must be called against top-level directory")
endif()

# Instruct CMake to run moc, uic, and rrc automatically when needed.
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTOUIC ON)
set(CMAKE_AUTORCC ON)

# Find the Qt libraries
find_package(Qt5Core REQUIRED)
find_package(Qt5Widgets REQUIRED)
find_package(Qt5OpenGL REQUIRED)
find_package(Qt5Network REQUIRED)
find_package(Qt5Concurrent REQUIRED)

set(SRCS src/main.cpp
    src/app.cpp
    src/args.cpp
    src/arrow.cpp
    src/axes.cpp
    src/background.cpp
    src/bbox.cpp
    src/busy.cpp
    src/camera.cpp
    src/color.cpp
    src/documentation.cpp
    src/editor.cpp
    src/language.cpp
    src/script.cpp
    src/settings.cpp
    src/shader.cpp
    src/shape.cpp
    src/view.cpp
    src/window.cpp

    src/guile/formatter.cpp
    src/guile/interpreter.cpp
    src/guile/language.cpp
    src/guile/syntax.cpp

    src/python/formatter.cpp
    src/python/interpreter.cpp
    src/python/language.cpp
    src/python/syntax.cpp

    resources.qrc
)
set(QOBJECTS
    include/studio/app.hpp
    include/studio/busy.hpp
    include/studio/camera.hpp
    include/studio/documentation.hpp
    include/studio/editor.hpp
    include/studio/interpreter.hpp
    include/studio/language.hpp
    include/studio/shape.hpp
    include/studio/syntax.hpp
    include/studio/view.hpp
    include/studio/window.hpp

    include/studio/guile/interpreter.hpp
    include/studio/guile/syntax.hpp

    include/studio/python/interpreter.hpp
    include/studio/python/syntax.hpp
)

if(APPLE)
    add_executable(Studio MACOSX_BUNDLE ${SRCS} src/platform_darwin.mm ${QOBJECTS})
    set_target_properties(Studio PROPERTIES
           MACOSX_BUNDLE_INFO_PLIST ${CMAKE_CURRENT_SOURCE_DIR}/deploy/mac/Info.plist
    )
    find_library(COCOA_LIBRARY Cocoa)
    set(EXTRA_LIBS ${COCOA_LIBRARY})
else()
    add_executable(Studio ${SRCS} ${QOBJECTS})
endif()

if(UNIX AND NOT(APPLE))
    install(TARGETS Studio DESTINATION bin)
endif(UNIX AND NOT(APPLE))

target_include_directories(Studio
    PUBLIC include
    PRIVATE ${GUILE_INCLUDE_DIRS} ${Python3_INCLUDE_DIRS})
target_link_libraries(Studio five
    Qt5::Widgets
    Qt5::Gui
    Qt5::OpenGL
    Qt5::Concurrent
    ${GUILE_LIBRARIES}
    ${Python3_LIBRARIES}
    ${EXTRA_LIBS}
)

target_link_directories(Studio PUBLIC ${GUILE_LIBRARY_DIRS})
