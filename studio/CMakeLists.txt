# Sanity-checking to make sure CMake was run against the correct directory
if (NOT DEFINED LIBFIVE_BUILD_FROM_ROOT)
    message(FATAL_ERROR "CMake must be called against top-level directory")
endif()

# Instruct CMake to run moc, uic, and rrc automatically when needed.
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTOUIC ON)
set(CMAKE_AUTORCC ON)

# Find the Qt libraries
if (Qt5Core_FOUND)
    find_package(Qt5Core REQUIRED)
    find_package(Qt5Widgets REQUIRED)
    find_package(Qt5OpenGL REQUIRED)
    find_package(Qt5Network REQUIRED)
    find_package(Qt5Concurrent REQUIRED)
elseif (Qt6Core_FOUND)
    find_package(Qt6Core REQUIRED)
    find_package(Qt6Widgets REQUIRED)
    find_package(Qt6OpenGLWidgets REQUIRED)
    find_package(Qt6Network REQUIRED)
    find_package(Qt6Concurrent REQUIRED)
endif()

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
    src/syntax.cpp
    src/view.cpp
    src/window.cpp

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
)

if(STUDIO_WITH_GUILE)
    set(SRCS ${SRCS}
        src/guile/formatter.cpp
        src/guile/interpreter.cpp
        src/guile/language.cpp
        src/guile/syntax.cpp
    )
endif(STUDIO_WITH_GUILE)

if(STUDIO_WITH_PYTHON)
    set(SRCS ${SRCS}
        src/python/formatter.cpp
        src/python/interpreter.cpp
        src/python/language.cpp
        src/python/syntax.cpp
    )
endif(STUDIO_WITH_PYTHON)

if(APPLE)
    add_executable(Studio MACOSX_BUNDLE ${SRCS} src/platform_darwin.mm ${QOBJECTS})
    set_target_properties(Studio PROPERTIES
           MACOSX_BUNDLE_INFO_PLIST ${CMAKE_CURRENT_SOURCE_DIR}/deploy/mac/Info.plist
    )
    find_library(COCOA_LIBRARY Cocoa)
    set(EXTRA_LIBS ${COCOA_LIBRARY})
elseif(WIN32)
    add_executable(Studio WIN32 ${SRCS} ${QOBJECTS} deploy/windows/icon.rc)
else()
    add_executable(Studio ${SRCS} ${QOBJECTS})
endif()

if(UNIX AND NOT(APPLE))
    install(TARGETS Studio DESTINATION bin)
endif(UNIX AND NOT(APPLE))

target_include_directories(Studio PUBLIC include)
target_link_libraries(Studio libfive libfive-stdlib ${EXTRA_LIBS})

if (Qt5Core_FOUND)
    target_link_libraries(Studio libfive libfive-stdlib
        Qt5::Widgets
        Qt5::Gui
        Qt5::OpenGL
        Qt5::Concurrent
    )
elseif (Qt6Core_FOUND)
    target_link_libraries(Studio
        Qt::Widgets
        Qt::Gui
        Qt::OpenGLWidgets
        Qt::Concurrent
    )
endif()

if(STUDIO_WITH_GUILE)
    target_include_directories(Studio PRIVATE ${GUILE_INCLUDE_DIRS})
    target_link_directories(Studio PUBLIC ${GUILE_LIBRARY_DIRS})
    target_link_libraries(Studio ${GUILE_LIBRARIES})
    set_property(
        SOURCE src/editor.cpp
        APPEND PROPERTY COMPILE_DEFINITIONS
        STUDIO_WITH_GUILE)
    add_dependencies(Studio libfive-guile)
endif(STUDIO_WITH_GUILE)

if(STUDIO_WITH_PYTHON)
    target_include_directories(Studio PRIVATE ${Python3_INCLUDE_DIRS})
    target_link_libraries(Studio ${Python3_LIBRARIES})
    set_property(
        SOURCE src/editor.cpp
        APPEND PROPERTY COMPILE_DEFINITIONS
        STUDIO_WITH_PYTHON)
    add_dependencies(Studio libfive-python)
endif(STUDIO_WITH_PYTHON)

if (MSVC)
    # Copy the DLLs into the build directory, so that Studio.exe can find them
    add_custom_command(TARGET Studio POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
            $<TARGET_FILE:libfive>
            $<TARGET_FILE_DIR:Studio>)
    add_custom_command(TARGET Studio POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
            $<TARGET_FILE:libfive-stdlib>
            $<TARGET_FILE_DIR:Studio>)
endif(MSVC)
