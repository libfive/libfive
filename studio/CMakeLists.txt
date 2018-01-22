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
    src/arrow.cpp
    src/axes.cpp
    src/background.cpp
    src/bbox.cpp
    src/bars.cpp
    src/busy.cpp
    src/camera.cpp
    src/documentation.cpp
    src/icosphere.cpp
    src/script.cpp
    src/settings.cpp
    src/shader.cpp
    src/syntax.cpp
    src/shape.cpp
    src/color.cpp
    src/editor.cpp
    src/interpreter.cpp
    src/view.cpp
    src/window.cpp

    resources.qrc
)
set(QOBJECTS
    include/studio/app.hpp
    include/studio/busy.hpp
    include/studio/camera.hpp
    include/studio/documentation.hpp
    include/studio/settings.hpp
    include/studio/syntax.hpp
    include/studio/shape.hpp
    include/studio/editor.hpp
    include/studio/interpreter.hpp
    include/studio/view.hpp
    include/studio/window.hpp
)

if(APPLE)
    add_executable(Studio MACOSX_BUNDLE ${SRCS} ${QOBJECTS})
    set_target_properties(Studio PROPERTIES
           MACOSX_BUNDLE_INFO_PLIST ${CMAKE_CURRENT_SOURCE_DIR}/deploy/mac/Info.plist
    )
else()
    add_executable(Studio ${SRCS} ${QOBJECTS})
endif()

if(UNIX AND NOT(APPLE))
    install(TARGETS Studio DESTINATION bin RENAME studio)
endif(UNIX AND NOT(APPLE))

target_include_directories(Studio PUBLIC include)
target_link_libraries(Studio five-guile
    Qt5::Widgets
    Qt5::Gui
    Qt5::OpenGL
    Qt5::Concurrent
)
