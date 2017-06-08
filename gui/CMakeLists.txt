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
    src/axes.cpp
    src/background.cpp
    src/syntax.cpp
    src/color.cpp
    src/editor.cpp
    src/interpreter.cpp
    src/view.cpp

    resources.qrc
)
set(QOBJECTS
    include/gui/app.hpp
    include/gui/syntax.hpp
    include/gui/editor.hpp
    include/gui/interpreter.hpp
    include/gui/view.hpp
)

if(APPLE)
    add_executable(Studio MACOSX_BUNDLE ${SRCS} ${QOBJECTS})

    # Set application Info.plist file
    set_target_properties(Studio PROPERTIES
        MACOSX_BUNDLE_INFO_PLIST ${CMAKE_CURRENT_SOURCE_DIR}/deploy/mac/Info.plist
    )
else(APPLE)
    add_executable(Studio ${SRCS})
endif()

target_include_directories(Studio PUBLIC include)
target_link_libraries(Studio guile-ao-kernel
    Qt5::Widgets
    Qt5::Gui
    Qt5::OpenGL
    Qt5::Network
    Qt5::Concurrent
)

################################################################################
