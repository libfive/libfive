project (efsw)

cmake_minimum_required(VERSION 2.8)

option (VERBOSE "Build efsw with verbose mode.")
option (STATIC_LIB "Build efsw as a static library")
option (BUILD_TEST_APP "Build the test app")

if (VERBOSE)
	add_definitions(-DEFSW_VERBOSE)
endif()

set(SRCS
	src/efsw/Debug.cpp
	src/efsw/DirectorySnapshot.cpp
	src/efsw/DirectorySnapshotDiff.cpp
	src/efsw/DirWatcherGeneric.cpp
	src/efsw/FileInfo.cpp
	src/efsw/FileSystem.cpp
	src/efsw/FileWatcher.cpp
	src/efsw/FileWatcherCWrapper.cpp
	src/efsw/FileWatcherFSEvents.cpp
	src/efsw/FileWatcherGeneric.cpp
	src/efsw/FileWatcherImpl.cpp
	src/efsw/FileWatcherInotify.cpp
	src/efsw/FileWatcherKqueue.cpp
	src/efsw/FileWatcherWin32.cpp
	src/efsw/Log.cpp
	src/efsw/Mutex.cpp
	src/efsw/String.cpp
	src/efsw/System.cpp
	src/efsw/Thread.cpp
	src/efsw/Watcher.cpp
	src/efsw/WatcherFSEvents.cpp
	src/efsw/WatcherGeneric.cpp
	src/efsw/WatcherInotify.cpp
	src/efsw/WatcherKqueue.cpp
	src/efsw/WatcherWin32.cpp)

include_directories(include src)	

if (WIN32)
	list (APPEND SRCS
		  src/efsw/platform/win/FileSystemImpl.cpp
		  src/efsw/platform/win/MutexImpl.cpp
		  src/efsw/platform/win/SystemImpl.cpp
		  src/efsw/platform/win/ThreadImpl.cpp)
else ()
    list (APPEND SRCS
          src/efsw/platform/posix/FileSystemImpl.cpp
          src/efsw/platform/posix/MutexImpl.cpp
          src/efsw/platform/posix/SystemImpl.cpp
          src/efsw/platform/posix/ThreadImpl.cpp)
endif()          

if (APPLE)
	list (REMOVE_ITEM SRCS
		  "src/efsw/WatcherInotify.cpp"
		  "src/efsw/WatcherWin32.cpp"
		  "src/efsw/FileWatcherInotify.cpp"
		  "src/efsw/FileWatcherWin32.cpp")

	exec_program(uname ARGS -v  OUTPUT_VARIABLE OSX_VERSION)
   	string(REGEX MATCH "[0-9]+" OSX_VERSION ${OSX_VERSION})
   	if (NOT OSX_VERSION GREATER 9)
    	add_definitions(-DEFSW_FSEVENTS_NOT_SUPPORTED)
   	endif()  
elseif (WIN32)
	list (REMOVE_ITEM SRCS 
		  "src/efsw/WatcherKqueue.cpp" 
		  "src/efsw/WatcherFSEvents.cpp" 
		  "src/efsw/WatcherInotify.cpp"
		  "src/efsw/FileWatcherKqueue.cpp"
		  "src/efsw/FileWatcherInotify.cpp"
		  "src/efsw/FileWatcherFSEvents.cpp")
elseif (${CMAKE_SYSTEM_NAME} MATCHES "Linux")
	list (REMOVE_ITEM SRCS
		  "src/efsw/WatcherKqueue.cpp"
		  "src/efsw/WatcherFSEvents.cpp"
		  "src/efsw/WatcherWin32.cpp"
		  "src/efsw/FileWatcherKqueue.cpp"
		  "src/efsw/FileWatcherWin32.cpp"
		  "src/efsw/FileWatcherFSEvents.cpp")

	if (NOT EXISTS "/usr/include/sys/inotify.h" AND NOT EXISTS "/usr/local/include/sys/inotify.h")
		add_definitions(-DEFSW_INOTIFY_NOSYS)
	endif()	  
elseif (${CMAKE_SYSTEM_NAME} MATCHES "FreeBSD")
	list (REMOVE_ITEM SRCS
		  "src/efsw/WatcherInotify.cpp"
		  "src/efsw/WatcherWin32.cpp"
		  "src/efsw/WatcherFSEvents.cpp"
		  "src/efsw/FileWatcherInotify.cpp"
		  "src/efsw/FileWatcherWin32.cpp"
		  "src/efsw/FileWatcherFSEvents.cpp")		  
endif()

if (MSVC)
	add_definitions(-D_SCL_SECURE_NO_WARNINGS)
else ()
	add_definitions(-Wall -Wno-long-long)
endif()

if (${CMAKE_BUILD_TYPE} MATCHES "Debug")
	add_definitions(-DDEBUG)
elseif (${CMAKE_BUILD_TYPE} MATCHES "Release")
	add_definitions(-DNDEBUG)
endif()

if (STATIC_LIB)
	add_library(efsw STATIC ${SRCS})
else()
	add_library(efsw SHARED ${SRCS})
endif()

if (APPLE)
	set(MAC_LIBS "-framework CoreFoundation" "-framework CoreServices")
	target_link_libraries(efsw ${MAC_LIBS})
elseif (NOT (${CMAKE_SYSTEM_NAME} MATCHES "Haiku") AND NOT WIN32)
	target_link_libraries(efsw pthread)
endif()	

set(LIB_DESTINATION "${CMAKE_INSTALL_PREFIX}/lib")
install(TARGETS efsw LIBRARY DESTINATION ${LIB_DESTINATION} ARCHIVE DESTINATION ${LIB_DESTINATION} RUNTIME DESTINATION ${LIB_DESTINATION})
file (GLOB HDRS "include/efsw/*.h" "include/efsw/*.hpp")
install (FILES ${HDRS} DESTINATION ${CMAKE_INSTALL_PREFIX}/include/efsw)

if (BUILD_TEST_APP)
    add_executable(efsw-test src/test/efsw-test.cpp)
    target_link_libraries(efsw-test efsw)
endif()
