cmake_minimum_required(VERSION 3.7)
project(cppfind)

set(CMAKE_CXX_STANDARD 11)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")


set(Boost_USE_MULTITHREADED ON)
find_package(Boost REQUIRED COMPONENTS system filesystem)

if (DEFINED ENV{XFIND_PATH})
        set(XFIND_PATH $ENV{XFIND_PATH})
        message("XFIND_PATH: ${XFIND_PATH}")
else()
        set(XFIND_PATH $ENV{HOME}/src/xfind)
        message("XFIND_PATH: ${XFIND_PATH}")
endif()

include_directories(${XFIND_PATH}/cpp/cppfind/include)
include_directories(${Boost_INCLUDE_DIRS})

set(SOURCE_FILES include/common.h src/common.cpp include/config.h
        include/FileTypes.h src/FileTypes.cpp
        include/FileUtil.h src/FileUtil.cpp
        include/StringUtil.h src/StringUtil.cpp
        include/FindException.h src/FindException.cpp
        include/FindSettings.h src/FindSettings.cpp
        include/FindFile.h src/FindFile.cpp
        include/FindOption.h src/FindOption.cpp
        include/FindOptions.h src/FindOptions.cpp
        include/FindPattern.h src/FindPattern.cpp
        include/Finder.h src/Finder.cpp)

add_library(xfind ${SOURCE_FILES})

target_link_libraries(xfind ${Boost_LIBRARIES})

add_executable(cppfind cppfind.cpp)

target_link_libraries(cppfind xfind)

set(TEST_FILES tests/FileTypesTest.cpp
        tests/FileUtilTest.cpp
        tests/FinderTest.cpp
        tests/FindFileTest.cpp
        tests/FindOptionsTest.cpp
        tests/FindSettingsTest.cpp
        tests/tests-main.cpp)

add_executable(cppfind-tests ${TEST_FILES})

target_link_libraries(cppfind-tests xfind)

unset(XFIND_PATH CACHE)
