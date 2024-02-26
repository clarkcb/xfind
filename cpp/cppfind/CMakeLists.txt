cmake_minimum_required(VERSION 3.27.8)
project(cppfind VERSION 0.1.0 LANGUAGES CXX)

#message("PROJECT_NAME: ${PROJECT_NAME}")
#message("PROJECT_SOURCE_DIR: ${PROJECT_SOURCE_DIR}")

set(CMAKE_CXX_STANDARD 20)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++20")

if (DEFINED ENV{XFIND_PATH})
        set(XFIND_PATH $ENV{XFIND_PATH})
        message("XFIND_PATH (from env): ${XFIND_PATH}")
else()
        set(XFIND_PATH $ENV{HOME}/src/xfind)
        message("XFIND_PATH (from home): ${XFIND_PATH}")
endif()

include_directories(${XFIND_PATH}/cpp/cppfind/include)
include_directories(/usr/local/include)


################################################################################
# Library section
################################################################################

add_library(${PROJECT_NAME} STATIC)

# https://crascit.com/2016/01/31/enhanced-source-file-handling-with-target_sources/
target_sources(${PROJECT_NAME}
        PUBLIC
        # cppfind.h
        # https://stackoverflow.com/a/62465051/1002072
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include/cppfind.h>
        $<INSTALL_INTERFACE:include/cppfind.h>
        PRIVATE
        include/common.h src/common.cpp
        include/FindConfig.h src/FindConfig.cpp
        include/FileTypes.h src/FileTypes.cpp
        include/FileUtil.h src/FileUtil.cpp
        include/StringUtil.h src/StringUtil.cpp
        include/FindException.h src/FindException.cpp
        include/FindSettings.h src/FindSettings.cpp
        include/FileResult.h src/FileResult.cpp
        include/FindOption.h src/FindOption.cpp
        include/FindOptions.h src/FindOptions.cpp
        include/RegexPattern.h src/RegexPattern.cpp
        include/Finder.h src/Finder.cpp)

#target_link_libraries(${PROJECT_NAME} ${Boost_LIBRARIES})

install(TARGETS ${PROJECT_NAME}
        EXPORT ${PROJECT_NAME}Targets
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib
        RUNTIME DESTINATION bin
        INCLUDES DESTINATION include)

install(EXPORT ${PROJECT_NAME}Targets
        FILE ${PROJECT_NAME}Targets.cmake
        # NAMESPACE ${PROJECT_NAME}::
        DESTINATION lib/cmake/${PROJECT_NAME})

include(CMakePackageConfigHelpers)
write_basic_package_version_file(
        "${PROJECT_NAME}ConfigVersion.cmake"
        VERSION ${${PROJECT_NAME}_VERSION}
        COMPATIBILITY AnyNewerVersion)

install(FILES "${PROJECT_NAME}Config.cmake" "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}ConfigVersion.cmake"
        DESTINATION lib/cmake/${PROJECT_NAME})


################################################################################
# Executable section
################################################################################

add_executable(${PROJECT_NAME}app cppfind.cpp)

target_link_libraries(${PROJECT_NAME}app ${PROJECT_NAME})


################################################################################
# Testing section
################################################################################

Include(FetchContent)

FetchContent_Declare(
        Catch2
        GIT_REPOSITORY https://github.com/catchorg/Catch2.git
        #GIT_TAG        v2.13.9
        GIT_TAG        v3.5.3
)

FetchContent_MakeAvailable(Catch2)

set(TEST_FILES tests/FileResultTest.cpp
        tests/FileTypesTest.cpp
        tests/FileUtilTest.cpp
        tests/FinderTest.cpp
        tests/FindOptionsTest.cpp
        tests/FindSettingsTest.cpp)

add_executable(cppfind-tests ${TEST_FILES})

#target_link_libraries(cppfind-tests ${PROJECT_NAME} Catch2::Catch2)
target_link_libraries(cppfind-tests ${PROJECT_NAME} Catch2::Catch2WithMain)

unset(XFIND_PATH CACHE)
