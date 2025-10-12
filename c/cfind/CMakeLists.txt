cmake_minimum_required(VERSION 4.00)
project(cfind VERSION 0.1.0 LANGUAGES C)

if (NOT DEFINED CMAKE_C_STANDARD)
        set(CMAKE_C_STANDARD 11)
endif()


if (DEFINED ENV{XFIND_PATH})
        set(XFIND_PATH $ENV{XFIND_PATH})
        message("XFIND_PATH (from env): ${XFIND_PATH}")
else()
        set(XFIND_PATH $ENV{HOME}/src/xfind)
        message("XFIND_PATH (from home): ${XFIND_PATH}")
endif()

include_directories(${XFIND_PATH}/c/cfind/include)
include_directories(/usr/local/include)


################################################################################
# Dependencies section
################################################################################

find_package(cjson REQUIRED)


################################################################################
# Library section
################################################################################

add_library(${PROJECT_NAME} STATIC)

# https://crascit.com/2016/01/31/enhanced-source-file-handling-with-target_sources/
target_sources(${PROJECT_NAME}
        PUBLIC
        # cfind.h
        # https://stackoverflow.com/a/62465051/1002072
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include/cfind.h>
        $<INSTALL_INTERFACE:include/cfind.h>
        PRIVATE
        include/color.h
        include/common.h src/common.c
        include/config.h src/config.c
        include/argtokennode.h src/argtokennode.c
        include/pathnode.h src/pathnode.c
        include/fileresults.h src/fileresults.c
        include/filetypes.h src/filetypes.c
        include/fileutil.h src/fileutil.c
        include/finderr.h src/finderr.c
        include/findoptions.h src/findoptions.c
        include/findsettings.h src/findsettings.c
        include/intnode.h src/intnode.c
        include/regexnode.h src/regexnode.c
        include/stringarray.h src/stringarray.c
        include/stringnode.h src/stringnode.c
        include/finder.h src/finder.c)

target_link_libraries(${PROJECT_NAME} PUBLIC cjson)


################################################################################
# Executable section
################################################################################

add_executable(${PROJECT_NAME}app cfind.c)

target_link_libraries(${PROJECT_NAME}app ${PROJECT_NAME})


################################################################################
# Tests section
################################################################################

set(TEST_FILES
        test/test_common.h test/test_common.c
        test/test_filetypes.h test/test_filetypes.c
        test/test_fileutil.h test/test_fileutil.c
        test/test_finder.h test/test_finder.c
        test/test_findoptions.h test/test_findoptions.c
        test/test_findsettings.h test/test_findsettings.c
        test/test_cfind.c)

add_executable(${PROJECT_NAME}-tests ${TEST_FILES})

target_link_libraries(${PROJECT_NAME}-tests ${PROJECT_NAME})

unset(XFIND_PATH CACHE)
