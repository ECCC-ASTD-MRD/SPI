# Build ISO 8601 build timestamp target
# How to use:
#   - add as a target to a build
#   - add_dependencies(eerUtils$ENV{OMPI} build)
#   - and include the file build.h to have the BUILD_TIMESTAMP variable #defined
include_directories(${CMAKE_CURRENT_SOURCE_DIR})

function(ec_build_info)
   if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/.git)
      execute_process(COMMAND git describe --always
         OUTPUT_VARIABLE BUILD_INFO)
      string(STRIP ${BUILD_INFO} BUILD_INFO)
   endif()

   FILE (WRITE ${CMAKE_BINARY_DIR}/build_info.cmake "string(TIMESTAMP BUILD_TIMESTAMP UTC)\n")
   FILE (APPEND ${CMAKE_BINARY_DIR}/build_info.cmake "file(WRITE build_info.h \"#ifndef _BUILD_INFO_H\\n\")\n")
   FILE (APPEND ${CMAKE_BINARY_DIR}/build_info.cmake "file(APPEND build_info.h \"#define _BUILD_INFO_H\\n\\n\")\n")
   FILE (APPEND ${CMAKE_BINARY_DIR}/build_info.cmake "file(APPEND build_info.h \"#define BUILD_TIMESTAMP \\\"\${BUILD_TIMESTAMP}\\\"\\n\")\n")
   FILE (APPEND ${CMAKE_BINARY_DIR}/build_info.cmake "file(APPEND build_info.h \"#define BUILD_INFO      \\\"${BUILD_INFO}\\\"\\n\")\n")
   FILE (APPEND ${CMAKE_BINARY_DIR}/build_info.cmake "file(APPEND build_info.h \"#define BUILD_ARCH      \\\"${EC_OS}/${EC_COMPILER}-${EC_COMPILER_VERSION}\\\"\\n\")\n")
   FILE (APPEND ${CMAKE_BINARY_DIR}/build_info.cmake "file(APPEND build_info.h \"#define BUILD_USER      \\\"$ENV{USER}\\\"\\n\\n\")\n")
   FILE (APPEND ${CMAKE_BINARY_DIR}/build_info.cmake "file(APPEND build_info.h \"#define VERSION         \\\"${VERSION}\\\"\\n\")\n")
   FILE (APPEND ${CMAKE_BINARY_DIR}/build_info.cmake "file(APPEND build_info.h \"#define DESCRIPTION     \\\"${DESCRIPTION}\\\"\\n\\n\")\n")
   FILE (APPEND ${CMAKE_BINARY_DIR}/build_info.cmake "file(APPEND build_info.h \"#endif // _BUILD_INFO_H\\n\")\n")
   ADD_CUSTOM_TARGET (
      build_info
      COMMAND ${CMAKE_COMMAND} -P ${CMAKE_BINARY_DIR}/build_info.cmake
      ADD_DEPENDENCIES ${CMAKE_BINARY_DIR}/build_info.cmake)
   include_directories(${CMAKE_BINARY_DIR})
endfunction()