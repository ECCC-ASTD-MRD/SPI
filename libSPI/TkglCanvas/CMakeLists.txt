cmake_minimum_required(VERSION 3.16)
#----- Append EC specific module path
list(APPEND CMAKE_MODULE_PATH $ENV{EC_CMAKE_MODULE_PATH})
include(ec_utils)

ec_parse_version()
ec_build_include()
project(${NAME} VERSION ${VERSION} DESCRIPTION "${DESCRIPTION}")

set(CMAKE_INSTALL_PREFIX "" CACHE PATH "..." FORCE)
set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

message(STATUS "Build architecture ${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR}")

include(compiler_rules)

message(STATUS "Generating libTkglCanvas librairie")

file(GLOB PROJECT_INCLUDE_FILES generic/*.h)
file(GLOB PROJECT_SOURCE_FILES generic/*.c)

add_library(TkglCanvas SHARED ${PROJECT_INCLUDE_FILES} ${PROJECT_SOURCE_FILES})

find_package(TCL REQUIRED)
add_compile_definitions(HAVE_TCL)
include_directories(${TCL_INCLUDE_PATH} ${TK_INCLUDE_PATH})
include_directories($ENV{TCL_SRC_DIR}/unix $ENV{TCL_SRC_DIR}/generic)
include_directories($ENV{TK_SRC_DIR}/unix $ENV{TK_SRC_DIR}/generic)

find_package(OpenGL REQUIRED)

if(${CMAKE_SYSTEM_PROCESSOR} STREQUAL "x86_64")
   add_compile_options(-fPIC -DSTDC_HEADERS)
endif()

add_compile_definitions(_${CMAKE_SYSTEM_NAME}_ TCL_THREADS _GNU_SOURCE)
add_compile_definitions(PACKAGE_NAME=\"${NAME}\" PACKAGE_VERSION=\"${VERSION}\")

set_target_properties(TkglCanvas PROPERTIES VERSION ${PROJECT_VERSION})
set_target_properties(TkglCanvas PROPERTIES PUBLIC_HEADER "${PROJECT_INCLUDE_FILES}")

add_dependencies(TkglCanvas timestamp)

target_link_libraries(TkglCanvas ${TCL_LIBRARIES} ${TK_LIBRARIES} ${OPENGL_LIBRARIES})

install(TARGETS TkglCanvas
   LIBRARY DESTINATION TCL/lib/${NAME}${VERSION}
   PUBLIC_HEADER DESTINATION TCL/include)

include(ec_build_tcl_pkgindex)