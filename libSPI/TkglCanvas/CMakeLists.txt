cmake_minimum_required(VERSION 3.16)

#----- Append EC specific module path
list(APPEND CMAKE_MODULE_PATH $ENV{EC_CMAKE_MODULE_PATH})

include(ec_utils)
ec_parse_manifest() # Parse MANIFEST file
ec_build_info()     # Generate build include file

#set(DOXYGEN_FOUND True)
# include(doxygen)    # Doxygen target

project(${NAME} VERSION ${VERSION} DESCRIPTION "${DESCRIPTION}")

set(CMAKE_INSTALL_PREFIX "" CACHE PATH "..." FORCE)
set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

message(STATUS "Build architecture ${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR}")

include(compiler_rules)

message(STATUS "Generating libTkglCanvas librairie")

file(GLOB PROJECT_INCLUDE_FILES generic/*.h)
file(GLOB PROJECT_SOURCE_FILES generic/*.c)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/generic)

add_library(${NAME} SHARED ${PROJECT_INCLUDE_FILES} ${PROJECT_SOURCE_FILES})

find_package(TCL ${TCL_REQ_VERSION} REQUIRED)
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

set_target_properties(${NAME} PROPERTIES VERSION ${PROJECT_VERSION})
set_target_properties(${NAME} PROPERTIES PUBLIC_HEADER "${PROJECT_INCLUDE_FILES}")

add_dependencies(${NAME} build_info)

target_link_libraries(${NAME} ${TCL_LIBRARIES} ${TK_LIBRARIES} ${OPENGL_LIBRARIES})

install(TARGETS ${NAME}
   LIBRARY DESTINATION TCL/lib/${NAME}${VERSION}
   PUBLIC_HEADER DESTINATION TCL/include)

include(ec_build_tcl_pkgindex)