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

message(STATUS "Generating libTclSystem librairie")

file(GLOB PROJECT_INCLUDE_FILES generic/*.h)
file(GLOB PROJECT_SOURCE_FILES generic/*.c)

add_library(TclSystem SHARED ${PROJECT_INCLUDE_FILES} ${PROJECT_SOURCE_FILES})

find_package(TCL REQUIRED)
add_compile_definitions(HAVE_TCL)
include_directories(${TCL_INCLUDE_PATH})

find_package(EER ${EER_REQ_VERSION} REQUIRED)
if (EER_FOUND) 
   add_compile_definitions(HAVE_EER)
   include_directories(${EER_INCLUDE_DIR})
endif()

if(${CMAKE_SYSTEM_PROCESSOR} STREQUAL "x86_64")
   add_compile_options(-fPIC -DSTDC_HEADERS)
endif()

add_compile_definitions(_${CMAKE_SYSTEM_NAME}_ TCL_THREADS _GNU_SOURCE)
add_compile_definitions(PACKAGE_NAME=\"${NAME}\" PACKAGE_VERSION=\"${VERSION}\")

set_target_properties(TclSystem PROPERTIES VERSION ${PROJECT_VERSION})
set_target_properties(TclSystem PROPERTIES PUBLIC_HEADER "${PROJECT_INCLUDE_FILES}")

add_dependencies(TclSystem timestamp)

target_link_libraries(TclSystem ${EER_LIBRARIES})
target_link_libraries(TclSystem ${TCL_LIBRARIES})

install(TARGETS TclSystem
   LIBRARY DESTINATION TCL/lib/${NAME}${VERSION}
   PUBLIC_HEADER DESTINATION TCL/include)

include(ec_build_tcl_pkgindex)