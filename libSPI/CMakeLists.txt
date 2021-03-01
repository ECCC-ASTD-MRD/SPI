cmake_minimum_required(VERSION 3.16)

#----- Append EC specific module path
list(APPEND CMAKE_MODULE_PATH $ENV{EC_CMAKE_MODULE_PATH})

include(ec_init)
include(ec_parse_manifest) # Parse MANIFEST file

project(${NAME} VERSION ${VERSION} DESCRIPTION "${DESCRIPTION}")

message(STATUS "Build architecture ${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR}")

enable_language(C)
include(compiler_presets)
#include(compiler_rules)

#----- Add some common compiling options
add_compile_definitions(_${CMAKE_SYSTEM_NAME}_ _GNU_SOURCE STDC_HEADERS)

#----- Add intel library dir to RPATH if needed
if(DEFINED ENV{INTELCOMP_HOME})
   list(APPEND CMAKE_INSTALL_RPATH "$ENV{INTELCOMP_HOME}/lib/intel64_lin")
endif()

#----- Required packages
find_package(TCL ${TCL_REQ_VERSION} REQUIRED)
if(TCL_FOUND)
   add_library(TCL::TCL SHARED IMPORTED)
   set_target_properties(TCL::TCL PROPERTIES IMPORTED_LOCATION ${TCL_LIBRARY} IMPORTED_NO_SONAME TRUE)
   target_include_directories(TCL::TCL INTERFACE ${TCL_INCLUDE_PATH})
   target_include_directories(TCL::TCL INTERFACE $ENV{TCL_SRC_DIR}/unix $ENV{TCL_SRC_DIR}/generic)
   target_compile_definitions(TCL::TCL INTERFACE HAVE_TCL TCL_THREADS)
endif()
if(TK_FOUND)
   add_library(TCL::TK SHARED IMPORTED)
   set_target_properties(TCL::TK PROPERTIES IMPORTED_LOCATION ${TK_LIBRARY} IMPORTED_NO_SONAME TRUE)
   target_include_directories(TCL::TK INTERFACE ${TK_INCLUDE_PATH})
   target_include_directories(TCL::TK INTERFACE $ENV{TK_SRC_DIR}/unix $ENV{TK_SRC_DIR}/generic)
   target_compile_definitions(TCL::TK INTERFACE HAVE_TK _TK_SOURCE)
endif()

find_package(OpenGL REQUIRED)
message(STATUS "OPENGL_LIBRARIES: ${OPENGL_LIBRARIES}\n-- OPENGL_glu_LIBRARY: ${OPENGL_glu_LIBRARY}")

find_package(eerUtils ${EER_REQ_VERSION} CONFIG REQUIRED)

#----- Optional packages
find_package(OpenMP COMPONENTS C)
find_package(RMN ${RMN_REQ_VERSION} COMPONENTS SHARED OPTIONAL)
find_package(VGRID ${VGRID_REQ_VERSION} COMPONENTS SHARED OPTIONAL)
find_package(GDB)
find_package(ECCODES ${ECCODES_REQ_VERSION})
find_package(ECBUFR ${ECBUFR_REQ_VERSION})
find_package(FLT ${FLT_REQ_VERSION})
find_package(URP ${URP_REQ_VERSION})
find_package(DistanceMetrics ${DISTANCEMETRICS_REQ_VERSION} CONFIG COMPONENTS static)

find_package(GDAL ${GDAL_REQ_VERSION})
if(GDAL_FOUND)
   target_compile_definitions(GDAL::GDAL INTERFACE HAVE_GDAL)
endif()

find_package(SQLite3)
if (SQLite3_FOUND)
   target_compile_definitions(SQLite::SQLite3 INTERFACE HAVE_SQLITE3)
endif()

find_package(R)
if(R_FOUND)
   target_compile_definitions(R::R INTERFACE R_DEFAULT_RHOME=\"${R_HOME}\")
endif()

#----- Generate the libraries
add_subdirectory(TclSystem)
add_subdirectory(TkglCanvas)
add_subdirectory(TclGeoEER)
if(R_FOUND)
   add_subdirectory(TclR)
   add_subdirectory(TclRDevice)
endif()
