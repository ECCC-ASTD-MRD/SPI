cmake_minimum_required(VERSION 3.12)

#----- Append EC specific module path
list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/../cmake_rpn)

include(ec_init)      # Initialize compilers and ec specific functions
include(ec_build_tcl_pkgindex)
ec_git_version()      # Get version from git 
ec_parse_manifest()   # Parse MANIFEST file

project(${NAME} DESCRIPTION "${DESCRIPTION}")
set(PROJECT_VERSION ${VERSION})

message(STATUS "Build architecture ${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR}")

enable_language(C)
include(ec_compiler_presets)
include(ec_shared_lib)
include(ec_doxygen)           # Doxygen target

#----- Look for libeerUtils
ec_install_prefix(eerUtils ${EER_REQ_VERSION} eer_prefix)
set(eerUtils_ROOT ${CMAKE_INSTALL_PREFIX}/${eer_prefix})

ec_install_prefix()  # Define install prefix  
set(TCL_ROOT ${CMAKE_INSTALL_PREFIX}/TCL) 
set(GDB_ROOT ${CMAKE_INSTALL_PREFIX}) 

set(CMAKE_C_STANDARD 99)
set(CMAKE_C_EXTENSIONS OFF)

#----- Parse a shell configuration file
macro(ec_parse_config config)

   file(STRINGS ${config} definitions)
   foreach(line ${definitions})
      string(REGEX MATCH "[#]|([A-Z,a-z,_]+)[ ]*(=)[ ]*(.*)" res ${line})
      set(LBL1 ${CMAKE_MATCH_1})
      set(LBL2 ${CMAKE_MATCH_2})
      set(LBL3 ${CMAKE_MATCH_3})
#      message("..${CMAKE_MATCH_1}..${CMAKE_MATCH_2}..${CMAKE_MATCH_3}..")
   
      #----- Skip comment lines
      if("${res}" STREQUAL "" OR "${res}" MATCHES "#" OR "${LBL3}" STREQUAL "")
         continue()
      endif()

       string(REGEX REPLACE "'" "" val ${LBL3})
       set(${LBL1} ${val}) 
   endforeach()
endmacro()

#----- Add some common compiling options
add_compile_definitions(_${CMAKE_SYSTEM_NAME}_ _GNU_SOURCE _TK_SOURCE STDC_HEADERS)
#add_compile_options("-Winline")
ec_parse_config(${CMAKE_INSTALL_PREFIX}/TCL/lib/tclConfig.sh)
ec_parse_config(${CMAKE_INSTALL_PREFIX}/TCL/lib/tkConfig.sh)

#----- Add intel library dir to RPATH if needed
if(DEFINED ENV{INTELCOMP_HOME})
   list(APPEND CMAKE_INSTALL_RPATH "$ENV{INTELCOMP_HOME}/lib/intel64_lin")
   add_link_options(-lintlc -lifcore -lifport -limf)
endif()

#----- Required packages
find_package(TCL ${TCL_REQ_VERSION} REQUIRED)
if(TCL_FOUND)
   add_library(TCL::TCL SHARED IMPORTED)
   set_target_properties(TCL::TCL PROPERTIES IMPORTED_LOCATION ${TCL_LIBRARY} IMPORTED_NO_SONAME TRUE)
   target_include_directories(TCL::TCL INTERFACE ${TCL_INCLUDE_PATH})
   target_include_directories(TCL::TCL INTERFACE ${TCL_SRC_DIR}/unix ${TCL_SRC_DIR}/generic)
   target_compile_definitions(TCL::TCL INTERFACE HAVE_TCL TCL_THREADS)
endif()
if(TK_FOUND)
   add_library(TCL::TK SHARED IMPORTED)
   set_target_properties(TCL::TK PROPERTIES IMPORTED_LOCATION ${TK_LIBRARY} IMPORTED_NO_SONAME TRUE)
   target_include_directories(TCL::TK INTERFACE ${TK_INCLUDE_PATH})
   target_include_directories(TCL::TK INTERFACE ${TK_SRC_DIR}/unix ${TK_SRC_DIR}/generic)
   target_compile_definitions(TCL::TK INTERFACE HAVE_TK _TK_SOURCE)
endif()

#----- Use LEGACY OpenGL, not GVNC (causes segfault in batch mode)
cmake_policy(SET CMP0072 OLD)
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

#----- Copy local share definitions (CMC constant files and GRIB adjustment for ARGOS GRIB1)
install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/share/grib_api ${CMAKE_CURRENT_SOURCE_DIR}/share/rmn DESTINATION share USE_SOURCE_PERMISSIONS)
install(DIRECTORY ${CMAKE_INSTALL_PREFIX}/TCL     DESTINATION . USE_SOURCE_PERMISSIONS) 
install(DIRECTORY ${CMAKE_INSTALL_PREFIX}/GL      DESTINATION . USE_SOURCE_PERMISSIONS) 
install(DIRECTORY ${CMAKE_INSTALL_PREFIX}/include DESTINATION . USE_SOURCE_PERMISSIONS) 
install(DIRECTORY ${CMAKE_INSTALL_PREFIX}/lib     DESTINATION . USE_SOURCE_PERMISSIONS) 

#ec_build_config()          # Create build configuration script
ec_prepare_ssm()           # Prepare ssm packaging files

set(CPACK_GENERATOR "TGZ")
set(CPACK_PACKAGE_VENDOR "ECCC")
set(CPACK_PACKAGE_CONTACT "${MAINTAINER}")
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/../LICENSE")
set(CPACK_RESOURCE_FILE_README "${CMAKE_CURRENT_SOURCE_DIR}/../README.md")
set(CPACK_OUTPUT_FILE_PREFIX "${CMAKE_CURRENT_BINARY_DIR}/package")
set(CPACK_PACKAGE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
set(CPACK_PACKAGE_FILE_NAME "${CMAKE_INSTALL_PREFIX}")
set(CPACK_SOURCE_PACKAGE_FILE_NAME "${PROJECT_NAME}-${PROJECT_VERSION}")
include(CPack)
