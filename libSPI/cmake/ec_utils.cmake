 #----- Compiler selection
if(NOT DEFINED EC_COMPILER_SUITE)
   set(EC_COMPILER_SUITE gnu)
   if(DEFINED ENV{INTEL_LICENSE_FILE})
      set(EC_COMPILER_SUITE intel)
   endif()
#   if(DEFINED ENV{CRAYPE_VERSION})
#      set(CMAKE_SYSTEM_NAME CrayLinuxEnvironment)
#   endif()
endif()

if(EC_COMPILER_SUITE MATCHES gnu)
   set(CMAKE_C_COMPILER gcc)
   set(CMAKE_CXX_COMPILER c++)
   set(CMAKE_Fortran_COMPILER gfortran)
elseif(EC_COMPILER_SUITE MATCHES intel)
   set(CMAKE_C_COMPILER icc)
   set(CMAKE_CXX_COMPILER icpc)
   set(CMAKE_Fortran_COMPILER ifort)
elseif(EC_COMPILER_SUITE MATCHES pgi)
   set(CMAKE_C_COMPILER pgcc)
   set(CMAKE_CXX_COMPILER pgc)
   set(CMAKE_Fortran_COMPILER pgfortran)
endif()

#----- Prepare some variables
if(DEFINED ENV{EC_INCLUDE_PATH})
   string(REPLACE " " ";" EC_INCLUDE_PATH    $ENV{EC_INCLUDE_PATH})
endif()
if(DEFINED ENV{EC_LD_LIBRARY_PATH})
   string(REPLACE " " ";" EC_LD_LIBRARY_PATH $ENV{EC_LD_LIBRARY_PATH})
endif()

if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set(CMAKE_INSTALL_PREFIX "" CACHE PATH "..." FORCE)
endif()

include(ec_build_info)
include(ec_parse_manifest)
include(dump_cmake_variables)

macro(ec_getvar)
   #----- Get name and version of operating system
   execute_process(COMMAND sh "-c" "${CMAKE_CURRENT_SOURCE_DIR}/os.sh" OUTPUT_VARIABLE EC_OS)
   message(STATUS "Operating system is: ${EC_OS}")

   #----- Get name and version of compiler
   execute_process(COMMAND sh "-c" "${CMAKE_CURRENT_SOURCE_DIR}/compiler.sh ${EC_COMPILER_SUITE}" OUTPUT_VARIABLE EC_COMPILER_SUITE_VERSION)
   message(STATUS "Compiler version is: ${EC_COMPILER_SUITE} ${EC_COMPILER_SUITE_VERSION}") 
endmacro()

