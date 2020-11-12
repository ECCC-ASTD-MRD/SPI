# TODO We should have code to check the version

# [[DOC]] for find_package (lists variables that are set automatically by CMake)
# https://cmake.org/cmake/help/v3.0/command/find_package.html
# [[DOC]] https://cmake.org/cmake/help/v3.14/manual/cmake-developer.7.html
set(URP_VERSION ${URP_FIND_VERSION})

find_path(URP_INCLUDE_DIR
   NAMES URP.h
   HINTS $ENV{URP_ROOT}/include ENV C_INCLUDE_PATH)

# [[DOC]] for find_library https://cmake.org/cmake/help/latest/command/find_library.html
find_library(URP_LIBRARY
   NAMES urp
   PATHS $ENV{URP_ROOT}/lib ENV LIBRARY_PATH)
find_library(MUT_LIBRARY
   NAMES mut
   PATHS $ENV{URP_ROOT}/lib ENV LIBRARY_PATH)
find_library(DRP_LIBRARY
   NAMES drp
   PATHS $ENV{URP_ROOT}/lib ENV LIBRARY_PATH)
find_library(DSP_LIBRARY
   NAMES dsp
   PATHS $ENV{URP_ROOT}/lib ENV LIBRARY_PATH)
find_library(BZ2_LIBRARY
   NAMES bz2
   PATHS $ENV{URP_ROOT}/lib ENV LIBRARY_PATH)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(URP DEFAULT_MSG URP_LIBRARY URP_INCLUDE_DIR)
mark_as_advanced(URP_INCLUDE_DIR URP_LIBRARY MUT_LIBRARY DRP_LIBRARY DSP_LIBRARY)

set(URP_INCLUDE_DIRS ${URP_INCLUDE_DIR})
set(URP_LIBRARIES ${MUT_LIBRARY} ${DRP_LIBRARY} ${URP_LIBRARY} ${DSP_LIBRARY} ${BZ2_LIBRARY})

# urp_lib_flags="-lmut -ldrp -lurp -ldsp -lm -lxml2 -lz -lbz2"
