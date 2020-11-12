# TODO We should have code to check the version

# [[DOC]] for find_package (lists variables that are set automatically by CMake)
# https://cmake.org/cmake/help/v3.0/command/find_package.html
# [[DOC]] https://cmake.org/cmake/help/v3.14/manual/cmake-developer.7.html
set(EER_VERSION ${EER_FIND_VERSION})
find_path(EER_INCLUDE_DIR
   NAMES eerUtils.h
   PATHS $ENV{EER_ROOT}/include ${EC_INCLUDE_PATH} NO_DEFAULT_PATH)
       
# [[DOC]] for find_library https://cmake.org/cmake/help/latest/command/find_library.html
if("OMPI" IN_LIST EER_FIND_COMPONENTS)
   find_library(EER_LIBRARY
      NAMES eerUtils-ompi
      PATHS $ENV{EER_ROOT}/lib ${EC_LD_LIBRARY_PATH} NO_DEFAULT_PATH)
else()
   find_library(EER_LIBRARY
      NAMES eerUtils
      PATHS $ENV{EER_ROOT}/lib ${EC_LD_LIBRARY_PATH} NO_DEFAULT_PATH)
endif()

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(EER DEFAULT_MSG EER_LIBRARY EER_INCLUDE_DIR)
mark_as_advanced(EER_INCLUDE_DIR EER_LIBRARY)

set(EER_INCLUDE_DIRS ${EER_INCLUDE_DIR})
set(EER_LIBRARIES ${EER_LIBRARY})
