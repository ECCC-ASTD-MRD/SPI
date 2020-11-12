cmake_minimum_required(VERSION 3.16)

#----- Append EC specific module path
list(APPEND CMAKE_MODULE_PATH $ENV{EC_CMAKE_MODULE_PATH})

include(ec_utils)
ec_parse_manifest() # Parse MANIFEST file
ec_build_info()     # Generate build include file

#set(DOXYGEN_FOUND True)
# include(doxygen)    # Doxygen targetinclude(ec_utils)

project(${NAME} VERSION ${VERSION} DESCRIPTION "${DESCRIPTION}")

set(CMAKE_INSTALL_PREFIX "" CACHE PATH "..." FORCE)
set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

message(STATUS "Build architecture ${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR}")

include(compiler_rules)

message(STATUS "Generating libTclGeoEER librairie")

file(GLOB PROJECT_INCLUDE_FILES generic/*.h)
file(GLOB PROJECT_SOURCE_FILES generic/*.c)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/generic)

find_package(BISON REQUIRED)
find_package(FLEX REQUIRED)
BISON_TARGET(Parser generic/Data_Calc.y ${CMAKE_CURRENT_BINARY_DIR}/Data_Parser.c COMPILE_FLAGS "-p vexpr_")
FLEX_TARGET(Lexer generic/Data_Calc.l ${CMAKE_CURRENT_BINARY_DIR}/Data_Lexer.c COMPILE_FLAGS "-P vexpr_")
ADD_FLEX_BISON_DEPENDENCY(Lexer Parser)

add_library(${NAME} SHARED ${PROJECT_INCLUDE_FILES} ${PROJECT_SOURCE_FILES} ${BISON_Parser_OUTPUTS} ${FLEX_Lexer_OUTPUTS})

find_package(TCL REQUIRED)
add_compile_definitions(HAVE_TCL)
include_directories(${TCL_INCLUDE_PATH} ${TK_INCLUDE_PATH})
include_directories($ENV{TCL_SRC_DIR}/unix $ENV{TCL_SRC_DIR}/generic)
include_directories($ENV{TK_SRC_DIR}/unix $ENV{TK_SRC_DIR}/generic)
target_link_libraries(${NAME} ${TCL_LIBRARIES})

find_package(RMN ${RMN_REQ_VERSION} COMPONENTS beta shared OPTIONAL)
if (RMN_FOUND) 
   add_compile_definitions(HAVE_RMN)
   include_directories(${RMN_INCLUDE_DIR})
   target_link_libraries(${NAME} ${RMN_LIBRARIES})
endif()

find_package(VGRID ${VGRID_REQ_VERSION} COMPONENTS shared OPTIONAL)
if(VGRID_FOUND)
   add_compile_definitions(HAVE_VGRID)
   include_directories(${VGRID_INCLUDE_DIR})
   target_link_libraries(${NAME} ${VGRID_LIBRARIES})
endif()

find_package(EER ${EER_REQ_VERSION} REQUIRED)
if (EER_FOUND) 
   add_compile_definitions(HAVE_EER)
   include_directories(${EER_INCLUDE_DIR})
  target_link_libraries(${NAME} ${EER_LIBRARIES})
endif()

find_package(GDAL)
if(GDAL_FOUND)
   add_compile_definitions(HAVE_GDAL)
   include_directories(${GDAL_INCLUDE_DIR})
   target_link_libraries(${NAME} ${GDAL_LIBRARIES})
endif()

find_package(SQLite3)
if (SQLite3_FOUND) 
   add_compile_definitions(HAVE_SQLITE3)
   include_directories(${SQLite3_INCLUDE_DIRS})
   target_link_libraries(${NAME} ${SQLite3_LIBRARIES})
endif()

find_package(GDB)
if (GDB_FOUND)
   add_compile_definitions(HAVE_GDB)
   include_directories(${GDB_INCLUDE_DIR})
   target_link_libraries(${NAME} ${GDB_LIBRARIES})
endif()

find_package(ECCODES REQUIRED)
if (ECCODES_FOUND)
   add_compile_definitions(HAVE_ECCODES)
   include_directories(${ECCODES_INCLUDE_DIR})
   target_link_libraries(${NAME} ${ECCODES_LIBRARIES})
endif()

find_package(ECBUFR 0.8.7 REQUIRED)
if (ECBUFR_FOUND)
   add_compile_definitions(HAVE_ECBUFR)
   include_directories(${ECBUFR_INCLUDE_DIR})
   target_link_libraries(${NAME} ${ECBUFR_LIBRARIES})
endif()

find_package(FLT REQUIRED)
if (FLT_FOUND)
   add_compile_definitions(HAVE_FLT)
   include_directories(${FLT_INCLUDE_DIR})
   target_link_libraries(${NAME} ${FLT_LIBRARIES})
endif()

find_package(URP REQUIRED)
if (URP_FOUND)
   add_compile_definitions(HAVE_URP)
   include_directories(${URP_INCLUDE_DIR})
   target_link_libraries(${NAME} ${URP_LIBRARIES})
endif()

find_package(OpenGL REQUIRED)
if (OpenGL_FOUND)
   include_directories(${OpenGL_INCLUDE_DIR})
   target_link_libraries(${NAME} ${OpenGL_LIBRARIES})
endif()

find_package(OpenMP) 
if (OpenMP_FOUND)
   add_compile_options(${OpenMP_C_FLAGS})
endif()

if(${CMAKE_SYSTEM_PROCESSOR} STREQUAL "x86_64")
   add_compile_options(-fPIC -DSTDC_HEADERS)
endif()

add_compile_definitions(_${CMAKE_SYSTEM_NAME}_ TCL_THREADS _GNU_SOURCE)
add_compile_definitions(PACKAGE_NAME=\"${NAME}\" PACKAGE_VERSION=\"${VERSION}\")
add_compile_definitions(HAVE_GPC _TK_SOURCE)

set_target_properties(${NAME} PROPERTIES VERSION ${PROJECT_VERSION})
set_target_properties(${NAME} PROPERTIES PUBLIC_HEADER "${PROJECT_INCLUDE_FILES}")

add_dependencies(${NAME} build_info)

install(TARGETS ${NAME}
   LIBRARY DESTINATION TCL/lib/${NAME}${VERSION}
   PUBLIC_HEADER DESTINATION TCL/include)

include(ec_build_tcl_pkgindex)