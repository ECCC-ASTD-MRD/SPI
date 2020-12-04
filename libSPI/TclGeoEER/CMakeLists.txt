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

find_package(TCL ${TCL_REQ_VERSION} REQUIRED)
add_compile_definitions(HAVE_TCL)
include_directories(${TCL_INCLUDE_PATH} ${TK_INCLUDE_PATH})
include_directories($ENV{TCL_SRC_DIR}/unix $ENV{TCL_SRC_DIR}/generic)
include_directories($ENV{TK_SRC_DIR}/unix $ENV{TK_SRC_DIR}/generic)
target_link_libraries(${NAME} ${TCL_LIBRARY} ${TK_LIBRARY})

#----- Need to add glCanvas librariesfor dependencies not to fail down the line
target_link_libraries(${NAME} $ENV{TCL_ROOT}/lib/TkglCanvas8.6.0/libTkglCanvas.so.8.6.0)

find_package(eerUtils ${EER_REQ_VERSION} CONFIG REQUIRED)
target_link_libraries(${NAME} eerUtils::eerUtils)

find_package(RMN ${RMN_REQ_VERSION} COMPONENTS beta shared OPTIONAL)
if(RMN_FOUND)
   target_link_libraries(${NAME} RMN::RMN)
endif()

find_package(VGRID ${VGRID_REQ_VERSION} COMPONENTS shared OPTIONAL)
if(VGRID_FOUND)
   target_link_libraries(${NAME} VGRID::VGRID)
endif()

find_package(GDAL ${GDAL_REQ_VERSION})
if(GDAL_FOUND)
   target_compile_definitions(GDAL::GDAL INTERFACE HAVE_GDAL)
   target_link_libraries(${NAME} GDAL::GDAL)
endif()

find_package(SQLite3)
if (SQLite3_FOUND)
   target_compile_definitions(SQLite::SQLite3 INTERFACE HAVE_SQLITE3)
   target_link_libraries(${NAME} SQLite::SQLite3)
endif()

find_package(GDB)
if (GDB_FOUND)
   target_link_libraries(${NAME} GDB::GDB)
endif()

find_package(ECCODES ${ECCODES_REQ_VERSION})
if (ECCODES_FOUND)
   target_link_libraries(${NAME} ECCODES::ECCODES)
endif()

find_package(ECBUFR ${ECBUFR_REQ_VERSION})
if (ECBUFR_FOUND)
   target_link_libraries(${NAME} ECBUFR::ECBUFR)
endif()

find_package(FLT ${FLT_REQ_VERSION})
if (FLT_FOUND)
   target_link_libraries(${NAME} FLT::FLT)
endif()

find_package(URP ${URP_REQ_VERSION})
if (URP_FOUND)
   target_link_libraries(${NAME} URP::URP)
endif()

find_package(DistanceMetrics ${DISTANCEMETRICS_REQ_VERSION} CONFIG COMPONENTS static)
if (DistanceMetrics_FOUND)
   target_link_libraries(${NAME} DistanceMetrics::DistanceMetrics)
endif()

find_package(OpenGL REQUIRED)
target_link_libraries(${NAME} OpenGL::GL)

find_package(OpenMP COMPONENTS C)
if (OpenMP_FOUND)
   target_link_libraries(${NAME} OpenMP::OpenMP_C)
endif()

add_compile_definitions(_${CMAKE_SYSTEM_NAME}_ TCL_THREADS _GNU_SOURCE STDC_HEADERS)
add_compile_definitions(PACKAGE_NAME=\"${NAME}\" PACKAGE_VERSION=\"${VERSION}\")
add_compile_definitions(HAVE_GPC _TK_SOURCE)

set_target_properties(${NAME} PROPERTIES
   VERSION                    ${PROJECT_VERSION}
   PUBLIC_HEADER              "${PROJECT_INCLUDE_FILES}"
   POSITION_INDEPENDENT_CODE  ON
)

add_dependencies(${NAME} build_info)

install(TARGETS ${NAME}
   LIBRARY DESTINATION TCL/lib/${NAME}${VERSION}
   PUBLIC_HEADER DESTINATION TCL/include)

include(ec_build_tcl_pkgindex)
