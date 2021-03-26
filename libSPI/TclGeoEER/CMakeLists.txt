ec_parse_manifest() # Parse MANIFEST file

#set(DOXYGEN_FOUND True)
# include(doxygen)    # Doxygen targetinclude(ec_utils)

project(${NAME} VERSION ${VERSION} DESCRIPTION "${DESCRIPTION}")

message(STATUS "Generating ${NAME} library")

file(GLOB PROJECT_INCLUDE_FILES generic/*.h)
file(GLOB PROJECT_SOURCE_FILES generic/*.c)

find_package(BISON REQUIRED)
find_package(FLEX REQUIRED)
BISON_TARGET(Parser generic/Data_Calc.y ${CMAKE_CURRENT_BINARY_DIR}/Data_Parser.c COMPILE_FLAGS "-p vexpr_")
FLEX_TARGET(Lexer generic/Data_Calc.l ${CMAKE_CURRENT_BINARY_DIR}/Data_Lexer.c COMPILE_FLAGS "-P vexpr_")
ADD_FLEX_BISON_DEPENDENCY(Lexer Parser)

add_library(${NAME} SHARED ${PROJECT_INCLUDE_FILES} ${PROJECT_SOURCE_FILES} ${BISON_Parser_OUTPUTS} ${FLEX_Lexer_OUTPUTS})

set_target_properties(${NAME} PROPERTIES
   VERSION                    ${PROJECT_VERSION}
   PUBLIC_HEADER              "${PROJECT_INCLUDE_FILES}"
   POSITION_INDEPENDENT_CODE  ON
   EC_LIB_INSTALL_PATH        TCL/lib/${NAME}${VERSION}
)

#----- Required libs
target_link_libraries(${NAME} TkglCanvas)
target_link_libraries(${NAME} TCL::TCL TCL::TK)
target_link_libraries(${NAME} eerUtils::eerUtils)
target_link_libraries(${NAME} OpenGL::GLU)

#----- Optional libs
ec_target_link_library_if(${NAME} RMN_FOUND              RMN::RMN)
ec_target_link_library_if(${NAME} VGRID_FOUND            VGRID::VGRID)
ec_target_link_library_if(${NAME} GDAL_FOUND             GDAL::GDAL)
ec_target_link_library_if(${NAME} SQLite3_FOUND          SQLite::SQLite3)
ec_target_link_library_if(${NAME} GDB_FOUND              GDB::GDB)
ec_target_link_library_if(${NAME} ECCODES_FOUND          ECCODES::ECCODES)
ec_target_link_library_if(${NAME} ECBUFR_FOUND           ECBUFR::ECBUFR)
ec_target_link_library_if(${NAME} FLT_FOUND              FLT::FLT)
ec_target_link_library_if(${NAME} URP_FOUND              URP::URP)
ec_target_link_library_if(${NAME} OpenMP_FOUND           OpenMP::OpenMP_C)
ec_target_link_library_if(${NAME} DistanceMetrics_FOUND  DistanceMetrics::DistanceMetrics)

#----- RPATH handling
ec_target_rpath_from_libs(${NAME})

#----- Compilation  stuff
target_compile_definitions(${NAME} PRIVATE PACKAGE_NAME=\"${NAME}\" PACKAGE_VERSION=\"${VERSION}\")
target_include_directories(${NAME} PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/generic)

install(TARGETS ${NAME}
   LIBRARY        DESTINATION TCL/lib/${NAME}${VERSION}
   PUBLIC_HEADER  DESTINATION TCL/include
)

ec_build_tcl_pkgindex()
