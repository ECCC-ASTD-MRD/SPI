ec_parse_manifest() # Parse MANIFEST file

#set(DOXYGEN_FOUND True)
# include(doxygen)    # Doxygen target_link_libraries

project(${NAME} VERSION ${VERSION} DESCRIPTION "${DESCRIPTION}")

message(STATUS "Generating ${NAME} library")

file(GLOB PROJECT_INCLUDE_FILES generic/*.h)
file(GLOB PROJECT_SOURCE_FILES generic/*.c)

add_library(${NAME} SHARED ${PROJECT_INCLUDE_FILES} ${PROJECT_SOURCE_FILES})

set_target_properties(${NAME} PROPERTIES
   VERSION                    ${PROJECT_VERSION}
   PUBLIC_HEADER              "${PROJECT_INCLUDE_FILES}"
   POSITION_INDEPENDENT_CODE  ON
   EC_LIB_INSTALL_PATH        TCL/lib/${NAME}${VERSION}
)

#----- Required libs
target_link_libraries(${NAME} TCL::TCL TCL::TK)
target_link_libraries(${NAME} R::R)
target_link_libraries(${NAME} TkglCanvas)
target_link_libraries(${NAME} OpenGL::GLU OpenGL::GL)

#----- RPATH handling
ec_target_rpath_from_libs(${NAME})

#----- Compilation  stuff
target_compile_definitions(${NAME} PRIVATE PACKAGE_NAME=\"${NAME}\" PACKAGE_VERSION=\"${VERSION}\")
target_include_directories(${NAME} PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/generic)

install(TARGETS ${NAME}
   LIBRARY        DESTINATION TCL/lib/${NAME}${VERSION}
   PUBLIC_HEADER  DESTINATION TCL/include
)

include(ec_build_tcl_pkgindex)
