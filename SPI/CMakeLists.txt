cmake_minimum_required(VERSION 3.12)

#----- Append EC specific module path
list(APPEND CMAKE_MODULE_PATH $ENV{EC_CMAKE_MODULE_PATH} ${CMAKE_SOURCE_DIR}/cmake_rpn/modules)

set(EC_NO_BUILD true)      # No build necessary
include(ec_init)           # Initialise compilers and ec specific functions
ec_git_version()           # Get version from git state
ec_parse_manifest()        # Parse MANIFEST file

project(${NAME} DESCRIPTION "${DESCRIPTION}" LANGUAGES NONE)
set(PROJECT_VERSION ${VERSION})

#ec_build_info()            # Generate build include file
string(TIMESTAMP BUILD_TIMESTAMP UTC)

# Define install prefix  
set(PACKAGE_PREFIX "${NAME}_${PROJECT_VERSION}_all")
set(CMAKE_INSTALL_PREFIX ${CMAKE_INSTALL_PREFIX}/${PACKAGE_PREFIX})

set(ENV_CMDS_EXT_VERSION $ENV{CMDS_EXT_VERSION})
configure_file(VERSION.in ${CMAKE_BINARY_DIR}/VERSION @ONLY)

install(DIRECTORY bin/ DESTINATION bin USE_SOURCE_PERMISSIONS) 
install(DIRECTORY tcl/ DESTINATION tcl USE_SOURCE_PERMISSIONS) 
install(DIRECTORY share/ DESTINATION share USE_SOURCE_PERMISSIONS) 
install(DIRECTORY data/ DESTINATION data USE_SOURCE_PERMISSIONS) 
install(FILES .profile_spi ${CMAKE_BINARY_DIR}/VERSION DESTINATION .) 
install(PROGRAMS tclsh tclshd wish DESTINATION .) 

# Packaging
set(EC_PLAT "all")
set(EC_COMP "")
ec_prepare_ssm()                                              # Prepare ssm packaging files

set(CPACK_GENERATOR "TGZ")
set(CPACK_PACKAGE_VENDOR "ECCC")
set(CPACK_PACKAGE_CONTACT "${MAINTAINER}")
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/../LICENSE")
set(CPACK_RESOURCE_FILE_README "${CMAKE_CURRENT_SOURCE_DIR}/../README.md")
set(CPACK_OUTPUT_FILE_PREFIX "${CMAKE_CURRENT_BINARY_DIR}/package")
set(CPACK_PACKAGE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
set(CPACK_PACKAGE_FILE_NAME "${PACKAGE_PREFIX}")
set(CPACK_SOURCE_PACKAGE_FILE_NAME "${PROJECT_NAME}-${PROJECT_VERSION}")
include(CPack)

