
include(ExternalProject)

if(CMAKE_SYSTEM_NAME STREQUAL Linux)
    set(subdir unix)
else()
    set(subdir macosx)
endif()

set(tcl_thing tcl8.5.11)
ExternalProject_Add(
    tcl8.5.11
    DOWNLOAD_COMMAND cp -R /home/phc001/workspace/maestro/src/tcl/tcl8.5.11 ${CMAKE_CURRENT_SOURCE_DIR}/${tcl_thing}
    SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/${tcl_thing}/${subdir}
    CONFIGURE_COMMAND cd ${CMAKE_CURRENT_SOURCE_DIR}/${tcl_thing}/${subdir} && ./configure
                      --enable-threads
                      --enable-shared
                      --prefix=${CMAKE_INSTALL_PREFIX}
    PREFIX ${CMAKE_CURRENT_SOURCE_DIR}/${tcl_thing}/${surbir}
    BUILD_IN_SOURCE 1
    BUILD_COMMAND make
    INSTALL_COMMAND make install
)



#   #----- Tcl
#   echo "- Building ${TMP_PATH}/tcl${TCL_VERSION}/build.log"
#   mkdir ${TMP_PATH}/tcl${TCL_VERSION}
#   cd ${TMP_PATH}/tcl${TCL_VERSION}
#   (${SSM_DEV}/src/ext/tcl${TCL_VERSION}/unix/configure --prefix=${LIB_PATH}/TCL --enable-threads  --enable-64bit
#   make install) > ${TMP_PATH}/tcl${TCL_VERSION}/build.log 2>&1 || { err=$?; printf "\n*** ERROR *** : An error occurred (code $err). Check log for details.\n"; exit $err; }
#
