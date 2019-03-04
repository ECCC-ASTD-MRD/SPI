#!/bin/bash

CI_PROJECT_DIR=$1
CI_BUILD_REF=$2

#----- Got to project 
cd ${CI_PROJECT_DIR}

#----- Initialize environment
. SPI/VERSION

export SPI_LIB=${SSM_DEV}/workspace/libSPI_${SPI_VERSION}${SSM_COMP}_${ORDENV_PLAT}
export SPI_PATH=${CI_PROJECT_DIR}/SPI
export CI_SPI_IN=${CI_DATA}/SPI/in
export CI_SPI_OUT=${CI_DATA}/SPI/out/${CI_BUILD_REF}

#----- Launch tests
mkdir -p $CI_SPI_OUT
cd SPI/share/examples/script
./TCL_TestAll.tcl ${CI_BUILD_REF} > ${CI_PROJECT_DIR}/CI.log
echo "Status: $?" >> ${CI_PROJECT_DIR}/CI.log

