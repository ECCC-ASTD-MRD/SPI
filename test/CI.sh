#!/bin/bash

CI_PROJECT_DIR=$1
CI_BUILD_REF=$2

#----- Got to project 
cd ${CI_PROJECT_DIR}

#----- Initialize environment
. SPI/VERSION

export SPI_LIB=${SSM_DEV}/workspace/libSPI_${SPI_VERSION}${SSM_COMP}_${ORDENV_PLAT}
export SPI_PATH=${CI_PROJECT_DIR}/SPI
export CI_SPI_IN=/home/nil000/links/eccc-ppp1/storage/SPI/DataIn
export CI_SPI_OUT=/home/nil000/links/eccc-ppp1/storage/SPI/DataOut/${CI_BUILD_REF}
#export CI_SPI_OUT=`mktemp -d`

#----- Launch tests
mkdir -p $CI_SPI_OUT
cd SPI/share/examples/script
echo "Path  : ${CI_SPI_OUT}" > ${CI_PROJECT_DIR}/CI
echo "Log   : SPI-${CI_BUILD_REF}.log" >> ${CI_PROJECT_DIR}/CI
./TCL_TestAll.tcl ${CI_BUILD_REF} > SPI-${CI_BUILD_REF}.log
echo "Status: $?" >> ${CI_PROJECT_DIR}/CI

