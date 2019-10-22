#!/bin/bash

CI_PROJECT_DIR=$1
CI_BUILD_REF=$2
#CI_DATA=/home/nil000/links/eccc-ppp4/storage/CI/SPI

#----- Got to project 
cd ${CI_PROJECT_DIR}

#----- Initialize environment
. SPI/VERSION
. ssmuse-sh -x eccc/mrd/rpn/libs/${RMN_VERSION}
. ssmuse-sh -x eccc/mrd/rpn/utils/${RMN_VERSION}

export LD_LIBRARY_PATH=${SSM_DEV}/workspace/eerUtils_${EER_VERSION}${SSM_COMP}_${ORDENV_PLAT}/lib:${LD_LIBRARY_PATH}
export SPI_LIB=${SSM_DEV}/workspace/libSPI_${SPI_VERSION}${SSM_COMP}_${ORDENV_PLAT}
export SPI_PATH=${CI_PROJECT_DIR}/SPI
export CI_DATA_IN=${CI_DATA}/SPI/in
export CI_DATA_OUT=${CI_DATA}/SPI/out/${CI_BUILD_REF}


#----- Launch tests
mkdir -p $CI_DATA_OUT
cd SPI/share/examples/script
./TCL_TestAll.tcl ${CI_BUILD_REF} > ${CI_PROJECT_DIR}/CI.log
echo "Status: $?" >> ${CI_PROJECT_DIR}/CI.log

