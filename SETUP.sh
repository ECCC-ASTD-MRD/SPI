export CC=icc
export CXX=icpc
export FC=ifort
. ssmuse-sh -d main/opt/cmake/cmake-3.16.4
. ssmuse-sh -x hpco/exp/intelpsxe-cluster-19.0.3.199
. r.load.dot rpn/code-tools/1.5.0
. r.load.dot rpn/utils/19.7.0
. r.load.dot rpn/libs/19.7.0
# source /home/phc001/workspace/spooki/SETUP_ubuntu-18.04-amd64-64

