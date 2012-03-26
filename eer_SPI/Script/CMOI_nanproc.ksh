#!/bin/ksh

. ~/.profile 2>/dev/null

#----- 

file=$1
user=$2
host=$3

#---- 

set -x 
#nanproc -bs -p b -f $file
echo "/ignore/path/nanproc -bs -p b -f ${file}" | ssh -T -X -i $HOME/.ssh/id_dsa_nanproc ${user}@${host}
