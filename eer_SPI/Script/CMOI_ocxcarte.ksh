#!/bin/ksh

. ~/.profile 2>/dev/null

export OPERATIONAL=YES
export JOBNAME=r1

cd $HOME/.spi/Tmp/ 

#----- 

no=$1
file=$2
mask=$3
user=$4
host=$5

#---- 

set -x 
#ocxcarte -t -f $no -d difax -r systime -i ${file}.gif" | ssh -n -T -x -i $env(HOME)/.ssh/id_dsa_ocxcarte $GDefs(TransmitUser) $GDefs(FrontEnd)
echo "/ignore/path/ocxcarte -t -f $no -d difax -r systime -m ${mask} -i ${file}" | ssh -T -X -i $HOME/.ssh/id_dsa_ocxcarte ${user}@${host}
