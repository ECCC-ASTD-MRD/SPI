#!/bin/ksh

. ~/.profile 2>/dev/null

#export DISPLAY=$DISPLAY
#export TERM=$TERM

#----- 

file=$1
user=$2
host=$3
token=$4

#---- 

set -x 
#/opt/mm/bin/amxmit -s $file
echo "/opt/mm/bin/amxmit -s ${token} ${file}" | ssh -T -X -i $HOME/.ssh/id_dsa_amxmit ${user}@${host}
