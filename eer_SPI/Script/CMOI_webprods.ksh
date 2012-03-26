#!/bin/ksh

. ~/.profile 2>/dev/null

#----- 

filein=$1
fileout=$2
user=$3
host=$4

#---- 

set -x 
#webprods -f ${file}.png -s weather -D 0 -p eer/data/vaac/current/${prefix}_${name}_traj_satnet.png
echo "/ignore/path/webprods -f ${filein} -s weather -D 0 -p ${fileout}" | ssh -T -X -i $HOME/.ssh/id_dsa_webprods ${user}@${host}
