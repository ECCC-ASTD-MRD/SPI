#!/bin/ksh

. ~/.profile 2>/dev/null

#----- 

filein=$1
token=$2
hostpds=$3
user=$4
host=$5

#---- 

set -x 
#ftppds_eer -c ${client_name} -i ${SNDFILE} -t P -r G1 -h ${OCM_RUNHOUR} -p ${heure} -u ${client_name}  -H pds-op.cmc.ec.gc.ca

echo "/ignore/path/ftppds_eer -c ${token} -i ${filein} -t P -r G1 -h 00 -p 000 -u ${token} -H scmc_grib@${hostpds}.cmc.ec.gc.ca" | ssh -T -X -i $HOME/.ssh/id_dsa_ftppds ${user}@${host}
