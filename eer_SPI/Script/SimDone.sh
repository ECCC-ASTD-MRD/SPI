#!/bin/ksh
#=============================================================================
# Environnement Canada
# Centre Meteorologique Canadien
# Dorval, Quebec
#
# Projet     : Interface pour le modele CANERM.
# Nom        : <SimDone.sh>
# Creation   : Juin 1998 - J.P. Gauthier - CMC/CMOE
#
# Description: Effectue le changement d'etat d'une simulation dans le
#              pool d'une experience.
#
# Parametres :
#    ${1}    : Path du fichier pool de l'experience
#    ${2}    : Path de la ligne pool de la simulation
#    ${3}    : Status de la simulation (0=ok, sinon erreur)
#
# Retour     :
#    Aucun.
#
# Remarques  :
#=============================================================================

Pool=${1}
Line=${2}
Status=${3}

line=`cat $Line`

echo $line
#----- Get pool parts
start=`echo ${line} | cut -d: -f1`
end=`echo ${line} | cut -d: -f3-`
state=`echo ${line} | cut -d: -f2`

#----- Set Simulation state
token=`echo ${state} | cut -d= -f1`
if [[ $Status -eq 0 ]]; then
   state="${token}=1"
else
   state="${token}=-1"
fi

#----- Replace pool info.
cp ${Pool} ${Pool}.exec
grep -v "${start}:.*:${end}" ${Pool}.exec > ${Pool}
echo "${start}:${state}:${end}" >> ${Pool}





