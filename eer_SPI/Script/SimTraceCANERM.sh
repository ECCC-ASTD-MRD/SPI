#!/bin/sh
#=============================================================================
# Environnement Canada
# Centre Meteorologique Canadien
# Dorval, Quebec
#
# Projet     : Interface pour le modele CANERM.
# Nom        : <SimTraceCANERM.sh>
# Creation   : Mars 2000 - J.P. Gauthier - CMC/CMOE
#   
# Description: Recupere une trace des resultats de la simulation.
#
# Parametres : 
#    ${1}    : Path de la simulation
#    ${2}    : Path des traces
#    ${3}    : Nom de la job
#   
# Retour     :
#    Aucun.
#
# Remarques  :
#
# Modifications  :
#
#    Nom         : -
#    Date        : -
#    Description : -
#
#=============================================================================

SimPath="${1}"
TrcPath="${2}"
JobFile="${3}"

#----- Creer le repertoire qui contiendra la trace.

if [ ! -d ${TrcPath} ]
then
   mkdir ${TrcPath}
fi

cd ${TrcPath}

trace="C_`echo ${SimPath} | tr "/" "\n" | tail -2 | tr "\n" "."`"

SimPath="${SimPath}/tmp"

echo "\n###----- fichier composant le tape30 (data_std_sim)\n" >> ${trace}
cat  ${SimPath}/data_std_sim.eta >> ${trace}

echo "\n###----- script de lancement (eer???)\n" >> ${trace}
cat  ${SimPath}/${JobFile} >> ${trace}

echo "\n###----- output du script lancement (eer???.out)\n" >> ${trace}
cat  ${SimPath}/${JobFile}.out >> ${trace}

echo "\n###----- erreur du script lancement (eer???.err)\n" >> ${trace}
cat  ${SimPath}/${JobFile}.err >> ${trace}

echo "\n###----- Sortie du modele (canerm.out)\n" >> ${trace}
head -3000 ${SimPath}/canerm.out >> ${trace}

#----- Faire un menage dans les traces (15 jours de trace)

find . -name "C_*" -mtime +15 -exec rm -f {} \;
