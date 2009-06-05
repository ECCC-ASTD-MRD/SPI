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
#   
# Retour     :
#    Aucun.
#
# Remarques  :
#    - L'etat change de l'etat "Exec" a l'etat "Done" ou "Cont".
#
# Modifications  :
#
#    Nom         : -
#    Date        : -
#    Description : -
#
#=============================================================================

Line="${1}"
Pool="${2}"

line=`cat $Line`

#----- Creer la nouvelle ligne avec la nouvel etat.

start=`echo ${line} | cut -d: -f1`
end=`echo ${line} | cut -d: -f3-`

#----- Retirer l'ancienne ligne.

cp ${Pool} ${Pool}.exec

grep -v "${start}:.*2:${end}" ${Pool}.exec > ${Pool}
echo ${line} >> ${Pool}





