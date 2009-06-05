#!/bin/sh
#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadien
# Dorval, Quebec
#
# Projet     : Modele Trajectoire.
# Nom        : <TrajectLaunch.sh>
# Creation   : Fevrier 1998 - S. Trudel - CMC/CMOE
#
# But        : Permet de lancer le modele de trajectoire.
#
# Parametres :
#   ${1}     : repertoire pour les bin.
#   ${2}     : repertoire pour les scripts.
#   ${2}     : repertoire temporaire.
#
# Retour        :
#   exit_modele : flag pour la sortie du modele.
#
# Remarques  :
#   - IMPORTANT: modifier la sortie du modele pour une sortie propre.
#
# Modifications :
#
#   Nom         : -
#   Date        : -
#   Description : -
#
#===============================================================================

#----- Recuperer les arguments obligatoires.

Bin="${1}"
DirScr="${2}"
DirTmp="${3}"

#----- Definir les limites

ulimit -s 500000
ulimit -m unlimited
ulimit -d unlimited

#----- Se positionne dans le bon repertoire.

cd ${DirTmp}

#----- execute le modele.

${Bin} -i entre -fich10 `cat data_std_sim` -o traject.points

#----- Verifier l'execution du modele.

if test ! $?
then
   exit_modele=0
else
   exit_modele=1
fi

echo ${exit_modele} > ${DirTmp}/traject.exit

#----- Determiner la date d'anal-prog

${DirScr}/TrajectSplit.tcl ${DirTmp} traject.points

echo "PWJOB Traitement termine"

exit 0
