#!/bin/sh
#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadien
# Dorval, Quebec
#
# Projet     : Modele Trajectoire.
# Nom        : <TrajectKill.sh>
# Creation   : Mars 2000 - J.P. Gauthier- CMC/CMOE
#
# But        : Permet de nettoyer les repertoires lors d'une annulation.
#
# Parametres :
#   ${1}     : repertoire temporaire.
#
# Retour        :
#
# Remarques  :
#===============================================================================

#----- Recuperer les arguments obligatoires.

DirTmp="${1}"

#----- Recuperer l'information sur la simulation

pool=`cat ${DirTmp}/sim.pool`

#----- Supprimer le repertoire.

cd ${DirTmp}/../
rm -f -r ${DirTmp}

#----- Supprimer l'entree dans la liste de simulation.

cp TRAJECT.pool TRAJECT.pool.old
grep -v "$pool" TRAJECT.pool.old > TRAJECT.pool

exit 0
