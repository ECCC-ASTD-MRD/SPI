#!/bin/sh
#-----------------------------------------------------------------------------
#   Environnement Canada
#   Centre Meteorologique Canadien
#   Dorval, Quebec
#
#   Projet     : Gestion des "filesystems".
#   Nom        : <PipePut>
#   Creation   : Mai 1997 S. Trudel - CMC/CMOE
#   
#   But        : Creer des fichiers 'tar' a archiver de maniere efficace
#
#   Parametres :
#     ${1}     : nom du fichier a considerer.
#     ${2}     : nom de la machine de destination.
#     ${3}     : nom du fichier de sortie ( path compris ).
#   
#   Retour     :
#     Aucun.
#
#   Remarques  :
#     - Commande de Daniel Pelissier.
#
#     - le premier argument peut-etre egalement un repertoire.
#       Le mot fichier doit etre considerer au sens large.
#
#   Modifications :
#
#     Nom         : -
#     Date        : -
#     Description : -
#
#------------------------------------------------------------------------------
#set -x

JOB_ID=$$

#----- Recupere les divers arguments.

FileIn=${1}
HostDest=${2}
FileDest=${3}

#----- Repertoire temporaire.

SV_TMP="/tmp"

#----- Creer le "named pipe".

mknod ${SV_TMP}/${USER}_${JOB_ID} p

#----- Copier les fichiers demandes.

smcp ${SV_TMP}/${USER}_${JOB_ID} ${HostDest}:${FileDest} &

tar cvf ${SV_TMP}/${USER}_${JOB_ID} ${FileIn}

echo
echo "... copie effectuee !"

#----- Efface notre "named pipe".

rm ${SV_TMP}/${USER}_${JOB_ID}

exit 0
