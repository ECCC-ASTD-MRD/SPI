#!/bin/sh
#-----------------------------------------------------------------------------
#   Environnement Canada
#   Centre Meteorologique Canadien
#   Dorval, Quebec
#
#   Projet     : Gestion des "filesystems".
#   Nom        : <PipeGet>
#   Creation   : Mai 1997 S. Trudel - CMC/CMOE
#
#   But        : Extraire des fichiers 'tar' a archiver de maniere efficace
#
#   Parametres :
#     ${1}     : nom du fichier a creer.
#     ${2}     : nom de la machine de recuperation.
#     ${3}     : nom du fichier a recuperer ( path compris ).
#
#   Retour     :
#     Aucun.
#
#   Remarques  :
#     - Commande de Daniel Pelissier.
#
#     - le premier argument peut-etre egalement un repertoire.
#       Le mot fichier doit etre considerer au sens large.
#------------------------------------------------------------------------------

JOB_ID=$$

#----- Recupere les divers arguments.

FileOut=${1}
HostFrom=${2}
FileFrom=${3}

#----- Repertoire temporaire.

SV_TMP="/tmp"

#----- Creer le "named pipe".

mknod ${SV_TMP}/${USER}_${JOB_ID} p

#----- Copier les fichiers demandes.

smcp ${HostFrom}:${FileFrom} ${SV_TMP}/${USER}_${JOB_ID} &

tar xvf ${SV_TMP}/${USER}_${JOB_ID} ${FileOut}

echo
echo "... copie effectuee !"

#----- Efface notre "named pipe".

rm ${SV_TMP}/${USER}_${JOB_ID}

exit 0
