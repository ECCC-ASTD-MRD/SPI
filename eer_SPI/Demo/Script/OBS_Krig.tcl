#!/bin/sh
# the next line restarts using tclsh \
exec $SPI_PATH/tclsh "$0" "$@"
#============================================================================
# Environnement Canada
# Centre Meteorologique Canadien
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet     : Exemple de scripts.
# Fichier    : OBS_Krig.tcl
# Creation   : Mars 2005 - J.P. Gauthier - CMC/CMOE
# Description: Demonstration du krigging pour interpoler des obs sur une grille
#
# Parametres :
#
# Retour:
#
# Remarques  :
#
#============================================================================

package require TclData

puts \n[file tail [info script]]

#----- Ouvrir les fichiers d'entree (1) sortie (2)

catch { file delete -force DataOut/OBS_Krig.fstd }

fstdfile open 1 read  DataIn/pression.fstd
fstdfile open 2 write DataOut/OBS_Krig.fstd

#----- Recuperer le champs pour la grille d'interpolation

fstdfield read FLD 1 -1 "" -1 -1 -1 "" ""
vexpr FLD FLD<<0

#----- Recuperer l'observation a interpoler

set OBS [lindex [observation load DataIn/O3.20050302.obs] 0]

fstdfield gridinterp FLD $OBS LINEAR 0.0 1.0 10
fstdfield write FLD 2 -32 False
fstdfile close 2
