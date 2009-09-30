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
# Fichier    : OBS_Extract.tcl
# Creation   : Mai 2004 - J.P. Gauthier - CMC/CMOE
# Description: Extraire lav valeur d'un champs a la position des obs
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

fstdfile open 1 read  DataIn/pression.fstd
set obs [lindex [observation load DataIn/O3.20050302.obs] 0]

#----- Standard file field to be read

fstdfield read FLD 1 -1 "" -1 -1 -1 "" MN
fstdfield configure FLD -interpdegree LINEAR

#----- Extract field value a obs location

observation extract $obs FLD
observation write DataOut/OBS_Extract.obs [list $obs] "Extraction test"
