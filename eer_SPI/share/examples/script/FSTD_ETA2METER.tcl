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
# Fichier    : FSTD_ETA2SIGMA.tcl
# Creation   : Mai 2000 - J.P. Gauthier - CMC/CMOE
# Description: Interpoler des chamsp dans la verticale sur des niveaux sigma
#
# Parametres :
#
# Retour:
#
# Remarques  :
#
#============================================================================

package require TclData
#package require TclGeoEER

puts \n[file tail [info script]]

fstdfield ip1mode NEW

#----- Ouvrir les fichiers d'entree (1) sortie (2)

set levels { 1000 2000 3000 4000 5000 6000 7000 8000 9000 10000 }

fstdfile open 1 read  DataIn/2005102612_012
fstdfile open 2 write DataOut/FSTD_ETA2METER.fstd

#----- Get the grid dimensions

fstdfield read GZ 1 -1 "" -1 -1 -1 "" "GZ"
fstdfield create TO [fstdfield define GZ -NI] [fstdfield define GZ -NJ] [llength $levels]
fstdfield stats TO -leveltype MAGL -levels $levels
fstdfield readcube GZ

#----- Boucler sure les champs a interpoler
#----- TODO: GZ part is not right
foreach var { TT UU GZ } {
   fstdfield read FROM 1 -1 "" -1 -1 -1  "" $var
   fstdfield readcube FROM

   fstdfield verticalinterp TO FROM GZ GZ
   fstdfield write TO 2 -32 True
}

fstdfile close 1
fstdfile close 2
fstdfile close 3
