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
# Fichier    : FSTD_WindDirProfile.tcl
# Creation   : Mai 2000 - J.P. Gauthier - CMC/CMOE
# Description: Extraire un profil de direction de vents#
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

set lat 25
set lon -100

#----- Ouvrir les fichiers d'entree
fstdfile open FILE read  DataIn/2005102612_012

#----- Lire le champs de vents
fstdfield read UU FILE -1 "" -1 -1 -1 "" "UU"
fstdfield readcube UU

#----- Calcule le champs de directions
vexpr WD @UU@

#----- Extraire les valeurs aux niveaux
set listeWD {}
for { set k 0 } { $k < [fstdfield define UU -NK] } { incr k } {
   fstdfield stats WD -level $k
   lappend listeWD [fstdfield stats WD -coordvalue $lat $lon]
}
puts "   $listeWD"

fstdfile close FILE
