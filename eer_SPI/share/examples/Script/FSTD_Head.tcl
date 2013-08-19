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
# Fichier    : FSTD_Head.tcl
# Creation   : Decembre 2005 - J.P. Gauthier - CMC/CMOE
# Description: Afficher toute l'information sur les champs d'un fichier standard
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

fstdfile open FILE read [lindex $argv 0]

foreach field [fstdfield find FILE -1 "" { 0.0510 ETA } -1 -1 "" ""] {
   puts "   [fstdfield head FILE $field]"
}

fstdfile close FILE
