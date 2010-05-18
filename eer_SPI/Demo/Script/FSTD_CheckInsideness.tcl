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
# Fichier    : FSTD_CheckInsideness.tcl
# Creation   : Mai 2010 - J.P. Gauthier - CMC/CMOE
# Description: Tester si des latlon sont a l'interieur d'une grille (LAM)
#
# Parametres :
#   <File>   : Fichier standard
#
# Retour:
#
# Remarques  :
#
#============================================================================

package require TclData

puts \n[file tail [info script]]

set fstd   [lindex $argv 0]
set latlon [lindex $argv 1]
#----- Read in a field to get the grid
fstdfile open FSTDFILE read $fstd
fstdfield read FSTDFIELD FSTDFILE -1 "" -1 -1 -1 "P" ""

#----- Loop on the coodrinates
set f [open $latlon r]
while { ![eof $f] } {

   #----- Get the coordinates
   gets $f coords
   set lat [lindex $coords 0]
   set lon [lindex $coords 1]

   #----- Get the corresponding gridpoint (outside= -1,-1)
   set grid [fstdfield stats FSTDFIELD -coordpoint $lat $lon]

   if { [lindex $grid 0]==-1 } {
      puts "Coordinate outside ($lat,$lon)"
   }
}

fstdfile close 1
