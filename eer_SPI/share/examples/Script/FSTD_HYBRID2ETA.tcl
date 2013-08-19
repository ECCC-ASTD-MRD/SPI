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
# Fichier    : FSTD_HYBRID2ETA.tcl
# Creation   : Mai 2000 - J.P. Gauthier - CMC/CMOE
# Description: Interpoler des chamsp dans la verticale de hybride a eta
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

#----- Arguments
#   FSTD_HYBRID2ETA.tcl [file]

set file [lindex $argv 0]

set vars   { WE GZ HU QC TT ES HR WW UU FN }
set levels { 0.0000 0.0102 0.0233 0.0374 0.0508 0.0625 0.0720 0.0795 0.0852 0.0897 0.0941 0.0990
             0.1044 0.1104 0.1172 0.1248 0.1541 0.1667 0.1812 0.1976 0.2149 0.2331 0.2522 0.2721
             0.2928 0.3144 0.3369 0.3602 0.3843 0.4091 0.4348 0.4612 0.4883 0.5161 0.5446 0.5737
             0.6034 0.6337 0.6646 0.6959 0.7272 0.7567 0.7845 0.8104 0.8346 0.8571 0.8780 0.8973
             0.9151 0.9316 0.9467 0.9606 0.9733 0.9850 0.9950 1.0 }

fstdfield ip1mode OLD

#----- Open input and output files
set fields [fstdfile open MESOSTRATO read $file]
fstdfile open MESO write $file.eta

#----- Read surface pressure, we need it to go from HYBRID to ETA
fstdfield read P0 MESOSTRATO -1 "" -1 -1 -1 "" "P0"

#----- Create the destination grid
fstdfield create OUT [fstdfield define P0 -NI] [fstdfield define P0 -NJ] [llength $levels]
fstdfield stats OUT -leveltype ETA -levels $levels -top 10.0
fstdfield configure OUT -interpdegree LINEAR

#----- Interpolate3D fields
foreach var $vars {
   puts "   Interpolating $var"
   fstdfield read FLD MESOSTRATO -1 "" -1 -1 -1 "" "$var"
   fstdfield readcube FLD True

   fstdfield verticalinterp OUT FLD P0 P0
   fstdfield write OUT MESO 0 True
}

#----- Close the files
fstdfile close MESOSTRATO MESO
