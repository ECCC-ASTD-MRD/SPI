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
# Fichier    : OBS_Interp.tcl
# Creation   : Decembre 2005 - J.P. Gauthier - CMC/CMOE
# Description: Interpolation de champs a la position d'observations
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

#----- Input files

set obs DataIn/aircraft_obs_expl.obs
set fst DataIn/2005102612_012
set out DataOut/OBS_Interp.obs

#----- Standard file field to be read

fstdfile open 1 read $fst
fstdfield read FLD 1 -1 "" -1 -1 -1 "" TT
fstdfield read GZ  1 -1 "" -1 -1 -1 "" GZ
fstdfield copy OBS FLD

#----- Read the obs

set obs [lindex [observation load $obs] 0]

#----- Setup the output file

set fout [open $out w]
puts $fout "Obs 3.1"
puts $fout "Interpolated values"
puts $fout "ID                LAT       LON   ELEV ELEVTYP DATA.OBS DATA.FLD DATA.OBS-FLD"

#----- loop on all observation locations

for { set i 0 } { $i<[observation define $obs -NB] } { incr i } {

   #----- Get the observation's position (3D)

   set coords [observation define $obs -COORD $i]

   #----- Do the vertical interpolation to the right level

   fstdfield stats OBS -leveltype MASL -levels [lindex $coords 2]
   fstdfield verticalinterp OBS FLD - GZ

   #----- Do the horizontal interpolation to the right spot

   set value [fstdfield stats OBS -coordvalue [lindex $coords 0] [lindex $coords 1]]

   puts $fout [format "\"%-20s\" %7.4f %8.4f %10.2f %i %10.3e %10.3e %10.3e" [observation define $obs -ID $i] [lindex $coords 0] [lindex $coords 1] [lindex $coords 2] 0 \
      [observation define $obs -DATA $i] $value [expr [observation define $obs -DATA $i]-$value]]
}

close $fout
fstdfile close 1
