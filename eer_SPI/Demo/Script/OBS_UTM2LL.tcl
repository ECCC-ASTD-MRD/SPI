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
# Fichier    : OBS_UTM2LL.tcl
# Creation   : Janvier 2007 - J.P. Gauthier - CMC/CMOE
# Description: Sauvegarder un ficheir ascii en coord UTM en fichier obs en coord LatLon
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

#----- Create UTM referential

georef create UTMREF
georef define UTMREF -projection {PROJCS["WGS_1984_UTM_Zone_14N",GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Transverse_Mercator"],PARAMETER["False_Easting",500000.0],PARAMETER["False_Northing",0.0],PARAMETER["Central_Meridian",-99.0],PARAMETER["Scale_Factor",0.9996],PARAMETER["Latitude_Of_Origin",0.0],UNIT["Meter",1.0]]}

#----- Translation in XY

set dx 633683.3125
set dy 3923941.0

set dx 633756.04798
set dy 3924024.06671
puts [georef project UTMREF 633756.04798 3924024.06671]
exit

#----- Open the ASCII file

set in  [open DataIn/OBS_UTM2LL.txt r]
set out [open DataOut/OBS_UTM2LL.obs w]

#----- Output header

puts $out "Obs 3.1"
puts $out "Obs from UTM to LL"
puts $out "ID LAT LON ELEV ELEVTYP DATA.Conc"

#----- Loop on te TXT file

while { ![eof $in] } {

   gets $in line

   if { $line!="" } {
      set id [lindex $line 0]
      set x  [lindex $line 1]
      set y  [lindex $line 2]
      set z  [lindex $line 3]
      set v  [lindex $line 4]

      #----- Reproject to LL

      set ll [georef project UTMREF [expr $x+$dx] [expr $y+$dy]]

      #----- Save to OBS file

      puts $out "$id [lindex $ll 0] [lindex $ll 1] $z MAGL $v"
   }
}

close $in
close $out
