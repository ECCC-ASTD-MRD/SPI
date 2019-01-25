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
# Fichier    : GDAL_PixelCoord.tcl
# Creation   : Mai 2010 - J.P. Gauthier - CMC/CMOE
# Description: Extraire les lat-lons-valeurs de chaque pixel.
#
# Parametres :
#   <Fichier>: fichier de bande GDAL
# Retour:
#
# Remarques  :
#
#============================================================================

package require TclData
#package require TclGeoEER
package require Logger

Log::Start [info script] 0.1

set file [lindex $argv 0]
set x0   [lindex $argv 1]
set y0   [lindex $argv 2]
set x1   [lindex $argv 3]
set y1   [lindex $argv 4]

#set x1 [gdalband define BAND -width]
#set y1 [gdalband define BAND -height]

#----- Ouverture du fichier
set bands [gdalfile open FILE read $file]
gdalband read BAND $bands

for { set x $x0 } { $x<$x1 } { incr x } {
   for { set y $y0} { $y<$y1 } { incr y } {
      set coords [gdalband stats BAND -gridpoint $x $y]
      set val    [gdalband stats BAND -gridvalue $x $y]

      puts "   $x $y $coords $val"
   }
}

Log::End