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

puts \n[file tail [info script]]

set file [lindex $argv 0]

#----- Ouverture du fichier
set bands [gdalfile open FILE read $file]
gdalband read BAND $bands

for { set x 0 } { $x<[gdalband define BAND -width] } { incr x } {
   for { set y 0 } { $y<[gdalband define BAND -height] } { incr y } {
      set coords [gdalband stats BAND -gridpoint $x $y]
      set val    [gdalband stats BAND -gridvalue $x $y]

      puts "$x $y $coords $val"
   }
}
