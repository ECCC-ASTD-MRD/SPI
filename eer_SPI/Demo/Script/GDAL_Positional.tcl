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
# Fichier    : GDAL_Basic.tcl
# Creation   : Mai 2000 - J.P. Gauthier - CMC/CMOE
# Description: Demonstration de base des fonctions GDAL pour les donneees raster
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

#----- Ouverture du fichier
set bands [gdalfile open GDAL read /data/goodenough/afsr005/Projects/Rick/2008-12-31_024723DH.00400.sar.nc]
puts "   found: \n\t[join $bands \n\t]"

#----- Lecture des bandes
gdalband read RASTER [list [lindex $bands 8]]
gdalband read LAT [list [lindex $bands 11]]
gdalband read LON [list [lindex $bands 12]]

#----- converions pixel-latlon et inverse
set ll [gdalband stats RASTER -project 100 100]
set xy [gdalband stats RASTER -unproject [lindex $ll 0] [lindex $ll 1]]
puts "   Before positionnal:"
puts "      LL->XY : $ll = $xy"
puts "      Min    : [gdalband stats RASTER -min]"
puts "      Max    : [gdalband stats RASTER -max]"

#----- Assigner les latlon
puts "\n   Corner latlon=[gdalband stats LAT -gridvalue 0 0] [gdalband stats LON -gridvalue 0 0]\n"
gdalband define RASTER -positional LON LAT

#----- converions pixel-latlon et inverse
set ll [gdalband stats RASTER -project 100 100]
set xy [gdalband stats RASTER -unproject [lindex $ll 0] [lindex $ll 1]]
puts "   After positional:"
puts "      LL->XY : $ll = $xy"
puts "      Min    : [gdalband stats RASTER -min]"
puts "      Max    : [gdalband stats RASTER -max]"

#----- Configure the associated map to the limits of the data
set map [gdalband configure RASTER -colormap]
colormap configure $map -min red [lindex [lindex [gdalband stats RASTER -min] 0] 0]
colormap configure $map -max red [lindex [lindex [gdalband stats RASTER -max] 0] 0]

#----- Display (if running in SPI)
catch {
   projection configure $Page::Data(Frame) -data RASTER
   #Mapper::UpdateData $Page::Data(Frame) RASTER
}
gdalfile close GDAL

