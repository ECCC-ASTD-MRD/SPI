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
#package require TclGeoEER
package require Logger

Log::Start [info script] 0.1

#----- Ouverture du fichier
set bands [gdalfile open GDAL read $env(CI_SPI_IN)/SAR.tif]
puts "   found: \n\t[join $bands \n\t]"

#----- Lecture des bandes
gdalband read RASTER [list [lindex $bands 0]]
gdalband read LAT [list [lindex $bands 4]]
gdalband read LON [list [lindex $bands 5]]

#----- converions pixel-latlon et inverse
set ll [gdalband stats RASTER -project 100 100]
set xy [gdalband stats RASTER -unproject [lindex $ll 0] [lindex $ll 1]]
puts "   Before positionnal:"
puts "      LL->XY : $ll = $xy"
puts "      Min    : [gdalband stats RASTER -min]"
puts "      Max    : [gdalband stats RASTER -max]"

#----- Assigner les latlon
puts "\n   Corner latlon (0,0)=[gdalband stats LAT -gridvalue 0 0] [gdalband stats LON -gridvalue 0 0]\n"
gdalband define RASTER -positional LON LAT

#----- converions pixel-latlon et inverse
set ll [gdalband stats RASTER -project 100 100]
set xy [gdalband stats RASTER -unproject [lindex $ll 0] [lindex $ll 1]]
puts "   After positional:"
puts "      LL->XY : $ll = $xy"
puts "      Min    : [gdalband stats RASTER -min]"
puts "      Max    : [gdalband stats RASTER -max]"

#----- Configure the associated map to the limits of the data (if running in SPI)
catch {
   set map [gdalband configure RASTER -colormap]
   colormap configure $map -min red [lindex [lindex [gdalband stats RASTER -min] 0] 0]
   colormap configure $map -max red [lindex [lindex [gdalband stats RASTER -max] 0] 0]

   #----- Display
   projection configure $Page::Data(Frame) -data RASTER
   #Mapper::UpdateData $Page::Data(Frame) RASTER
}
gdalfile close GDAL

Log::End
