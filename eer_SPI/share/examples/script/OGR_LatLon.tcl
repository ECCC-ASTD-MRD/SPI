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
# Fichier    : OGR_LatLon.tcl
# Creation   : Novembre 2007 - J.P. Gauthier - CMC/CMOE
# Description: Generer un fichier de latlon a un interval specifique (Pour Lewis)
#
# Parametres :
#   <Delta>  : Delta en degre entre les latlon
#
# Retour:
#
# Remarques  :
#
#============================================================================

package require TclData
#package require TclGeoEER

puts \n[file tail [info script]]

set Delta [lindex $argv 0]

#----- Open the output shape file

catch { file delete -force DataOut/OGR_LatLon.shp }
ogrfile open FILE write DataOut/OGR_LatLon.shp "ESRI Shapefile"

#----- Create the ouput layer and its field

ogrlayer create FILE LATLON "LatLon"
ogrlayer define LATLON -field Coord Real

ogrgeometry create LINE "Line String"

set nb 0
for { set lon -180.0 } { $lon<180.0 } { set lon [expr $lon+$Delta] } {

   ogrlayer define LATLON -nb [incr nb]
   ogrlayer define LATLON -feature [expr $nb-1] Coord $lon

   ogrgeometry define LINE -points {}
   for { set lat [expr -90+$Delta] } { $lat<[expr 90.0-$Delta] } { set lat [expr $lat+$Delta] } {
      ogrgeometry define LINE -addpoint $lon $lat
   }
   ogrlayer define LATLON -geometry [expr $nb-1] False LINE
}

for { set lat [expr -90+$Delta] } { $lat<[expr 90.0-$Delta] } { set lat [expr $lat+$Delta] } {

   ogrlayer define LATLON -nb [incr nb]

   ogrgeometry define LINE -points {}
   for { set lon -180.0 } { $lon<180.0 } { set lon [expr $lon+$Delta] } {
      ogrgeometry define LINE -addpoint $lon $lat
   }

   ogrlayer define LATLON -feature [expr $nb-1] Coord $lat
   ogrlayer define LATLON -geometry [expr $nb-1] False LINE
}

ogrfile close FILE
