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
# Fichier    : GDAL_CoordTest.tcl
# Creation   : Mai 2000 - J.P. Gauthier - CMC/CMOE
# Description: Test de projection des coordonees et valeur entre GDAL et GRIB
#              pour valider le d/callage de 0.5 des grilles GDAL definie par le coin
#
# Parametres :
#
# Retour:
#
# Remarques  :
#
#============================================================================

package require TclData
package require Logger

Log::Start [info script] 0.1

set grib  /data/aqli06/afsuair/cmaq/aqm.20160701/aqm.t12z.pm25.f01.148.grib2
#set grib  DataIn/CMC_reg_PRES_SFC_0_ps60km_2011052500_P000.grib2

#----- Afficher la liste des formats reconnus
set bands [gdalfile open GDAL read $grib]

#----- Lecture des bandes
gdalband read RASTER $bands
#gdalband configure RASTER -interpdegree NEAREST

set ll [gdalband stats RASTER -gridpoint 0.0 0.0]
set xy [gdalband stats RASTER -coordpoint [lindex $ll 0] [lindex $ll 1]]

puts "GDAL"
puts "   Pixel projection 0 0 -> $ll -> $xy"
puts "   Pixel value 0 0 : [gdalband stats RASTER -gridvalue 0.0 0.0]"
puts "   Pixel value ll  : [gdalband stats RASTER -coordvalue [lindex $ll 0] [lindex $ll 1]]"

gribfile open GRIB read $grib
gribfield read FLD GRIB 0
#gribfield configure FLD -interpdegree NEAREST

set ll [gribfield stats FLD -gridpoint  0 94]
set xy [gribfield stats FLD -coordpoint [lindex $ll 0] [lindex $ll 1]]

puts "GRIB"
puts "   Pixel projection 0 94 -> $ll -> $xy"
puts "   Pixel value 0 0 : [gribfield stats FLD -gridvalue 0 94]"
puts "   Pixel value ll  : [gribfield stats FLD -coordvalue [lindex $ll 0] [lindex $ll 1]]"

puts "STATION"
set lat 45.4680555556
set lon -73.7411111111

puts "   gdal pos ll  : [gdalband stats RASTER -coordpoint $lat $lon]"
puts "   grib pos ll  : [gribfield stats FLD -coordpoint $lat $lon]"

puts "   gdal value ll  : [gdalband stats RASTER -coordvalue $lat $lon]"
puts "   grib value ll  : [gribfield stats FLD   -coordvalue $lat $lon]"

Log::End