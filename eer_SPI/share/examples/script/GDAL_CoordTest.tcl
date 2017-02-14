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

#----- Afficher la liste des formats reconnus
set bands [gdalfile open GDAL read DataIn/CMC_reg_PRES_SFC_0_ps60km_2011052500_P000.grib2]

#----- Lecture des bandes
gdalband read RASTER $bands

set ll [gdalband stats RASTER -gridpoint 0.0 0.0]
set xy [gdalband stats RASTER -coordpoint [lindex $ll 0] [lindex $ll 1]]

puts "GDAL"
puts "   Pixel projection 0 0 -> $ll -> $xy"
puts "   Pixel value 0 0 : [gdalband stats RASTER -gridvalue 0.0 0.0]"
puts "   Pixel value ll  : [gdalband stats RASTER -coordvalue [lindex $ll 0] [lindex $ll 1]]"

gribfile open GRIB read DataIn/CMC_reg_PRES_SFC_0_ps60km_2011052500_P000.grib2
gribfield read FLD GRIB 0

set ll [gribfield stats FLD -gridpoint  0 94]
set xy [gribfield stats FLD -coordpoint [lindex $ll 0] [lindex $ll 1]]

puts "GRIB"
puts "   Pixel projection 0 0 -> $ll -> $xy"
puts "   Pixel value 0 0 : [gribfield stats FLD -gridvalue 0 94]"
puts "   Pixel value ll  : [gribfield stats FLD -coordvalue [lindex $ll 0] [lindex $ll 1]]"

Log::End