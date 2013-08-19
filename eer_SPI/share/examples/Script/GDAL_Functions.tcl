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
# Fichier    : GDAL_Functions.tcl
# Creation   : Mars 2011 - J.P. Gauthier - CMC/CMOE
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

puts \n[file tail [info script]]

set bands [gdalfile open FILE1 read DataIn/MONTREAL_Building-fraction.tif]
gdalband read BANDDEST $bands

set bands [gdalfile open FILE2 read DataIn/MONTREAL_building-heights.tif]
gdalband read BANDSRC $bands

set layers [ogrfile open FILE3 read DataIn/bat_2d_st.shp]
eval ogrlayer read LAYERSRC [lindex $layers 0]

#----- Test raster interpolation
puts "   Interpolating raster in CONSERVATIVE Mode"
gdalband clear BANDDEST 0
gdalband gridinterp BANDDEST BANDSRC CONSERVATIVE 1
gdalband configure BANDDEST -desc "Raster to Raster conservative"

#----- Test vector interpolation
puts "   Interpolating layer area in CONSERVATIVE mode"
gdalband copy BANDDESTV BANDDEST
gdalband clear BANDDESTV 0
gdalband gridinterp BANDDESTV LAYERSRC CONSERVATIVE FEATURE_AREA
gdalband configure BANDDESTV -desc "Layer to Raster conservative area"

catch { file delete DataOut/GDAL_Functions.tif }
gdalfile open FILEOUT write DataOut/GDAL_Functions.tif "GeoTIFF"
gdalband write { BANDDEST BANDDESTV } FILEOUT
gdalfile close FILEOUT

gdalband free BANDDEST BANDESTV BANDSRC
ogrlayer free LAYERSRC