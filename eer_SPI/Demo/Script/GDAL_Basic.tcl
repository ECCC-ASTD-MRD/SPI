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

catch { eval file delete [glob -nocomplain DataOut/GDAL_Basic.*] }

#----- Afficher la liste des formats reconnus

puts "   Available formats:\n\t\t[join [gdalfile format] "\n\t\t"]"

#----- Ouverture d'un fichier GTIF

set bands [gdalfile open GDAL read DataIn/srtm_n045w074_badmedian3x3]
#set bands [gdalfile open GDAL read /tmp/T_PAGX40_C_BIRK_20111230091501.h5]
#set bands [gdalfile open GDAL read HDF5:"/tmp/T_PAGX40_C_BIRK_20111230091501.h5"://dataset1/data1/data]

puts "   found: $bands"

#----- Affichage des mete-donnees

puts "   Metadata: [gdalfile metadata GDAL]"

#----- Lecture des bandes

gdalband read RASTER $bands

puts "   Min     : [gdalband stats RASTER -min]"
puts "   Max     : [gdalband stats RASTER -max]"

#----- converions pixel-latlon et inverse

set ll [gdalband project RASTER 100 100]
set xy [gdalband unproject RASTER [lindex $ll 0] [lindex $ll 1]]

puts "   Pixel projection 100 100 -> $ll -> $xy"
gdalfile close GDAL

puts "    Creating a band "
gdalband create NEWRASTER 900 900 1 Byte

puts "    Saving direct mode"
gdalfile open NEWFILE write DataOut/GDAL_Basic.envi "ENVI"
gdalband write NEWRASTER NEWFILE
gdalfile close NEWFILE

puts "    Saving copy mode (PNG)"
gdalband stats NEWRASTER -nodata 100
gdalband clear NEWRASTER

gdalfile createcopy  DataOut/GDAL_Basic.png [list RASTER RASTER RASTER] "PNG"

puts "    Freeing a band "
gdalband free NEWRASTER
