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

catch { eval file delete [glob -nocomplain DataOut/GDAL_Basic.*] }

#----- Afficher la liste des formats reconnus
puts "   Available formats:\n\t\t[join [gdalfile format] "\n\t\t"]"

#----- Ouverture d'un fichier GTIF
#set bands [gdalfile open GDAL read /fs/cetus/fs2/ops/cmoe/afsr002/DataIn/hrm_dsm_1m/z001003.adf]
#set bands [gdalfile open GDAL read DataIn/srtm_n045w074_badmedian3x3]
#set bands [gdalfile open GDAL read /tmp/T_PAGX40_C_BIRK_20111230091501.h5]
#set bands [gdalfile open GDAL read HDF5:"/tmp/T_PAGX40_C_BIRK_20111230091501.h5"://dataset1/data1/data]
#set bands [gdalfile open GDAL read DataIn/SAR.tif]
set bands [gdalfile open GDAL read DataIn/2010081900_000024p.grb]

puts "   found: $bands"

#----- Affichage des mete-donnees
puts "   Metadata          (file): [gdalfile metadata GDAL]"

#----- Lecture des bandes
gdalband read RASTER $bands

puts "   Metadata original (band): [gdalband define RASTER -metadata]"
gdalband define RASTER -metadata [list toto=123 titi=456]
puts "   Metadata changed  (band): [gdalband define RASTER -metadata]"

puts "   Min     : [gdalband stats RASTER -min]"
puts "   Max     : [gdalband stats RASTER -max]"

puts "   Histogram  : [gdalband stats RASTER -histogram 0]"
puts "   Stretch 10%: [gdalband stats RASTER -stretch 0 PERCENT_CLIP 10 90]"

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

gdalband stats NEWRASTER -nodata 100
gdalband clear NEWRASTER

gdalfile close NEWFILE

puts "    Saving copy mode (PNG)"

gdalfile createcopy DataOut/GDAL_Basic.png [list RASTER RASTER RASTER] "PNG"

puts "    Saving copy mode in memory (PNG)"
set bin [gdalfile createcopy /vsimem/GDAL_Basic.png [list RASTER RASTER RASTER] "PNG"]
set f [open DataOut/GDAL_Basic_MEM.png w]
fconfigure $f -translation binary
puts $f $bin
close $f


puts "    Freeing a band "
gdalband free NEWRASTER

Log::End
