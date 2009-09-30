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

#----- Afficher la liste des formats reconnus

puts [gdalfile format]

#----- Ouverture d'un fichier GTIF

set bands [gdalfile open GDAL read DataIn/srtm_n045w074_badmedian3x3]
puts "   found: $bands"

#----- Affichage des mete-donnees

puts "   metadata: [gdalfile metadata GDAL]"

#----- Lecture des bandes

gdalband read RASTER $bands

#----- converions pixel-latlon et inverse

set ll [gdalband project RASTER 100 100]
set xy [gdalband unproject RASTER [lindex $ll 0] [lindex $ll 1]]

puts "   pixel 100 100 -> $ll -> $xy"

gdalfile close GDAL
