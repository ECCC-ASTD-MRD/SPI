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
# Fichier    : FSTD_Export.tcl
# Creation   : Janvier 2010 - J.P. Gauthier - CMC/CMOE
# Description: Exporter des champs FSTD en GeoTIFF et autres
#
# Parametres :
#
# Retour:
#
# Remarques  :
#   On interpolle sur une grille latlon a 0.1 degree
#============================================================================

package require TclData

puts \n[file tail [info script]]

set file [lindex $argv 0]

#----- Ouvrir le fichier standard
fstdfile open FILEIN read $file
fstdfield read FLD FILEIN -1 "" -1 -1 -1 "" "UV"

colormap create CMAP
colormap read CMAP DataIn/OTH_ASAR.rgba
fstdfield configure FLD -colormap CMAP -min 0 -max 50

#----- Recuperer les limites du champs
set extent [georef limit [fstdfield define FLD -georef]]
set res    0.01
set dx     [expr [lindex $extent 3]-[lindex $extent 1]]
set dy     [expr [lindex $extent 2]-[lindex $extent 0]]
set width  [expr int(ceil($dx/$res))+1]
set height [expr int(ceil($dy/$res))+1]

#----- Si le champs est 2D
if { $dx!=0 && $dy!=0 } {

   puts "   Processing: $file"
   puts "   Extent    : $extent"
   puts "   Resolution: $res degrees"
   puts "   Dimension : $width x $height ($dx x $dy)"

   #----- Creer le fichier de rasterization
   catch { file delete -force DataOut/[file rootname [file tail $file]].map.tif }
   gdalfile open FILEOUT write DataOut/[file rootname [file tail $file]].map.tif GeoTiff

   #----- Creer la bande et la configurer
#   gdalband create RASTER $width $height 1 Float32
#   gdalband create RASTER $width $height 1 Byte
   gdalband create RASTER $width $height 4 Byte
   gdalband define RASTER -transform [list [lindex $extent 1] $res 0.000000000000000 [lindex $extent 2] 0.000000000000000 -$res]
   gdalband configure RASTER -desc FLD

   #----- Effectuer l'interpolation
   gdalband gridinterp RASTER FLD

   #----- On sauvegarde le tout
   gdalband write RASTER FILEOUT { COMPRESS=NONE PROFILE=GeoTIFF }
   gdalfile close FILEOUT
   gdalband free RASTER
}

fstdfile close FILEIN