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
# Fichier    : OGR_Rasterize.tcl
# Creation   : Decembre 2005 - J.P. Gauthier - CMC/CMOE
# Description: Rasterization de donnees vectorielles dans une bande GDAL
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

proc OGRRasterize { File } {

   #----- Ouvrir les donnees vectorielles

   set layer [lindex [ogrfile open SHAPE read $File] 0]
   eval ogrlayer read LAYER $layer

   #----- Recuperer les limites qui couvre les donnees vectorielles

   set extent [ogrlayer stat LAYER -extent]
   set res    50.0
   set dx     [expr [lindex $extent 2]-[lindex $extent 0]]
   set dy     [expr [lindex $extent 3]-[lindex $extent 1]]

   if { $dx!=0 && $dy!=0 } {

      set width  [expr int(ceil($dx/$res))+1]
      set height [expr int(ceil($dy/$res))+1]

      puts ""
      puts "   Processing: $File"
      puts "   Extent    : $extent"
      puts "   Resolution: $res metres"
      puts "   Dimension : $width x $height"

      #----- Creer le fichier de rasterization

      catch { file delete -force DataOut/[file rootname [file tail $File]].tif }
      gdalfile open FILEOUT write DataOut/[file rootname [file tail $File]].tif GeoTiff

      gdalband create RASTER $width $height 1 Byte
      gdalband define RASTER -projection [ogrlayer define LAYER -projection]
      gdalband define RASTER -transform [list [lindex $extent 0] $res 0.000000000000000 [lindex $extent 1] 0.000000000000000 $res]

      #----- Effectuer la rasterization (import)

      gdalband gridinterp RASTER LAYER FAST

      #----- On sauvegarde le tout
      gdalband write RASTER FILEOUT { COMPRESS=NONE PROFILE=GeoTIFF }
      gdalfile close FILEOUT
      gdalband free RASTER
   }

   ogrfile close SHAPE
   ogrlayer free LAYER
}

#----- Process tout les fichier shape du repertoire

foreach file [glob -nocomplain DataIn/noire*.shp] {
   OGRRasterize $file
}

