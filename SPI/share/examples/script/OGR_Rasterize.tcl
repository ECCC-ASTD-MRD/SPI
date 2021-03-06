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
#package require TclGeoEER
package require Logger

Log::Start [info script] 0.1

proc OGRRasterize { File } {
   global env
   
   #----- Ouvrir les donnees vectorielles
   set layer [lindex [ogrfile open SHAPE read $File] 0]
   eval ogrlayer read LAYER $layer

   #----- Recuperer les limites qui couvre les donnees vectorielles
   set extent [ogrlayer stats LAYER -extent]
   set res    50.0
   set dx     [expr [lindex $extent 2]-[lindex $extent 0]]
   set dy     [expr [lindex $extent 3]-[lindex $extent 1]]

   if { $dx!=0 && $dy!=0 } {

      set width  [expr int(ceil($dx/$res))+1]
      set height [expr int(ceil($dy/$res))+1]

      Log::Print INFO "Processing: $File"
      Log::Print INFO "   Extent    : $extent"
      Log::Print INFO "   Resolution: $res metres"
      Log::Print INFO "   Dimension : $width x $height"

      #----- Creer le fichier de rasterization

      catch { file delete -force $env(CI_DATA_OUT)/[file rootname [file tail $File]].tif }
      gdalfile open FILEOUT write $env(CI_DATA_OUT)/[file rootname [file tail $File]].tif GeoTiff

      gdalband create RASTER $width $height 1 Byte
      gdalband define RASTER -projection [ogrlayer define LAYER -projection]
      gdalband define RASTER -transform [list [lindex $extent 0] $res 0.0 [lindex $extent 1] 0.0 $res]

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

foreach file [glob -nocomplain $env(CI_DATA_IN)/noire*.shp] {
   OGRRasterize $file
}

Log::End