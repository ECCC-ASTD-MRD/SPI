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
# Fichier    : OGR_TrimOverlap.tcl
# Creation   : Mai 2010 - J.P. Gauthier - CMC/CMOE
# Description: Ne garder que les features ayant la valeur maximale d'un champs
#              en un meme point afin d'enlever les supperpositions de geometrie
#
# Parametres :
#
# Retour:
#
# Remarques  :
#   - For Toronto, use the following indexes for coverage feature
#        BATHWEST      5014
#        BEACH         5732
#        SPIT          292
#        WATRR         1034
#        WOODB         343
#        PARKDALE      2132
#        OSS           25471
#        NDT           3054
#        DUNDAS        2810
#============================================================================

package require TclData
#package require TclGeoEER
package require Logger

Log::Start [info script] 0.1

#----- Recperer les parametres
set File     [lindex $argv 0]
set Feature  [lindex $argv 1]
set Field    Elevation
set Field    ZCOORD_MAX
set Min      0
set Max      1000.0
set Res      1.0

#----- Copy file since we'll modify it
set name [file rootname $File]
foreach file [glob ${name}.*] {
   file copy -force $file ${name}_Trim[file extension $file]
}
set File ${name}_Trim.shp

#----- Ouvrir les donnees vectorielles
Log::Print INFO "Reading input file"
set layer [lindex [ogrfile open SHAPE append $File] 0]
eval ogrlayer read LAYER $layer

#----- Recuperer les limites qui couvre les donnees vectorielles
if { $Feature!="" } {
   set extent [ogrgeometry stat [ogrlayer define LAYER -geometry $Feature] -extent]
} else {
   set extent [ogrlayer stat LAYER -extent]
}
set dx     [expr [lindex $extent 2]-[lindex $extent 0]]
set dy     [expr [lindex $extent 3]-[lindex $extent 1]]

if { $dx!=0 && $dy!=0 } {

   #----- Definir les parametre de la bande raster
   set width  [expr int(ceil($dx/$Res))+1]
   set height [expr int(ceil($dy/$Res))+1]

   Log::Print INFO "Creating raster index"
   Log::Print INFO "   Extent    : $extent"
   Log::Print INFO "   Resolution: $Res metres"
   Log::Print INFO "   Dimension : $width x $height"

   #----- Creer la bande de rasterization
   gdalband create RASTER $width $height 1 Int32
   gdalband define RASTER -projection [ogrlayer define LAYER -projection]
   gdalband define RASTER -transform [list [lindex $extent 0] $Res 0.000000000000000 [lindex $extent 1] 0.000000000000000 $Res]
   gdalband stats RASTER -nodata -1

   #----- Initialiser les bandes de calculs
   gdalband copy RASTERMAX RASTER
   gdalband copy RASTERID RASTER
   gdalband clear RASTERMAX
   gdalband clear RASTERID

   Log::Print INFO "Ordering features ([ogrlayer define LAYER -nb])"
   for { set f 0 } { $f < [ogrlayer define LAYER -nb] } { incr f } {
      Log::Print DEBUG "   $f"

      #----- Rasterisation d'une feature a la fois
      ogrlayer define LAYER -featureselect [list [list - # [list $f]]]

      gdalband clear RASTER
      gdalband gridinterp RASTER LAYER FAST $Field

      #----- Verification de l'overlapp
      vexpr RASTER ifelse(RASTER<$Min,0,ifelse(RASTER>$Max,0,RASTER))
      vexpr RASTER ifelse(RASTER>RASTERMAX,RASTER,0)
      vexpr RASTERID ifelse(RASTER,$f,RASTERID)
      vexpr RASTERMAX ifelse(RASTER>RASTERMAX,RASTER,RASTERMAX)
   }

   #----- Ecrire les bandes pour finaliser
   Log::Print INFO "Getting feature histogram"

   set name ${File}.tif
   file delete -force $name
   gdalfile open NEWFILE write $name "GeoTiff"
   gdalband write RASTERID NEWFILE
   gdalfile close NEWFILE
   set bands [gdalfile open NEWFILE read $name]
   gdalband read RASTERID $bands

   #----- Recuperer l'histogramme des valeurs
   set vals [gdalband stats RASTERID -histogram 0 0 [expr [ogrlayer define LAYER -nb]-1] [ogrlayer define LAYER -nb] False]

   #----- Si une valeur n'est pas dans l'histogramme, on s'en debarasse
   Log::Print INFO "Cleaning up features ([llength $vals])"
   set n 0
   foreach v $vals {
      if { $v==0 } {
         Log::Print INFO "Deleting $n"
         ogrlayer delete LAYER $n
      }
      incr n
   }
}

ogrfile close SHAPE
ogrlayer free LAYER

#----- Merger everything in a single file
#exec /cnfs/ops/cmoe/afsr005/Lib/Linux/gdal-1.7.1/bin/ogr2ogr -update -append Toronto.shp $File -nln Toronto

Log::End