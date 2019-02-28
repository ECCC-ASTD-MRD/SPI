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
# Fichier    : ASC2SHP.tcl
# Creation   : Avril 2006 - J.P. Gauthier - CMC/CMOE
# Description: Conversion d'un fichier ASC en Shapefile
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

#----- Path de repertoire a traiter en parametre
set Data(Path) $env(CI_SPI_IN)/10101214.ASC
set Data(Nb)   0

#----- Create the shapefile
catch { file delete $env(CI_SPI_OUT)/OGR_ASC2SHP.shp }
ogrfile open SHAPEFILE write $env(CI_SPI_OUT)/OGR_ASC2SHP.shp "ESRI Shapefile"

#----- Create a layer within it
ogrlayer create SHAPEFILE LAYER "Grid"

#----- Define the fields and type for this layer
ogrlayer define LAYER -field NO2 Real

#----- Create temporary geometry used to fill up the layer
ogrgeometry create POLY "Polygon"
ogrgeometry create RING "Linear Ring"

#----- Open the ASC file and loop on it
set f [open $Data(Path) r]
while { ![eof $f] } {

   gets $f line

   #----- If we have a valid line
   if { $line!="" && [string index $line 0]!="*" } {

      #----- Extract the info we need form this line
      set val  [lindex $line 1]
      set lat0 [lindex $line 3]
      set lat1 [lindex $line 4]
      set lat2 [lindex $line 5]
      set lat3 [lindex $line 6]
      set lon0 [lindex $line 7]
      set lon1 [lindex $line 8]
      set lon2 [lindex $line 9]
      set lon3 [lindex $line 10]

      #----- Add a feature to the layer
      ogrlayer define LAYER -nb [incr Data(Nb)]
      set no [expr $Data(Nb)-1]

      #----- Fill in the geometry with the data
      #----- For some reason, point 2 and 3 are inverted
      ogrgeometry define RING -points {}
      ogrgeometry define RING -addpoint $lon0 $lat0
      ogrgeometry define RING -addpoint $lon1 $lat1
      ogrgeometry define RING -addpoint $lon3 $lat3
      ogrgeometry define RING -addpoint $lon2 $lat2
      ogrgeometry define RING -addpoint $lon0 $lat0
      ogrgeometry define POLY -geometry False RING

      #----- Set the value for the fields
      ogrlayer define LAYER -feature $no NO2 $val

      #----- Set the geometry
      ogrlayer define LAYER -geometry $no False POLY
   }
}

#----- close the file
ogrfile close SHAPEFILE

Log::End