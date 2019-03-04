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
# Fichier    : OGR_GFA2SHP.tcl
# Creation   : Mars 2015 - J.P. Gauthier - CMC/CMOE
# Description: Conversion d'un fichier CSV des zones GFA en Shapefile
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
set Data(Path) $env(CI_DATA_IN)/GFACN.csv
set Data(Nb)   0
set Data(Name) "" 

#----- Create the shapefile
catch { file delete $env(CI_DATA_OUT)/GFA.shp }
ogrfile open SHAPEFILE write $env(CI_DATA_OUT)/GFA.shp "ESRI Shapefile"

#----- Create a layer within it
ogrlayer create SHAPEFILE LAYER "GFA"

#----- Define the fields and type for this layer
ogrlayer define LAYER -field Name String

#----- Create temporary geometry used to fill up the layer
ogrgeometry create POLY "Polygon"
ogrgeometry create RING "Linear Ring"

#----- Open the CSV file and loop on it
set f [open $Data(Path) r]
gets $f line
while { ![eof $f] } {

   gets $f line
   set line [split $line ,]
   
   #----- If we have a valid line
   if { $line!="" } {
      lappend Coords([lindex $line 0]) [lindex $line 2] [lindex $line 1]
   }
}

foreach area [array names Coords] {

   #----- Add a feature to the layer
   ogrlayer define LAYER -nb [incr Data(Nb)]
   set no [expr $Data(Nb)-1]

   #----- Set the value for the fields
   ogrlayer define LAYER -feature $no Name $area
         
   #----- Fill in the geometry with the data
   ogrgeometry define RING -points $Coords($area)
   ogrgeometry define POLY -geometry False RING  
   
   #----- Set the geometry
   ogrlayer define LAYER -geometry $no False POLY
}

#----- close the file
ogrfile close SHAPEFILE

Log::End