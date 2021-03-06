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
# Fichier    : OGR_FCST2SHP.tcl
# Creation   : Decembre 2009 - J.P. Gauthier - CMC/CMOE
# Description: Creer un fichier Shapefile avec les localisations des previsions
#              officielles
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

set f [open $env(CI_DATA_IN)/FCST.txt]

#----- Creation du fichier
catch { file delete $env(CI_DATA_OUT)/OGR_FCST2SHP.shp }
ogrfile open FILE write $env(CI_DATA_OUT)/OGR_FCST2SHP.shp "ESRI Shapefile"

#----- Creation du layer et des champs
ogrlayer create FILE FCST "Forecast"
ogrlayer define FCST -field Id String
ogrlayer define FCST -field Name String
ogrlayer define FCST -field Province String

ogrgeometry create POINT "Point"

set nb 0

#----- Boucle sur les localisation definie dans le fichier texte
while { ![eof $f] } {
   gets $f line

   set id   [lindex $line 0]
   set name [lrange $line 1 end-3]
   set prov [lindex $line end-2]
   set lat  [lindex $line end-1]
   set lon  [lindex $line end]

   ogrgeometry define POINT -points { }
   ogrgeometry define POINT -addpoint $lon $lat

   ogrlayer define FCST -nb [incr nb]
   set no [expr $nb-1]
   ogrlayer define FCST -feature $no Id $id
   ogrlayer define FCST -feature $no Name $name
   ogrlayer define FCST -feature $no Province $prov
   ogrlayer define FCST -geometry $no False POINT
}

ogrfile close FILE

Log::End