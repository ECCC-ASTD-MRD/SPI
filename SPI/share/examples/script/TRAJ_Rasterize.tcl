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
# Fichier    : TRAJ_Rasterize.tcl
# Creation   : Novembre 2011 - J.P. Gauthier - CMC/CMOE
# Description: Faire le compte du passage de trajectoires sur une grille.
#
# Parametres :
#   <trajs>  : Fichiers trajectoires
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

#----- Remove previous results
file delete $env(CI_SPI_OUT)/TRAJ_Rasterize.fstd DataOut/TRAJ_Rasterize.shp

#----- We have to create an OGR object. this is done by transforming to shapefile
ogrfile open FILE write $env(CI_SPI_OUT)/TRAJ_Rasterize.shp "ESRI Shapefile"
ogrlayer create FILE LAYER "Traj"

#----- Define the HEIGHT field for trajectory start level
ogrlayer define LAYER -field HEIGHT Real

ogrgeometry create LINE "Line String"

#----- Loop on the files
set nb 0
foreach file $argv {

   set trajs [trajectory load $file]
   Log::Print "Processing file $file"

   #----- Loop on the trajectories
   foreach traj $trajs {

      ogrgeometry define LINE -points { }

      #----- Loop on the parcels
      foreach parcel [trajectory define $traj -PARCELS] {

         #----- Add the parcel to the linestring
         ogrgeometry define LINE -addpoint [lindex $parcel 2] [lindex $parcel 1]
      }

      #----- Add a feature to the layer
      ogrlayer define LAYER -nb [incr nb]

      #----- Set it's height field to the trajectory start height
      ogrlayer define LAYER -feature  [expr $nb-1] HEIGHT [trajectory define $traj -LEVEL]

      #----- Add the linestring to the layer
      ogrlayer define LAYER -geometry [expr $nb-1] False LINE
   }
}

#----- Close and save
ogrfile close FILE

#----- Reload the shapefile
set layer [ogrfile open FILE append $env(CI_SPI_OUT)/TRAJ_Rasterize.shp]
eval ogrlayer read LAYER [lindex $layer 0]

#----- Open input grid and output file
fstdfile open GRIDFILE   read  $env(CI_SPI_IN)/2005120600_012
fstdfile open RESULTFILE write $env(CI_SPI_OUT)/TRAJ_Rasterize.fstd

#-- Copie des tictac
foreach tictac { >> ^^ } {
   fstdfield read GRID GRIDFILE -1 "" -1 -1 -1 "" "$tictac"
   fstdfield write GRID RESULTFILE 0 True
}

#-- Read input grid
fstdfield read GRID GRIDFILE -1 "" -1 -1 -1 "" AC
fstdfield stats GRID -nodata 0.0

#----- Copy for sum field
fstdfield copy SUM GRID
fstdfield clear SUM 0.0

#----- Loop on the features (trajectories)
for { set f 0 } { $f<[ogrlayer define LAYER -nb] } { incr f } {

   #----- Make sure we only rasterize one feature at a time
   ogrlayer define LAYER -featureselect [list [list - # $f]]

   #----- Rasterize to grid with value 1
   fstdfield clear GRID 0.0
   fstdfield gridinterp GRID LAYER INTERSECT 1

   #----- sum
   vexpr SUM SUM+GRID
}

#----- Save results
fstdfield define SUM -NOMVAR TRAJ -IP1 1200
fstdfield write SUM RESULTFILE -32 True

fstdfile close GRIDFILE RESULTFILE

Log::End