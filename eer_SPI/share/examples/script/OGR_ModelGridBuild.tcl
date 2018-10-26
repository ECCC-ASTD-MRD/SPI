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
# Fichier    : OGR_ModelIndexBuild.tcl
# Creation   : Septembre 2010 - J.P. Gauthier - CMC/CMOE
# Description: Creer un index de couverture des diverses grilles GEM
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

set ops /space/hall1/sitestore/eccc/cmod/prod/hubs/gridpt/dbase
set par /space/hall1/sitestore/eccc/cmod/prod/hubs/gridpt/par/dbase

set anal     $ops/anal
set prog     $ops/prog
set prog_par $par/prog

set names  {      "GDPS"        "RDPS"        "GEPS"            "REPS"             "HRDPS_National"    "HRDPS_Caps"          "RAQDPS"   "CAPSOCE"          "GLSOCE"         "GSL"        "RIOPS"             "SHOP"                 "GDWPS"         RDWPS_ERI       RDWPS_HUM       RDWPS_ONT       RDWPS_SUP        REWPS }
set vars   {      P0            P0            P0                P0                 P0                  P0                    P0         GL                 GL               GL            GL                  TM                    GL              GL              GL              GL              GL               GL    }
set models [list  $prog/glbhyb $prog/reghyb  $prog/ens.glbmodel $prog/ens.regmodel $prog/lam/nat.model $prog/lam/caps.model  $prog/mach $prog/lam/caps.oce $prog_par/glsoce $prog/gsloce  $prog/riops.native  $anal/shop/stlawrence $prog/gdwps/glb $prog/rdwps/eri $prog/rdwps/hum $prog/rdwps/ont $prog/rdwps/sup $prog/rewps]

#----- Initialiser la geometrie
ogrgeometry create POINT "Point"

#----- Loop on the models
foreach model $models var $vars name $names {
    
   Log::Print INFO "Processing $model"
         
   #----- Pick the last file
   set file [lindex [lsort -dictionary -increasing [glob -nocomplain $model/\[1-2\]?????????_000*]] end]

   if { ![llength $file] } {
      Log::Print WARNING "No data is available of model $model"
      continue
   }

   fstdfile open RPNFILE read $file

   #----- Read a field on the grid
   if { [catch { fstdfield read VAR RPNFILE -1 "" -1 -1 -1 "" $var} ] } {
      fstdfile close RPNFILE
      continue
   }

   #----- Open GEM index file
   eval file delete [glob -nocomplain DataOut/ModelGrid_$name*]
   ogrfile open INDEXFILE write DataOut/ModelGrid_$name.shp "ESRI Shapefile"
   ogrlayer create INDEXFILE INDEX $model
   
   ogrlayer define INDEX -field NAME String
   ogrlayer define INDEX -nb [llength [fstdfield define VAR -GRIDID]]
   ogrgeometry create MPOINT "Multi Point"
   
   #----- Loop on sub grids
   for { set s 0 } { $s < [llength [fstdfield define VAR -GRIDID]] } { incr s } {
      fstdfield define VAR -grid [expr $s+1]

      foreach { lat lon } [fstdfield stats VAR -grid] {
         ogrgeometry define POINT -points [list $lon $lat]
         ogrgeometry define MPOINT -addgeometry False POINT
      }
   
      fstdfile close RPNFILE
   
      if { $s!=0 } {
         ogrlayer define INDEX -feature $s NAME "$name ($s)"
      } else {
         ogrlayer define INDEX -feature $s NAME $name
      }
      ogrlayer define INDEX -geometry $s False MPOINT
   }
   
   ogrfile close INDEXFILE
   ogrlayer free INDEX
   ogrgeometry free MPOINT
}

cd DataOut
exec zip ./ModelGrid[clock format [clock seconds] -format "%Y%m%d" -timezone :UTC].zip ModelGrid_*
cd ..

Log::End
