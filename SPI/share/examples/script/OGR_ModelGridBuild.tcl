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

set multi    0
set ops /space/hall1/sitestore/eccc/cmod/prod/hubs/gridpt/dbase
set par /space/hall1/sitestore/eccc/cmod/prod/hubs/gridpt/par/dbase

set anal     $ops/anal
set prog     $ops/prog
set prog_par $par/prog

set names  {      "GDPS"        "RDPS"        "GEPS"            "REPS"             "HRDPS_National"    "HRDPS_Caps"          "RAQDPS"   "CAPSOCE"          "GLSOCE"         "GSL"        "RIOPS"             "SHOP"                 "GDWPS"         RDWPS_ERI       RDWPS_HUM       RDWPS_ONT       RDWPS_SUP        REWPS }
set vars   {      P0            P0            P0                P0                 P0                  P0                    P0         GL                 GL               GL            GL                  TM                    GL              GL              GL              GL              GL               GL    }
set models [list  $prog/glbhyb $prog/reghyb  $prog/ens.glbmodel $prog/ens.regmodel $prog/lam/nat.model $prog/lam/caps.model  $prog/mach $prog/lam/caps.oce $prog_par/glsoce $prog/gsloce  $prog/riops.native  $anal/shop/stlawrence $prog/gdwps/glb $prog/rdwps/eri $prog/rdwps/hum $prog/rdwps/ont $prog/rdwps/sup $prog/rewps]
set names  {      "CAPSOCE"          "GLSOCE"         "GSL"        "RIOPS"             "SHOP"                 "GDWPS"         RDWPS_ERI       RDWPS_HUM       RDWPS_ONT       RDWPS_SUP        REWPS }
set vars   {      GL                 GL               GL            GL                  TM                    GL              GL              GL              GL              GL               GL    }
set models [list  $prog/lam/caps.oce $prog_par/glsoce $prog/gsloce  $prog/riops.native  $anal/shop/stlawrence $prog/gdwps/glb $prog/rdwps/eri $prog/rdwps/hum $prog/rdwps/ont $prog/rdwps/sup $prog/rewps]

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
   set ns [llength [fstdfield define VAR -GRIDID]]
   set ns [expr $ns==0?1:$ns]
   set ni [fstdfield define VAR -NI]
   set nj [fstdfield define VAR -NJ]
   
   #----- Open GEM index file
   eval file delete [glob -nocomplain /home/nil000/Projects/eerSPI/eer_SPI/share/examples/script/DataOut/ModelGrid_$name*]
   ogrfile open INDEXFILE write /home/nil000/Projects/eerSPI/eer_SPI/share/examples/script/DataOut/ModelGrid_$name.shp "ESRI Shapefile"
   ogrlayer create INDEXFILE INDEX $model
   
   if { $multi } {
      ogrlayer define INDEX -field NAME String
      ogrlayer define INDEX -nb $ns
   } else {
      ogrlayer define INDEX -field I Integer
      ogrlayer define INDEX -field J Integer   
      ogrlayer define INDEX -nb [expr $ni*$nj]
   }
   
   #----- Loop on sub grids
   set n 0
   for { set s 0 } { $s < $ns } { incr s } {
      fstdfield define VAR -grid [expr $s+1]
      ogrgeometry free MPOINT
      ogrgeometry create MPOINT "Multi Point"
      
      foreach { lat lon } [fstdfield stats VAR -grid] {
         ogrgeometry define POINT -points [list $lon $lat]
         if { $multi } {
            ogrgeometry define MPOINT -addgeometry False POINT
         } else {
            ogrlayer define INDEX -feature $n I [expr $n%$ni]
            ogrlayer define INDEX -feature $n J [expr $n/$ni]
            ogrlayer define INDEX -geometry $n False POINT
         }
         incr n
      }
   
      if { $multi } {
         if { $s!=0 } {
            ogrlayer define INDEX -feature $s NAME "$name ($s)"
         } else {
            ogrlayer define INDEX -feature $s NAME $name
         }
         ogrlayer define INDEX -geometry $s False MPOINT
      }
   }
   
   fstdfile close RPNFILE
   ogrfile close INDEXFILE
   ogrlayer free INDEX
}

cd DataOut
exec zip ./ModelGrid[clock format [clock seconds] -format "%Y%m%d" -timezone :UTC].zip ModelGrid_*
cd ..

Log::End
