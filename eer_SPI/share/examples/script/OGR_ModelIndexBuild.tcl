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

#                 0             1                    2                     3                   4                 5             6                   7                      8
set names  {     "RDPS"        "HRDPS National"     "HRDPS Caps"          "CAPSOCE"           "GLSOCE"          "GSL"         "RIOPS"             "SHOP"                 "WEBTIDE"}
set vars   {      P0            P0                   P0                    GL                  GL                GL            GL                  TM                     M2      }
#                 0             1                    2                     3                   4                 5             6                   7                        8       9       10    11     12  13
set subs   {      { "" }        { "" }               { "" }                { "" }              { "" }            { "" }        { "" }              { "" }                 { arctic9 ne_pac4 nwatl sshelf h3o stle400 } }
set models [list  $prog/reghyb  $prog/lam/nat.model  $prog/lam/caps.model  $prog/lam/caps.oce  $prog_par/glsoce  $prog/gsloce  $prog/riops.native  $anal/shop/stlawrence  $env(WEBTIDE_DATA)]

set nb 0
foreach s $subs { incr nb [llength $s] }

#----- Open GEM index file
eval file delete [glob -nocomplain DataOut/ModelDomain*]
ogrfile open INDEXFILE write DataOut/ModelDomain.shp "ESRI Shapefile"
ogrlayer create INDEXFILE INDEX "Domain"

ogrlayer define INDEX -field NAME String
ogrlayer define INDEX -field ID   Integer
ogrlayer define INDEX -nb $nb

#----- Initialiser la geometrie
ogrgeometry create TIN   "Multi Polygon"
ogrgeometry create TRI   "Polygon"
ogrgeometry create POLY  "Polygon"
ogrgeometry create RING  "Linear Ring"
ogrgeometry create MPOINT "Multi Point"
ogrgeometry create POINT "Point"

#----- Loop on the models
set no 0
set mdl 0

foreach model $models sub $subs var $vars name $names {

   foreach s $sub {
   
      Log::Print INFO "Processing $model $s"
            
      #----- Pick the last file
      if { $name=="WEBTIDE"} {
         set file [glob -nocomplain $model/$s/*.fstd]
      } else {
         set file [lindex [lsort -dictionary -increasing [glob -nocomplain $model/$s/\[1-2\]?????????_000*]] end]
      }
      if { ![llength $file] } {
         Log::Print WARNING "No data is available of model $model/$s"
         continue
      }

      fstdfile open RPNFILE read $file

      #----- Read P0 since it's always available
      if { [catch { fstdfield read VAR RPNFILE -1 "" -1 -1 -1 "" $var} ] } {
         fstdfile close RPNFILE
         continue
      }

      #----- Get the limits
      set ni  [fstdfield define VAR -NI]
      set nj  [fstdfield define VAR -NJ]
      set gr  [fstdfield define VAR -GRTYP]
   
      ogrgeometry define RING -points {}

      switch $gr {
         "Y" {
            fstdfield read LAT RPNFILE -1 "" -1 -1 -1 "" ^^
            fstdfield read LON RPNFILE -1 "" -1 -1 -1 "" >>
            
            ogrgeometry define MPOINT -geometry False {}
            for { set i 0 } { $i<$ni } { incr i } {
               set lat [fstdfield stats LAT -gridvalue $i 0]
               
               if { [fstdfield define LON -NJ]==1 } {
                  set lon [fstdfield stats LON -gridvalue $i 0]
               } else {
                  set lon [fstdfield stats LON -gridvalue 0 $i]
               }
               
               ogrgeometry define POINT -points [list $lon $lat]
               ogrgeometry define MPOINT -addgeometry False POINT
            }
            set geom [ogrgeometry stats MPOINT -convexhull]
         }
         "M" {
            fstdfield read IDX RPNFILE -1 "" -1 -1 -1 "" ##
            fstdfield read LAT RPNFILE -1 "" -1 -1 -1 "" ^^
            fstdfield read LON RPNFILE -1 "" -1 -1 -1 "" >>
            
            ogrgeometry define TIN    -geometry False {}
            for { set i 0 } { $i<[fstdfield define IDX -NI] } { incr i 3 } {
               set i0 [fstdfield stats IDX -gridvalue $i          0]
               set i1 [fstdfield stats IDX -gridvalue [expr $i+1] 0]
               set i2 [fstdfield stats IDX -gridvalue [expr $i+2] 0]

               set lo0 [fstdfield stats LON -gridvalue $i0 0]; set lo0 [expr $lo0>90?$lo0-360:$lo0]
               set la0 [fstdfield stats LAT -gridvalue $i0 0]
               set lo1 [fstdfield stats LON -gridvalue $i1 0]; set lo1 [expr $lo1>90?$lo1-360:$lo1]
               set la1 [fstdfield stats LAT -gridvalue $i1 0]
               set lo2 [fstdfield stats LON -gridvalue $i2 0]; set lo2 [expr $lo2>90?$lo2-360:$lo2]
               set la2 [fstdfield stats LAT -gridvalue $i2 0]
               
               ogrgeometry define RING -points [list $lo0 $la0 $lo1 $la1 $lo2 $la2 $lo0 $la0]
               ogrgeometry define TRI -geometry False RING
               ogrgeometry define TIN -addgeometry False TRI
            }
            set geom [ogrgeometry stats TIN -dissolve]
#            set geom [ogrgeometry stats TIN -convexhull]
         }
         default {
            #----- Bottom
            set j 0
            for { set i 0 } { $i<$ni } { incr i } {
               set ll [fstdfield stats VAR -gridpoint $i $j]
               ogrgeometry define RING -addpoint [lindex $ll 1] [lindex $ll 0]
            }

            #----- Right
            set i [expr $ni-1]
            for { set j 0 } { $j<$nj } { incr j } {
               set ll [fstdfield stats VAR -gridpoint $i $j]
               ogrgeometry define RING -addpoint [lindex $ll 1] [lindex $ll 0]
            }

            #----- Top
            set j [expr $nj-1]
            for { set i [expr $ni -1] } { $i>=0 } { incr i -1 } {
               set ll [fstdfield stats VAR -gridpoint $i $j]
               ogrgeometry define RING -addpoint [lindex $ll 1] [lindex $ll 0]
            }

            #----- Left
            set i 0
            for { set j [expr $nj-1] } { $j>=0 } { incr j -1 } {
               set ll [fstdfield stats VAR -gridpoint $i $j]
               ogrgeometry define RING -addpoint [lindex $ll 1] [lindex $ll 0]
            }

            #----- Close the polygon
            set ll [fstdfield stats VAR -gridpoint 0 0]
            ogrgeometry define RING -addpoint [lindex $ll 1] [lindex $ll 0]
            ogrgeometry define POLY -geometry False RING
            set geom POLY
         }
      }

      fstdfile close RPNFILE
      
      if { $s!="" } {
         ogrlayer define INDEX -feature $no NAME "$name ($s)"
      } else {
         ogrlayer define INDEX -feature $no NAME $name
      }
      ogrlayer define INDEX -feature $no ID $mdl
      ogrlayer define INDEX -geometry $no False $geom
      incr no
   }
   incr mdl
}

ogrfile close INDEXFILE

Log::End
