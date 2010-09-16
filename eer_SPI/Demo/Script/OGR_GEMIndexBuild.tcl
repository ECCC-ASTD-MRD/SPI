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
# Fichier    : OGR_GEMIndexBuild.tcl
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

puts \n[file tail [info script]]

set ops $env(CMCGRIDF)/prog
set par $env(CMCGRIDF)/../par/dbase/prog

set names  { "Regional" "Lam Arctic" "Lam Est" "Lam Maritimes" "Lam Ouest" }
set models [list $par/reghyb $ops/lam/arctic.eta $ops/lam/east.eta $ops/lam/maritimes.eta $ops/lam/west.eta]

#----- Open GEM index file
eval file delete [glob DataOut/GEM.*]
ogrfile open INDEXFILE write DataOut/GEM.shp "ESRI Shapefile"
ogrlayer create INDEXFILE INDEX "Coverage"

ogrlayer define INDEX -field NAME String
ogrlayer define INDEX -nb [llength $models]

#----- Initialiser la geometrie
ogrgeometry create POLY "Polygon"
ogrgeometry create RING "Linear Ring"

#----- Loop on the models
set no 0
foreach model $models name $names {

   puts "Processing $model"

   #----- Pick the last file
   set file [lindex [lsort -dictionary -increasing [glob $model/??????????_???]] end]

   if { ![llength $file] } {
      puts "No data is available of model $model"
      exit;
   }

   fstdfile open GEMFILE read $file

   #----- Read P0 since it's always available
   fstdfield read P0 GEMFILE -1 "" -1 -1 -1 "" "P0"

   #----- Get the limits
   set ni [fstdfield define P0 -NI]
   set nj [fstdfield define P0 -NJ]

   ogrgeometry define RING -points {}

   #----- Bottom
   set j 0
   for { set i 0 } { $i<$ni } { incr i } {
      set ll [fstdfield stats P0 -gridpoint $i $j]
      ogrgeometry define RING -addpoint [lindex $ll 1] [lindex $ll 0]
   }

   #----- Right
   set i [expr $ni-1]
   for { set j 0 } { $j<$nj } { incr j } {
      set ll [fstdfield stats P0 -gridpoint $i $j]
      ogrgeometry define RING -addpoint [lindex $ll 1] [lindex $ll 0]
   }

   #----- Top
   set j [expr $nj-1]
   for { set i [expr $ni -1] } { $i>=0 } { incr i -1 } {
      set ll [fstdfield stats P0 -gridpoint $i $j]
      ogrgeometry define RING -addpoint [lindex $ll 1] [lindex $ll 0]
   }

   #----- Left
   set i 0
   for { set j [expr $nj-1] } { $j>=0 } { incr j -1 } {
      set ll [fstdfield stats P0 -gridpoint $i $j]
      ogrgeometry define RING -addpoint [lindex $ll 1] [lindex $ll 0]
   }

   #----- Close the polygon
   set ll [fstdfield stats P0 -gridpoint 0 0]
   ogrgeometry define RING -addpoint [lindex $ll 1] [lindex $ll 0]
   ogrgeometry define POLY -geometry False RING

   ogrlayer define INDEX -feature $no NAME $name
   ogrlayer define INDEX -geometry $no False POLY

   incr no
   fstdfile close GEMFILE
}

ogrfile close INDEXFILE