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
# Fichier    : GDAL_HWSDConvert.tcl
# Creation   : Mai 2000 - J.P. Gauthier - CMC/CMOE
# Description: Convert HWSD soil index into their respective values
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

set file  $env(GEO_PATH)/HWSD/hwsd.bil
set types { tgrav tsand tsilt tclay tref toc sgrav ssand ssilt sclay sref soc }
set idxs  { 2 3 4 5 6 7 8 9 10 11 12 13 }
set wtile 1800
set htile 900

#----- Create lookup table (LUT) for the various types
vector create VECTOR
vector dim VECTOR { mu tgrav tsand tsilt tclay tref toc sgrav ssand ssilt sclay sref soc }

#----- Set max memory right now, it'll speed up the allocation
vector mem VECTOR 20000

puts "   Reading correspondance table"
set f [open $env(GEO_PATH)/HWSD/hwsd.csv]

gets $f line
while { ![eof $f] } {
   set line [split [gets $f] ,]

   #----- Skip empty lines
   if { [lindex $line 0]!="" } {
      lappend params([lindex $line 0]) [lrange $line 1 end]
   }
}

#----- Add item for index 0 (to cover nodata value 0)
lappend params(0) { 0 0 0 0 0 0 0 0 0 0 0 0 0 0 }

Log::Print INFO "Building correspondance vector"
foreach name [lsort -integer -increasing [array names params]] {

   #----- Reset counts
   foreach type $types {
      set $type 0
   }

   #----- There might be multiple types so we have to calculate the % per types
   #      ex: SAND = [SHARE(1)*SAND(1) + SHARE(2)*SAND(2) + SHARE(3)*SAND(3) ...]/100.
   foreach param $params($name) {
      set per   [lindex $param 0]
      foreach type $types idx $idxs {
         if { [set p [lindex $param $idx]]!="" } {
            eval set $type \[expr \$$type+$per*$p\]
         }
      }
   }
   foreach type $types {
      eval set $type \[expr \$$type/100.0\]
   }
   vector append VECTOR [list $name $tgrav $tsand $tsilt $tclay $tref $toc $sgrav $ssand $ssilt $sclay $sref $soc]
}

#----- Open raster file
set bandidxs [gdalfile open HWSDFILE read $file]

set h [gdalfile height HWSDFILE]
set w [gdalfile width HWSDFILE]
Log::Print INFO "Raster size: ${w}x${h}, tile will be ${wtile}x${htile}"

#----- Loop over the data by tiles since it's too big to fit in memory
for { set y 0 } { $y<$h } { incr y $htile } {
   for { set x 0 } { $x<$w } { incr x $wtile } {
      Log::Print INFO "Processing : ${x},${y}"

      #----- Read tile and extract georeference
      gdalband read HWSDTILE $bandidxs $x $y [expr $x+$wtile-1] [expr $y+$htile-1]
      set ll   [gdalband stats HWSDTILE -gridpoint $x $y]
      set tr   [gdalband define HWSDTILE -transform]
      set tr   [list [lindex $ll 1] [lindex $tr 1] [lindex $tr 2] [lindex $ll 0] [lindex $tr 4] [lindex $tr 5]]
      set zero 0

      #----- Loop on soil types
      foreach band $types {
         Log::Print INFO "Processing $band"
         flush stdout

         #----- Convert to float for LUT to give percentages with decimals
         vexpr (Float32)$band slut(HWSDTILE,VECTOR.mu,VECTOR.$band)
         gdalband define $band -georef [gdalfile georef HWSDFILE] -transform $tr
         gdalband configure $band -desc $band
         if { [lindex [lindex [gdalband stats $band -max] 0] 0]==0.0 } {
            incr zero
         }
      }

      #----- Write all types to the file if there is a need to (not water only)
      if { $zero==[llength $types] } {
         Log::Print WARNING "Empty tile, not saving."
      } else {
         Log::Print INFO "Saving tile."
         gdalfile open OUTFILE write $env(CI_SPI_OUT)/HWSD-$x-$y.tif "GeoTIFF"
         gdalband write $types OUTFILE
         gdalfile close OUTFILE
      }
   }
}

gdalfile close HWSDFILE

Log::End