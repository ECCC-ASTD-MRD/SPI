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
# Fichier    : OGR_Interp.tcl
# Creation   : Fevrier 2008 - J.P. Gauthier - CMC/CMOE
# Description: Démonstration de base des fonctions RADAR
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
package require Grid
package require Logger

Log::Start [info script] 0.1

#----- Site parameters
set lat  49.0161
set lon  -122.4877
set elev 230.0
set dt   3.0

file delete -force $env(CI_DATA_OUT)/RADAR_EchoTop.fstd

fstdfile open FILE write $env(CI_DATA_OUT)/RADAR_EchoTop.fstd

#----- Initialize RPN grid
set grid [Grid::CreateL [expr $lat-$dt] [expr $lon-$dt] [expr $lat+$dt] [expr $lon+$dt] 0.01 0.01]
Grid::Write FILE $grid 1 2 3 False

#----- Loop on the radar files
foreach file [glob $env(CI_DATA_IN)/*IRIS*] {
   puts "   Processing file $file"
   set scans [radarfile open RADARSITE read $file]
   set out   [open DataOut/[file tail $file] w]

   #----- Read the first volume scan
   radarscan read SCAN1 RADARSITE 0

   #----- Set radar location since sometimes (most of it) it is not in the file itself
   radarscan define SCAN1 -LOCATION $lat $lon $elev
   set date [radarscan define SCAN1 -DATE]

   #----- Copy first sweep and clear to 0
   vexpr TOP SCAN1()()(0)
   radarscan stats TOP -nodata 0.0
   radarscan clear TOP

   #----- Get top sweep with 7Dbz minimum echo
   set k 0
   set echo 7
   foreach sweep [radarscan define SCAN1 -SWEEPANGLE] {
      puts "      Checking sweep level $sweep"
      vexpr TOP ifelse(SCAN1()()($k)>$echo,$k,TOP)
      incr k
   }

   #----- Process top to get height in meter
   puts "      Calculating echo top"
   puts $out [format "%10s %10s %10s %10s %10s %10s" Azimuth Bin Sweep Lat Lon Top(m)]
   for { set i 0 } { $i<[radarscan define TOP -NBAZIMUTH] } { incr i } {
      for { set j 0 } { $j<[radarscan define TOP -NBBIN] } { incr j } {
         set k [radarscan stats TOP -gridvalue $i $j]
         if { $k>0 } {
            radarscan stats SCAN1 -levelindex [expr int($k)]
            set h [radarscan stats SCAN1 -height $i $j $k]
            set ll [radarscan stats SCAN1 -gridpoint $i $j]
            radarscan stats TOP -gridvalue $i $j $h
            puts $out [format "%10i %10i %10i %10.6f %10.6f %10.2f" $i $j [expr int($k)] [lindex $ll 0] [lindex $ll 1] $h]
         }
      }
   }

   radarfile close RADARSITE
   close $out

   puts "      Interpolating on RPN field"
   radarscan stats TOP -levelindex 0

   fstdfield gridinterp $grid TOP
   fstdfield define $grid -IP1 12000 -DATEO [fstdstamp fromseconds $date] -NOMVAR TOP
   fstdfield write $grid FILE -32 False
}

fstdfile close FILE

Log::End