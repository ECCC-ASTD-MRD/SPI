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

proc GridDefineLL { Lat0 Lon0 Lat1 Lon1 DLat DLon { ETIKET GRID }} {

   set ni [expr int(ceil(($Lon1-$Lon0)/$DLon))]
   set nj [expr int(ceil(($Lat1-$Lat0)/$DLat))]

   fstdfield create GRIDLLTIC $ni 1 1
   fstdfield create GRIDLLTAC 1 $nj 1

   fstdfield define GRIDLLTIC -GRTYP L 0 0 1.0 1.0
   fstdfield define GRIDLLTAC -GRTYP L 0 0 1.0 1.0

   fstdfield define GRIDLLTIC -DEET 0 -NPAS 0 -IP1 0 -IP2 0 -IP3 0 -ETIKET $ETIKET -DATYP 2 -NOMVAR ">>" -TYPVAR X
   fstdfield define GRIDLLTAC -DEET 0 -NPAS 0 -IP1 0 -IP2 0 -IP3 0 -ETIKET $ETIKET -DATYP 2 -NOMVAR "^^" -TYPVAR X

   #----- Compute tictic grid coordinates.

   set lon $Lon0
   for { set i 0 } { $i < $ni } { incr i } {
      fstdfield stats GRIDLLTIC -gridvalue $i 0 $lon
      set lon [expr $lon+$DLon]
   }

   #----- Compute tactac grid coordinates.

   set lat $Lat0
   for { set j 0 } { $j < $nj } { incr j } {
      fstdfield stats GRIDLLTAC -gridvalue 0 $j $lat
      set lat [expr $lat+$DLat]
   }

   fstdfield create GRIDLLMEM $ni $nj 1
   fstdfield define GRIDLLMEM -ETIKET $ETIKET -NOMVAR "GRID" -TYPVAR X -IG1 0 -IG2 0 -IG3 0 -IG4 0 -GRTYP Z
   fstdfield define GRIDLLMEM -positional GRIDLLTIC GRIDLLTAC

   return GRIDLLMEM
}

package require TclData

puts \n[file tail [info script]]

#----- Open the radar file

set scans [radarfile open RADARSITE read DataIn/200609032150..CONVOL.URP.WUJ.RADAR.IRIS]
set lat  49.0161
set lon  -122.4877
set elev 230.0
set dt   3.0

#----- Read the first scan
radarscan read SCAN1 RADARSITE 0

#----- Set radar location since sometimes (most of it) it is not in the file itself
radarscan define SCAN1 -LOCATION $lat $lon $elev

#----- Copy first sweep and clear to 0
vexpr TOP SCAN1()()(0)
radarscan stats TOP -nodata 0.0
radarscan clear TOP

#----- Get top
set k 0
set min [radarscan define SCAN1 -NOISE]
set min 7
foreach sweep [radarscan define SCAN1 -SWEEPANGLE] {
   puts "   Checking sweep level $sweep"
   vexpr TOP ifelse(SCAN1()()($k)>$min,$k,TOP)
   incr k
}

puts "   Calculating echo top"
for { set i 0 } { $i<[radarscan define TOP -NBAZIMUTH] } { incr i } {
   puts "   Azimuth $i"
   for { set j 0 } { $j<[radarscan define TOP -NBBIN] } { incr j } {
      set k [radarscan stats TOP -gridvalue $i $j]
      set h [radarscan stats SCAN1 -height $i $j $k]
      if { $k>0 } {
         radarscan stats TOP -gridvalue $i $j $h
      }
   }
}

puts "   Interpolating on RPN field"
GridDefineLL [expr $lat-$dt] [expr $lon-$dt] [expr $lat+$dt] [expr $lon+$dt] 0.01 0.01
radarscan stats TOP -level 1

fstdfield gridinterp GRIDLLMEM TOP
fstdfield define GRIDLLMEM -IP1 12000

fstdfile open FILE write DataOut/RADAR_EchoTop.fstd
fstdfield write GRIDLLTIC FILE -32 True
fstdfield write GRIDLLTAC FILE -32 True
fstdfield write GRIDLLMEM FILE -32 True
fstdfile close FILE
