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
# Fichier    : FSTD_ETA2MASL_MaskTOPO.tcl
# Creation   : Mars 2010 - Stephane Gaudreault, J.P. Gauthier - CMC/CMOE
# Description: Interpoler des champs dans la verticale de ETA a MASL et masquer les montagnes
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

set levels { 10 15.0 30.0 35.0 44.0 51.0 57.2 59.0 67.6 72.4 81.0 85.6 94.4 99.0 107.6 112.4 121.0 125.6 134.4 139.0 147.6 152.4 161.0 165.6 174.4 179.0 187.6 192.4 201.0 205.6 214.4 219.0 227.6 232.4 241.0 245.6 254.4 259.0 267.6 272.4 281.0 285.6 294.6 300.2 310.4 318.8 332.2 345.8 365.6 387.0 417.0 450.8 495.0 546.2 610.8 685.4 776.8 882.6 1000}

catch { file delete $env(CI_SPI_OUT)/FSTD_ETA2MASL_MaskTOPO.fstd }

#----- Open RPN in/out files
fstdfile open IN read $env(CI_SPI_IN)/m2009030112_009
fstdfile open OUT write $env(CI_SPI_OUT)/FSTD_ETA2MASL_MaskTOPO.fstd

#----- Copy grid descriptors
fstdfield read TIC IN -1 "" -1 -1 -1 "" >>
fstdfield read TAC IN -1 "" -1 -1 -1 "" ^^
fstdfield write TIC OUT 0 True
fstdfield write TAC OUT 0 True

#----- Read GZ to use as vertical reference
fstdfield read GZFROM IN -1 "" -1 -1 -1  "" GZ
fstdfield readcube GZFROM

#----- Create output grid in vertical MASL
fstdfield create TO [fstdfield define GZFROM -NI] [fstdfield define GZFROM -NJ] [llength $levels]
fstdfield stats TO -leveltype MASL -levels $levels -nodata 0
fstdfield clear TO
fstdfield configure TO -interpdegree LINEAR

#----- Loop on fields to interpolate
foreach var { HU TH } {

   Log::Print INFO "Interpolatin $var"
   fstdfield read FROM IN -1 "" -1 -1 -1  "" $var
   fstdfield readcube FROM
   fstdfield verticalinterp TO FROM "" GZFROM

   Log::Print INFO "Masking topo for $var"
   fstdfield stats GZFROM -levelindex 0

   #----- Loop on first level of GZ and reset values at levels under it
   for { set i 0 } { $i<[fstdfield define GZFROM -NI] } { incr i } {
      for { set j 0 } { $j<[fstdfield define GZFROM -NJ] } { incr j } {
         set gz [expr [fstdfield stats GZFROM -gridvalue $i $j]*10.0]

         #----- Loop on data levels lower than GZ
         set nolevel 0
         foreach level $levels {
            if { $level>$gz } {
               break;
            }
            fstdfield stats TO -levelindex $nolevel
            fstdfield stats TO -gridvalue $i $j 0
            incr nolevel
         }
      }
   }

   fstdfield write TO OUT 0 True
}

fstdfile close IN OUT

Log::End