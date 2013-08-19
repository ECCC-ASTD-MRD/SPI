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
# Fichier    : FSTD_ETA2HYBRID.tcl
# Creation   : Mai 2000 - J.P. Gauthier - CMC/CMOE
# Description: Interpoler des champs dans la verticale de eta a hybrid
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

puts \n[file tail [info script]]

#----- Arguments
#   FSTD_ETA2HYBRID.tcl [file]

set file [lindex $argv 0]

set vars   { WE GZ HU QC TT ES HR WW UU FN }
set levels { 0.0001 0.0002 0.0004 0.0006 0.0010 0.0015 0.0022 0.0032 0.0043 0.0058 0.0075 0.0095 0.0119 0.0146
             0.0177 0.0210 0.0247 0.0287 0.0330 0.0375 0.0422 0.0472 0.0524 0.0576 0.0630 0.0684 0.0738 0.0791
             0.0843 0.0893 0.0942 0.0988 0.1030 0.1080 0.1130 0.1190 0.1260 0.1340 0.1420 0.1520 0.1630 0.1750
             0.1890 0.2060 0.2230 0.2410 0.2600 0.2790 0.3000 0.3210 0.3440 0.3670 0.3900 0.4150 0.4400 0.4670
             0.4930 0.5210 0.5490 0.5780 0.6070 0.6370 0.6680 0.6990 0.7300 0.7590 0.7870 0.8120 0.8360 0.8590
             0.8790 0.8980 0.9160 0.9320 0.9470 0.9610 0.9740 0.9850 0.9950 1.0000 }

#----- Use new IP1s (Long incomprehensible numbers)
fstdfield ip1mode NEW

#----- Open input and output files
set fields [fstdfile open MESO read $file]
fstdfile open MESOSTRATO write $file.hyb

#----- Copy grid descriptors
fstdfield read TIC MESO -1 "" -1 -1 -1 "" >>
fstdfield read TAC MESO -1 "" -1 -1 -1 "" ^^
fstdfield write TIC MESOSTRATO 0 True
fstdfield write TAC MESOSTRATO 0 True

#----- Read surface pressure, we need it to go from ETA to HYBRID
fstdfield read P0 MESO -1 "" -1 -1 -1 "" "P0"

#----- Create the destination grid
fstdfield create OUT [fstdfield define P0 -NI] [fstdfield define P0 -NJ] [llength $levels]
fstdfield stats OUT -leveltype HYBRID -levels $levels -top 0.1 -ref 800.0 -coef 1.6
fstdfield configure OUT -interpdegree LINEAR

#----- Interpolate3D fields
foreach var $vars {
   puts "   Interpolating $var"
   fstdfield read FLD MESO -1 "" -1 -1 -1 "" "$var"
   fstdfield readcube FLD True
   fstdfield stats FLD -leveltype ETA -top 10.0

   fstdfield verticalinterp OUT FLD P0 P0
   fstdfield write OUT MESOSTRATO 0 True
}

#----- Close the files
fstdfile close MESO MESOSTRATO
