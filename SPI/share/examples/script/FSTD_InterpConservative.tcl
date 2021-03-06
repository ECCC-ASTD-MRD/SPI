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
# Fichier    : FSTD_InterpConservative.tcl
# Creation   : Mai 2000 - J.P. Gauthier - CMC/CMOE
# Description: Interpoler des champs dans l'horizontale en conservant la masse.
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

catch { file delete $env(CI_DATA_OUT)/FSTD_InterpConservative.fstd }

fstdfile open 1 read $env(CI_DATA_IN)/2005120600_012
fstdfile open 2 read $env(CI_DATA_IN)/2005102612_012
fstdfile open 3 write $env(CI_DATA_OUT)/FSTD_InterpConservative.fstd

fstdfield read TO 2 -1 "" -1 -1 -1 "" "P0"
fstdfield stats TO -nodata 0.0

set f [open $env(CI_DATA_OUT)/FSTD_InterpConservative.idx w]
#set f [open $env(CI_DATA_OUT)/FSTD_InterpConservative.idx r]
#fconfigure $f -encoding binary -translation binary
#set index [read $f]
#binary scan $index f* data
#puts [llength $data]

foreach fld [fstdfield find 1 -1 "" -1 -1 -1 "" "O3"] {
   puts "   Processing: $fld"
   fstdfield read IN 1 $fld
   fstdfield clear TO
   fstdfield define TO -IP1 [fstdfield define IN -IP1]
   fstdfield gridinterp TO IN CONSERVATIVE 1 index
#   fstdfield gridinterp TO IN AVERAGE_VARIANCE TO 1
   fstdfield write TO 3 -32 False
}

puts $f $index
close $f

fstdfile close 1 2 3

Log::End