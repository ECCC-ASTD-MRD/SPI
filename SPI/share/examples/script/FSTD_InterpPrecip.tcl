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
# Fichier    : FSTD_InterpPrecip.tcl
# Creation   : Novembre 2017 - J.P. Gauthier - CMC/CMOE
# Description: Interpoler des champs de precip dans l'horizontale
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

catch { file delete $env(CI_SPI_OUT)/FSTD_InterpPrecip.fstd }

fstdfile open YY  read $env(CI_SPI_IN)/YY.fstd
fstdfile open LL  read $env(CI_SPI_IN)/LL.fstd
fstdfile open OUT write $env(CI_SPI_OUT)/FSTD_InterpPrecip.fstd

fstdfield read TO LL -1 "" -1 -1 -1 "" "P0"
fstdfield stats TO -nodata 0.0

set f [open $env(CI_SPI_OUT)/FSTD_InterpPrecip.idx w]
#set f [open $env(CI_SPI_OUT)/FSTD_InterpPrecip.idx r]
#fconfigure $f -encoding binary -translation binary
#set index [read $f]
#binary scan $index f* data
#puts [llength $data]

foreach fld [fstdfield find YY -1 "" -1 -1 -1 "" "PR"] {
   puts "   Processing: $fld"
   fstdfield read IN YY $fld
   fstdfield copydesc TO OUT
   
   fstdfield clear TO
   fstdfield gridinterp TO IN LINEAR
   fstdfield define TO -IP1 [fstdfield define IN -IP1] -ETIKET LINEAR
   fstdfield write TO OUT -32 False

   fstdfield clear TO
   fstdfield define IN -grid 1
   fstdfield gridinterp TO IN CONSERVATIVE 1 index
   fstdfield define TO -IP1 [fstdfield define IN -IP1] -ETIKET CONSER1
   fstdfield write TO OUT -32 False
   
   fstdfield clear TO
   fstdfield define IN -grid 2
   fstdfield gridinterp TO IN CONSERVATIVE 1 index
   fstdfield define TO -IP1 [fstdfield define IN -IP1] -ETIKET CONSER2
   fstdfield write TO OUT -32 False
}

fstdfile close YY LL OUT

puts $f $index
close $f

Log::End