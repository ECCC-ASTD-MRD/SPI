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
# Fichier    : FSTD_InterpYY.tcl
# Creation   : Decembre 2014 - J.P. Gauthier - CMC/CMOE
# Description: Interpoler des champs dans des grilles U (Yin Yang).
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

catch { file delete DataOut/FSTD_InterpYY.fstd }

fstdfile open 1 read DataIn/AreaEms.YinYang.fst
fstdfile open 2 read DataIn/AreaEms.LatLon.fst
fstdfile open 3 write DataOut/FSTD_InterpYY.fstd

fstdfield read TIC 1 -1 "" -1 -1 -1 "" ^>
fstdfield write TIC 3 0 True

fstdfield read TO 1 -1 "" -1 -1 -1 "" "EHG0"
fstdfield stats TO -nodata 0.0

fstdfield read FROM 2 -1 "" -1 -1 -1 "" "EHG0"
#----- Test linear interpolation (ezscint knows how to work with U grids)
fstdfield clear TO
fstdfield gridinterp TO FROM LINEAR 1
fstdfield define TO -ETIKET LINEAR
fstdfield write TO 3 -32 False

#----- Test conservative (process all sub grids together)
fstdfield clear TO

#----- Process YINYANG together
fstdfield gridinterp TO FROM CONSERVATIVE 1 
fstdfield define TO -ETIKET YIYACONS
fstdfield write TO 3 -32 False

#----- Test conservative (process sub grids individually)
fstdfield clear TO

#----- Process YIN
fstdfield define TO -grid 1
fstdfield gridinterp TO FROM CONSERVATIVE 1 
fstdfield define TO -ETIKET YICONS
fstdfield write TO 3 -32 False

#----- Process YANG
fstdfield clear TO

fstdfield define TO -grid 2
fstdfield gridinterp TO FROM CONSERVATIVE 1 
fstdfield define TO -ETIKET YACONS

#----- Write both
fstdfield write TO 3 -32 False

fstdfile close 1 2 3

Log::End