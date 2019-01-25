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
# Fichier    : FSTD_TestTypes.tcl
# Creation   : Mars 2005 - J.P. Gauthier - CMC/CMOE
# Description: Tester les different types de donnees
#
# Parametres :
#
# Retour:
#
# Remarques  :
#   - Int64 and Uint64 don't work in RPN
#   - Float64 is not that well supported in RPN so use at you own risks
#============================================================================

package require TclData
#package require TclGeoEER
package require Logger

Log::Start [info script] 0.1

catch { file delete DataOut/FSTD_TestTypes.fstd }

fstdfile open 1 write DataOut/FSTD_TestTypes.fstd

#foreach type { UByte Byte UInt16 Int16 UInt32 Int32 UInt64 Int64 Float32 Float64 } val { 2 3 4 5 6 7 8 9 10 11 } {
foreach type { UByte Byte UInt16 Int16 UInt32 Int32 UInt64 Int64 Float32 Float64 } val { 2 4 2 4 2 4 2 4 5 5 } {

   Log::Print INFO "Creating field of type $type"

   fstdfield create GRID 229 229 1 $type
#   fstdfield create GRID 229 1 1 $type
   fstdfield define GRID -GRTYP N 170 356 21000.0 8.0
   fstdfield define GRID -DEET 0 -NPAS 0 -IP1 0 -IP2 0 -IP3 0 -ETIKET "$type" -NOMVAR GRID -TYPVAR X

   fstdfield stats GRID -nodata $val
   fstdfield clear GRID
#   fstdfield stats GRID -gridvalue 10 10 -1
#   fstdfield stats GRID -gridvalue 100 100 [expr $val*10]

   fstdfield write GRID 1 0 False
}

fstdfile close 1

Log::End