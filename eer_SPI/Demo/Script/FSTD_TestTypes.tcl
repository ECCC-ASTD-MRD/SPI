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
#
#============================================================================

package require TclData

puts \n[file tail [info script]]

catch { file delete DataOut/FSTD_TestTypes.fstd }

fstdfile open 1 write DataOut/FSTD_TestTypes.fstd

foreach type { UByte Byte UInt16 Int16 UInt32 Int32 Float32 Float64 } val { 2 3 4 5 6 7 10 11 } {

   puts "   Creating field of type $type"

   fstdfield create GRID 229 229 1 $type
   fstdfield define GRID -GRTYP N 170 356 21000.0 8.0
   fstdfield define GRID -DEET 0 -NPAS 0 -IP1 0 -IP2 0 -IP3 0 -ETIKET "$type" -NOMVAR GRID -TYPVAR X

   fstdfield stats GRID -nodata $val
   fstdfield clear GRID

   fstdfield write GRID 1 0 False
}

fstdfile close 1