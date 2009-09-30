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
# Fichier    : FSTD_Leak.tcl
# Creation   : Mars 2009 - J.P. Gauthier - CMC/CMOE
# Description: Tester les Memory Leak
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

fstdfile open FILE read [lindex $argv 0]

for { set n 0 } { $n<1000 } { incr n } {
   puts $n
   foreach field [fstdfield find FILE -1 "" -1 -1 -1 "" ""] {
      fstdfield head FILE $field
      fstdfield read FIELD FILE $field
      fstdfield stats FIELD -coordvalue 50 -90
   }
}

fstdfile close FILE
