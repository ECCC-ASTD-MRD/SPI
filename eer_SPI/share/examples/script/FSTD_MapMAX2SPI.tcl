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
# Fichier    : FSTD_MapMAX2SPI.tcl
# Creation   : Mai 2000 - J.P. Gauthier - CMC/CMOE
# Description: Extraire les palettes du format FSTD
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

#----- Ouvrir les fichiers d'entree (1) sortie (2)
# DataIn/palettes.fst
 
fstdfile open FILE read [lindex $argv 0]

foreach field [fstdfield find FILE -1 "" -1 -1 -1 "" ""] {

   fstdfield read MAP FILE $field

   Log::Print INFO "Extracting REC_[fstdfield define MAP -ETIKET].rgba"

   set f [open DataOut/REC_[fstdfield define MAP -ETIKET].rgba w]
   puts $f "0 100 100 100 100 100 LINEAR 1 0"

   for { set j 0 } { $j < 256 } { incr j } {
      puts -nonewline $f [format "%03i " $j]
      for { set i 0 } { $i < 3 } { incr i } {
         puts -nonewline $f [format "%03i " [expr int([fstdfield stats MAP -gridvalue $i $j])]]
      }
      puts $f "255"
   }
   close $f
}

fstdfile close FILE

Log::End