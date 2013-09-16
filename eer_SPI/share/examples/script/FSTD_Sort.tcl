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
# Fichier    : FSTD_Sort.tcl
# Creation   : Mars 2005 - J.P. Gauthier - CMC/CMOE
# Description: On trie une liste de champs selon chaque point de grille afin
#              par exemple d'obtenir un rang centile
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

fstdfile open 1 read  DataIn/2005120600_012

#----- Recuperer le champs pour la grille d'interpolation
set fldlist {}

puts  -nonewline "Before sort:"

foreach fld [fstdfield find 1 -1 "" -1 -1 -1 "" "O3"] {
   fstdfield read FLD$fld 1 $fld
   lappend fldlist FLD$fld
   puts  -nonewline " [fstdfield stats FLD$fld -gridvalue 0 0]"
}

fstdfield sort $fldlist

puts -nonewline "\n\nAfter sort :"
foreach field $fldlist {
   puts -nonewline " [fstdfield stats $field -gridvalue 0 0]"
}

Log::End