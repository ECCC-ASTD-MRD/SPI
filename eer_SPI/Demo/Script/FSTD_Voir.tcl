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
# Fichier    : FSTD_Voir.tcl
# Creation   : Mai 2000 - J.P. Gauthier - CMC/CMOE
# Description: Afficher le contenu d'un fichier standard a la manierer de voir (RPN)
#
# Parametres :
#   <File>   : Fichier standard
#
# Retour:
#
# Remarques  :
#
#============================================================================

package require TclData

puts \n[file tail [info script]]

set file [lindex $argv 0]

puts "   NOMV\tTV\tETIQUETTE\tNI\tNJ\tNK\tIP1\tIP2\tIP3"

foreach field [fstdfile open 1 read $file] {
   puts "   [lindex $field 2]\t[lindex $field 3]\t[lindex $field 7]    \t[lindex $field 11]\t[lindex $field 12]\t[lindex $field 13]\t[lindex $field 4]\t[lindex $field 5]\t[lindex $field 6]"
}

fstdfile close 1
