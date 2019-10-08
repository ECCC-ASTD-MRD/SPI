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
#package require TclGeoEER
package require Logger

Log::Start [info script] 0.1

set file [lindex $argv 0]

puts "   NOMV\tTV\tETIQUETTE\tNI\tNJ\tNK\tDATEO\t\tIP1\tIP2\tIP3\tDEET\tNPAS\tDTY\tG IG1\tIG2\tIG3\tIG4\n"

#head.NOMVAR,head.TYPVAR,head.IP1,head.IP2,head.IP3,head.ETIKET,head.DATEO,head.DATEV,ni,nj,nj,grtyp[0],head.IG1,head.IG2,head.IG3,head.IG4);

foreach field [fstdfile open 1 read $file EXTENDED] {
#   puts $field
   puts "   [lindex $field 2]\t[lindex $field 3]\t[lindex $field 7]    \t[lindex $field 12]\t[lindex $field 13]\t[lindex $field 14]\t[lindex $field 8]\t[lindex $field 4]\t[lindex $field 5]\t[lindex $field 6]\t[lindex $field 9]\t[lindex $field 10]\t[lindex $field 20] [lindex $field 21]\t[lindex $field 15] [lindex $field 16]\t[lindex $field 17]\t[lindex $field 18]\t[lindex $field 19]"
   
   }

fstdfile close 1

Log::End