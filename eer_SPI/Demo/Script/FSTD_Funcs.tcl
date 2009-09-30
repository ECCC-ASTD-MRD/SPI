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
# Fichier    : FSTD_Funcs.tcl
# Creation   : Juillet 2009 - J.P. Gauthier - CMC/CMOE
# Description: Test de fonctions variee
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

puts "Testing stamp functions:"
puts "   seconds: [set secs [clock scan "20090916 00:00" -gmt True]]"
puts "   stamp  : [set stamp [fstdstamp fromseconds $secs]]"
puts "   incr   : [set stamp [fstdstamp incr $stamp 024]]"
puts "   date   : [exec r.date $stamp]"

#----- Ouvrir les fichiers d'entree (1) sortie (2)
puts "Testing file IO functions:"
fstdfile open 1 read DataIn/2005102612_012
fstdfield read TT 1 -1 "" 12000 -1 -1 "" "TT"
fstdfield configure TT -intervals { -20 0 20 }

fstdfield stats TT -limits { 10 100 10 100 0 0 }

set n 0
foreach contour [fstdfield stats TT -coordcontour] {
   puts "Contour [incr n] length [llength $contour]"
}

#----- Teste la creation d'un masque de grille plus petite dans un grille plus grande
fstdfile open 2 read DataIn/2006122900_000.eta
fstdfield read GZ 2 -1 "" 12000 -1 -1 "" "GZ"
fstdfield stats GZ -nodata 0.0
vexpr GZ GZ<<0
vexpr TT TT<<1

fstdfield configure GZ -extrapdegree VALUE
fstdfield gridinterp GZ TT

fstdfile open 3 write DataOut/2006122900_000.eta.mask
fstdfield write GZ 3 -32 True
fstdfield read TIC 2 -1 "" -1 -1 -1 "" ">>"
fstdfield read TAC 2 -1 "" -1 -1 -1 "" "^^"
fstdfield write TIC 3 -32 True
fstdfield write TAC 3 -32 True
fstdfile close 1
fstdfile close 2
fstdfile close 3

#----- Test l'ouverture de plus de 1000 fichiers
puts "Testing multiple file open:"
for { set i 0 } { $i<1000 } { incr i } {
   puts "Opening file $i"
   fstdfile open FILE$i read DataIn/2005102612_012
   fstdfile close FILE$i
}