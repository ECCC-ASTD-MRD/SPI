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

set secs [clock seconds]
puts "Testing stamp functions for [clock format $secs]"
puts "   seconds: $secs"
puts "   stamp  : [set stamp [fstdstamp fromseconds $secs]]"
puts "   incr 24: [set stamp [fstdstamp incr $stamp 024]]"
puts "   r.date : [exec r.date $stamp]"
puts "   date   : [clock format [fstdstamp toseconds $stamp]]"

puts "\nTesting ip conversion :"
puts "   ip1 12000      : [fstdgrid convip 12000]"
puts "   1000.0 PRESSURE: [fstdgrid convip 1000.0 PRESSURE]"
puts "   0.9    ETA     : [fstdgrid convip 0.9 ETA]"
puts "   2500   MASL    : [fstdgrid convip 2500.0 MASL]"
puts "   2500   MAGL    : [fstdgrid convip 2500.0 MAGL]"

puts "\nTesting integer data"
fstdfield create FLDINT 10 10 1 UInt32
fstdfield stats FLDINT -gridvalue 5 5 20061231
puts "   20061231 = [fstdfield stats FLDINT -gridvalue 5 5]"

set inter { -20 0 20 }
puts "\nTesting contour extraction ($inter):"
fstdfile open 1 read DataIn/2005102612_012
fstdfield read TT 1 -1 "" 12000 -1 -1 "" "TT"
fstdfield configure TT -intervals $inter

fstdfield stats TT -limits { 10 10 0 100 100 0 }

set n 0
foreach contour [fstdfield stats TT -coordcontour] {
   puts "  Contour [incr n] length [llength $contour]"
}

puts "\nTesting parser's slicers"
puts "   min=[vexpr NIL smin(TT)]"
puts "   min((10,20),(10,20))=[vexpr NIL smin(TT((10,20),(10,20)))]"

vexpr NIL TT((10,20),(10,20))
puts "   data=[join [fstdfield define NIL -DATA] \n]"

puts "\nTesting vertical profile of wind"
fstdfield read UU 1 -1 "" -1 -1 -1 "" "UU"
fstdfield readcube UU

vexpr WS (\[UU\] * 0.51444)
vexpr WD @UU@
fstdfield vertical PROFIL_WS WS [list 50.0 -150.0]
fstdfield vertical PROFIL_WD WD [list 50.0 -150.0]

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
fstdfile close 1 2 3

#----- Test l'ouverture de plus de 1000 fichiers
puts "\nTesting multiple file open:"
for { set i 0 } { $i<=1001 } { incr i } {
   puts "   Opening file $i"
   fstdfile open FILE$i read DataIn/2005102612_012
   fstdfile close FILE$i
}

fstdfield free FLDINT NIL TT GZ TIC TAC