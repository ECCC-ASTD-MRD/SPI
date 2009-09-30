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
# Fichier    : FSTD_GridFunc.tcl
# Creation   : Mai 2000 - J.P. Gauthier - CMC/CMOE
# Description: Demontrer les fonctions de calcul grille (Wrapper RPN)
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

set ig [fstdgrid cigaxg N 1 2 3 4]
set xg [fstdgrid cxgaig N 1 2 3 4]

set cg [fstdgrid cigaxg S 250 1354 11242 42099]
puts "   ig=$cg"

set pi    [lindex $cg 0]
set pj    [lindex $cg 1]
set d60   [lindex $cg 2]
set dgrw  [lindex $cg 3]

set xy [fstdgrid xyfll -4.099899769 145.0610046 1 $dgrw 2]

# "  D60, DGRW,  PI,  PJ = " 25000.00000 135.4000092 -372.3423157 -77.59349060
puts "   old  -372.3423157 -77.59349060 25000.00000 135.4000092 "
puts "   $pi $pj $d60 $dgrw"
puts "   ISO = [expr [lindex $xy 0]/$d60+$pi] JSO = [expr [lindex $xy 1]/$d60 + $pj]"
