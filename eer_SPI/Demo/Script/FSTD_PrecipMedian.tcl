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
# Fichier    : FSTD_PrecipMedian.tcl
# Creation   : Juin 2010 - J.P. Gauthier - CMC/CMOE
# Description: Calcul de la mediane pour les precips (Faut pas tenir compte des zeros)
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

fstdfile open 1 read DataIn/RT.fstd
fstdfile open 2 write DataOut/FSTD_PrecipMedian.fstd

set fldlist {}

puts "   Reading fields"
foreach fld [fstdfield find 1 -1 "" -1 -1 -1 "" "RT"] {
   fstdfield read FLD$fld 1 $fld
   lappend fldlist FLD$fld
}
set n [llength $fldlist]

#----- Sort all the fields gridpoints
puts "   Sorting them"
fstdfield sort $fldlist

#----- Make buffer copies for calculations
fstdfield copy ZEROS FLD$fld
vexpr ZEROS ZEROS<<0
fstdfield copy MEDIAN ZEROS

#----- Count number of zeros cause we dont want them to influance the median
puts "   Calculating indexes"
foreach field $fldlist {
   vexpr ZEROS ifelse($field==0,ZEROS+1,ZEROS)
}
#----- Get the median index in the field list by gridpoint
vexpr IDX ceil(ZEROS+($n-ZEROS)/2)

#----- Extract the median by using the field at IDX in the list
puts "   Calculating median"
set i 0
foreach field $fldlist {
   vexpr MEDIAN ifelse(IDX==$i,$field,MEDIAN)
   incr i
}

#----- Write output median
fstdfield read TIC   1 -1 "" -1 -1  -1 "" ">>"
fstdfield read TAC   1 -1 "" -1 -1  -1 "" "^^"
fstdfield write TIC 2 0 True
fstdfield write TAC 2 0 True
fstdfield write MEDIAN 2 0 Fals

fstdfile close 1 2