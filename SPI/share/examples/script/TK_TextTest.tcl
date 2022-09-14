#!/bin/sh
# the next line restarts using tclsh \
exec $SPI_PATH/wish "$0" "$@"
#============================================================================
# Environnement Canada
# Centre Meteorologique Canadien
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet     : Exemple de scripts.
# Fichier    : TK_TextTest.tcl
# Creation   : June 2022 - J.P. Gauthier - CMC/CMOE
# Description: Test des polices de caracteres
#
# Parametres :
#
# Retour:
#
# Remarques  :
#
#============================================================================

puts \n[file tail [info script]]

package require TkglCanvas

glcanvas .cv -width 400 -height 800
pack .cv -fill both -expand true

set f    1
set size -18
set fonts { charter courier fix helvetica arial lucida "new century schoolbook" terminal times }

foreach font $fonts {
    
    font create Font$f -family $font -weight bold -size $size
    .cv create text 10 [expr $f*20] -text "Testing font $font size $size" -font Font$f -anchor nw
    incr f
}

image create photo TMPIMG
.cv buffer TMPIMG 0 0 400 800
TMPIMG write TK_TextTest.ppm -format ppm
       
exec -ignorestderr convert TK_TextTest.ppm TK_TextTest.png
file delete $File.ppm

exit

