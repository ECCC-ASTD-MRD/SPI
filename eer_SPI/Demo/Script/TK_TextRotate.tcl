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
# Fichier    : TK_TextRotate.tcl
# Creation   : Mars 2005 - J.P. Gauthier - CMC/CMOE
# Description: Demonstration de l'angle de rotation des textes
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

font create FontX -family Courier -weight bold -size -12

glcanvas .cv
pack .cv -fill both -expand true

.cv create text 200 200 -text "ALLO\nMon\n\ttoi\nLLLLLOOOOOLLL" -angle 45 -tag TEXT -font FontX

set angle 0
while {1} {
   .cv itemconf TEXT -angle [incr angle 1]
   update idletasks
   after 10
}
