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
# Fichier    : TK_BasicViewport.tcl
# Creation   : Mai 2000 - J.P. Gauthier - CMC/CMOE
# Description: Creer une fenetre avec un viewport en projection orthographique
#              centree sur l'Amerique du Nord
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
package require TkGeoEER

projcam create CAM -lens 1.0

projection create PROJ -type orthographic -location 46.836559 -71.198 -scale 1 \
      -mapcoast 1 -maplake 1 -maproad 2  -mappolit 1 -mapadmin 1 -maprail 0 \
      -maptopo 0 -maptext 0  -mapcoord 0 5 10

glcanvas .map -width 400 -height 400
pack .map -fill both -expand true

.map create viewport -x 1 -y 1 -width 399 -height 399 -bg white\
   -colorcoast black -colorlake blue -colorriver blue \
   -colorpolit black -coloradmin black -colorcity black \
   -colorroad black -colorrail black -colorutil black -colorutil black -colorcoord black \
   -projection PROJ -camera CAM -command vp1  -anchor nw -tags MAP

update idletasks

 .map postscript -file DataOut/TK_BasicViewport.ps
 
image create photo TMPIMG 
.map buffer TMPIMG 1 1 400 400
TMPIMG write "DataOut/TK_BasicViewport.png" -format png
