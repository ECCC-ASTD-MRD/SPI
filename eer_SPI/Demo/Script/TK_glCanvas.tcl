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
# Fichier    : TK_glCanvas.tcl
# Creation   : Mai 2000 - J.P. Gauthier - CMC/CMOE
# Description: Demonstration su canvas opengl compare avec le canvas standard de tk
#
# Parametres :
#
# Retour:
#
# Remarques  :
#
#============================================================================

package require TkglCanvas
package require TkViewport

#----- Creation des fonctions de manipulation des objets du canvas

namespace eval Shape {
   variable Data

   set Data(X0)   0
   set Data(Y0)   0
   set Data(Grid) 0
}

proc Shape::Move { Canvas Tag X Y } {
   variable Data

   $Canvas move $Tag   [expr $X-$Data(X0)] [expr $Y-$Data(Y0)]
   $Canvas move BS$Tag [expr $X-$Data(X0)] [expr $Y-$Data(Y0)]

   set Data(X0) $X
   set Data(Y0) $Y
}

proc Shape::Set { X Y Canvas Act Tag } {
   variable Data

   set Data(X0) $X
   set Data(Y0) $Y
   if { $Act } {
      $Canvas itemconfigure $Tag -transparency 50
   }
}

proc Shape::BindMove { Canvas Tag Act args } {

   $Canvas bind $Tag <Enter>            "$Canvas configure -cursor hand1"
   $Canvas bind $Tag <Leave>            "$Canvas configure -cursor left_ptr"
   $Canvas bind $Tag <ButtonPress-1>    "Shape::Set %X %Y $Canvas $Act $Tag"

   if { $Act } {
      $Canvas bind $Tag <ButtonRelease-1> "$Canvas itemconfigure $Tag -transparency 100"
   }

   if { $args!="" } {
      $Canvas bind $Tag <B1-Motion>    "Shape::Move $Canvas $Tag %X %Y ; $args"
   } else {
      $Canvas bind $Tag <B1-Motion>    "Shape::Move $Canvas $Tag %X %Y"
   }
}

#----- Creation des deux type de canvas

glcanvas .glcanvas -width 300 -height 300 -bg grey -relief sunken -bd 1 -highlightthickness 0
canvas .canvas -width 300 -height 300 -bg grey -relief sunken -bd 1 -highlightthickness 0
pack .glcanvas .canvas -fill both -expand true

#----- Creation des objets dans les canvas

button .glcanvas.test -text "OpenGL" -highlightthickness 0 -bd 1
button .canvas.test   -text "  Tk  " -highlightthickness 0 -bd 1

projection create proj
projection configure proj -location 0 0 -scale 10 -type orthographic -mapcoast 1
projcam create cam
.glcanvas create viewport -x 50 -y 150 -width 150 -height 150 -anchor nw -tags VP -projection proj -camera cam -command VP

proc draw { Canvas Act } {

   $Canvas create line 10 10 60 60  100 30 200 200 100 200 250 120 300 300 -fill blue -width 2 -splinestep 10 -smooth True -tag A -arrow both -dash ","
   $Canvas create line 300 0 0 300 -fill blue -width 2 -tag B -arrow both

   Shape::BindMove $Canvas A $Act
   Shape::BindMove $Canvas B $Act
   $Canvas create bitmap 130 130 -bitmap @DataIn/string_test.xbm -disabledbitmap @DataIn/flag_hor_small.xbm -tag C -foreground red -background green -anchor se
   Shape::BindMove $Canvas C $Act

   $Canvas create polygon 100 100 100 200 200 200 120 150 -smooth bezier -stipple @DataIn/rayhor04.xbm -fill darkgreen -outline green -width 4 -tag D -dash "."
   Shape::BindMove $Canvas D $Act

   $Canvas create rectangle 10 10 100 120 -width 2 -outline pink -fill purple  -tag E  -stipple @DataIn/rayver04.xbm -dash "2 2"
   $Canvas create oval 10 10 100 120 -width 1 -outline lightblue -fill brown  -tag E -dash "4 2"
   Shape::BindMove $Canvas E $Act

   $Canvas create arc 200 50 300 100 -start 30  -extent 70 -width 2 -outline black -fill gray90 -tag F -style pieslice -stipple @DataIn/rayhor04.xbm -dash "8 2"
   $Canvas create arc 200 50 300 100 -start 115 -extent 90 -width 2 -outline black -fill gray90 -tag F -style chord    -stipple @DataIn/rayhor04.xbm
   $Canvas create arc 200 50 300 100 -start 210 -extent 90 -width 2 -outline black -fill gray90 -tag F -style arc      -stipple @DataIn/rayhor04.xbm
   $Canvas create arc 200 50 300 100 -start 305 -extent 70 -width 2 -outline black -fill gray90 -tag F -style pieslice -stipple @DataIn/rayhor04.xbm
   Shape::BindMove $Canvas F $Act

   $Canvas create text 10 250 -text "Yoyqqqoyoy \u00e9" -tag G -font FONT -anchor w -stipple @DataIn/rayhor04.xbm
   Shape::BindMove $Canvas G $Act

   $Canvas create window 300 300 -window $Canvas.test -anchor se
}
font create FONT -family arial -weight bold -size 12 -slant italic
font create FONT2 -family courier -size 20 -slant italic

draw .glcanvas 1
draw .canvas 0

update idletasks

.glcanvas postscript -file DataOut/TK_glCanvas_gl.ps
.canvas postscript -file DataOut/TK_glCanvas_tk.ps


