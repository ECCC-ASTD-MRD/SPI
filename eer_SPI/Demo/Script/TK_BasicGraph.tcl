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
# Fichier    : TK_BasicGraph.tcl
# Creation   : Juillet 2010 - J.P. Gauthier - CMC/CMOE
# Description: Demonstration du graph
#
# Parametres :
#
# Retour:
#============================================================================

puts \n[file tail [info script]]

package require TkglCanvas
package require TkViewport

font create FONT1 -family arial -weight bold -size 12 -slant italic
font create FONT2 -family courier -size 20 -slant italic

#----- Create graph
glcanvas .glcanvas -width 1000 -height 600 -bg grey -relief sunken -bd 1 -highlightthickness 0
pack .glcanvas -side top -fill both -expand True

.glcanvas create graph -x 5 -y 5 -width 990 -height 590 -anchor nw -xlegend 5 -ylegend 5 -command graphtest \
   -fg black -bg gray -fill white -tags "GRAPH" -font FONT2 -title "Graph Test\nDoes it look ok ?"

#----- Get the item clicked with button 1
bind .glcanvas <Button-1> { Pick %x %y }

#----- Create axis
graphaxis create AXISX
graphaxis create AXISY

#----- Create date
vector create DATA
vector dim    DATA { X Y }

#----- Create graphitem
graphitem create ITEM
graphitem configure ITEM -xaxis AXISX -yaxis AXISY -xdata DATA.X -ydata DATA.Y -orient X -desc "O3" \
   -outline black -fill gray90 -iconfill red -iconoutline black -transparency 75 -width 1 -size 3 -value False -font FONT1 \
   -type HISTOGRAM -font XFont12 -icon CIRCLE -bitmap "" -stipple "" -image ""

#----- Load observation data into graph
set file /home/afsr/005/public_html/SPI/Script/DataIn/O3.20050302.obs
set obss [observation load $file]

#for { set n 0 } { $n<[observation define [lindex $obss 0] -NB] } { incr n }
for { set n 0 } { $n<50 } { incr n } {

   set id [observation define [lindex $obss 0] -ID $n]

   foreach obs $obss {

      set sec [expr [observation define $obs -DATE]+$n*60]
      set val [observation define $obs -DATA $n]
      if { $val=="-" } { set val 0 }

      vector append DATA [list $sec $val]
      break
   }
}

#----- Configure graph axis
graphaxis configure AXISX -font FONT1 -type LINEAR -color blue -gridcolor lightblue -dash "." -position LL -width 1  \
   -min [vector stats DATA.X -min] -max [vector stats DATA.X -max] -intervals [lsort -increasing [vector get DATA.X]] \
   -unit "Temps" -format "TIME" -angle 30
graphaxis configure AXISY -font FONT1  -type LINEAR -color yellow -gridcolor gold -gridwidth 1 -dash "." -position LL -width 1 \
   -min 0 -max [vector stats DATA.Y -max] -unit Pbb -increment 10

#----- Redraw graph
.glcanvas itemconf GRAPH -item ITEM

#----- This proc retreives the item clicked on
proc Pick { X Y } {
   puts "Picked $X $Y: [graphtest -pick $X $Y]"
}

#image create photo TMPIMG -width 300 -height 300 -format window -data .glcanvas
#TMPIMG write "DataOut/TK_glCanvas.png" -format png
#TMPIMG write "DataOut/TK_glCanvas.ppm" -format ppm

