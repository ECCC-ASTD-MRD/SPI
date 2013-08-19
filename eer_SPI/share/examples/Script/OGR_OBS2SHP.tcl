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
# Fichier    : OGR_OBS2SHP.tcl
# Creation   : Mars 2008 - J.P. Gauthier - CMC/CMOE
# Description: Creer un fichier Shapefile a partir de donnees d'observations
#
# Parametres :
#
# Retour:
#
# Remarques  :
#
#============================================================================

package require TclData
#package require TclGeoEER

puts \n[file tail [info script]]

set f [open DataIn/2005_0823-0830_stroke_cg.txt.spi.obs]

#----- Creation du fichier

catch { file delete DataOut/OGR_OBS2SHP.shp }
ogrfile open FILE write DataOut/OGR_OBS2SHP.shp "ESRI Shapefile"

#----- Creation du layer et des champs

ogrlayer create FILE STROKE "Stroke"
ogrlayer define STROKE -field Ka Real
ogrlayer define STROKE -field N1 Real
ogrlayer define STROKE -field N2 Real
ogrlayer define STROKE -field Date String

ogrgeometry create POINT "Point"

set nb 0

#----- Boucle sur les regions definie dans le fichier texte

gets $f line
gets $f line
gets $f line

while { ![eof $f] } {
   gets $f line

   set sec [clock scan [lindex [split [lindex $line 0] .] 0] -gmt True]
   set la  [lindex $line 1]
   set lo  [lindex $line 2]
   set ka  [lindex $line 3]
   set n1  [lindex $line 5]
   set n2  [lindex $line 6]

   ogrgeometry define POINT -points { }
   ogrgeometry define POINT -addpoint $lo $la

   ogrlayer define STROKE -nb [incr nb]
   set no [expr $nb-1]
   ogrlayer define STROKE -feature $no Ka $ka
   ogrlayer define STROKE -feature $no N1 $n1
   ogrlayer define STROKE -feature $no N2 $n2
   ogrlayer define STROKE -feature $no Date [lindex $line 0]
   ogrlayer define STROKE -geometry $no False POINT
}

ogrfile close FILE
