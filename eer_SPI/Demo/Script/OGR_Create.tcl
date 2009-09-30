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
# Fichier    : OGR_Create.tcl
# Creation   : Mars 2005 - J.P. Gauthier - CMC/CMOE
# Description: Creer un fichier Shapefile a partir de coordonnees latlon
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

set f [open DataIn/MWO.reg]

#----- Creation du fichier

catch { file delete DataOut/OGR_Create.shp }
ogrfile open FILE write DataOut/OGR_Create.shp "ESRI Shapefile"

#----- Creation du layer et des champs

ogrlayer create FILE AREA "MWO"
ogrlayer define AREA -field ENGLISH  String
ogrlayer define AREA -field FRANCAIS String

ogrgeometry create POLY "Polygon"
ogrgeometry create RING "Linear Ring"

set nb 0
set side 0

#----- Boucle sur les regions definie dans le fichier texte

while { ![eof $f] } {
   gets $f line

   if { [string index $line 0]!="#" && [llength $line]==2 } {

      ogrgeometry define RING -points {}

      foreach { y x z } [lindex $line 1] {

         if { $x<0 && $side } {
            set x [expr 360.0+$x]
         }
         ogrgeometry define RING -addpoint $x $y
      }
      ogrgeometry define POLY -geometry False RING

      ogrlayer define AREA -nb [incr nb]
      set no [expr $nb-1]
      ogrlayer define AREA -feature $no FRANCAIS [lindex [lindex $line 0] 0]
      ogrlayer define AREA -feature $no ENGLISH  [lindex [lindex $line 0] 1]

      ogrlayer define AREA -geometry $no False POLY
   }
}

ogrfile close FILE
