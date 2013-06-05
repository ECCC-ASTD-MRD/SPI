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
# Fichier    : OGR_Export2txt.tcl
# Creation   : Mars 2011 - Lucie Boucher - CMC/AQMAS
# Description: Exporter un shapefile en txt
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

set file [lindex $argv 0]

#----- Open the file
set layers [ogrfile open FILE read $file]

#----- Read the layer
ogrlayer read LAYER FILE 0

#----- Loop on the features
for { set f 0 } { $f < [ogrlayer define LAYER -nb] } { incr f } {

   #----- Get feature field needed
   set nom  [ogrlayer define LAYER -feature $f NOM]
   set name [ogrlayer define LAYER -feature $f NAME]

   puts "Name : $name"
   puts "Nom : $nom"

   #----- Get feature's geometry
   set geom [ogrlayer define LAYER -geometry $f]

   #----- Make sure we use the first ring
   while { [ogrgeometry define $geom -nbsub]>0 } {
      set geom [lindex [ogrgeometry define $geom -geometry] 0]
   }

   #----- Print coordinates
   foreach { x y } [ogrgeometry define $geom -points] {
      puts "$y $x"
   }

   puts ""
}
