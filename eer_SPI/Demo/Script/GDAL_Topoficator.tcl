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
# Fichier    : GDAL_Topoficator.tcl
# Creation   : Ajnvier 2006 - J.P. Gauthier - CMC/CMOE
# Description: Ce script extrait la topo raster (ex SRTM30)
#              sur le domaine d'une autre fichier raster en
#              remplacant les donnees manquantes par celle
#              de GDB
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

gdalfile error QUIET

#----- Creer une projection pur acceder a la topo de GDB a la resolution 64

projection create PROJ
projection configure PROJ -mapres 64

#----- Ouvrir le fichier de sortie

catch { file delete  [lindex $argv 0].R.tif }
gdalfile open FILEOUT  write [lindex $argv 0].R.tif GeoTiff

#----- Ouvrir le fichier de topo d'entree

set band [gdalfile open FILETOPO read [lindex $argv 1]]
gdalband read TOPO $band

#----- Lire une bande du fichier raster pour obtenir le domaine

set bands [gdalfile open FILEIN read [lindex $argv 0]]
gdalband read BAND [list [lindex $bands 0]]

#----- Recupere les dimensions

set w [gdalband configure BAND -width]
set h [gdalband configure BAND -height]

#----- Creer la bande de sortie

gdalband create TOPOOUT $w $h 1 Float32
gdalband define TOPOOUT -georef [gdalband define BAND -georef]

#----- On boucle sur le domaine

for { set x 0 } { $x<$w } { incr x } {
   for { set y 0 } { $y<$h } { incr y } {

      #----- LatLon du domaine

      set ll [gdalband stats TOPOOUT -project $x $y]

      #----- Valeur de la topo a ce point

      set value [gdalband stats TOPO -coordvalue [lindex $ll 0] [lindex $ll 1]]

      #----- Si invalide utilise GDB

      if { $value<-900 } {
         set value [projection data PROJ -TOPO [lindex $ll 0] [lindex $ll 1]]
      }

      #----- Inserer ce point

      gdalband stats TOPOOUT -gridvalue $x $y 0 $value
   }
}

#----- Ecrire la bande

gdalband write TOPOOUT FILEOUT

#----- on ferme tout et bye bye
gdalfile close FILEIN FILEOUT FILETOPO

