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
# Fichier    : OGR_IndexBuild.tcl
# Creation   : Decembre 2002 - J.P. Gauthier - CMC/CMOE
# Description: Creation d'un index geographiqe de couverture e donnees raster
#              dans un fichier Shapefile pouvant ensuite etre utiliser dans SPI
#              pour la selection des donnees
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

gdalfile error QUIET

proc Parser { Path } {
   foreach file [glob -nocomplain $Path/*] {
      if { [file isdirectory $file] } {
         Parser $file
      } else {
         Indexer $file
      }
   }
}

proc Indexer { File } {
   variable Data

   puts "   Checking $File"

   eval set bad [catch { set bands [gdalfile open FILE read $File] }]

   #----- si le fichier est valide

   if { !$bad } {
      #----- Recuperer les limites et les convertir en latlon

      set width  [expr [gdalfile width FILE]-1]
      set height [expr [gdalfile height FILE]-1]
      set xy     [gdalfile project FILE 0 0]

      #----- Si les coordonnees sont valides

      set lat [lindex $xy 0]
      set lon [lindex $xy 1]

      if { $lat<=90.0 && $lat>=-90.0 && ($lat!=0.0 && $lon!=0.0) } {

         puts "      File is valid ($lat $lon)"

         #----- Creer le polygone de la couverture des donnees raster

         ogrlayer define INDEX -nb [incr Data(Nb)]
         set no [expr $Data(Nb)-1]

         ogrgeometry define RING -points {}

         for { set x 0 } { $x < [expr $width-$Data(DX)] } { incr x $Data(DX) } {
            set xy [gdalfile project FILE $x 0]
            ogrgeometry define RING -addpoint [lindex $xy 1] [lindex $xy 0]
         }

         for { set y 0 } { $y < [expr $height-$Data(DX)] } { incr y $Data(DX) } {
            set xy [gdalfile project FILE $width $y]
            ogrgeometry define RING -addpoint [lindex $xy 1] [lindex $xy 0]
         }

         for { set x $width } { $x > $Data(DX) } { incr x -$Data(DX) } {
            set xy [gdalfile project FILE $x $height]
            ogrgeometry define RING -addpoint [lindex $xy 1] [lindex $xy 0]
         }

         for { set y $height } { $y > $Data(DX) } { incr y -$Data(DX) } {
            set xy [gdalfile project FILE 0 $y]
            ogrgeometry define RING -addpoint [lindex $xy 1] [lindex $xy 0]
         }

         set xy [gdalfile project FILE 0 0]
         ogrgeometry define RING -addpoint [lindex $xy 1] [lindex $xy 0]
         ogrgeometry define POLY -geometry False RING

         #----- Inserer le path du fichier associe

         ogrlayer define INDEX -feature $no IDX_PATH [string range $File [string length $Data(Path)] end]
         ogrlayer define INDEX -geometry $no False POLY
      }
      gdalfile close FILE
   }
}

#----- Path de repertoire a traiter en parametre

set Data(Path) [lindex $argv 0]
set Data(Nb)   0
set Data(DX)   50

#----- Supprimer l<index si il existe deja

catch { file delete -force $Data(Path)/Index }

#----- Creer le nouveaux fichier/repertoire d'index

file mkdir $Data(Path)/Index
ogrfile open INDEXFILE write $Data(Path)/Index/Index.shp "ESRI Shapefile"
ogrlayer create INDEXFILE INDEX "Index"

#----- Champs qui contient le path

ogrlayer define INDEX -field IDX_PATH String

#----- Initialiser la geometrie

ogrgeometry create POLY "Polygon"
ogrgeometry create RING "Linear Ring"

#----- Parcourir chaque fichier dans le repertoire

Parser $Data(Path)

#----- That's it

puts "   Processed $Data(Nb) valid file"

ogrfile close INDEXFILE
