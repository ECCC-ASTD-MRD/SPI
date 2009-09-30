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
# Fichier    : OGR_Functions.tcl
# Creation   : Decembre 2005 - J.P. Gauthier - CMC/CMOE
# Description: Exemple de mdiverses fonctions avec des donnees vectorielles
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

#----- Open the files

set layers0 [ogrfile open FILE0 read DataIn/Volcano.shp "ESRI Shapefile"]

#----- Create latlon referential

georef create LLREF
georef define LLREF -projection {GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]}

#----- Read the layers

puts  "   Layers              : $layers0"
ogrlayer read LAYER0 FILE0 [lindex [lindex $layers0 0] 1]

#----- Get the referential

set ref0  [ogrlayer define LAYER0 -georef]
puts  "   Reference object    : $ref0"
puts  "   Reference projection: [georef define $ref0 -projection]"

#----- transform a whole layer

#ogrlayer stats LAYER0 -transform LLREF $ref0

#----- Make a buffer on the layer

ogrlayer stats LAYER0 -buffer .001 3

#----- Get some geometry

set geom0 [ogrlayer define LAYER0 -geometry 0]
set geom1 [ogrlayer define LAYER0 -geometry 1]

#----- Play some trick with the geometry

puts  "   Copied geom object  : [ogrgeometry copy GEOM2 $geom1]"
puts  "   WKT output          : [ogrgeometry define $geom1 -wkt]"
puts  "   GML output          : [ogrgeometry define $geom1 -gml]"
puts  "   KML output          : [ogrgeometry define $geom1 -kml]"

set sub [ogrgeometry define $geom0 -geometry]
puts  "   Geom object         : $geom0"
puts  "   Sub geom object     : $sub"
puts  "   Geom type           : [ogrgeometry define $geom0 -type]"
puts  "   Sub geom type       : [ogrgeometry define $sub -type]"
puts  "   Sub geom length     : [ogrgeometry define $sub -nb]"
puts  "   Sub geom points     : [ogrgeometry define $sub -points]"

#----- Apply a transform to a single geometry object

#ogrgeometry stats $geom0 -transform LLREF $ref0
puts  "   Sub geom transformed: [ogrgeometry define $sub -points]"

#----- Try a union

puts  "   Geom area           : [ogrgeometry stats $geom0 -area]"
set union [ogrgeometry stats $geom0 -union $geom1]
puts  "   Union area          : [ogrgeometry stats $union -area]"

#----- Let's create a point

ogrgeometry create POINT Polygon
ogrgeometry define POINT -points { 10 10 11 11 20 11 10 10 }
puts  "   Obj area            : [ogrgeometry stats POINT -area]"

#----- Try some sql

ogrlayer sqlselect LAYERRESULT FILE0 { SELECT * FROM Volcano WHERE ENGLISH="South America" }
puts  "   Selected number of feature : [ogrlayer define LAYERRESULT -nb]"

ogrfile close FILE0
