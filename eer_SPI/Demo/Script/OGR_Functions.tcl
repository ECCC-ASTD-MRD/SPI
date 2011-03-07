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
set layers0 [ogrfile open FILE0 read DataIn/Volcano.shp]

#----- Create latlon referential
georef create LLREF
georef define LLREF -projection {GEOGCS["GCS_WGS_1984",DATUM["WGS_1984",SPHEROID["WGS84",6378137.0,298.257223563]],PRIMEM["Greenwich",0],UNIT["Degree",0.0174532925199432958]]}

#----- Read the layers
puts  "   Layers              : $layers0"
ogrlayer read LAYER0 FILE0 [lindex [lindex $layers0 0] 1]

#----- Get the referential
set ref0  [ogrlayer define LAYER0 -georef]
puts "   Reference object    : $ref0"
puts "   Reference projection: [georef define $ref0 -projection]"
puts "   LatLon    projection: [georef define LLREF -projection]"
puts "   Test reference same : [georef isequal $ref0 LLREF]"

#----- Make a buffer on the layer
#ogrlayer stats LAYER0 -buffer 1.0 20

#----- transform a whole layer
ogrlayer stats LAYER0 -transform LLREF $ref0

#----- simplify the geometry
ogrlayer stats LAYER0 -simplify  3

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
puts  "   Sub geom nb points  : [ogrgeometry define $sub -nb]"
puts  "   Sub geom points     : [ogrgeometry define $sub -points]"

#----- Apply a transform to a single geometry object
ogrgeometry stats $geom0 -transform LLREF $ref0
puts  "   Sub geom transformed: [ogrgeometry define $sub -points]"

#----- Try a union
puts  "   Geom area           : [ogrgeometry stats $geom0 -area]"
puts  "   Geom perimeter      : [ogrgeometry stats $geom0 -length]"
puts  "   Geom centroid       : [ogrgeometry stats $geom0 -centroid]"
set union [ogrgeometry stats $geom0 -union $geom1]
puts  "   Union area          : [ogrgeometry stats $union -area]"

set inter [ogrgeometry stats $geom0 -intersection $geom1]
puts  "   Intersection area   : [ogrgeometry stats $inter -area]"

#----- Let's create a point
ogrgeometry create POLY "Polygon"
ogrgeometry create RING "Linear Ring"
ogrgeometry define RING -points { 10 10 11 11 20 11 10 10 }
ogrgeometry define POLY -geom True RING
puts  "   Obj area            : [ogrgeometry stats POLY -area]"
puts  "   Obj points          : [ogrgeometry define [ogrgeometry define POLY -geom] -points]"

set geom [ogrgeometry stats POLY -buffer 1.0 10]
puts  "   Obj buffered points : [ogrgeometry define [ogrgeometry define $geom -geom] -points]"

#----- Try some sql
ogrlayer sqlselect LAYERRESULT FILE0 { SELECT * FROM Volcano WHERE ENGLISH="South America" }
puts  "   Selected number of feature : [ogrlayer define LAYERRESULT -nb]"

set reqlayer Volcano
ogrlayer sqlselect LAYERRESULT FILE0 "SELECT * FROM $reqlayer WHERE ENGLISH NOT IN (\"South America\",\"Tau Ceti\")"
puts  "   Selected number of feature : [ogrlayer define LAYERRESULT -nb]"


set layers [ogrfile open FILE1 read /cnfs/ops/production/cmoe/geo/CanVec/031/h/031h05/031h05_6_0_BS_2010009_2.shp]
puts $layers
ogrlayer sqlselect LAYERRESULT FILE1 "SELECT * FROM 031h05_6_0_BS_2010009_2 WHERE function = 1"
puts  "   Selected number of feature : [ogrlayer define LAYERRESULT -nb]"

ogrlayer free LAYER0
ogrlayer free LAYERRESULT
ogrfile close FILE0
