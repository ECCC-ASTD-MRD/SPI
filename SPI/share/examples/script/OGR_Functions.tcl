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
#package require TclGeoEER
package require Logger

Log::Start [info script] 0.1

#----- Open the files
set layers0 [ogrfile open FILE0 read $env(CI_DATA_IN)/Volcano.shp]
#set layers0 [ogrfile open FILE0 read DataIn/noire_areas.shp]

#----- Create latlon referential
georef create LLREF
georef define LLREF -projection {GEOGCS["GCS_WGS_1984",DATUM["WGS_1984",SPHEROID["WGS84",6378137.0,298.257223563]],PRIMEM["Greenwich",0],UNIT["Degree",0.0174532925199432958]]}

#----- Read the layers
puts  "   Layers              : $layers0"
ogrlayer read LAYER0 FILE0 [lindex [lindex $layers0 0] 1]
puts  "   Layer centroid     :  [ogrlayer stats LAYER0 -centroid]"

#----- Get the referential
set ref0  [ogrlayer define LAYER0 -georef]
puts "   Reference object    : $ref0"
puts "   Reference projection: [georef define $ref0 -projection]"
puts "   LatLon    projection: [georef define LLREF -projection]"
puts "   Test reference same : [georef isequal $ref0 LLREF]"

#----- Copy a georef
georef copy CREF  $ref0

#----- simplify the geometry
ogrlayer stats LAYER0 -segmentize 1.0
ogrlayer stats LAYER0 -simplify  3

#----- Make a buffer on the layer
#ogrlayer stats LAYER0 -buffer 1.0 20

#----- transform a whole layer
ogrlayer stats LAYER0 -transform LLREF $ref0

puts "   Invalid items       : [ogrlayer stats LAYER0 -invalid]"

#----- Get some geometry
set geom0 [ogrlayer define LAYER0 -geometry 0]
set geom1 [ogrlayer define LAYER0 -geometry 1]

#----- Play some trick with the geometry
puts  "   Copied geom object  : [ogrgeometry copy GEOM2 $geom1]"
puts  "   WKT output          : [ogrgeometry define $geom1 -wkt]"
puts  "   GML output          : [ogrgeometry define $geom1 -gml]"
puts  "   KML output          : [ogrgeometry define $geom1 -kml]"
puts  "   JSON output         : [ogrgeometry define $geom1 -json]"
puts  "   WKT (topoint) output: [ogrgeometry define [ogrgeometry stats $geom1 -topoint] -wkt]"


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

#----- Try a some operators
puts  "   Geom empty ?        : [ogrgeometry stats $geom0 -isempty]"
puts  "   Geom valid ?        : [ogrgeometry stats $geom0 -isvalid]"
puts  "   Geom simple ?       : [ogrgeometry stats $geom0 -issimple]"
puts  "   Geom is ring ?      : [ogrgeometry stats $geom0 -isring]"
puts  "   Geom area           : [ogrgeometry stats $geom0 -area]"
puts  "   Geom perimeter      : [ogrgeometry stats $geom0 -length]"
puts  "   Geom centroid       : [ogrgeometry stats $geom0 -centroid]"

set tri  [ogrgeometry stats $geom0 -delaunay 0.01]
puts  "   Delaunay            : [ogrgeometry define $tri -wkt]"

ogrgeometry stats $geom0 -segmentize 1.0
puts  "   Geom segmentized    : [ogrgeometry define $sub -points]"

ogrgeometry stats $geom0 -simplify 10.0
puts  "   Geom simplified     : [ogrgeometry define $sub -points]"

set union [ogrgeometry stats $geom0 -union $geom1]
puts  "   Union area          : [ogrgeometry stats $union -area]"

set inter [ogrgeometry stats $geom0 -intersection $geom1]
puts  "   Intersection area   : [ogrgeometry stats $inter -area]"

set hull [ogrgeometry stats $geom0 -convexhull]
puts  "   Convex hull area    : [ogrgeometry stats $hull -area]"

puts  "   Point on surface    : [ogrgeometry stats $geom0 -pointonsurface]"

#----- Let's create a polygon
ogrgeometry create POLY "Polygon"
ogrgeometry create RING "Linear Ring"
ogrgeometry define RING -points { 0 0 0 10 10 10 10 0 0 0 }
ogrgeometry define POLY -geometry True RING
puts  "   Obj area            : [ogrgeometry stats POLY -area]"
puts  "   Obj points          : [ogrgeometry define [ogrgeometry define POLY -geometry] -points]"
puts  "   Obj minimum angle   : [ogrgeometry stats POLY -anglemin]"

#----- Make a buffer around it
set geom [ogrgeometry stats POLY -buffer 1.0 20]
puts  "   Obj buffered points : [ogrgeometry define [ogrgeometry define $geom -geometry] -points]"

#----- Try some sql
ogrlayer sqlselect LAYERRESULT FILE0 { SELECT * FROM Volcano WHERE ENGLISH = 'South America' }
puts  "   Selected number of feature : [ogrlayer define LAYERRESULT -nb]"

set reqlayer Volcano
ogrlayer sqlselect LAYERRESULT FILE0 "SELECT * FROM $reqlayer WHERE ENGLISH NOT IN ('South America','Tau Ceti')"
puts  "   Selected number of feature : [ogrlayer define LAYERRESULT -nb]"

catch { eval file delete [glob -nocomplain $env(CI_DATA_OUT)/OGR_Functions*] }

ogrfile open RESULT write $env(CI_DATA_OUT)/OGR_Functions.shp "ESRI Shapefile"
ogrlayer write LAYER0 RESULT
ogrfile close RESULT

ogrfile open RESULT write $env(CI_DATA_OUT)/OGR_Functions_Copy.shp "ESRI Shapefile"
ogrlayer copy LAYER1 LAYER0
ogrlayer write LAYER1 RESULT
ogrfile close RESULT

ogrlayer free LAYER0 LAYERRESULT
ogrfile close FILE0

Log::End
