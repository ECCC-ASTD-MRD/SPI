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
# Fichier    : OGR_Basic.tcl
# Creation   : Mai 2000 - J.P. Gauthier - CMC/CMOE
# Description: Demonstration de base des fonctions OGR pour les donneees vectorielles
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

#----- Afficher la liste des formats reconnus

puts "   Available formats:\n\t\t[join [ogrfile format] "\n\t\t"]"

#----- Ouverture d'un fichier SHP

set layers [ogrfile open OGRFILE read DataIn/Volcano.shp]
eval ogrlayer read LAYER [lindex $layers 0]

#----- Afficher quelques informations sur le layer

puts  "   Name       : [ogrlayer define LAYER -name]"
puts  "   Fields     : [ogrlayer define LAYER -field]"
puts  "   NB Feature : [ogrlayer define LAYER -nb]"

for { set i 0 } { $i<[ogrlayer define LAYER -nb] } { incr i } {
   puts  "      Fields($i)  : [ogrlayer define LAYER -feature $i]"
   puts  "      Geometry($i): [ogrlayer define LAYER -geometry $i]"
}

#----- Changement de la projection

ogrlayer define LAYER -projection { PROJCS["Sphere_ARC_INFO_Lambert_Conformal_Conic",GEOGCS["GCS_Sphere_ARC_INFO",DATUM["D_Sphere_ARC_INFO",SPHEROID["Sphere_ARC_INFO",6370997.0,0.0]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Lambert_Conformal_Conic_2SP"],PARAMETER["False_Easting",0.0],PARAMETER["False_Northing",0.0],PARAMETER["Central_Meridian",-97.0],PARAMETER["Standard_Parallel_1",33.0],PARAMETER["Standard_Parallel_2",45.0],PARAMETER["Latitude_Of_Origin",40.0],UNIT["Meter",1.0]] }

ogrfile close OGRFILE

#----- Ouverture d'un fichier WFS

#set layers [ogrfile open WFSFILE read WFS:http://demo.opengeo.org/geoserver/wfs?VERSION=1.1.0&SERVICE=WFS]
#set layers [ogrfile open WFSFILE read WFS:http://goodenough.cmc.ec.gc.ca:9090?VERSION=1.1.0&SERVICE=WFS]
set layers [ogrfile open WFSFILE read WFS:http://www.weather.gov/forecasts/xml/OGC_services/ndfdOWSserver.php?SERVICE=WFS&VERSION=1.0.0&bbox=43.00,-82.00 33.00,-72.00&resolution=40&time=2011-02-02T23:00:00&TYPENAME=Forecast_Gml2Point]

puts "WFS layers available: $layers"
ogrlayer read WFSLAYER WFSFILE 0
