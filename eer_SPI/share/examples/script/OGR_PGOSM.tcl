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
# Fichier    : OGR_PGOSM.tcl
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

georef create GOOGLE { PROJCS["unnamed",GEOGCS["unnamed ellipse",DATUM["unknown",SPHEROID["unnamed",6378137,0]],PRIMEM["Greenwich",0],UNIT["degree",0.0174532925199433]],PROJECTION["Mercator_2SP"],PARAMETER["standard_parallel_1",0],PARAMETER["central_meridian",0],PARAMETER["false_easting",0],PARAMETER["false_northing",0],UNIT["Meter",1],EXTENSION["PROJ4","+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"]] }

set layers0 [ogrfile open PGOSM read "PG: host=aqdb.cmc.ec.gc.ca port=5432 user=afsr005 dbname=osm"]
puts "Layers available: $layers0"

set bbox [join [concat [georef unproject GOOGLE 47.74 -64.96] [georef unproject GOOGLE 47.498 -64.678]] ,]

Log::Print INFO "Extracting everything"
eval ogrlayer sqlselect ALL PGOSM \{ SELECT * FROM planet_osm_polygon WHERE (way && ST_MakeEnvelope($bbox)) \}
Log::Print INFO "   Selected number of feature : [ogrlayer define ALL -nb]"
  
catch { eval file delete [glob DataOut/osmall.*] }
ogrfile open OSMALL write DataOut/osmall.shp "ESRI Shapefile"
ogrlayer write ALL OSMALL
ogrfile close OSMALL
ogrlayer free ALL

Log::Print INFO "Extracting coastlines"
eval ogrlayer sqlselect COAST PGOSM \{ SELECT * FROM planet_osm_polygon WHERE \"natural\"='coastline' AND (way && ST_MakeEnvelope($bbox)) \}
Log::Print INFO "   Selected number of feature : [ogrlayer define COAST -nb]"
 
catch { eval file delete [glob DataOut/osmcoast.*] }
ogrfile open OSMCOAST write DataOut/osmcoast.shp "ESRI Shapefile"
ogrlayer write COAST OSMCOAST
ogrfile close OSMCOAST
ogrlayer free COAST

Log::Print INFO "Extracting lakes"
eval ogrlayer sqlselect LAKE PGOSM \{ SELECT * FROM planet_osm_polygon WHERE (\"natural\"='water' OR place='sea' OR waterway='riverbank' OR waterway='canal') AND (way && ST_MakeEnvelope($bbox)) \}
Log::Print INFO "   Selected number of feature : [ogrlayer define LAKE -nb]"

catch { eval file delete [glob DataOut/osmlake.*] }
ogrfile open OSMLAKE write DataOut/osmlake.shp "ESRI Shapefile"
ogrlayer write LAKE OSMLAKE
ogrfile close OSMLAKE
ogrlayer free LAKE


ogrfile close PGOSM

Log::End