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
# Fichier    : MDL_Functions.tcl
# Creation   : Janvier 2010 - J.P. Gauthier - CMC/CMOE
# Description: Test de lecture de fichier Collada DAE
#
# Parametres :
#
# Retour:
#
# Remarques  :
#   Aucune.
#
#============================================================================

package require TclData
#package require TclGeoEER
package require Logger

Log::Start [info script] 0.1

#model read DAE /users/dor/afsr/005/Data/model.dae
#set models [glob -tails -directory /cnfs/ops/cmoe/afsr005/Projects/UrbanX/Collada *]
#set models TourSunlife
#set models AldredBuilding
#foreach model $models {
#   puts "   Reading $model"
#   model read $model /cnfs/ops/cmoe/afsr005/Projects/UrbanX/Collada/$model/doc.kml
#   model configure $model -outline blue -width 1
#}

#model read DAE /cnfs/ops/cmoe/afsr005/Projects/UrbanX/Collada/StadeOlympiqueEtBiodome/models/model.dae
#model read DAE /cnfs/ops/cmoe/afsr005/Projects/UrbanX/Collada/StadeOlympiqueEtBiodome/doc.kml
#model configure DAE -outline black -width 1 -renderface False
#model matrix DAE -locate 46.8086773762 -71.2179046536 125
#model matrix DAE -scale 1 1 1

#   model read GML /local/disk1/afsr005/3DModel/CityGML/CityGML_British_Ordnance_Survey_v1.0.0.xml
#   georef create REF { PROJCS["NAD_1983_MTM_8",GEOGCS["GCS_North_American_1983",DATUM["D_North_American_1983",SPHEROID["GRS_1980",6378137.0,298.257222101]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Transverse_Mercator"],PARAMETER["False_Easting",304800.0],PARAMETER["False_Northing",0.0],PARAMETER["Central_Meridian",-73.5],PARAMETER["Scale_Factor",0.9999],PARAMETER["Latitude_Of_Origin",0.0],UNIT["Meter",1.0]]}
#   model read GML "/local/disk2/afsr005/PortMtl-quaiAlexandra/Port.gml"
#   model define GML -georef REF

georef create REF { PROJCS["NAD_1983_MTM_8",GEOGCS["GCS_North_American_1983",DATUM["D_North_American_1983",SPHEROID["GRS_1980",6378137.0,298.257222101]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Transverse_Mercator"],PARAMETER["False_Easting",304800.0],PARAMETER["False_Northing",0.0],PARAMETER["Central_Meridian",-73.5],PARAMETER["Scale_Factor",0.9999],PARAMETER["Latitude_Of_Origin",0.0],UNIT["Meter",1.0]]}

set models [glob -tails -directory /cnfs/ops/production/cmoe/geo/Vector/Cities/Montreal/CityGML_LOD2_20140115 *.gml]
foreach model $models  {
   puts "   Reading $model"
   model read $model "/cnfs/ops/production/cmoe/geo/Vector/Cities/Montreal/CityGML_LOD2_20140115/$model"
   model define $model -georef REF
}

#----- Read a field into which to rasterize the model
file copy -force DataIn/Montreal.fstd DataOut/Montreal.fstd
fstdfile open FILE append DataOut/Montreal.fstd
fstdfield read FLD FILE -1 "" -1 -1 -1 "" "IBLK"

#----- Clear field
fstdfield clear FLD 0.0

#----- Rasterize the model (max height)
foreach model $models {
   puts "   Rasterizing $model"
   fstdfield gridinterp FLD $model FAST
}

#----- Save mask
fstdfield define FLD -NOMVAR MASK
fstdfield write FLD FILE 0 True
fstdfile close FILE


Log::End