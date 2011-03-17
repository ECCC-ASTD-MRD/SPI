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
# Fichier    : OGR_Import.tcl
# Creation   : Fevrier 2007 - J.P. Gauthier - CMC/CMOE
# Description: Importer des donnees FSTD en couche (Shapefile)
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

#----- Lire le champs de temperature

fstdfile open FSTDFILE read DataIn/2005102612_012
fstdfield read FLDTT FSTDFILE -1 "" -1 -1 -1 "" TT
fstdfield read FLDUU FSTDFILE -1 "" -1 -1 -1 "" UU
fstdfield read FLDP0 FSTDFILE -1 "" -1 -1 -1 "" P0

#----- Configurer le type de sortie

fstdfield configure FLDTT -desc Temp -rendertexture 1
fstdfield configure FLDUU -desc Wind -rendervector ARROW
fstdfield configure FLDP0 -desc Pressure

catch { file delete DataOut/OGR_Import.shp }

#----- Creer referentiel polaire stereographique
georef create REF { PROJCS["WGS 84 / UPS North",GEOGCS["WGS 84",DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563]],PRIMEM["Greenwich",0],UNIT["degree",0.0174532925199433]],PROJECTION["Polar_Stereographic"],PARAMETER["latitude_of_origin",90],PARAMETER["central_meridian",0],PARAMETER["scale_factor",0.994],PARAMETER["false_easting",2000000],PARAMETER["false_northing",2000000],UNIT["metre",1]] }

#----- Creer la couche avec le bon referential

ogrfile open OGRFILE write DataOut/OGR_Import.shp "ESRI Shapefile"
ogrlayer create OGRFILE OGRLAYER "Data" REF

#----- Importer les donnees RPN dans la couche

ogrlayer import OGRLAYER [list FLDTT FLDUU FLDP0]

#----- Fermer le fichier afin de sauvegarder le tout

ogrfile close OGRFILE
