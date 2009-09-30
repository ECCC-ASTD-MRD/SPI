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

#----- Creer la couche Shapefile

catch { file delete DataOut/OGR_Import.shp }

ogrfile open OGRFILE write DataOut/OGR_Import.shp "ESRI Shapefile"
ogrlayer create OGRFILE OGRLAYER "Data"

#----- Importer les donnees RPN dans la couche

ogrlayer import OGRLAYER [list FLDTT FLDUU FLDP0]

#----- Fermer le fichier afin de sauvegarder le tout

ogrfile close OGRFILE
