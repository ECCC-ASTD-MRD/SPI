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
# Fichier    : GDAL_Slope.tcl
# Creation   : Mars 2007 - J.P. Gauthier - CMC/CMOE
# Description: Demonstration des fonctions de calculs de pente, aspect, ...
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

gdalfile error QUIET

#----- Ouverture d'un fichier GTIF
set bands [gdalfile open FILE read $env(CI_SPI_IN)/srtm_n045w074_badmedian3x3]
gdalband read BAND $bands

#----- Calcul de la pente en degree
vexpr SLOPE dslopedeg(BAND)
gdalband configure SLOPE -desc "Slope in degree"

#----- Calcul de la pente en pourcentage
vexpr SLOPE100 dslope100(BAND)
gdalband configure SLOPE100 -desc "Slope in percent"

#----- Calcul de la courbature du profile
vexpr PCURV dprofcurve(BAND)
gdalband configure PCURV -desc "Profile curvature"

#----- Calcul de la courbarture tangentielle
vexpr TCURV dtangcurve(BAND)
gdalband configure TCURV -desc "Tangential curvature"

#----- Calcul de l'aspect (angle de la pente en XY)
vexpr ASPEC daspect(BAND)
gdalband configure ASPEC -desc "Aspect in degree"

#----- Calcul de la derivee partielle de deuxieme ordre dxy
vexpr DXX   ddxysecond(BAND)
gdalband configure DXX -desc "Second order partial dxy derivative"

#----- Sauvegrader les resultata
catch { file delete $env(CI_SPI_OUT)/GDAL_Slope.tif }
gdalfile open FILEOUT write $env(CI_SPI_OUT)/GDAL_Slope.tif "GeoTIFF"
gdalband write { SLOPE SLOPE100 PCURV TCURV ASPEC DXX } FILEOUT
gdalfile metadata FILEOUT { Test1=yoyoy TEST2=yaya }
gdalfile close FILEOUT

Log::End
