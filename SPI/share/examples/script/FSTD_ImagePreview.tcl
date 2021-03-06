#!/bin/sh
# the next line restarts using tclsh \
exec $SPI_PATH/wish "$0" "$@"
#============================================================================
# Environnement Canada
# Centre Meteorologique Canadien
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet     : Exemple de scripts.
# Fichier    : FSTD_ImagePreview.tcl
# Creation   : Mars 2005 - J.P. Gauthier - CMC/CMOE
# Description: Recuperer un preview du champs en image TK
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

#----- Creer la palette de rendue
colormap create CMAP
colormap read CMAP $env(CI_DATA_IN)/REC_Col.std1.rgba

#----- Recuperer le champs
fstdfile open 1 read $env(CI_DATA_IN)/2005102612_012
fstdfield read FLD 1 -1 "" -1 -1 -1 "" TT
fstdfield configure FLD -colormap CMAP -intervals { -80 -70 -60 -50 }

#----- Generere le preview
image create photo IMG -width 229 -height 229
fstdfield stats FLD -image IMG

#----- Sauvegarder l'image du preview
IMG write $env(CI_DATA_OUT)/FSTD_ImagePreview.gif

Log::End