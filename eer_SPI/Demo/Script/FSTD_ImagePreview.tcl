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

puts \n[file tail [info script]]

#----- Creer la palette de rendue

colormap create CMAP
colormap read CMAP DataIn/REC_Col.std1.rgba

#----- Recuperer le champs

fstdfile open 1 read DataIn/2005102612_012
fstdfield read FLD 1 -1 "" -1 -1 -1 "" TT
fstdfield configure FLD -colormap CMAP

#----- Generere le preview

image create photo IMG -width 229 -height 229
fstdfield stats FLD -image IMG

#----- Sauvegarder l'image du preview

IMG write DataOut/FSTD_ImagePreview.gif

exit