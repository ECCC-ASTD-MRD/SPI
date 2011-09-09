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
# Fichier    : FSTD_DarinDensity.tcl
# Creation   : Mars 2007 - J.P. Gauthier - CMC/CMOE
# Description: Calcule la densite de drainage sur une grille RPN en utilisant
#              des donnees vectorielles de lacs et rivieres (Shapefile)
#
#      somme(longueur cours d'eau) + somme(périmètre étendues d'eau) DD
#      -------------------------------------------------------------
#              aire de la maille - aire des étendues d'eau
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

#----- Ouvrir les fichiers d'entree et de sortie

catch { file delete -force DataOut/FSTD_DrainDensity.fstd }
fstdfile open FSTDOUT write DataOut/FSTD_DrainDensity.fstd
fstdfile open FSTDIN  read  DataIn/noire.fst

#----- Copier les TIC TAC

fstdfield read TIC FSTDIN -1 "" -1 -1 -1  "" ">>"
fstdfield read TAC FSTDIN -1 "" -1 -1 -1  "" "^^"
fstdfield write TIC FSTDOUT -32 False
fstdfield write TAC FSTDOUT -32 False

#----- Lire le champs de la grille de calcul

fstdfield read FLD FSTDIN -1 "" -1 -1 -1  "" "ELEV"

#----- Creer les champs de calculs

vexpr FLD      FLD<<0
vexpr RIVERSUM FLD<<0
vexpr LAKESUM  FLD<<0
vexpr LAKEAREA FLD<<0
vexpr MASK     FLD<<0

#----- Lire la donnee des rivieres

set layers [ogrfile open FILE1 read DataIn/noire_lines.shp]
ogrlayer read LINES FILE1 0

#----- Lire la donnee des lacs

set layers [ogrfile open FILE2 read DataIn/noire_areas.shp]
ogrlayer read AREAS FILE2 0

#----- Lire la donnee du bassin (masque)

set layers [ogrfile open FILE3 read DataIn/noire_sousbasssins.shp]
ogrlayer read BASSIN FILE3 0

#----- Interpolation (rasterisation) selon les longueurs et couvertures

fstdfield gridinterp RIVERSUM LINES  LENGTH_CONSERVATIVE FEATURE_LENGTH
fstdfield gridinterp LAKESUM  AREAS  LENGTH_CONSERVATIVE FEATURE_LENGTH
fstdfield gridinterp LAKEAREA AREAS  CONSERVATIVE FEATURE_AREA
fstdfield gridinterp MASK     BASSIN FAST

#----- Effectuer le calculs de drainage

vexpr FLD ifelse(MASK,(RIVERSUM+LAKESUM)/(darea(FLD)-LAKEAREA),0)

#----- Sauvagerdons le tout

fstdfield define FLD -NOMVAR DRN
fstdfield write FLD FSTDOUT -32 False
fstdfield define RIVERSUM -NOMVAR RSUM
fstdfield write RIVERSUM FSTDOUT -32 False
fstdfield define LAKESUM -NOMVAR LSUM
fstdfield write LAKESUM FSTDOUT -32 False
fstdfield define LAKEAREA -NOMVAR LARE
fstdfield write LAKEAREA FSTDOUT -32 False

fstdfile close FSTDIN FSTDOUT
