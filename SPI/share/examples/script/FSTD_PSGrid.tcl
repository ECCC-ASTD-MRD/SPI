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
# Fichier    : FSTD_PSGrid.tcl
# Creation   : Mars 2005 - J.P. Gauthier - CMC/CMOE
# Description: Creer un champs sur une grille PS
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

#----- Definier les parametre de la grille
set NI    340
set NJ    240
set nhem  1
set dlat  44.852
set dlon -98.424
set dd60  1.0
set xg3   21000.0

#---- Calculer les parametres xg necessaires
set xg4 [expr (270.0-$dlon+360.0)/360.0]
set xg4 [expr ($xg4-floor($xg4))*360.0]

set xy  [fstdgrid xyfll $dlat $dlon $dd60 $xg4 $nhem]

set xg1 [expr ((($NI-1.0)/2.0) * $xg3 - [lindex $xy 0]) / $xg3 + 1.0]
set xg2 [expr ((($NJ-1.0)/2.0) * $xg3 - [lindex $xy 1]) / $xg3 + 1.0]

#----- Creer le champs
fstdfield create GRID $NI $NJ 1

#----- Lui assigner une grille PS nord
Log::Print DEBUG "$xg1 $xg2 $xg3 $xg4"
fstdfield define GRID -GRTYP N $xg1 $xg2 $xg3 $xg4

#----- Initialiser certains paramtres
fstdfield define GRID -DEET 0 -NPAS 0 -IP1 0 -IP2 0 -IP3 0 -ETIKET "GRID" -DATYP 2 -NOMVAR GRID -TYPVAR X

#----- Sauvegrader le champs

fstdfile open 2 write DataOut/FSTD_PSGrid.fstd
fstdfield write GRID 2 -16 True

fstdfile close 2

Log::End