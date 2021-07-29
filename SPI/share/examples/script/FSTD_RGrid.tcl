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
# Fichier    : FSTD_InterpHorizontal.tcl
# Creation   : Mai 2000 - J.P. Gauthier - CMC/CMOE
# Description: Interpoler des champs dans l'horizontale sur unr grille
#              polaire stereographique nord de 229 par 229
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

file delete $env(CI_DATA_OUT)/FSTD_RGrid.fstd

#----- Ouvrir les fichiers d'entree (1) sortie (2)
fstdfile open FILEIN read  $env(CI_DATA_IN)/2005102612_012
fstdfile open FILEOUT write $env(CI_DATA_OUT)/FSTD_RGrid.fstd

#----- Creer un champs sur grille Radar
fstdfield create GRID 360 48 1
fstdfield define GRID -GRTYP R 45.53 -75.63 0.0 240 5.0 1.0

#----- Boucler sur les champs a interpoler
foreach var { TT UU GZ } {
   fstdfield read FROM FILEIN -1 "" -1 -1 -1  "" $var
   fstdfield gridinterp GRID FROM
   fstdfield write GRID FILEOUT -16 True
}

fstdfile close FILEIN FILEOUT

Log::End