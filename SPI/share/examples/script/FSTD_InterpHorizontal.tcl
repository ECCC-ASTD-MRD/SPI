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

file delete $env(CI_DATA_OUT)/FSTD_InterpHorizontal.fstd

#----- Ouvrir les fichiers d'entree (1) sortie (2)
fstdfile open FILEIN read  $env(CI_DATA_IN)/2005102612_012
fstdfile open FILEOUT write $env(CI_DATA_OUT)/FSTD_InterpHorizontal.fstd

#----- Creer un champs sur grille PS de 229x229 en specifiant les IG1 IG2 IG3 et IG4
fstdfield create GRID 229 229 1
fstdfield define GRID -GRTYP N 115.0 300.0 25000.0 350.0

#----- Boucler sur les champs a interpoler
foreach var { TT UU GZ } {
   fstdfield read FROM FILEIN -1 "" -1 -1 -1  "" $var
   fstdfield gridinterp GRID FROM
   fstdfield write GRID FILEOUT -16 True
}

#----- Teste l'extrapolation
fstdfield stats FROM -nodata 10.0
fstdfield clear FROM
fstdfield configure FROM -extrapdegree VALUE
fstdfield gridinterp FROM GRID
fstdfield write FROM FILEOUT -32 False

#----- Test l'interpolation dans une grille M
fstdfield read FROM FILEIN -1 "" -1 -1 -1  "" TT
fstdfile open FILEMSH read $env(CI_DATA_IN)/shop.fst
fstdfield read MESHE FILEMSH -1 "" -1 -1 -1 "" WVMD
fstdfield copydesc MESHE FILEOUT
fstdfield gridinterp MESHE FROM
fstdfield write MESHE FILEOUT -16 True

fstdfile close FILEIN FILEOUT FILEMSH

Log::End