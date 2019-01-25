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
# Fichier    : FSTD_InterpCube.tcl
# Creation   : Mai 2000 - J.P. Gauthier - CMC/CMOE
# Description: Interpoler des champs dans l'horizontaleet la verticale sur
#              des niveaux galchen.
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

#----- Ouvrir les fichiers d'entree (1) sortie (2)
fstdfile open 1 read  DataIn/2005102612_012
fstdfile open 2 write DataOut/FSTD_InterpCube.fstd

#----- Liste des niveaux galchen dans lesquels on interpole
set levels { 0 15 55 120 195 285 390 510 650 815 1010 1235 1500 1810 2170 2590 3085
           3660 4335 5126 6045 7121 8380 9850 11560 13574 15920 18671 21890 25635 }

#----- Creer un champs sur niveaux galchen
fstdfield create TO 229 229 [llength $levels]
fstdfield stats  TO -leveltype GALCHEN -levels $levels -top 30000

#----- Creer un champs sur grille PS de 229x229 en specifiant les IG1 IG2 IG3 et IG4
fstdfield create GRID 229 229 1
fstdfield define GRID -GRTYP N 115.0 300.0 25000.0 350.0

#----- Lire le cube de GZ (On a besoin des GZ pour passer de pression/sigma/eta a galchen/meter)
fstdfield read GZFROM 1 -1 "" -1 -1 -1  "" GZ
fstdfield readcube GZFROM

#----- Interpoler horizontalement les GZ
fstdfield gridinterp GRID GZFROM
fstdfield copy GZFROM GRID
fstdfield write GZFROM 2 -32 True

#----- Boucler sur les champs a interpoler
foreach var { TT UU GZ } {

   #----- Lire tout les niveaux du champs
   fstdfield read FROM 1 -1 "" -1 -1 -1  "" $var
   fstdfield readcube FROM

   #----- Interpoler horizontalement le cube de donnees
   fstdfield gridinterp GRID FROM

   #----- Interpoler verticalement sur les niveaux galchen
   fstdfield verticalinterp TO GRID GZFROM GZFROM
   fstdfield write TO 2 -32 True
}

fstdfile close 1 2

Log::End