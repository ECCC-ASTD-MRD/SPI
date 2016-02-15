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
# Fichier    : FSTD_ETA2SIGMA.tcl
# Creation   : Mai 2000 - J.P. Gauthier - CMC/CMOE
# Description: Interpoler des chamsp dans la verticale sur des niveaux sigma
#
# Parametres :
#
# Retour:
#
# Remarques  :
#
# Modifications :
#
#============================================================================

package require TclData
#package require TclGeoEER
package require Logger

Log::Start [info script] 0.1

#fstdfield ip1mode NEW

file delete -force DataOut/FSTD_ETA2SIMGA.fstd

#----- Ouvrir les fichiers d'entree (1) sortie (2)

fstdfile open 1 read  DataIn/2005102612_012
fstdfile open 2 write DataOut/FSTD_ETA2SIMGA.fstd

#----- Liste des niveaux sur lesquels on veut interpoler

set sigma { 0.10 0.15 0.20 0.25 0.30 0.35 0.40 0.45 0.50 0.55 0.60 0.63 0.66 0.67 0.72 0.75 0.78 0.80 0.81
            0.82 0.83 0.84 0.85 0.86 0.87 0.88 0.89 0.90 0.91 0.92 0.93 0.94 0.95 0.96 0.97 0.98 0.99 1.00  }

#----- On a besoin de la pression au sol pour interpoler

fstdfield read P0 1 -1 "" -1 -1 -1  "" P0

#----- Creer le champs dans lequel nous allons interpoler
fstdfield create TO [fstdfield define P0 -NI] [fstdfield define P0 -NJ] [llength $sigma]
fstdfield stats  TO -leveltype SIGMA -levels $sigma
fstdfield configure TO -interpdegree LINEAR

#----- Boucler sure les champs a interpoler

foreach var { TT UU GZ } {
   fstdfield read FROM 1 -1 "" -1 -1 -1  "" $var
   fstdfield readcube FROM
   fstdfield stats  FROM -leveltype ETA -top 10.0
   fstdfield verticalinterp TO FROM P0 P0
   fstdfield write TO 2 -32 True
}

fstdfile close 1
fstdfile close 2

Log::End