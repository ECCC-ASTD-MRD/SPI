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
# Fichier    : FSTD_Flux.tcl
# Creation   : Avril 2005 - J.P. Gauthier - CMC/CMOE
# Description: Calculer le flux traversant une coupr vertical
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

fstdfield vector { UU VV }

#----- Ouvrir les fichiers d'entree (1) sortie (2)
fstdfile open 1 read  DataIn/2005102612_012c

#----- coordonnees de la coupe
set coords { 17.74 -101.26 17.75 -98.59 16.95 -97.96 }
set total 0

#----- Creer une projection geographique afin de "sampler" la coupe aux Km
projection create PROJ

#----- Recuperer le path a chaque km
set path [projection function PROJ -path $coords 1000]

#----- Recuper la distance totale
set dist [projection function PROJ -dist $path 0]

#----- Boucler sur les pas de temps
foreach field [fstdfield find 1 -1 "" 850 -1 -1 "" "CV"] {

   #----- Lire les concentrations et les vents
   fstdfield read CONC 1 $field
   fstdfield read WIND 1 [fstdfield define CONC -DATEV] "" -1 -1 -1 "" "UU"

   #----- Effectuer la coupe verticale pour les concentrations et les vents
   fstdfield vertical XWIND WIND $path
   fstdfield vertical XCONC CONC $path

   #----- Multipler la composante y par la concentration
   vexpr FLUX XWIND\[1\]*XCONC

   #----- Calculer la somme dans le temps
   set total [expr $total+[vexpr FLUX ssum(FLUX)/(3600*3)]]
}

#----- Flux total dans le temps
Log::Print INFO "Flux total: $total"

Log::End