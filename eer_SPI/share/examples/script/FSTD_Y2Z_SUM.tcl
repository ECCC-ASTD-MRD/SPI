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
# Fichier    : FSTD_Y2Z_SUM.tcl
# Creation   : Novembre 2011 - J.P. Gauthier - CMC/CMOE
# Description: Interpoler un champ sur grille Y (nuage de points) sur un grille
#             en faisant le somme par point de grille.
#
# Parametres :
#   <File>   : Fichier standard
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

#-- Liste des especes incluses dans les fichiers d'emissions
set Species { S2 S4 NO N2 }
set Hours   24
set Days    2

file delete DataOut/FSTD_Y2Z_SUM.fstd

fstdfile open YFILE      read  DataIn/major.fstd
fstdfile open GRIDFILE   read  DataIn/2005120600_012
fstdfile open RESULTFILE write DataOut/FSTD_Y2Z_SUM.fstd

#-- Copie des tictac
foreach tictac { >> ^^ } {
   fstdfield read GRID GRIDFILE -1 "" -1 -1 -1 "" "$tictac"
   fstdfield write GRID RESULTFILE 0 True

   fstdfield read GRID YFILE -1 "" -1 -1 -1 "" "$tictac"
   fstdfield write GRID RESULTFILE 0 True
}

#-- Champ de travail
fstdfield read GRID GRIDFILE -1 "" -1 -1 -1 "" AC
fstdfield stats GRID -nodata 0.0

# -- On boucle sur les jours de 1 a 7
for { set d 1 } { $d <= $Days } { incr d } {

   # -- On boucle sur les heures de 1 a 24
   for { set t 0 } { $t < $Hours } { incr t } {

      set ip2 [format "%i%02i" $d $t]

      Log::Print INFO "Processing IP2 $ip2"

      foreach specie $Species {
         Log::Print INFO "Processing specie $specie"

         fstdfield read YFIELD YFILE -1 "" 12001 $ip2  -1 "" $specie

         fstdfield clear GRID 0.0
         fstdfield gridinterp GRID YFIELD SUM True
         fstdfield define GRID -NOMVAR $specie -IP1 12001 -IP2 $ip2 -DATEO [fstdfield define YFIELD -DATEO] -ETIKET [fstdfield define YFIELD -ETIKET]

         fstdfield write GRID RESULTFILE -32 True
      }
   }
}

Log::Print INFO "Processing point cloud"
fstdfield read GRID GRIDFILE -1 "" -1 -1 -1 "" O3

fstdfield gridinterp YFIELD GRID MAXIMUM True 
fstdfield define YFIELD -NOMVAR MAX -IP1 12001 -IP2 $ip2 -DATEO [fstdfield define YFIELD -DATEO] -ETIKET [fstdfield define YFIELD -ETIKET]
fstdfield write YFIELD RESULTFILE -32 True

fstdfield gridinterp YFIELD GRID MINIMUM True 
fstdfield define YFIELD -NOMVAR MIN -IP1 12001 -IP2 $ip2 -DATEO [fstdfield define YFIELD -DATEO] -ETIKET [fstdfield define YFIELD -ETIKET]
fstdfield write YFIELD RESULTFILE -32 True

fstdfield gridinterp YFIELD GRID AVERAGE True 
fstdfield define YFIELD -NOMVAR AVG -IP1 12001 -IP2 $ip2 -DATEO [fstdfield define YFIELD -DATEO] -ETIKET [fstdfield define YFIELD -ETIKET]
fstdfield write YFIELD RESULTFILE -32 True

fstdfile close YFILE GRIDFILE RESULTFILE

Log::End
