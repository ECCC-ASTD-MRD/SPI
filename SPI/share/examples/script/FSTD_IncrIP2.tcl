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
# Fichier    : FSTD_IncrIP2.tcl
# Creation   : Mai 2000 - J.P. Gauthier - CMC/CMOE
# Description: Incrementer les IP2 de 24 heures
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

#----- Ouvrir les fichiers d'entree (1) sortie (2)
fstdfile open 1 read  $env(CI_DATA_IN)/2005102612_012
fstdfile open 2 write $env(CI_DATA_OUT)/FSTD_IncrIP2.fstd

#----- Boucler sur les champs a incrementer
foreach var { TT UU } {

   #----- lire le champs
   fstdfield read FLD 1 -1 "" -1 -1 -1  "" $var

   #----- Recuperer le ip2
   set ip2 [fstdfield define FLD -IP2]

   #----- Incrementer le ip2
   fstdfield define FLD -IP2 [incr ip2 24]
   fstdfield write FLD 2 0 True
}

fstdfile close 1 2

Log::End