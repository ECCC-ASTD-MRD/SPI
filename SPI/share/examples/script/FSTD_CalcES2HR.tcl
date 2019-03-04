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
# Fichier    : FSTD_CalcES2HR.tcl
# Creation   : Mai 2000 - J.P. Gauthier - CMC/CMOE
# Description: Cacluler l'humidite relative
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

fstdfield ip1mode NEW

#----- Ouvrir les fichiers d'entree (1) sortie (2)

file delete -force $env(CI_DATA_OUT)/FSTD_CalcES2HR.fstd

fstdfile open 1 read  $env(CI_DATA_IN)/2005102612_012
fstdfile open 2 write $env(CI_DATA_OUT)/FSTD_CalcES2HR.fstd

foreach field [fstdfield find 1 -1 "" -1 -1 -1 "" "ES"] {

   #----- Lire les champs d'entree

   fstdfield read ES 1 $field
   fstdfield read TT 1 [fstdfield define ES -DATEV] [fstdfield define ES -ETIKET] [fstdfield define ES -IP1] [fstdfield define ES -IP2] [fstdfield define ES -IP3] [fstdfield define ES -TYPVAR] TT

   #----- Effectuer les calculs
   vexpr TT min(TT,-20.0)
   vexpr ES clamp(ES,0.0,20.0)
   vexpr PV 10^(9.4041-2354.0/(TT+273.0))
   vexpr HR 10^(9.4041-2354.0/((TT-ES)+273.0))/PV

   #----- Redifinir le NOMVAR en consequence

   fstdfield define PV -NOMVAR "PV" -IP1 -1
   fstdfield define HR -NOMVAR "HR" -IP1 -1

   #----- Ecrire les champs resultants

   fstdfield write PV 2 -32 True True
   fstdfield write HR 2 -16 True True

}

fstdfile close 1
fstdfile close 2

Log::End