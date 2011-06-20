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

puts \n[file tail [info script]]

#----- Ouvrir les fichiers d'entree (1) sortie (2)

fstdfile open 1 read  DataIn/2005102612_012
fstdfile open 2 write DataOut/FSTD_CalcES2HR.fstd

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

   fstdfield define PV -NOMVAR "PV"
   fstdfield define HR -NOMVAR "HR"

   #----- Ecrire les champs resultants

   fstdfield write PV 2 -16 True
   fstdfield write HR 2 -16 True

}

fstdfile close 1
fstdfile close 2
