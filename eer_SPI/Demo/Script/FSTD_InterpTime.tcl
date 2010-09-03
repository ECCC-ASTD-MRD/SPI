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
# Fichier    : FSTD_InterpTime.tcl
# Creation   : Mai 2000 - J.P. Gauthier - CMC/CMOE
# Description: Interpoler des chamsp dans le temps
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
fstdfile open 2 write DataOut/FSTD_InterpTime.fstd

#----- Lire les champs aux temps T(0) et T(1)

fstdfield read T0 1 -1 "" 12000 18 -1  "" ES
fstdfield read T1 1 -1 "" 12000 24 -1  "" ES

#----- Recuperer la date de validitee au temps T(0)

set stamp0 [fstdfield define T0 -DATEV]

#----- Interpoler a toute les demi-heure sur 3 heures

for { set hour 0.1 } { $hour < 3 } { set hour [expr $hour+0.1] } {

   set stamp [fstdstamp incr $stamp0 $hour]
   fstdfield timeinterp TX T0 T1 $stamp
   fstdfield write TX 2 -32 False
}

fstdfile close 1 2
