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

puts \n[file tail [info script]]

#----- Ouvrir les fichiers d'entree (1) sortie (2)

fstdfile open 1 read  DataIn/2005102612_012
fstdfile open 2 write DataOut/FSTD_InterpHorizontal.fstd

#----- Creer un champs sur grille PS de 229x229 en specifiant les IG1 IG2 IG3 et IG4

fstdfield create GRID 229 229 1
fstdfield define GRID -GRTYP N 115.0 300.0 25000.0 350.0

#----- Boucler sur les champs a interpoler

foreach var { TT UU GZ } {
   fstdfield read FROM 1 -1 "" -1 -1 -1  "" $var
   fstdfield gridinterp GRID FROM
   fstdfield write GRID 2 -16 True
}

fstdfile close 1 2
