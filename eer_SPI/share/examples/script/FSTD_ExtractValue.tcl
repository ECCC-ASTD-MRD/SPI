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
# Fichier    : FSTD_ExtractValue.tcl
# Creation   : Mai 2000 - J.P. Gauthier - CMC/CMOE
# Description: Extraire la valeur des champs a certains points et formatter le tout
#              dans un fichier ascii
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

puts \n[file tail [info script]]

#----- Ouvrir les fichiers d'entree (1) sortie (2)

fstdfile open 1 read  DataIn/2005102612_012

#----- List des point de grille (i,j) qui nous interessent

set ijs  { 172 216 172 217 172 218 172 219
           173 216 173 217 173 218 173 219
           174 216 174 217 174 218 174 219  }

#----- Procedure d'extraction de valeur

proc extract { Field File } {
   global ijs

   #----- Ouvrir le fichier de sortie

   set f [open $File w]

   #----- Ecrire l'entete

   puts $f "   #i j lat lon value"

   #----- Boucler sur les coordonnees qui nous interesse

   foreach { i j } $ijs {

      #----- Extraire la valeur au point de grille

      set val [fstdfield stats $Field -gridvalue $i $j]

      #----- Recuperre la coordonnee latlon du point de grille

      set coords [fstdfield stats $Field -gridpoint $i $j]

      #----- Ecrire la ligne

      puts $f "   $i $j [lindex $coords 0] [lindex $coords 1] $val"
   }

   close $f
}

#----- Composante UU du vent en metre/s
fstdfield vector { UU VV }

fstdfield read FLD 1 -1 "" -1 -1 -1 "" "UU"
vexpr FLDUU FLD\[0\]*.5144
extract FLDUU DataOut/UU.0.dat

#----- Composante VV du vent en metre/s

vexpr FLDVV FLD\[1\]*.5144
extract FLDVV DataOut/VV.0.dat

#----- Mouvement vertical en metre/s

fstdfield read FLD 1 -1 "" -1 -1 -1 "" "WE"
extract FLD DataOut/FSTD_ExtractValue.txt
