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
# Fichier    : FSTD_8HourAverage.tcl
# Creation   : Mai 2000 - J.P. Gauthier - CMC/CMOE
# Description: Demontrer les fonctions de calcul grille (Wrapper RPN)
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

set fields [fstdfile open 1 read  DataIn/2005120600_012]
fstdfile open 2 write DataOut/FSTD_8HourAverage.fstd

#----- Recuperer les TICTAC

fstdfield read TIC 1 -1 "" -1 -1 -1 "" >>
fstdfield read TAC 1 -1 "" -1 -1 -1 "" ^^
fstdfield write TIC 2 0 1
fstdfield write TAC 2 0 1

#----- Recuperer les O3

set f ""
foreach field $fields {
   if { [lindex $field 2]=="O3" } {
      lappend f $field
   }
}

set fields $f

#----- Trier en ordre croissant de IP2

set fields [lsort -index 5 -integer -increasing $fields]


for { set i 7 } { $i < [llength $fields] } { incr i } {

   #----- Pour un range de 8 pas de temps

   set range [lrange $fields [expr $i-7] $i]

   fstdfield free TOT

   #----- Totaliser les champs

   foreach field $range {

      fstdfield read FLD 1 [lindex $field 1]
      if { [fstdfield is TOT] } {
         vexpr TOT TOT+FLD
      } else {
         fstdfield copy TOT FLD
      }
   }

   #----- Diviser

   vexpr TOT TOT/8.0

   #----- Parametres de la derniere heure du range

   fstdfield define TOT -NOMVAR "O38H" -NPAS [fstdfield define FLD -NPAS] -IP2 [fstdfield define FLD -IP2]
   fstdfield write TOT 2 -32 1
}

fstdfile close 1
fstdfile close 2
