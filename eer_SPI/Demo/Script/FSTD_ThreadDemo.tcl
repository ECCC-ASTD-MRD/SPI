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
# Fichier    : FSTD_ThreadDemo.tcl
# Creation   : Mars 2005 - J.P. Gauthier - CMC/CMOE
# Description: Copier des champs sur 4 thread
#
# Parametres :
#
# Retour:
#
# Remarques  :
#
#============================================================================

puts \n[file tail [info script]]

#----- On verifie si les threads son activees dans TCL

if { !$tcl_platform(threaded) } {
   puts stderr "   Thread not enabled"
   exit 1
}

package require TclData

#----- Inclure le package de manipulation des threads

package require Thread

#----- Procedure de creation d'une thread

proc ThreadTest { } {

   #----- Creer la thread en lui passant le code a executer

   return [thread::create -joinable {

      #----- Chaque thread est indepandante alors inclure les extensions dans la thread courante

      package require TclData

      #----- Definir les procedures de la thread a etre executer plus tard

      proc read { File No } {
         global env

         set fldid FLD[thread::id]
         set flds  [fstdfile open $No read $File]

         foreach fld $flds {
            fstdfield read $fldid $No [lindex $fld 1]
            puts "   [thread::id] $fld [fstdfield stats $fldid -gridvalue 1 1]"
         }

         fstdfile close $No
      }

      proc readvar { FileId VAR } {
         global env

         set fldid FLD[thread::id]

         foreach fld [fstdfield find $FileId -1 "" -1 -1 -1 "" $VAR] {
            fstdfield read $fldid $FileId $fld
            puts "   [thread::id] $VAR $fld [fstdfield stats $fldid -gridvalue 1 1]"
         }
      }

      #----- mettre la thread en attente

      thread::wait
   }]
}

#----- Creer 4 thread

set tid1 [ThreadTest]
set tid2 [ThreadTest]
set tid3 [ThreadTest]
set tid4 [ThreadTest]

#thread::send -async $tid1 "read $path/2003090412_000m 1"
#thread::send -async $tid2 "read $path/2003090412_000m 2"
#thread::send -async $tid3 "read $path/2003090412_024c 3"
#thread::send -async $tid4 "read $path/2003090412_036c 4"

#----- Ouvrir les fichiers d'entree (1)

fstdfile open 1 read  DataIn/2005102612_012c

#----- Activer les thread en leur donnant un travaile

thread::send -async $tid1 "readvar 1 WD"
thread::send -async $tid2 "readvar 1 DD"
thread::send -async $tid3 "readvar 1 TT"
thread::send -async $tid4 "readvar 1 AV"

#----- Terminer les thread apres leur traitement

thread::send -async $tid1 "thread::exit"
thread::send -async $tid2 "thread::exit"
thread::send -async $tid3 "thread::exit"
thread::send -async $tid4 "thread::exit"

#----- Attendre la fin de toute les threads avant de quitter

thread::join $tid1
thread::join $tid2
thread::join $tid3
thread::join $tid4

fstdfile close 1
