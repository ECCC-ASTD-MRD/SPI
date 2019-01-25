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

package require Logger

Log::Start [info script] 0.1

#----- On verifie si les threads son activees dans TCL

if { !$tcl_platform(threaded) } {
   Log::Print ERROR "Thread not enabled"
   Log::End 1
}

package require TclData

#----- Inclure le package de manipulation des threads

package require Thread

#----- Procedure de creation d'une thread

proc ThreadTest { } {

   #----- Creer la thread en lui passant le code a executer

   return [thread::create -joinable {

   for { set i 0 } { $i <10000 } { incr i } {
  tsv::set toto tata$i 30
#tsv::lock toto { catch { tsv::unset toto } }
#if { [tsv::names toto]!="" } {
#puts stderr [tsv::names toto]
 # tsv::unset toto 
#}
#puts [tsv::unset toto ddd]
#tsv::array reset toto {}
catch { tsv::unset toto } 
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

thread::send -async $tid1 "thread::exit"
thread::send -async $tid2 "thread::exit"
thread::send -async $tid3 "thread::exit"
thread::send -async $tid4 "thread::exit"

#----- Attendre la fin de toute les threads avant de quitter

thread::join $tid1
thread::join $tid2
thread::join $tid3
thread::join $tid4

Log::End
