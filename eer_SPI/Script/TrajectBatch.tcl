#!/bin/sh
# the next line restarts using tclsh \
exec $SPI_PATH/tclsh "$0" "$@"
#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet     : Interface pour le modele Trajectoire.
# Fichier    : TrajectBatch.tcl
# Creation   : Decembre 2000 - J.P. Gauthier - CMC/CMOE
#
# Description: Permet de lancer des trajectories en mode "batch".
#
#
# Remarques  :
#   Aucune.
#===============================================================================

#----- Definitions des donnees

set Path   [lindex $argv 0]    ;#Repertoire de travail
set Bin    [lindex $argv 1]    ;#Modele a utiliser
set DeltaS [lindex $argv 2]    ;#Delta de depart des trajectoires (Heures)
set DeltaL [lindex $argv 3]    ;#Delta de duree des trajectoires  (Heures)
set Date0  [lindex $argv 4]    ;#Date de depart en (Heures)
set Retro  [lindex $argv 5]    ;#Mode

cd $Path

#----- Recherche de l'index de la date

proc GetDateIdx { Dates Date T } {

   #----- On recherche la date disponible la plus proche de la date desiree

   set idx 0
   while { 1 } {
      if { $Date==[lindex $Dates $idx] } {
         break
      }
      if { $Date>[lindex $Dates $idx] } {
         if { $Date<[lindex $Dates [expr $idx+1]] } {
       if { $T } {
               incr idx
            }
            break
         }
         if { $Date==[lindex $Dates [expr $idx+1]] } {
            incr idx
            break
         }
      }

      incr idx

      if { $idx>[llength $Dates] } {
         break
      }
   }

   #----- On retourne l'index de cette date

   return $idx
}

#----- On recupere les fichiers

set files [exec cat $Path/data_std_sim]
set datel ""

foreach file $files {
   set f [file tail $file]
   set s [split $f _]
   set d [string range [exec r.date [lindex $s 0] +[lindex $s 1]] 0 9]
   lappend datel $d
}

#----- Determiner les dates limites

puts "Dates disponibles: $datel"
set DateS [lindex $datel 0]
set DateE [lindex $datel end]

if { $Retro } {
   set Date1 [string range [exec r.date $Date0 -$DeltaL] 0 9]
} else {
   set Date1 [string range [exec r.date $Date0 +$DeltaL] 0 9]
}

#----- On boucle sur le range de dates

while { ($Date0<=$DateE && !$Retro) || ($Date1>=$DateS && $Retro) } {

   #----- Determiner la fin de cette trajectoire

   if { $Retro } {
      set data [lrange $files [GetDateIdx $datel $Date1 0] [GetDateIdx $datel $Date0 1]]
      puts "DWJOB Retro Traj from $Date0 to $Date1 for $DeltaL\n$data"
   } else {
      set data [lrange $files [GetDateIdx $datel $Date0 0] [GetDateIdx $datel $Date1 1]]
      puts "DWJOB Traj from $Date0 to $Date1 for $DeltaL\n$data"
   }

   #----- Creer le fichier de directives

   set YYYY [string range $Date0 0 3]
   set MM   [string range $Date0 4 5]
   set DD   [string range $Date0 6 7]
   set HH   [string range $Date0 8 9]

   set entre [split [exec cat entre] "\n"]
   set entre [lrange $entre  0 [expr [llength $entre]-5]]

   set f [open entre.$Date0 w]
   foreach line $entre {
      puts  $f $line
   }

   puts $f "${YYYY}     Annee de l'accident\n${MM}       Mois de l'accident\n${DD}       Jour de l'accident\n${HH}       Heure de l'accident"
   close $f

   #----- Lancer le modele

   catch { eval exec $env(SPI_PATH)/Bin/$env(ARCH)/$Bin -i entre.$Date0 -fich10 $data -o traject.points.$Date0 > out.$Date0 }

   #----- Determiner la date d'anal-prog

   exec $env(SPI_PATH)/Script/TrajectSplit.tcl $Path traject.points.$Date0

   #----- Ajouter au fichier resultat

   exec cat traject.points.$Date0 >> traject.points
   file delete traject.points.$Date0

   #----- Incrementer la date de depart/arrivee de la prochaine trajectoire

   if { $Retro } {
      set Date0 [string range [exec r.date $Date0 -$DeltaS] 0 9]
      set Date1 [string range [exec r.date $Date0 -$DeltaL] 0 9]
   } else {
      set Date0 [string range [exec r.date $Date0 +$DeltaS] 0 9]
      set Date1 [string range [exec r.date $Date0 +$DeltaL] 0 9]
   }
}

puts "DWJOB Traitement termine"

exit 0