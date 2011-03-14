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
# Fichier    : OBS_BURP.tcl
# Creation   : Novembre 2006 - J.P. Gauthier - CMC/CMOE
# Description: Démonstrations des fonctionnalitées des objets metobs (BURP/BUFR)
#
# Parametres :
#   <file>   : Burp file to read
# Retour:
#
# Remarques  :
#
#============================================================================

package require TclData

puts \n[file tail [info script]]

#----- Read the BURP (BUFR) Table

metobs table -readcmc
puts "   Old code 4065 = [metobs table -desc 4065]"

metobs table -desc 4065 "Changed to my new item" "Some units"
puts "   New code 4065 = [metobs table -desc 4065]"

metobs table -insert 69240 "New item added" "Some new added units"
puts "   New code 69240 = [metobs table -desc 69240]"

#----- Create a metobs object and read a file
metobs create BURPDATA
metobs read BURPDATA DataIn/2007021500_

#----- Output global info on the metobs

puts  "   Available info   : [metobs define BURPDATA -INFO]"
puts  "   Nb Stations      : [metobs define BURPDATA -NB]"
puts  "   Available Dates  : [metobs define BURPDATA -DATE]"
puts  "   Available Elems  : [metobs define BURPDATA -ELEMENT]"

#----- Parse the metobs data by elements

set idx 0
foreach id [metobs define BURPDATA -ID] {
   puts  "   Station ($idx) $id\t: [metobs define BURPDATA -NO $id] [metobs define BURPDATA -COORD $id]"

   set dates [metobs define BURPDATA -DATE $id]
   set elems [metobs define BURPDATA -ELEMENT $id]

   foreach date $dates  {
      puts  "      [clock format $date] :"
      foreach elem $elems {
          puts  "         $elem : [metobs define BURPDATA -ELEMENT $id $elem $date]"
      }
   }
   if { [incr idx]>10 } {
      break
   }
}

#----- Parse the metobs data by reports

set idx 0
foreach id [metobs define BURPDATA -ID] {
   puts  "   Station ($idx) $id\t: [metobs define BURPDATA -NO $id] [metobs define BURPDATA -COORD $id]"

   set dates [metobs define BURPDATA -DATE $id]
   foreach date $dates  {
      puts  "      [clock format $date] :"
      foreach report [metobs define BURPDATA -REPORT $id $date] {
         foreach elem [metreport define $report -ELEMENT] {
             puts  "         $elem : [metreport define $report -ELEMENT $elem]"
          }
      }
   }
   if { [incr idx]>10 } {
      break
   }
}

#----- Create a new obs

metobs create NEWOBS
metobs define NEWOBS -ID "Position 1"
metobs define NEWOBS -COORD "Position 1"  35.95 -95.66 100
metobs define NEWOBS -ELEMENT "Position 1" 24005 0 { 10 40 50 }
metobs define NEWOBS -ELEMENT "Position 1" 24005 1 20
metobs define NEWOBS -ELEMENT "Position 1" 5022 3 -95

puts [metobs define NEWOBS -ELEMENT "Position 1" "DIRECTION DU VENT A 10M" ]
puts [metobs define NEWOBS -ELEMENT "Position 1" 5022 ]

#----- Test the krigging

catch { file delete -force DataOut/OBS_BURP_Krig.fstd }
fstdfile open 1 read  DataIn/pression.fstd
fstdfile open 2 write DataOut/OBS_BURP_Krig.fstd

#----- Recuperer le champs pour la grille d'interpolation

fstdfield read FLD 1 -1 "" -1 -1 -1 "" ""

#----- Interpoler dans une grille (kriging)

fstdfield gridinterp FLD BURPDATA [clock seconds] 11012 LINEAR 0.0 1.0 10
fstdfield write FLD 2 -32 False
fstdfile close 2
