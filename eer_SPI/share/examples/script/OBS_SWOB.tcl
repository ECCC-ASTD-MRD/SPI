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
# Fichier    : OBS_SWOB.tcl
# Creation   : Mai 2008 - J.P. Gauthier - CMC/CMOE
# Description: Test de lecture de metobs SWOB
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

#----- Read standard table set
metobs table -readcmc

metobs create SWOBDATA DataIn/71216.xml

puts [metobs define SWOBDATA -ELEMENT]

#----- Output global info on the metobs
puts  "   Available info   : [metobs define SWOBDATA -INFO]"
puts  "   Nb Stations      : [metobs define SWOBDATA -NB]"
puts  "   Available Dates  : [metobs define SWOBDATA -DATE]"
puts  "   Available Elems  : [metobs define SWOBDATA -ELEMENT]"

#----- Parse the metobs data by elements
set idx 0
foreach id [metobs define SWOBDATA -ID] {
   puts  "   Station ($idx) $id\t: [metobs define SWOBDATA -NO $id] [metobs define SWOBDATA -COORD $id]"

   set dates [metobs define SWOBDATA -DATE $id]
   set elems [metobs define SWOBDATA -ELEMENT $id]

   foreach date $dates  {
      puts  "      [clock format $date] :"
      foreach elem $elems {
          puts  "         $elem : [metobs define SWOBDATA -ELEMENT $id $elem $date]"
      }
   }
   if { [incr idx]>10 } {
      break
   }
}

#----- Parse the metobs data by reports
set idx 0
foreach id [metobs define SWOBDATA -ID] {
   puts  "   Station ($idx) $id\t: [metobs define SWOBDATA -NO $id] [metobs define SWOBDATA -COORD $id]"

   set dates [metobs define SWOBDATA -DATE $id]
   foreach date $dates  {
      puts  "      [clock format $date] :"
      foreach report [metobs define SWOBDATA -REPORT $id $date] {
         foreach elem [metreport define $report -ELEMENT] {
             puts  "         $elem : [metreport define $report -ELEMENT $elem]"
          }
      }
   }
   if { [incr idx]>10 } {
      break
   }
}

Log::End