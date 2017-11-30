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
#package require TclGeoEER
package require Logger

Log::Start [info script] 0.1

#----- Read the BURP (BUFR) Table
metobs table -readcmc
puts "   Old code 4065 = [metobs table -desc 4065]"

metobs table -desc 4065 "Changed to my new item" "Some units"
puts "   New code 4065 = [metobs table -desc 4065]"

metobs table -insert 69240 "New item added" "Some new added units"
puts "   New code 69240 = [metobs table -desc 69240]"

#----- Create a new obs
metobs create NEWOBS
metobs define NEWOBS -ID "Position 1"
metobs define NEWOBS -COORD "Position 1"  35.95 -95.66 100
metobs define NEWOBS -ELEMENT "Position 1" 24005 0 { 10 40 50 }
metobs define NEWOBS -ELEMENT "Position 1" 24005 1 20
metobs define NEWOBS -ELEMENT "Position 1" 5022 3 -95

puts "Wind values :[metobs define NEWOBS -ELEMENT "Position 1" 24005]"
puts "5022 value  :[metobs define NEWOBS -ELEMENT "Position 1" 5022 ]"

#----- Create a metobs object and read a file
metobs create BURPDATA
#metobs read BURPDATA DataIn/2007021500_
metobs read BURPDATA /space/hall1/sitestore/eccc/cmd/a/jud000/maestro_archives/DG2H17UAAI_pre/banco.derialt.2016121600_ai
#metobs read BURPDATA /users/dor/afsr/005/Scripts/For/JeanMarc/obs/2011012000_sf

#----- Output global info on the metobs
puts  "   Available info   : [metobs define BURPDATA -INFO]"
puts  "   Nb Stations      : [metobs define BURPDATA -NB]"
puts  "   Available Dates  : [metobs define BURPDATA -DATE]"
puts  "   Available Elems  : [metobs define BURPDATA -ELEMENT]"
puts  "   Stations for date: [llength [metobs define BURPDATA -STATION "" [lindex [metobs define BURPDATA -DATE] 0]]]"
puts  "   Reports for date : [llength [metobs define BURPDATA -REPORT "" [lindex [metobs define BURPDATA -DATE] 0]]]"

#----- Parse the metobs data by elements
puts  "\n   Per stations:\n"

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
}

#----- Parse the metobs data by reports
puts  "\n   Per report:\n"

set idx 0
foreach id [metobs define BURPDATA -ID] {
   puts  "   Station ($idx) $id\t: [metobs define BURPDATA -NO $id] [metobs define BURPDATA -COORD $id]"

   set dates [metobs define BURPDATA -DATE $id]
   foreach date $dates  {
      puts  "      [clock format $date] :"
      foreach report [metobs define BURPDATA -REPORT $id $date] {
         puts  "\n         CODE([metreport define $report -CODETYPE])  BFAM([metreport define $report -FAMILY]) TYP([metreport define $report -TYPE]) STYP([metreport define $report -STYPE])"
          
         foreach elem [metreport define $report -ELEMENT] {
             puts  "         $elem : [metreport define $report -ELEMENT $elem]"
          }
      }
   }
}

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

Log::End
