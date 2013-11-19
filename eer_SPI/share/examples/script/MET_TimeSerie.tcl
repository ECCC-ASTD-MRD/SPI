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
# Fichier    : FSTD_MeteoTime.tcl
# Creation   : Mai 2000 - J.P. Gauthier - CMC/CMOE
# Description: Extraire une serie temporelle de diverses valeurs
# Parametres :
#   run    : Model run
#   hours  : Nb hours from start of model run
#   lat    : Latitude
#   lon    : Longitude
#   out    : Output file
#
# Retour:
#
# Remarques  :
#
#============================================================================

package require TclData
#package require TclGeoEER
package require Logger

namespace eval Met {
   variable Param

   #----- General parameters
   set Param(Job)     [info script]   ;#Job name
   set Param(Version) 0.1             ;#Job version

   set Param(Out)   /tmp/met.csv
   set Param(Lat)    25
   set Param(Lon)   -100
   set Param(Hours) 24
   set Param(Run)   r112
   set Param(Vars)  { { UU 93423264 95318840 } { TT 93423264 } { NT -1 } }

   set Param(CommandLine) "Arguments must be:
\t-run    : Model run
\t-hours  : Nb hours from start of model run
\t-lat    : Latitude
\t-lon    : Longitude
\t-out    : Output file"
}

proc Met::Process { } {
   global env
   variable Param

   set f [open $Param(Out) w]
   
   puts $f "Date (UTC),Wind dir (deg) at 10 meters,Windspeed (km/h) at 10 meters,Wind dir (deg) at 50 meters,Windspeed (km/h) at 50 meters, Temp (C), Cloud cover (%)"
   
   set run [string range [exec r.date $Param(Run)] 0 9]
   switch [string index $Param(Run) 0] {
      "r" { set path prog/reghyb }
      "g" { set path prog/glbhyb }
      default { Log::Print ERROR "Invalid model"; Log::End 1
      }
   }
   
   foreach file [lrange [lsort -increasing [glob $env(CMCGRIDF)/$path/${run}_???]] 0 $Param(Hours)] {

      Log::Print INFO "Processing $file"
      set vals {}
      fstdfile open METFILE read $file
      foreach var $Param(Vars) {
         
         foreach lvl [lrange $var 1 end] {
            fstdfield read FLD METFILE -1 "" $lvl -1 -1 "" "[lindex $var 0]"
            set val [fstdfield stats FLD -coordvalue $Param(Lat) $Param(Lon)]
            if { [llength $val]>1 } {
               lappend vals [format "%.3f" [lindex $val 1]]
               lappend vals [format "%.3f" [expr [lindex $val 0]*1.8519969184024652]]
            } else {
               lappend vals [format "%.3f" [lindex $val 0]]
            }
         }
         set date [clock format [fstdstamp toseconds [fstdfield define FLD -DATEV]] -format "%Y%m%d %H:%M"]
      }
      puts $f "$date,[join $vals ,]"   
      
      fstdfile close METFILE
   }
   close $f
}

#----- This is where it all starts
set Log::Param(Level) DEBUG      ;#Log level
set Log::Param(Time)  False      ;#Print the time
set Log::Param(Proc)  False      ;#Print the calling proc

Log::Start [info script] $Met::Param(Version)

#----- Check for number of arguments
if { [llength $argv]==0 } {
   Log::Print INFO "Invalid number of arguments:\n\n$Met::Param(CommandLine)"
   Log::End 1
}

#----- Parcourir la liste des parametres post-launch
for { set i 0 } { $i < $argc } { incr i } {
   switch -exact [string trimleft [lindex $argv $i] "-"] {
      run        { set i [Args::Parse $argv $argc $i 1 Met::Param(Run)]}
      lat        { set i [Args::Parse $argv $argc $i 1 Met::Param(Lat)] }
      lon        { set i [Args::Parse $argv $argc $i 1 Met::Param(Lon)] }
      hours      { set i [Args::Parse $argv $argc $i 1 Met::Param(Hours)] }
      out        { set i [Args::Parse $argv $argc $i 1 Met::Param(Out)] }
      help       { puts $Met::Param(CommandLine); Log::End 0 }
      default    { Log::Print INFO "Invalid argument [lindex $argv $i]:\n\n$Met::Param(CommandLine)"; Log::End 1 }
   }
}

Met::Process 

Log::End