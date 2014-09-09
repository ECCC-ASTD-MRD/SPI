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
   set Param(Vars)  { UU }
   set Param(IP1s)  { "1.0 ETA" }

   set Param(CommandLine) "Arguments must be:
\t-run    : Model run or path to data (${APP_COLOR_GREEN}$Param(Run)${APP_COLOR_RESET})
\t-hours  : Nb hours from start of model run (${APP_COLOR_GREEN}$Param(Hours)${APP_COLOR_RESET})
\t-lat    : Latitude
\t-lon    : Longitude
\t-vars   : Variables (${APP_COLOR_GREEN}$Param(Vars)${APP_COLOR_RESET})
\t-ip1s   : IP1(s) or level(s) and unit(s) (${APP_COLOR_GREEN}$Param(IP1s)${APP_COLOR_RESET})
\t-out    : Output file (${APP_COLOR_GREEN}$Param(Out)${APP_COLOR_RESET})"
}

proc Met::Process { } {
   global env
   variable Param

   set f [open $Param(Out) w]

   set head "Date (UTC),Hour (UTC),"
   foreach var $Param(Vars) {
      foreach ip1 $Param(IP1s) {
         set lvl [fstdgrid convip $ip1]
         if { $var=="UU" } {
            append head "Wind dir (deg) at $lvl ,Windspeed (km/h) at $lvl"
         } else {
            append head "$var at $lvl"
         }
      }
   }
   puts $f "$head"
   
   if { [file isdirectory $Param(Run)] } {
      set files [glob $Param(Run)/*_???]
   } else {
      set run [string range [exec r.date $Param(Run)] 0 9]
      switch [string index $Param(Run) 0] {
         "r" { set path prog/regeta }
         "g" { set path prog/glbeta }
         default { Log::Print ERROR "Invalid model"; Log::End 1 }
      }
      set files [glob $env(CMCGRIDF)/$path/${run}_???]
   }
   
   set date0 0
   foreach file [lsort -increasing $files] {

      Log::Print INFO "Processing $file"
      fstdfile open METFILE read $file
      
      foreach datev [fstdfile info METFILE DATEV [lindex $Param(Vars) 0]] {
         set vals {}
         
         if { $date0 && [fstdstamp diff [fstdstamp fromseconds $datev] $date0]>$Param(Hours) } {
            close $f
            return
         }
         
         foreach var $Param(Vars) {
            foreach ip1 $Param(IP1s) {
               fstdfield read FLD METFILE [fstdstamp fromseconds $datev] "" $ip1 -1 -1 "" "$var"
               
               #----- Get initial date
               if { !$date0 } {
                  set date0 [fstdfield define FLD -DATEV]
               }
               
               set val [fstdfield stats FLD -coordvalue $Param(Lat) $Param(Lon)]
               if { [llength $val]>1 } {
                  lappend vals [format "%.3f" [lindex $val 1]]
                  lappend vals [format "%.3f" [expr [lindex $val 0]*1.8519969184024652]]
               } else {
                  lappend vals [format "%.3f" [lindex $val 0]]
               }
            }
         }
         puts $f "[clock format $datev -format "%Y%m%d,%H:%M" -gmt True],[join $vals ,]"   
      }
      
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
      run        { set i [Args::Parse $argv $argc $i VALUE Met::Param(Run)] }
      lat        { set i [Args::Parse $argv $argc $i VALUE Met::Param(Lat)] }
      lon        { set i [Args::Parse $argv $argc $i VALUE Met::Param(Lon)] }
      hours      { set i [Args::Parse $argv $argc $i VALUE Met::Param(Hours)] }
      ip1s       { set i [Args::Parse $argv $argc $i LIST Met::Param(IP1s)] }
      vars       { set i [Args::Parse $argv $argc $i LIST Met::Param(Vars)] }
      out        { set i [Args::Parse $argv $argc $i VALUE Met::Param(Out)] }
      help       { puts $Met::Param(CommandLine); Log::End 0 }
      default    { Log::Print INFO "Invalid argument [lindex $argv $i]:\n\n$Met::Param(CommandLine)"; Log::End 1 }
   }
}

Met::Process 

Log::End