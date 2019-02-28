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
# Fichier    : FSTD_Profiler.tcl
# Creation   : Fevrier 2012 - J.P. Gauthier - CMC/CMOE
# Description: Extract time profile at specific coordinates
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

namespace eval Profiler {
   variable Data
   variable Param

   fstdfield vector { UU }

   set Param(Vars)      { ES TT UU VV }         ;# Variables to profile
   set Param(Models)    { glbeta regeta }       ;# Model list
   set Param(From)      0                       ;# Start hour
   set Param(To)        48                      ;# End hour
   set Param(Locations) {
     { YUL 45.0 -70.0 }
     { TOT 50.0 -75.0 }
   }

   set Param(Path)      $env(CMCGRIDF)/prog                    ;# Model database path
}

proc Profiler::Run { } {
   variable Data
   variable Param

   foreach model $Param(Models) {

      set run   [string range [exec r.date g100] 0 9] ;# Model run
      Log::Print INFO "Processing model $model"

      #----- Clear location data
      array unset Data

      #----- Loop on the time steps
      for { set t $Param(From) } { $t <= $Param(To) } { incr t 1 } {
         Log::Print INFO "   Processing time step $t"

         fstdfile open FILE read $Param(Path)/$model/${run}_[format "%03i" $t]

         foreach var $Param(Vars) {
            Log::Print INFO "      Processing variable $var"

            fstdfield read FIELD FILE -1 "" -1 -1 -1 "" "$var"
            fstdfield readcube FIELD

            #----- Do the profiles
            foreach loc $Param(Locations) {

               fstdfield vertical PROFIL FIELD [lrange $loc 1 end]
               lappend Data(Loc$var) [fstdfield define PROFIL -DATA 0]

            }
         }
         fstdfile close FILE
         break
      }

      Log::Print INFO "   Saving profiles"
      foreach var $Param(Vars) {
         foreach loc $Param(Locations) {
            set f [open $env(CI_SPI_OUT)/${model}_${run}_[lindex $loc 0]_$var.txt w+]
            puts $f [join $Data(Loc$var) \n]
            close $f
         }

      }
   }
   fstdfield free PROFIL FIELD DIR
}

Log::Start [info script] 0.1

Profiler::Run

Log::End