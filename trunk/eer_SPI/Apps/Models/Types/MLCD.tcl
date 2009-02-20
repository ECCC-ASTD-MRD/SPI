#!/software/cmc/tcl-8.4.5/bin/wish8.4
#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet     : Interface du modele MLCD.
# Fichier    : MLCD.tcl
# Creation   : Mars 2001 - J.P. Gauthier - CMC/CMOE
#
# Description: Description des interfaces et procedures relatives a
#              celles-ci pour le module MLCD.
#
# Remarques  :
#
#===============================================================================

#----- Fichiers complementaires

source $GDefs(Dir)/Apps/Models/Types/MLCD.ctes
source $GDefs(Dir)/Apps/Models/Types/MLCD.txt
source $GDefs(Dir)/Apps/Models/Types/MLCD.int

#-------------------------------------------------------------------------------
# Nom      : <MLCD::AskIfInitMeteoData>
# Creation : 29 September 2005 - A. Malo - CMC/CMOE
#
# But      : Ask user if initializing meteo data or not.
#
# Parametres :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc MLCD::AskIfInitMeteoData { } {

   global GDefs
   variable Sim
   variable Warning
   variable Error
   variable Lbl

   if { $Sim(ReleaseDateTimeChanged) && $Sim(SimDurationChanged) } { #----- Both release date-time and simulation duration have changed.

      set newdate "${Sim(AccYear)}-${Sim(AccMonth)}-${Sim(AccDay)} ${Sim(AccHour)}:${Sim(AccMin)} UTC."
      set olddate "${Sim(OldAccYear)}-${Sim(OldAccMonth)}-${Sim(OldAccDay)} ${Sim(OldAccHour)}:${Sim(OldAccMin)} UTC."
      set newdur "$Sim(DurMin) $Error(UnitMinutes)"
      set olddur "$Sim(OldDurMin) $Error(UnitMinutes)"
      set answer [Dialog::CreateDefault .mlcdnew 500 "[lindex $Lbl(Warning) $GDefs(Lang)]" "[lindex $Warning(ChangeReleaseDateTimeSimDur) $GDefs(Lang)]\n\n\t[lindex $Warning(ChangeReleaseDateTimeSimDur2) $GDefs(Lang)] $newdate\n\t[lindex $Warning(ChangeReleaseDateTimeSimDur3) $GDefs(Lang)] $olddate\n\n\t[lindex $Warning(ChangeReleaseDateTimeSimDur4) $GDefs(Lang)] $newdur\n\t[lindex $Warning(ChangeReleaseDateTimeSimDur5) $GDefs(Lang)] $olddur[lindex $Warning(ChangeReleaseDateTimeSimDur6) $GDefs(Lang)]" warning 0 [lindex $Lbl(Yes) $GDefs(Lang)] [lindex $Lbl(No) $GDefs(Lang)]]

      if { $answer == 1 } { #---- Answer is "No" : Do not re-initialize release date-time, simulation duration and meteo data.

         #----- Keep old release date-time as current date-time.
         set Sim(AccYear)     $Sim(OldAccYear)
         set Sim(AccMonth)    $Sim(OldAccMonth)
         set Sim(AccDay)      $Sim(OldAccDay)
         set Sim(AccHour)     $Sim(OldAccHour)
         set Sim(AccMin)      $Sim(OldAccMin)
         set Sim(AccStamp)    $Sim(OldAccStamp)
         set Sim(AccDateTime) $Sim(OldAccDateTime)
         set Sim(Second)      [lindex [MLCD::GetInfoDateTime "$Sim(AccDateTime)"] 5]

         #----- Keep old simulation duration as current simulation duration.
         set Sim(DurMin)   $Sim(OldDurMin)             ; #----- Simulation duration [min].
         set Sim(Duration) [expr int($Sim(DurMin)*60)] ; #----- Simulation duration [s].

      } else { #---- Answer is "Yes" : Re-initialize release date-time, simulation duration and meteo data.

         MLCD::InitMeteoData

      }

   } else {

      if { $Sim(ReleaseDateTimeChanged) } { #----- Release date-time has changed.

         set newdate "${Sim(AccYear)}-${Sim(AccMonth)}-${Sim(AccDay)} ${Sim(AccHour)}:${Sim(AccMin)} UTC."
         set olddate "${Sim(OldAccYear)}-${Sim(OldAccMonth)}-${Sim(OldAccDay)} ${Sim(OldAccHour)}:${Sim(OldAccMin)} UTC."
         set answer [Dialog::CreateDefault .mlcdnew 500 "[lindex $Lbl(Warning) $GDefs(Lang)]" "[lindex $Warning(ChangeReleaseDateTime) $GDefs(Lang)]\n\n\t[lindex $Warning(ChangeReleaseDateTime2) $GDefs(Lang)] $newdate\n\t[lindex $Warning(ChangeReleaseDateTime3) $GDefs(Lang)] $olddate[lindex $Warning(ChangeReleaseDateTime4) $GDefs(Lang)]" warning 0 [lindex $Lbl(Yes) $GDefs(Lang)] [lindex $Lbl(No) $GDefs(Lang)]]

         if { $answer == 1 } { #---- Answer is "No" : Do not re-initialize release date-time and meteo data.

            #----- Re-initialize old release date-time.
            set Sim(OldAccYear)     $Sim(AccYear)
            set Sim(OldAccMonth)    $Sim(AccMonth)
            set Sim(OldAccDay)      $Sim(AccDay)
            set Sim(OldAccHour)     $Sim(AccHour)
            set Sim(OldAccMin)      $Sim(AccMin)
            set Sim(OldAccStamp)    $Sim(AccStamp)
            set Sim(OldAccDateTime) $Sim(AccDateTime)
            set Sim(OldSecond)      $Sim(Second)

#             #----- Keep old release date-time as current date-time.
#             set Sim(AccYear)     $Sim(OldAccYear)
#             set Sim(AccMonth)    $Sim(OldAccMonth)
#             set Sim(AccDay)      $Sim(OldAccDay)
#             set Sim(AccHour)     $Sim(OldAccHour)
#             set Sim(AccMin)      $Sim(OldAccMin)
#             set Sim(AccStamp)    $Sim(OldAccStamp)
#             set Sim(AccDateTime) $Sim(OldAccDateTime)
#             set Sim(Second)      [lindex [MLCD::GetInfoDateTime "$Sim(AccDateTime)"] 5]

         } else { #---- Answer is "Yes" : Re-initialize release date-time and meteo data.

            MLCD::InitMeteoData

         }

      }

      if { $Sim(SimDurationChanged) } { #----- Simulation duration has changed.

         set newdur "$Sim(DurMin) $Error(UnitMinutes)"
         set olddur "$Sim(OldDurMin) $Error(UnitMinutes)"
         set answer [Dialog::CreateDefault .mlcdnew 500 "[lindex $Lbl(Warning) $GDefs(Lang)]" "[lindex $Warning(ChangeSimDuration) $GDefs(Lang)]\n\n\t[lindex $Warning(ChangeSimDuration2) $GDefs(Lang)] $newdur\n\t[lindex $Warning(ChangeSimDuration3) $GDefs(Lang)] $olddur[lindex $Warning(ChangeSimDuration4) $GDefs(Lang)]" warning 0 [lindex $Lbl(Yes) $GDefs(Lang)] [lindex $Lbl(No) $GDefs(Lang)]]

         if { $answer == 1 } { #---- Answer is "No" : Do not re-initialize simulation duration and meteo data.

            #----- Keep old simulation duration as current simulation duration.
            set Sim(DurMin)   $Sim(OldDurMin)             ; #----- Simulation duration [min].
            set Sim(Duration) [expr int($Sim(DurMin)*60)] ; #----- Simulation duration [s].

         } else { #---- Answer is "Yes" : Re-initialize simulation duration and meteo data.

            MLCD::InitMeteoData

         }

      }

   }

}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::CheckIfEmissionStartingTimeModified>
# Creation : 20 June 2007 - A. Malo - CMC/CMOE
#
# But      : Verify if emission starting time has been modified (1) or not (0).
#
# Parametres :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc MLCD::CheckIfEmissionStartingTimeModified { } {
   variable Sim

   set Sim(ReleaseDateTimeChanged) 0

   if { $Sim(AccStamp) != $Sim(OldAccStamp) } {
#       puts stdout "\n--->>> Emission starting time has been modified."
#       puts stdout "Old release date-time : $Sim(OldAccDateTime)"
#       puts stdout "New release date-time : $Sim(AccDateTime)"
      set Sim(ReleaseDateTimeChanged) 1 ; #----- Release date-time has changed.
   }

#    puts stdout "Sim(ReleaseDateTimeChanged) : $Sim(ReleaseDateTimeChanged)"

}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::CheckIfSimulationDurationModified>
# Creation : 20 June 2007 - A. Malo - CMC/CMOE
#
# But      : Verify if simulation duration has been modified (1) or not (0).
#
# Parametres :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc MLCD::CheckIfSimulationDurationModified { } {
   variable Sim

   set Sim(SimDurationChanged) 0

   if { $Sim(DurMin) != $Sim(OldDurMin) } {
#       puts stdout "\n--->>> Simulation duration has been modified."
#       puts stdout "Old simulation duration \[min\] : $Sim(OldDurMin)"
#       puts stdout "New simulation duration \[min\] : $Sim(DurMin)"
      set Sim(SimDurationChanged) 1 ; #----- Simulation duration has changed.
   }

#    puts stdout "Sim(SimDurationChanged)     : $Sim(SimDurationChanged)"

}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::ComputeNbValidWindProfiles>
# Creation : 14 November 2003 - A. Malo - CMC/CMOE
#
# But      : Compute number of valid wind profiles.
#
# Parametres :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc MLCD::ComputeNbValidWindProfiles { } {
   global   GDefs
   variable Sim
   variable Error
   variable Data

   #----- Initialize variables.
   set Sim(ObsValidNb)  0  ; #----- Set number of valid wind profiles.
   set Sim(ObsValidIdx) {} ; #----- Set list of indexes of valid wind profiles.

   for { set i 0 } { $i < $Sim(ObsMaxNb) } { incr i } { #----- Loop over total number of observations.

      set profile [lindex $Sim(WindProfile) $i] ; #----- Set wind profile.

      foreach level $profile { #----- Loop over vertical levels.

         set height    [lindex $level 0]
         set velocity  [lindex $level 1]
         set direction [lindex $level 2]

         if { $height != "" && $velocity != "" && $direction != "" } {
            #----- Compute number of valid wind profiles.
            incr Sim(ObsValidNb)
            lappend Sim(ObsValidIdx) $i
            break
         }

      }

   }

   #----- Verify that number of wind profile is not zero.
   if { $Sim(ObsValidNb) == 0 } {
      Dialog::CreateError .mlcdnew "[lindex $Error(ObsNbValidWindProfiles) $GDefs(Lang)]" $GDefs(Lang) 600
      TabFrame::Select $Data(TabFrameName) 2 ; #----- Select 'Meteo' tab.
      focus $Data(FrameWindProfile).l0.z
      return 0
   }

   return 1

}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::ExtractMetData>
# Creation : 19 June 2007 - A. Malo - CMC/CMOE
#
# But      : Extract meteorological data :
#              - Local parameters :
#              - Wind profile.
#
# Parametres :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc MLCD::ExtractMetData { } {
   global   GDefs
   variable Sim
   variable Error
   variable Warning
   variable Lbl
   variable Data

   for { set i 0 } { $i < $Sim(ObsNb) } { incr i } { #----- Loop over observation times.

      set time  [lindex $Sim(ObsTime)  $i] ; #----- Observation date-time.
      set stamp [lindex $Sim(ObsStamp) $i] ; #----- Observation date-time stamp.
      set file  [lindex $Sim(ObsFile)  $i] ; #----- Observation filename.

      set    text "\n\nExtracting local parameters and wind profile from :"
      append text "\n  - observation number : [expr $i + 1]/$Sim(ObsNb)"
      append text "\n  - date-time (stamp)  : $time ($stamp)"
      append text "\n  - standard file      : $file"
      append text "\n"

      puts stdout $text

      #----- Open standard file.
      fstdfile open UNITFILE read $file

      #----- Get wind profile data for the specified date-time stamp and lat-lon coordinates of the source.
      set WindProfile [MetData::Profile $stamp UU UNITFILE $Sim(Lat) $Sim(Lon)]

      #----- Verify if wind profile data has been found.
      if { $WindProfile == "" } {

         Debug::TraceProc "MLCD: Warning, unavailable wind profile."

         set info  [MLCD::GetInfoDateTime $time]
         set year  [lindex $info 0]
         set month [lindex $info 1]
         set day   [lindex $info 2]
         set hour  [lindex $info 3]
         set min   [lindex $info 4]

         set ObsNo [expr $i + 1]
         set msg "Observation \#$ObsNo : ${year}-${month}-${day} ${hour}:${min} UTC"

         set answer [Dialog::CreateDefault .mlcdnew 400 "[lindex $Lbl(Warning) $GDefs(Lang)]" "[lindex $Warning(WindProfData) $GDefs(Lang)]$msg[lindex $Warning(WindProfData2) $GDefs(Lang)]" warning 0 [lindex $Lbl(Yes) $GDefs(Lang)] [lindex $Lbl(No) $GDefs(Lang)]]

         if { $answer == 1 } { #---- Answer = "No"
            fstdfile close UNITFILE ; #----- Close standard file.
            return 0
         }

      }

      #----- Update Obukhov Length.
      set ObukhovLength [format "%.7f" [MetData::Obukhov $stamp UNITFILE $Sim(Lat) $Sim(Lon)]]

      #----- Update Roughness Length.
      set DefaultValue    [lindex [lindex $Sim(LocalParameters) $i] 0] ; #----- Default value for roughness length [m].
      set RoughnessLength $DefaultValue
      set MissingField    0
      set OffGrid         0

      set z0 [fstdfield find UNITFILE $stamp "" 1195 -1 -1 "" Z0]
      if { [llength $z0] } {
         fstdfield read TMP UNITFILE $z0

         set RoughnessLength [fstdfield stats TMP -coordvalue $Sim(Lat) $Sim(Lon)]
         if { $RoughnessLength == "-" } {
            #----- Use default value since off grid localisation.
            set OffGrid 1 ; #----- Off grid localisation. Using default value.
            set RoughnessLength $DefaultValue
         }
      } else {
         set MissingField 1 ; #----- Missing field. Using default value.
      }

      set RoughnessLength [format "%.7f" $RoughnessLength]
      if { $MissingField } {
         Debug::TraceProc "MLCD: Warning, missing field 'Z0'. Using default value for roughness length : $RoughnessLength \[m\]."
      }
      if { $OffGrid } {
         Debug::TraceProc "MLCD: Warning, off grid localisation. Using default value for roughness length : $RoughnessLength \[m\]."
      }

      #----- Update Precipitation Rate.
      set DefaultValue [lindex [lindex $Sim(LocalParameters) $i] 2] ; #----- Default value for precipitation rate [mm/h].
      set PrecipRate   $DefaultValue
      set MissingField 0
      set OffGrid      0

      set rt [fstdfield find UNITFILE $stamp "" -1 -1 -1 "" RT]
      if { [llength $rt] } {
         fstdfield read TMP UNITFILE $rt

         set PrecipRate [fstdfield stats TMP -coordvalue $Sim(Lat) $Sim(Lon)]
         if { $PrecipRate == "-" } {
            #----- Use default value since off grid localisation.
            set OffGrid 1 ; #----- Off grid localisation. Using default value.
            set PrecipRate $DefaultValue
         } else {
            #----- Convert precipitation rate from [m/s] to [mm/h].
            set PrecipRate [expr $PrecipRate*3.6e+06]
         }
      } else {
         set MissingField 1 ; #----- Missing field. Using default value.
      }

      set PrecipRate [format "%.7f" $PrecipRate]
      if { $MissingField } {
         Debug::TraceProc "MLCD: Warning, missing field 'RT'. Using default value for precipitation rate : $PrecipRate \[mm/h\]."
      }
      if { $OffGrid } {
         Debug::TraceProc "MLCD: Warning, off grid localisation. Using default value for precipitation rate : $PrecipRate \[mm/h\]."
      }

      #----- Set local parameters list.
      set Parameters [list $RoughnessLength $ObukhovLength $PrecipRate]

      Debug::TraceProc "MLCD: Local parameters: $Parameters"

      append text "\nLocal parameters (Roughness Length \[m\], Obukhov Length \[m\], Precipitation Rate \[mm/h\]): $Parameters"
      append text "\n\nWind Profile (Height \[m\], Wind Velocity \[m/s\], Wind Direction \[deg\]):"
      set level 0
      foreach prof $WindProfile {
         incr level
         set h [format "%15s" [format "%.7f" [lindex $prof 0]]]
         set v [format "%15s" [format "%.7f" [lindex $prof 1]]]
         set d [format "%15s" [format "%.7f" [lindex $prof 2]]]
         append text "\n[format "%2s" $level] $h $v $d"
      }

      append Sim(ObsList) $text

      if { $WindProfile != "" } {

         #----- Wind profile available.
         #----- Set wind profile according to number of specified vertical levels.

         set Profile {}
         for { set j 0 } { $j < $Sim(ObsNbLevels) } { incr j } {
            set level       [lindex $WindProfile $j]
            set height      [format "%.7f" [lindex $level 0]]
            set velocity    [format "%.7f" [lindex $level 1]]
            set direction   [format "%.7f" [lindex $level 2]]
            lappend Profile [list $height $velocity $direction]
         }
         for { set j $Sim(ObsNbLevels) } { $j < $Sim(ObsMaxNbLevels) } { incr j } {
            set height      ""
            set velocity    ""
            set direction   ""
            lappend Profile [list $height $velocity $direction]
         }

      } else {

         #----- No wind profile.

         set Profile {}
         for { set j 0 } { $j < $Sim(ObsMaxNbLevels) } { incr j } {
            set height    ""
            set velocity  ""
            set direction ""
            lappend Profile [list $height $velocity $direction]
         }

      }

      #----- Close standard file.
      fstdfile close UNITFILE

      #----- Add local parameters and wind profile to lists.
      set Sim(LocalParameters) [lreplace $Sim(LocalParameters) $i $i $Parameters]
      set Sim(WindProfile)     [lreplace $Sim(WindProfile) $i $i $Profile]

   }

   for { set i $Sim(ObsNb) } { $i < $Sim(ObsMaxNb) } { incr i } { #----- Loop over other observations.

      #----- Initialize local parameters for all other observations.
      set rough      0.5                                                    ; #----- Roughness length (Default=Forest 0.5) [m].
      set obukhovCte 0                                                      ; #----- Obukhov const.
      set obukhov    [format "%.7f" [MLCD::ObukhovFunc $obukhovCte $rough]] ; #----- Obukhov length [m].
      set precip     0.0                                                    ; #----- Precipitation rate [mm/h].

      set params "$rough $obukhov $precip"
      set Sim(LocalParameters) [lreplace $Sim(LocalParameters) $i $i $params]

      #----- Initialize wind profile for all other observations.
      set prof {}
      for { set j 0 } { $j < $Sim(ObsMaxNbLevels) } { incr j } {
         set height    ""
         set velocity  ""
         set direction ""
         lappend prof  [list $height $velocity $direction]
      }

      set Sim(WindProfile) [lreplace $Sim(WindProfile) $i $i $prof]

   }

#   puts stderr "Sim(LocalParameters) : $Sim(LocalParameters)"
#   puts stderr "Sim(WindProfile)     : $Sim(WindProfile)"

}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::FindMetData>
# Creation : 18 June 2007 - A. Malo - CMC/CMOE
#
# But      : Find available meteorological data files according to:
#              - meteorological model ;
#              - diagnostics/prognostics databases ;
#              - release date-time ;
#              - simulation duration.
#
# Parametres :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc MLCD::FindMetData { } {
   global   GDefs
   variable Sim
   variable Error
   variable Warning
   variable Lbl
   variable Data

   #----- Define mixed mode.
   if { $Sim(DBaseDiag) == $Sim(DBaseProg) } {
      set Mixed -1 ; #----- Ignored the latest run.
   } else {
      set Mixed 1  ; #----- Take into account the latest run.
   }

   #----- Get list of available meteorological files according to release date-time.
   set Sim(MetData) [MetData::File $Sim(AccStamp) $Sim(DBaseDiag) $Sim(DBaseProg) F $Mixed $Sim(ProgMeteoTimeStep)]

   #----- Number of available meteo files.
   set Sim(NbMetFiles) [llength $Sim(MetData)]

   #----- Verify that number of meteo data files is greater than 1.
   if { $Sim(NbMetFiles) <= 1 } {
      puts stderr "\n*** Error! Not enough available meteorological data files in database according to emission date-time."
      puts stderr "    Emission date-time (stamp) : $Sim(AccDateTime) ($Sim(AccStamp))"

      set StartingTime "$Sim(AccYear)-$Sim(AccMonth)-$Sim(AccDay) $Sim(AccHour):$Sim(AccMin) UTC"
      set PossibleTime [MLCD::FindRangeMetDateTime]
      set text "[lindex $Error(MetData) $GDefs(Lang)]\n\n\t[lindex $Error(MetData2) $GDefs(Lang)] $StartingTime"

      if { $PossibleTime != "" } {
         append text "\n\t[lindex $Error(MetData3) $GDefs(Lang)] $PossibleTime"
      }

      Dialog::CreateError .mlcdnew $text $GDefs(Lang) 900

      #----- Select first observation.
      MLCD::SelectObs 1

      #----- Select 'Model' tab.
      TabFrame::Select $Data(TabFrameName) 0

      return 0 ; #----- Error!
   }

   #----- Get first available met file.
   set list       [lindex $Sim(MetData) 0]
   set firststamp [lindex $list 0]             ; #----- Date-time stamp for first met file.
   set firstdate  [lindex $list 1]             ; #----- Date-time for first met file.

   #----- Get last available met file.
   set list       [lindex $Sim(MetData) end]
   set laststamp  [lindex $list 0]             ; #----- Date-time stamp for last met file.
   set lastdate   [lindex $list 1]             ; #----- Date-time for last met file.
   set lastfile   [file tail [lindex $list 2]] ; #----- Filename for last met file.
   set list       [split $lastfile "_"]
   set run        [lindex $list 0]             ; #----- Latest run.
   set extension  [lindex $list 1]             ; #----- Hours of latest run.

   #----- Compute simulation duration according to last available met data file.
   set SimDurHr  [fstdstamp diff $laststamp $Sim(AccStamp)] ; #----- [hr].
   set SimDurMin [expr round(double($SimDurHr) * 60.0)]     ; #----- [min].
   set SimDurSec [expr int($SimDurMin*60)]                  ; #----- [s].

   if { $SimDurHr <= 0 } {
      set message "Error! Not enough available meteorological data files in database according to emission date-time."
      puts stderr "\n*** $message"
      puts stderr "    - Emission date-time (stamp)   : $Sim(AccDateTime)00 ($Sim(AccStamp))"
      puts stderr "    - First date-time (stamp)      : $firstdate ($firststamp)"
      puts stderr "    - Last date-time (stamp)       : $lastdate ($laststamp)"
      return 0
   }

   if { $Sim(Duration) >= $SimDurSec } {

      #----- Here, simulation duration defined within interface is greater than (or equal to) simulation duration
      #----- computed according to available met files. Thus, simulation duration will be re-initialized.
      set OldSimDurMin  $Sim(DurMin)
      set Sim(Duration) $SimDurSec
      set Sim(DurMin)   $SimDurMin

      #----- Initialize old simulation duration.
      set Sim(OldDurMin) $Sim(DurMin)

      Dialog::CreateDefault .mlcdnew 500 "[lindex $Lbl(Warning) $GDefs(Lang)]" "[lindex $Warning(SimDuration) $GDefs(Lang)]\n\n\t[lindex $Warning(SimDuration2) $GDefs(Lang)] $Sim(DurMin) $Error(UnitMinutes)\n\t[lindex $Warning(SimDuration3) $GDefs(Lang)] $OldSimDurMin $Error(UnitMinutes)" warning 0 "OK"

      puts stdout "\n*** Warning: Re-initializing simulation duration according to available met files in database."
      puts stdout "             New simulation duration \[min\] : $Sim(DurMin)"
      puts stdout "             Old simulation duration \[min\] : $OldSimDurMin"

      #----- Redefine emission duration since simulation duration has changed.
      MLCD::SetEmissionDuration

   } else {

      #----- Here, simulation duration defined within interface is less than simulation duration
      #----- computed according to available met files.

      #----- Compute new ending simulation date-time [s] according to release date-time and simulation duration.
      #----- Ending simulation date-time [s] := starting release date-time [s] + simulation duration [s].
      set end [expr $Sim(Second) + $Sim(Duration)]

      #----- Convert new ending simulation date-time from [s] to human readable format.
      set year  [clock format $end -format "%Y" -gmt true] ; #----- YYYY.
      set month [clock format $end -format "%m" -gmt true] ; #----- MM.
      set day   [clock format $end -format "%d" -gmt true] ; #----- DD.
      set hour  [clock format $end -format "%H" -gmt true] ; #----- HH.
      set min   [clock format $end -format "%M" -gmt true] ; #----- mm.

      #----- Convert new ending simulation date-time from human readable format to CMC date-time stamp.
      set laststamp [GetDateTimeStamp "${year}${month}${day}${hour}${min}"]

      #----- Build temporary list of met stamps.
      set MeteoStamp {}
      foreach data $Sim(MetData) {
         lappend MeteoStamp [lindex $data 0]
      }

      #----- Search list of met stamps for which ending simulation date-time
      #----- falls between two following met stamps.
      for { set i 0 } { $i <= [expr [llength $MeteoStamp] - 2] } { incr i } {
         set j [expr $i + 1]
         set first  [lindex $MeteoStamp $i]
         set second [lindex $MeteoStamp $j]
         if { $laststamp > $first && $laststamp <= $second } {
            set idx $j
            break
         }
      }

      #----- Redefine list of available meteorological data files.
      set Sim(MetData) [lrange $Sim(MetData) 0 $idx]

   }

   #----- Redefine number of available meteo files.
   set Sim(NbMetFiles) [llength $Sim(MetData)]

   #----- First and last available met files.
   set list [lindex $Sim(MetData) 0]
   set firststamp [lindex $list 0]
   set firsttime  [lindex $list 1]
   set list [lindex $Sim(MetData) end]
   set laststamp [lindex $list 0]
   set lasttime  [lindex $list 1]

   #----- Count number of diagnostics and prognostics met files.
   set nbtrials 0
   set nbprogs  0
   foreach data $Sim(MetData) {

      set filename [lindex $data 2]

      if { [string match "*\/trial\/*" $filename] } {
         incr nbtrials
      } elseif { [string match "*\/prog\/*" $filename] } {
         incr nbprogs
      }

   }
   set Sim(NbMetDiagFiles) $nbtrials
   set Sim(NbMetProgFiles) $nbprogs

   #----- Print info regarding meteorological files.
   set text    "\nEmission date-time (stamp)      : $Sim(AccDateTime)00 ($Sim(AccStamp))"
   append text "\nFirst met date-time (stamp)     : $firsttime ($firststamp)"
   append text "\nLast met date-time (stamp)      : $lasttime ($laststamp)"
   append text "\nSimulation duration \[s\]         : $Sim(Duration)"
   append text "\nNumber of meteorological files  : $Sim(NbMetFiles)"
   append text "\nNumber of diagnostics met files : $Sim(NbMetDiagFiles)"
   append text "\nNumber of prognostics met files : $Sim(NbMetProgFiles)"
   append text "\n\nList of available meteorological files :"

   foreach file $Sim(MetData) {
      append text "\n$file"
   }

   puts stdout $text

   set Sim(MetList) $text

   return 1
}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::FindRangeMetDateTime>
# Creation : 15 January 2004 - A. Malo - CMC/CMOE
#
# But      : Find the oldest/latest meteorological date-time files.
#
# Parametres :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc MLCD::FindRangeMetDateTime { } {
   variable Sim

   if { $Sim(DBaseDiag) != $Sim(DBaseProg) } {
      #----- Search within prognostic/diagnostic databases.
      set progs  [glob -nocomplain $Sim(DBaseProg)/*_???]
      set trials [glob -nocomplain $Sim(DBaseDiag)/*_000]
   } else {
      #----- Search within common database.
      set progs  [glob -nocomplain $Sim(DBaseProg)/*_???]
      set trials $progs
   }

   #----- Sort files.
   set trials [lsort -dictionary -increasing $trials]
   set progs  [lsort -dictionary -increasing $progs]

   if { [llength $progs] == 0 || [llength $trials] == 0 } {
      return ""
   }

   set first [lindex $trials 0]
   set last  [lindex $progs end]

   set first [MLCD::GetValidDateTime $first]
   set last  [MLCD::GetValidDateTime $last]

   return "$first @ $last"

}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::GetCurrentObservationTime>
# Creation : 19 June 2007 - A. Malo - CMC/CMOE
#
# But      : Get current observation time according to (selected)
#            observation index.
#
# Parametres :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc MLCD::GetCurrentObservationTime { } {

   variable Sim

   #----- Get current observation time according to (selected) observation index.
   set time  [lindex $Sim(ObsTime) $Sim(ObsIdx)]

   set time  [MLCD::GetInfoDateTime $time]
   set year  [lindex $time 0]
   set month [lindex $time 1]
   set day   [lindex $time 2]
   set hour  [lindex $time 3]
   set min   [lindex $time 4]
   set sec   [lindex $time 5]

   #----- Set observation date.
   set date                [clock format $sec -format "%a %b %d %Y" -gmt true]
   set Sim(ObsTimeDate)    $date
   set Sim(ObsTimeDateSec) $sec
   set Sim(ObsTimeYear)    $year
   set Sim(ObsTimeMonth)   $month
   set Sim(ObsTimeDay)     $day

   #----- Set observation time.
   set Sim(ObsTimeHour)    $hour
   set Sim(ObsTimeMin)     $min

   #----- Save observation date and time for future verification.
   set Sim(OldObsTime) $Sim(ObsTime)

}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::GetDateTimeStamp>
# Creation : 18 June 2007 - A. Malo - CMC/CMOE
#
# But      : Get CMC date-time stamp.
#
# Parametres :
#    <datetime> : Date-time in human readable format "YYYYMMDDHHmmSS"
#
# Retour :
#
# Remarques :
#    DateTime must be at least 8-characters long; HHmmSS is optional.
#
#-------------------------------------------------------------------------------
proc MLCD::GetDateTimeStamp { DateTime } {

   if { [string length $DateTime] < 8 } {
      return ""
   }

   set year  [string range $DateTime 0 3]
   set month [string range $DateTime 4 5]
   set day   [string range $DateTime 6 7]
   set hour  [string range $DateTime 8 9]
   set min   [string range $DateTime 10 11]
   set sec   [string range $DateTime 12 13]

   if { $hour == "" } {
      set hour "00"
   }

   if { $min == "" } {
      set min "00"
   }

   if { $sec == "" } {
      set sec "00"
   }

   return [fstdstamp fromdate ${year}${month}${day} ${hour}${min}${sec}00]

}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::GetInfoDateTime>
# Creation : 14 November 2003 - A. Malo - CMC/CMOE
#
# But      : Get information in date-time.
#
# Parametres :
#    <DateTime> : Date-Time in human readable format "YYYYMMDDHHmm"
#
# Retour :
#
# Remarques :
#    - YYYY : Year
#    - MM   : Month
#    - DD   : Day
#    - HH   : Hour
#    - mm   : Minutes
#    - Returns a list containing the
#      1. Year,
#      2. Month,
#      3. Day,
#      4. Hour,
#      5. Minutes,
#      6. Integer Clock Value (in seconds) representing this date-time,
#      7. CMC date-time stamp associated to this date-time.
#
#-------------------------------------------------------------------------------
proc MLCD::GetInfoDateTime { DateTime } {

   set year  [string range $DateTime 0 3]
   set month [string range $DateTime 4 5]
   set day   [string range $DateTime 6 7]
   set hour  [string range $DateTime 8 9]
   set min   [string range $DateTime 10 11]
   set sec   [clock scan "$year$month$day $hour:$min" -gmt true]
   set stamp [GetDateTimeStamp $DateTime]

   set time  [list $year $month $day $hour $min $sec $stamp]

   return $time
}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::GetValidDateTime>
# Creation : 19 June 2007 - A. Malo - CMC/CMOE
#
# But      : Get valid date-time from filename.
#
# Parametres :
#    <filename> : Filename.
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc MLCD::GetValidDateTime { filename } {

   set list [split [file tail $filename] _]
   set date [lindex $list 0]
   set fcst [string trimleft [lindex $list 1] 0]
   if { $fcst == "" } {
      set fcst 0
   }

   set year  [string range $date 0 3]
   set month [string range $date 4 5]
   set day   [string range $date 6 7]
   set hour  [string range $date 8 9]
   set sec   [clock scan "$year$month$day $hour:00" -gmt true]
   set sec   [expr $sec + $fcst*3600]

   set year  [clock format $sec -format "%Y" -gmt true]
   set month [clock format $sec -format "%m" -gmt true]
   set day   [clock format $sec -format "%d" -gmt true]
   set hour  [clock format $sec -format "%H" -gmt true]
   set min   [clock format $sec -format "%M" -gmt true]

   set datetime "${year}-${month}-${day} ${hour}:${min} UTC"

   return $datetime

}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::InitAutomationParams>
# Creation : 9 December 2003 - A. Malo - CMC/CMOE
#
# But      : Initialize automation parameters:
#            - Number of observations;
#            - Number of vertical levels;
#            - Type of meteorological model;
#
# Parametres :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc MLCD::InitAutomationParams { } {

   variable Sim

   #----- Initialize variables.
   set Sim(ObsNb)             1                                 ; #----- Number of observations.
   set Sim(ObsNbLevels)       3                                 ; #----- Number of vertical levels in wind profile.
   set Sim(ObsMetModel)       [lindex $Sim(ObsListMetModel) 1]  ; #----- Type of meteorological model : 'REGIONAL'.
   set Sim(ObsNo)             1                                 ; #----- Current observation number.
   set Sim(ObsIdx)            [expr $Sim(ObsNo) - 1]            ; #----- Current observation index.
   set Sim(ObsPrevIdx)        $Sim(ObsIdx)                      ; #----- Previous observation index.

   #----- Set meteorological data directories according to meteorological model.
   MLCD::SetMetDataDir $Sim(ObsMetModel)

   #----- Save automation parameters for future verification.
   set Sim(OldObsNb)        $Sim(ObsNb)
   set Sim(OldObsNbLevels)  $Sim(ObsNbLevels)
   set Sim(OldObsMetModel)  $Sim(ObsMetModel)

}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::InitLocalParameters>
# Creation : 12 November 2003 - A. Malo - CMC/CMOE
#
# But      : Initialize local parameters.
#
# Parametres :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc MLCD::InitLocalParameters { } {

   variable Sim

   #----- Initialize variables.
   set Sim(ObsRough)        0.5                                                                    ; #----- Roughness length (Default=Forest 0.5) [m].
   set Sim(ObsObukhovCte)   0                                                                      ; #----- Obukhov const.
   set Sim(ObsObukhov)      [format "%.7f" [MLCD::ObukhovFunc $Sim(ObsObukhovCte) $Sim(ObsRough)]] ; #----- Obukhov length [m].
   set Sim(ObsPrecip)       0.0                                                                    ; #----- Precipitation rate [mm/h].
   set Sim(LocalParameters) {}

   for { set i 0 } { $i < $Sim(ObsMaxNb) } { incr i } { #----- Loop over number of observations.

      set params "$Sim(ObsRough) $Sim(ObsObukhov) $Sim(ObsPrecip)"
      lappend Sim(LocalParameters) "$params"

   }

   #----- Save local parameters for future verification.
   set Sim(OldLocalParameters) $Sim(LocalParameters)

#   puts stderr "Sim(LocalParameters): $Sim(LocalParameters)"


}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::InitMeteoData>
# Creation : 29 September 2005 - A. Malo - CMC/CMOE
#
# But      : Initialize meteo data:
#            - Observation Times;
#            - Local Parameters;
#            - Wind Profile.
#
# Parametres :
#    <Flag> : Flag indicating if it is needed to update edition observation
#             interface (1: default value) or not (0).
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc MLCD::InitMeteoData { { Flag 1 } } {

   variable Sim

   #----- Initialize old release date-time.
   set Sim(OldAccYear)     $Sim(AccYear)
   set Sim(OldAccMonth)    $Sim(AccMonth)
   set Sim(OldAccDay)      $Sim(AccDay)
   set Sim(OldAccHour)     $Sim(AccHour)
   set Sim(OldAccMin)      $Sim(AccMin)
   set Sim(OldSecond)      $Sim(Second)
   set Sim(OldAccStamp)    $Sim(AccStamp)
   set Sim(OldAccDateTime) $Sim(AccDateTime)

   #----- Initialize old simulation duration.
   set Sim(OldDurMin) $Sim(DurMin)

   #----- Initialize meteo data.
   MLCD::InitAutomationParams ; #----- Initialize automation parameters.
   MLCD::InitObservationTimes ; #----- Initialize observation times.
   MLCD::InitLocalParameters  ; #----- Initialize local parameters.
   MLCD::InitWindProfile      ; #----- Initialize wind profile.

   #----- Update edition observation interface.
   if { $Flag } {
      MLCD::UpdateObsInterface $Sim(ObsIdx)
   }

}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::InitNew>
# Creation : Octobre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialisation des parametres.
#
# Parametres :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc MLCD::InitNew { } {
   global GDefs
   variable Sim
   variable Data

   set Sim(BasePath)   [Exp::Path]
   set Sim(DBaseDiag)  ""
   set Sim(DBaseProg)  ""

   set Sim(Name)       $Exp::Data(Name)           ; #----- Location identification.
   set Sim(Pos)        $Exp::Data(Pos)            ; #----- Location list.
   set Sim(NoExp)      $Exp::Data(No)             ; #----- Experiment number.

   set MLCD::Sim(Names) {}
   foreach src $MLCD::Sim(Pos) {
      lappend  MLCD::Sim(Names) [lindex $src 0]
   }
   set src [lindex $MLCD::Sim(Pos) 0]

   set Sim(Src)               [lindex $src 0]             ; #----- Location.
   MLCD::SetSrc                                           ; #----- Set lat-lon coordinates of source.
   set Sim(DurMin)            360                         ; #----- Simulation duration [min].
   set Sim(Duration)          [expr int($Sim(DurMin)*60)] ; #----- Simulation duration [s].
   set Sim(OutputTimeStepMin) 5                           ; #----- Output time step [min].
   set Sim(ModelTimeStepMin)  5                           ; #----- Model time step [min].

   #----- Initialize release date-time.
   set Sim(Second)      [clock seconds]
   set Sim(AccYear)     [clock format $Sim(Second) -format "%Y" -gmt true]
   set Sim(AccMonth)    [clock format $Sim(Second) -format "%m" -gmt true]
   set Sim(AccDay)      [clock format $Sim(Second) -format "%d" -gmt true]
   set Sim(AccHour)     [clock format $Sim(Second) -format "%H" -gmt true]
   set Sim(AccMin)      [clock format $Sim(Second) -format "%M" -gmt true]
   set Sim(Second)      [clock scan "$Sim(AccYear)$Sim(AccMonth)$Sim(AccDay) $Sim(AccHour):$Sim(AccMin)" -gmt true]
   set Sim(AccStamp)    [GetDateTimeStamp "$Sim(AccYear)$Sim(AccMonth)$Sim(AccDay)$Sim(AccHour)$Sim(AccMin)"]
   set Sim(AccDateTime) "$Sim(AccYear)$Sim(AccMonth)$Sim(AccDay)$Sim(AccHour)$Sim(AccMin)"

   #----- Update emission starting time.
   MLCD::UpdateEmissionStartingTime

   #----- Prognostics meteorological time step [hr].
   set Sim(ProgMeteoTimeStep) 1

   #----- Initialize general and concentrations parameters.
   set Sim(Mode)           0        ; #----- Forward mode: 0; Backward mode: 1.
   set Sim(IsSigma)        1        ; #----- Mesoscale velocity fluctuations is on.
   set Sim(IsRanVar)       1        ; #----- Variable generator of gaussian numbers is on.
   set Sim(IsDiag)         0        ; #----- No diagnostics output file is generated.
   set Sim(IsConc)         1        ; #----- Concentrations calculations is on.
   set Sim(GridType)       4        ; #----- Type of grid is a fix Lat-Lon at variable resolution.
   set Sim(GridAlgo)       1        ; #----- Type of algorithm is 4 nearest grid points.
   set Sim(GridTypeS)      [lindex [lindex $Sim(ConcGrid) $GDefs(Lang)] $Sim(GridType)] ; #----- Grid type.
   set Sim(GridAlgoS)      [lindex [lindex $Sim(ConcAlgo) $GDefs(Lang)] $Sim(GridAlgo)] ; #----- Algorithm.
   set Sim(GridDomain)     100                                                          ; #----- Grid Domain [km].
   set Sim(VerticalLevels) [lindex $Sim(ConcVerticalLevelsVal) 0]                       ; #----- Vertical levels [m].

   #----- Initialize emission parameters.
   set Sim(EmDurationMin)     360             ; #----- Emission duration [min].
   set Sim(EmNumberParticles) 50000           ; #----- Number of particles.
   set Sim(EmTotMass)         1.0             ; #----- Total mass released [arbitrary mass unit].
   set Sim(EmIsoName)         "137-Cs"        ; #----- Isotope name.
   set Sim(EmDepVel)          1.00E-03        ; #----- Deposition velocity [m/s].
   set Sim(EmHalfLife)        9.47E+08        ; #----- Radioactive half-life [s].
   set Sim(EmWetScav)         1.00E+05        ; #----- Wet scavenging coefficient [dimensionless].
   set Sim(EmBottom)          0.0             ; #----- Emission bottom [m].
   set Sim(EmTop)             1.0             ; #----- Emission top [m].
   set Sim(EmRadius)          1.0             ; #----- Emission radius [m].

   #----- Initialize meteorological parameters.
   MLCD::InitMeteoData 0

   #----- Set meteorological data directories according to meteorological model.
   MLCD::SetMetDataDir $Sim(ObsMetModel)

   #----- Initialize
   set Sim(MetList) ""
   set Sim(ObsList) ""

   #----- Set previous tab no.
   set Sim(TabPrevNo)  0

}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::InitObservationTimes>
# Creation : 11 November 2003 - A. Malo - CMC/CMOE
#
# But      : Initialize observation times.
#
# Parametres :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc MLCD::InitObservationTimes { } {

   global   GDefs
   variable Sim
   variable Lbl
   variable Warning
   variable Error

   #----- Initialize observation time according to nearest lower accident hour.
   set Sim(ObsTime) "$Sim(AccYear)$Sim(AccMonth)$Sim(AccDay)$Sim(AccHour)00"

   #----- Define maximum number of observation according to
   #----- release date-time and 48-hr simulation duration.
   set sec0 [lindex [MLCD::GetInfoDateTime $Sim(ObsTime)] 5]
   set sec  [lindex [MLCD::GetInfoDateTime $Sim(AccDateTime)] 5]
   set sec1 [expr $sec + 48*3600]

   set year  [clock format $sec1 -format "%Y" -gmt true]
   set month [clock format $sec1 -format "%m" -gmt true]
   set day   [clock format $sec1 -format "%d" -gmt true]
   set hour  [clock format $sec1 -format "%H" -gmt true]
   set min   [clock format $sec1 -format "%M" -gmt true]
   set min   [string trimleft $min 0]
   if { $min == "" } { set min 0 }

   set sec1 [expr $sec1 - $min*60]
   set sec1 [expr $sec1 + int(ceil(double($min)/60.0)) * 60 * 60]
   set sec  [expr $sec1 - $sec0]

   set Sim(ObsMaxNb)     [expr int(double($sec)/3600.0) + 1]
   set Sim(ObsNb)        $Sim(ObsMaxNb)

#   puts stdout "\nMaximum number of observations : $Sim(ObsMaxNb)"

   #----- Initialize observation times according to 1-hour time interval between following observations.
   for { set i 1 } { $i < $Sim(ObsMaxNb) } { incr i } {

      set PrevTime [lindex $Sim(ObsTime) [expr $i-1]]

      set PrevTime [MLCD::GetInfoDateTime $PrevTime]
      set year     [lindex $PrevTime 0]
      set month    [lindex $PrevTime 1]
      set day      [lindex $PrevTime 2]
      set hour     [lindex $PrevTime 3]
      set min      [lindex $PrevTime 4]
      set sec      [lindex $PrevTime 5]

      #----- Increment observation time.
      set sec   [expr $sec + 3600]

      set year  [clock format $sec -format "%Y" -gmt true]
      set month [clock format $sec -format "%m" -gmt true]
      set day   [clock format $sec -format "%d" -gmt true]
      set hour  [clock format $sec -format "%H" -gmt true]
      set min   [clock format $sec -format "%M" -gmt true]
      lappend Sim(ObsTime) "$year$month$day$hour$min"

   }

#   puts stdout "Sim(ObsTime): $Sim(ObsTime)"

   #----- Get current observation time according to (selected) observation index.
   MLCD::GetCurrentObservationTime

}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::InitWindProfile>
# Creation : 12 November 2003 - A. Malo - CMC/CMOE
#
# But      : Initialize wind profile.
#
# Parametres :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc MLCD::InitWindProfile { } {
   variable Sim

   #----- Initialize variables.
   set Sim(WindProfile) {}

   for { set i 0 } { $i < $Sim(ObsMaxNb) } { incr i } {

      set prof {}

      for { set j 0 } { $j < $Sim(ObsMaxNbLevels) } { incr j } {

         set height    ""
         set velocity  ""
         set direction ""

         lappend prof [list $height $velocity $direction]
      }

      lappend Sim(WindProfile) $prof

   }

   #----- Save wind profile for future verification.
   set Sim(OldWindProfile) $Sim(WindProfile)

#   puts stderr "Sim(WindProfile): $Sim(WindProfile)"

}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::Max>
# Creation : 19 January 2004 - A. Malo - CMC/CMOE
#
# But      : Compute maximum argument.
#
# Parametres :
#     <args> : List of arguments.
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc MLCD::Max { args } {

   set args [lindex $args 0]

   set len [llength $args]

   if { $len < 1 } {
      puts stderr "*** Error! Wrong number of arguments in MLCD::Max procedure."
      exit
   }

   set max [lindex $args 0]

   for { set i 1 } { $i < $len } { incr i } {
      set element [lindex $args $i]
      set stderr "element: $element"
      if { $element > $max} {
         set max $element
      }
   }

   return $max

}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::MeteoParametersModified>
# Creation : 28 September 2005 - A. Malo - CMC/CMOE
#
# But      : Verify if meteorological parameters have been modified (1)
#            or not (0).
#
# Parametres :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc MLCD::MeteoParametersModified { } {
   variable Sim

   #----- Verify if automation parameters have been modified.
   if { $Sim(ObsNbLevels) != $Sim(OldObsNbLevels) || $Sim(ObsMetModel) !=  $Sim(OldObsMetModel) } {
      return 1
   }

   #----- Verify if date-times have been modified for each observation.
   foreach time $Sim(ObsTime) origtime $Sim(OldObsTime) {

      set stamp     [lindex [MLCD::GetInfoDateTime $time] 6]
      set origstamp [lindex [MLCD::GetInfoDateTime $origtime] 6]

      if { $stamp != $origstamp } {
         return 1
      }

   }

   #----- Verify if local parameters have been modified for each observation.
   foreach lp $Sim(LocalParameters) origlp $Sim(OldLocalParameters) {

      set rough       [lindex $lp 0]
      set obukhov     [lindex $lp 1]
      set precip      [lindex $lp 2]

      set origrough   [lindex $origlp 0]
      set origobukhov [lindex $origlp 1]
      set origprecip  [lindex $origlp 2]

      if { $rough != $origrough || $obukhov != $origobukhov || $precip != $origprecip } {
         return 1
      }

   }

   #----- Verify if wind profiles have been modified for each observation.
   foreach prof $Sim(WindProfile) origprof $Sim(OldWindProfile) {

      foreach level $prof origlevel $origprof {

         set height        [lindex $level 0]
         set velocity      [lindex $level 1]
         set direction     [lindex $level 2]

         set origheight    [lindex $origlevel 0]
         set origvelocity  [lindex $origlevel 1]
         set origdirection [lindex $origlevel 2]

         if { $height != $origheight || $velocity != $origvelocity || $direction != $origdirection } {
            return 1
         }

      }

   }

   return 0
}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::Min>
# Creation : 19 January 2004 - A. Malo - CMC/CMOE
#
# But      : Compute minimum argument.
#
# Parametres :
#     <args> : List of arguments.
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc MLCD::Min { args } {

   set args [lindex $args 0]

   set len [llength $args]

   if { $len < 1 } {
      puts stderr "*** Error! Wrong number of arguments in MLCD::Min procedure."
      exit
   }

   set min [lindex $args 0]

   for { set i 1 } { $i < $len } { incr i } {
      set element [lindex $args $i]
      if { $element < $min} {
         set min $element
      }
   }

   return $min

}

#----------------------------------------------------------------------------
# Nom      : <MLCD::ObukhovFunc>
# Creation : Novembre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Calculer la longueur d'Obukhov.
#
# Parametres :
#   <Sc>     : Atmospheric stability class
#   <Z0>     : Roughness length
#
# Retour:
#
# Remarques :
#    The Obukhov length is computed according to Shir & Shieh formula:
#    Journal of Applied Meteorology, 1974, Vol. 13, No. 2, pp. 189-190.
#
#----------------------------------------------------------------------------
proc MLCD::ObukhovFunc { Sc Z0 } {

   set fs [expr -4.0/(1.0 + 1.3 * pow(abs($Sc), 0.85))]

   if { $Sc >= 0 } {
      set sgn 1.0
   } else {
      set sgn -1.0
   }

   set Length [expr 1.0/($sgn * pow((0.216586 * log(1.2 + 10.0/$Z0)), 2) * pow(10, $fs))]

   return $Length
}

#----------------------------------------------------------------------------
# Nom        : <MLCD::PoolInfo>
# Creation   : Octobre 2001 - J.P. Gauthier - CMC/CMOE
#
# But        : Recuperer l'information descriptive d'une ligne pool.
#
# Parametres :
#   <Info>   : Ligne non modifiee du fichier pool
#
# Retour     :
#
# Remarques  :
#----------------------------------------------------------------------------
proc MLCD::PoolInfo { Info } {

   set Exp::Data(NoSim)  [Info::Strip $Info NoSim]
   set Exp::Data(NoPrev) [Info::Strip $Info NoPrev]
   set Exp::Data(State)  [Info::Strip $Info State]
   set Exp::Data(Desc)   "[Info::Strip $Info DurMin] Min [Info::Strip $Info AccYear][Info::Strip $Info AccMonth][Info::Strip $Info AccDay] [Info::Strip $Info AccHour]:[Info::Strip $Info AccMin] ($Exp::Data(NoSim))"
}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::RefreshMaxNbObsSpinBox>
# Creation : 20 June 2007 - A. Malo - CMC/CMOE
#
# But      : Refresh maximum number of observations in spinbox
#            within editing interface.
#
# Parametres :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc MLCD::RefreshMaxNbObsSpinBox { } {

   variable Sim
   variable Data

   $Data(ObsNoSpinBox) configure -to $Sim(ObsMaxNb)

}

#---------------------------------------------------------------------------
# Nom      : <MLCD::Result>
# Creation : Octobre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Recupere les donnees de simulations.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------
proc MLCD::Result { } {
   variable Sim

   set path [Exp::Path]/[Info::Path $Sim(Info) $Exp::Data(SelectSim)]
   SPI::FileOpen NEW FieldBox "(MLCD) $Exp::Data(No) $Exp::Data(Name)" "" "$path/pos $path/conc"
}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::SelectObs>
# Creation : 11 November 2003 - A. Malo - CMC/CMOE
#
# But      : Select an observation.
#
# Parametres :
#       <No> : Selected Observation Number.
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc MLCD::SelectObs { No } {
   variable Sim
   variable Data

   #----- Check if observation number is valid.
   if { $No > $Sim(ObsMaxNb) || $No < 1 } {
      puts stderr "*** Error! Selecting invalid observation \# $No. Observation number must be between 1 and $Sim(ObsMaxNb)."
      return 0
   }

   #----- Validate current observation (local parameters + wind profile).
   if { ![MLCD::ValidateCurrentObservation] } {
      #----- Modify selected observation to last one.
      set Sim(ObsNo) [expr $Sim(ObsIdx) + 1]
      return 0
   }

   #----- Find index corresponding to selected observation label.
   set Sim(ObsPrevIdx) $Sim(ObsIdx)   ; #----- Set previous observation index.
   set Sim(ObsIdx)     [expr $No - 1] ; #----- Set current observation index.

   #----- Set current observation number.
   if { $Sim(ObsNo) != $No } {
      set Sim(ObsNo) $No
   }

#    puts stdout "\nSim(ObsPrevIdx) : $Sim(ObsPrevIdx)"
#    puts stdout "Sim(ObsIdx)     : $Sim(ObsIdx)"
#    puts stdout "Sim(ObsNo)      : $Sim(ObsNo)"

   #----- Set previous observation.
   MLCD::SetPreviousObs $Sim(ObsPrevIdx)

   #----- Update observation interface.
   MLCD::UpdateObsInterface $Sim(ObsIdx)

   return 1

}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::SetEmissionDuration>
# Creation : 21 June 2007 - A. Malo - CMC/CMOE
#
# But      : Set release duration to simulation duration if
#            simulation duration has changed.
#
# Parametres :
#    <Flag> : Flag indicating if displaying pop-up warning message to user
#             (1: default value) or not (0).
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc MLCD::SetEmissionDuration { { Flag 1 } } {

   global GDefs
   variable Sim
   variable Lbl
   variable Warning
   variable Error

   if { $Sim(EmDurationMin) > $Sim(DurMin) } {

      set OldEmDurMin        $Sim(EmDurationMin)
      set Sim(EmDurationMin) $Sim(DurMin)

      if { $Flag } {
         Dialog::CreateDefault .mlcdnew 500 "[lindex $Lbl(Warning) $GDefs(Lang)]" "[lindex $Warning(EmDuration) $GDefs(Lang)]\n\n\t[lindex $Warning(EmDuration2) $GDefs(Lang)] $Sim(EmDurationMin) $Error(UnitMinutes)\n\t[lindex $Warning(EmDuration3) $GDefs(Lang)] $OldEmDurMin $Error(UnitMinutes)" warning 0 "OK"
      }

      puts stdout "\n*** Warning: Release duration is greater than simulation duration. Thus, release duration is reinitialized (equal to simulation duration)."
      puts stdout "             New release duration \[min\] : $Sim(EmDurationMin)"
      puts stdout "             Old release duration \[min\] : $OldEmDurMin"

   }

}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::SetMetData>
# Creation : 7 November 2003 - A. Malo - CMC/CMOE
#
# But      : Find wind profile and local parameters according to
#            - available meteorological data,
#            - met model,
#            - for each observation times.
#
# Parametres :
#    <Model> : Type of meteorological model.
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc MLCD::SetMetData { Model } {
   global   GDefs
   variable Sim
   variable Data
   variable Error
   variable Warning
   variable Lbl

   #----- Select first observation.
   MLCD::SelectObs 1

   .mlcdnew config -cursor watch
   update idletasks

   #----- Find available meteorological data files.
   if { ![MLCD::FindMetData] } {
      .mlcdnew config -cursor left_ptr
      return 0
   }

   #----- Set observation times according to available met data files.
   MLCD::SetObservationTimes

   #----- Initialize local parameters.
   MLCD::InitLocalParameters

   #----- Initialize wind profile.
   MLCD::InitWindProfile

   #----- Extract meteorological data: local parameters and wind profile.
   MLCD::ExtractMetData

   #----- Update interface.
   MLCD::UpdateObsInterface $Sim(ObsIdx)

   .mlcdnew config -cursor left_ptr

}

#----------------------------------------------------------------------------
# Nom        : <MLCD::SetMetDataDir>
# Creation   : 14 May 2004 - A. Malo - CMC/CMOE
#
# But        : Set (diagnostics and prognostics) meteorological data
#              directories.
#
# Parametres :
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------
proc MLCD::SetMetDataDir { MetModel } {
   variable Sim

   MetData::Path eta $MetModel MLCD::Sim(DBaseDiag) MLCD::Sim(DBaseProg)

}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::SetObsDate>
# Creation : 6 November 2003 - A. Malo - CMC/CMOE
#
# But      : Set observation date.
#
# Parametres :
#  <Seconds> : Time to be set.
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc MLCD::SetObsDate { Seconds } {
   variable Sim

   set Sim(ObsTimeDate)    [clock format $Seconds -format "%a %b %d %Y" -gmt true]
   set Sim(ObsTimeDateSec) $Seconds
   set Sim(ObsTimeYear)    [clock format $Seconds -format "%Y" -gmt true]
   set Sim(ObsTimeMonth)   [clock format $Seconds -format "%m" -gmt true]
   set Sim(ObsTimeDay)     [clock format $Seconds -format "%d" -gmt true]

   set time "$Sim(ObsTimeYear)$Sim(ObsTimeMonth)$Sim(ObsTimeDay)$Sim(ObsTimeHour)$Sim(ObsTimeMin)"
   set Sim(ObsTime) [lreplace $Sim(ObsTime) $Sim(ObsIdx) $Sim(ObsIdx) $time]

}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::SetObservationTimes>
# Creation : 19 June 2007 - A. Malo - CMC/CMOE
#
# But      : Set observation times according to available met data files.
#
# Parametres :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc MLCD::SetObservationTimes { } {

   variable Sim
   variable Data

   #----- Set number of observations and maximum number of observations.
   set Sim(ObsNb)    $Sim(NbMetFiles)
   set Sim(ObsMaxNb) $Sim(ObsNb)

   #----- Refresh maximum number of observations within spibox.
   MLCD::RefreshMaxNbObsSpinBox

   #----- Set observation times, stamps, files.
   set Sim(ObsTime)  {}
   set Sim(ObsStamp) {}
   set Sim(ObsFile)  {}

   foreach data $Sim(MetData) {

      set metdatetime [string range [lindex $data 1] 0 11]
      set metstamp    [lindex $data 0]
      set metfile     [lindex $data 2]
      lappend Sim(ObsTime)  "$metdatetime"
      lappend Sim(ObsStamp) "$metstamp"
      lappend Sim(ObsFile)  "$metfile"

   }

   if { $Sim(ObsNb) < $Sim(ObsMaxNb) } {

      for { set i $Sim(ObsNb) } { $i < $Sim(ObsMaxNb) } { incr i } {

         set PrevTime [lindex $Sim(ObsTime) [expr $i-1]]

         set PrevTime [MLCD::GetInfoDateTime $PrevTime]
         set year     [lindex $PrevTime 0]
         set month    [lindex $PrevTime 1]
         set day      [lindex $PrevTime 2]
         set hour     [lindex $PrevTime 3]
         set min      [lindex $PrevTime 4]
         set sec      [lindex $PrevTime 5]

         #----- Increment observation time.
         set sec   [expr $sec + 3600]

         set year  [clock format $sec -format "%Y" -gmt true]
         set month [clock format $sec -format "%m" -gmt true]
         set day   [clock format $sec -format "%d" -gmt true]
         set hour  [clock format $sec -format "%H" -gmt true]
         set min   [clock format $sec -format "%M" -gmt true]
         lappend Sim(ObsTime) "$year$month$day$hour$min"

      }

   }

   #----- Print info regarding observations.
   set text    "\nNumber of observations          : $Sim(ObsNb)"
   append text "\nMaximum number of observations  : $Sim(ObsMaxNb)"
   append text "\n\nList of observations :"

   for { set i 0 } { $i < $Sim(ObsNb) } { incr i } {
      set j [expr $i + 1]
      set stamp [lindex $Sim(ObsStamp) $i]
      set time  [lindex $Sim(ObsTime)  $i]
      set file  [lindex $Sim(ObsFile)  $i]

      append text "\n[format "%2s" $j] $stamp $time $file"
   }

   puts stdout $text

   set Sim(ObsList) $text

   #----- Get current observation time according to (selected) observation index.
   MLCD::GetCurrentObservationTime

}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::SetObukhovLength>
# Creation : 16 April 2004 - A. Malo - CMC/CMOE
#
# But      : Set atmospheric stability criterion and Obukhov length.
#
# Parametres :
# <AtmStability> : Atmospheric stability criterion value.
#
# Retour :
#
# Remarques :
#    Aucune.
#
#-------------------------------------------------------------------------------
proc MLCD::SetObukhovLength { AtmStability } {

   variable Sim

   #----- Set Atmospheric Stability Criterion.
   set Sim(ObsObukhovCte) $AtmStability

   #----- Set Obukhov Length [m].
   set Sim(ObsObukhov) [format "%.7f" [MLCD::ObukhovFunc $Sim(ObsObukhovCte) $Sim(ObsRough)]]

}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::SetPoolInfo>
# Creation : 11 March 2004 - A. Malo - CMC/CMOE
#
# But      : Set variables for pool info.
#
# Parametres :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc MLCD::SetPoolInfo { } {
   global GDefs
   variable Sim

   #----- Set flags relative to concentration calculations and mesoscale velocity fluctuations.

   set Sim(IsConcS)  [lindex [lindex $Sim(ListOptOnOff) $GDefs(Lang)] $Sim(IsConc)]
   set Sim(IsSigmaS) [lindex [lindex $Sim(ListOptOnOff) $GDefs(Lang)] $Sim(IsSigma)]

   #----- Set variables for pool info.

   set NA [lindex $Sim(NotAvailable) $GDefs(Lang)] ; #----- Not available variable.

   if { $Sim(IsConc) } {

      if { $Sim(GridType) == 1 } {

         #----- Time Variable Lat-Lon Grid at Constant Spatial Resolution.
         set Sim(GridDomain) $NA

      }

   } else {

      #----- No concentration calculations.
      set Sim(GridTypeS)      $NA
      set Sim(GridAlgoS)      $NA
      set Sim(GridDomain)     $NA
      set Sim(VerticalLevels) $NA

   }

}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::SetPrecipitationRate>
# Creation : 16 April 2004 - A. Malo - CMC/CMOE
#
# But      : Set precipitation rate.
#
# Parametres :
#   <PrecipRate> : Precipitation rate value [mm/h].
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc MLCD::SetPrecipitationRate { PrecipRate } {

   variable Sim

   #----- Set Precipitation Rate [mm/h].
   set Sim(ObsPrecip) $PrecipRate

}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::SetPreviousObs>
# Creation : 12 November 2003 - A. Malo - CMC/CMOE
#
# But      : Set previous observation:
#            - observation time,
#            - local parameters,
#            - wind profile.
#
# Parametres :
#      <Idx> : Index of observation time to set.
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc MLCD::SetPreviousObs { Idx } {
   variable Sim
   variable Data

   #----- Set observation time.
   set year  $Sim(ObsTimeYear)
   set month $Sim(ObsTimeMonth)
   set day   $Sim(ObsTimeDay)
   set hour  $Sim(ObsTimeHour)
   set min   $Sim(ObsTimeMin)

   set time "$year$month$day$hour$min"
   set Sim(ObsTime) [lreplace $Sim(ObsTime) $Idx $Idx $time]
#   puts stderr "Sim(ObsTime): $Sim(ObsTime)"

   #----- Set local parameters.
   set rough   $Sim(ObsRough)
   set obukhov $Sim(ObsObukhov)
   set precip  $Sim(ObsPrecip)

   set params "$rough $obukhov $precip"
   set Sim(LocalParameters) [lreplace $Sim(LocalParameters) $Idx $Idx $params]
#   puts stderr "Sim(LocalParameters): $Sim(LocalParameters)"

   #----- Set wind profile.
   set profile {}
   for { set i 0 } { $i < $Sim(ObsMaxNbLevels) } { incr i } {
      set height      [$Data(FrameWindProfile).l$i.z get]
      set velocity    [$Data(FrameWindProfile).l$i.v get]
      set direction   [$Data(FrameWindProfile).l$i.d get]
      lappend profile [list $height $velocity $direction]
   }

   set Sim(WindProfile) [lreplace $Sim(WindProfile) $Idx $Idx $profile]
#   puts stderr "Sim(WindProfile): $Sim(WindProfile)"

}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::SetReleaseDateTime>
# Creation : 3 October 2005 - A. Malo - CMC/CMOE
#
# But      : Set release date-time.
#
# Parametres :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc MLCD::SetReleaseDateTime { } {
   variable Sim

   set Sim(AccYear)  [clock format $Sim(Second) -format "%Y" -gmt true]
   set Sim(AccMonth) [clock format $Sim(Second) -format "%m" -gmt true]
   set Sim(AccDay)   [clock format $Sim(Second) -format "%d" -gmt true]

   if { [string length $Sim(AccMin)] == 1 } {
      set Sim(AccMin) "0$Sim(AccMin)"
   }

   set Sim(AccDateTime) "$Sim(AccYear)$Sim(AccMonth)$Sim(AccDay)$Sim(AccHour)$Sim(AccMin)"
   set info             [MLCD::GetInfoDateTime $Sim(AccDateTime)]
   set Sim(Second)      [lindex $info 5]
   set Sim(AccStamp)    [lindex $info 6]
}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::SetRoughnessLength>
# Creation : 16 April 2004 - A. Malo - CMC/CMOE
#
# But      : Set roughness length and Obukhov length.
#
# Parametres :
#  <RoughLength> : Roughness length value [m].
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc MLCD::SetRoughnessLength { RoughLength } {

   variable Sim

   #----- Set Roughness Length [m].
   set Sim(ObsRough) $RoughLength

   #----- Set Obukhov Length [m].
   set Sim(ObsObukhov) [format "%.7f" [MLCD::ObukhovFunc $Sim(ObsObukhovCte) $Sim(ObsRough)]]

}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::SetSrc>
# Creation : Janvier 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialiser les coordonnees de la source
#
# Parametres :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc MLCD::SetSrc { } {
   variable Sim

   set idx [lsearch -exact $MLCD::Sim(Names) $MLCD::Sim(Src)]
   set MLCD::Sim(Lat) [format "%.6f" [lindex [lindex $MLCD::Sim(Pos) $idx] 1]]
   set MLCD::Sim(Lon) [format "%.6f" [lindex [lindex $MLCD::Sim(Pos) $idx] 2]]
}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::SimInitLaunch>
# Creation : Octobre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialiser le lancement en creant les fichiers de directives et
#            en verifiant les selections de l'usager.
#
# Parametres :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc MLCD::SimInitLaunch { } {
   global GDefs
   variable Sim
   variable Data
   variable Error

   #----- Validate model tab.
   if { ![MLCD::ValidateModelTab] } {
      return 0
   }

   #----- Validate emission tab.
   if { ![MLCD::ValidateEmissionTab] } {
      return 0
   }

   #----- Validate meteo tab.
   if { ![MLCD::ValidateMeteoTab 1] } {
      return 0
   }

   set Sim(NoSim)         [Info::Request $Sim(BasePath)/MLCD.pool] ; #----- Simulation number.
   set Sim(Duration)      [expr int($Sim(DurMin)*60)]              ; #----- Simulation duration [s].
   set Sim(EmDurationSec) [expr int($Sim(EmDurationMin)*60)]       ; #----- Emission duration [s].

   #----- Create simulation directory.
   set Sim(Path) "$Sim(BasePath)/MLCD.$Sim(NoSim).$Sim(AccYear)$Sim(AccMonth)$Sim(AccDay).$Sim(AccHour)$Sim(AccMin)"
   file mkdir $Sim(Path)

   #----- Create output file containing list of available meteorological files.
   if { $Sim(MetList) != "" } {
      set file [open $Sim(Path)/list_metfiles.txt w 0644]
      puts $file $Sim(MetList)
      close $file
   }

   #----- Create output file containing list of observation files.
   if { $Sim(ObsList) != "" } {
      set file [open $Sim(Path)/list_obsfiles.txt w 0644]
      puts $file $Sim(ObsList)
      close $file
   }

   #----- Create emission input file.
   set file [open $Sim(Path)/input.dat w 0644]

   #----- Convert half-life period from [s] to [days].
   set HalfLifeDays [format "%13.7e" [expr double($Sim(EmHalfLife))/86400.0]]

   puts $file "$Sim(Name)"
   puts $file "H"
   puts $file "$Sim(EmDurationSec) $Sim(Duration) $Sim(IsSigma)"
   puts $file "$Sim(EmNumberParticles) $Sim(EmTotMass) $Sim(EmDepVel) $HalfLifeDays $Sim(EmWetScav)"
   puts $file "$Sim(Lon) $Sim(Lat)"
   puts $file "$Sim(EmBottom) $Sim(EmTop) $Sim(EmRadius)"
   puts $file "$Sim(AccYear) $Sim(AccMonth) $Sim(AccDay) $Sim(AccHour) $Sim(AccMin)"
   puts $file "$Sim(NbVerticalLevels)"
   foreach level $Sim(VerticalLevels) {
      puts $file [format "%.2f" $level]
   }
   close $file


   #---- Create met windfield input file.
   set file [open $Sim(Path)/winddata.dat w 0644]

   puts $file "$Sim(Name)"
   puts $file "$Sim(ObsValidNb)"

   #----- Initialize variables.
   set NewObsTime         {}
   set NewLocalParameters {}
   set NewWindProfile     {}

   #----- Loop over valid observations.
   for { set i 0 } { $i < $Sim(ObsValidNb) } { incr i } {

      #----- Get valid observation index.
      set idx   [lindex $Sim(ObsValidIdx) $i]

      #----- Get observation time.
      set time  [lindex $Sim(ObsTime) $idx]
      lappend   NewObsTime $time

      set time  [MLCD::GetInfoDateTime $time]
      set year  [lindex $time 0]
      set month [lindex $time 1]
      set day   [lindex $time 2]
      set hour  [lindex $time 3]
      set min   [lindex $time 4]

      puts $file ""
      puts $file "$year $month $day $hour $min"

      #----- Get local parameters.
      set params    [lindex $Sim(LocalParameters) $idx]
      lappend       NewLocalParameters $params

      set roughness [lindex $params 0]
      set obukhov   [lindex $params 1]
      set precip    [lindex $params 2]

      #----- Get wind profile.
      set windprof [lindex $Sim(WindProfile) $idx]

      set nblevels 0
      set prof {}

      foreach level $windprof { #----- Loop over levels in wind profile.

         set height    [lindex $level 0]
         set velocity  [lindex $level 1]
         set direction [lindex $level 2]

         if { $height != "" && $velocity != "" && $direction != "" } {
            incr nblevels
            lappend prof $level
         }

      }

      if { $nblevels > 0 } {
         lappend NewWindProfile $prof
      }

      puts $file "$roughness $obukhov $precip $nblevels"
      foreach level $prof {
         puts $file "  $level"
      }

   }

   close $file

#   puts stderr "NewObsTime         : $NewObsTime"
#   puts stderr "NewLocalParameters : $NewLocalParameters"
#   puts stderr "NewWindProfile     : $NewWindProfile"

   set Sim(ObsTime)         $NewObsTime
   set Sim(LocalParameters) $NewLocalParameters
   set Sim(WindProfile)     $NewWindProfile

   #----- Set variables for pool info.
   MLCD::SetPoolInfo

   return 1

}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::SimLaunchNew>
# Creation : Octobre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Executer les scripts permettant de lancer le modele.
#
# Parametres :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc MLCD::SimLaunchNew { } {
   global GDefs
   variable Sim

   if { [MLCD::SimInitLaunch] } {

      destroy .mlcdnew

set pool [Info::Code ::MLCD::Sim $Sim(Info) :]

      exec echo "$pool" >> $Sim(BasePath)/MLCD.pool
      exec echo "$pool" >> $Sim(Path)/sim.pool

      #----- Set variables.
      set ModelBin           "$GDefs(Dir)/Bin/$GDefs(Arch)/mlcd"
      set EmissionFile       "input.dat"
      set WindFieldFile      "winddata.dat"
      set PositionsFile      "pos"
      set ConcentrationsFile "conc"
      set ErrorFile          "out.error.txt"
      set DiagFile           "out.diag.txt"
      set OutTimeFile        "out.timing.txt"
      set ScriptFile         "$Sim(Path)/exec_mlcd.ksh"
      set OutputFile         "$Sim(Path)/out.exec_mlcd.txt"

      set NbLinesPos   4
      set NbLinesBegin 7
      set NbLinesEnd   9
      set NbLinesOther 4

      set NbOutputTimeSteps     [expr $Sim(DurMin)/$Sim(OutputTimeStepMin)]
      set NbOutputTimeStepsTrue $NbOutputTimeSteps
      set NbModelTimeSteps      [expr $Sim(DurMin)/$Sim(ModelTimeStepMin)]
      set NbLinesConcFirst      0
      set NbLinesPosFirst       0
      set NbLinesOtherFirst     0

      if { $Sim(IsConc) } {

         #----- Concentrations are computed.

         if { $Sim(GridType) == 2 || $Sim(GridType) == 3 || $Sim(GridType) == 4 } {

            #----- - Cylindrical Equidistant (Lat-Lon) time-fixed grid at constant spatial resolution.
            #----- - Cylindrical Equidistant (Lat-Lon) time-fixed grid at constant spatial resolution
            #-----   with a fine constant resolution core.
            #----- - Cylindrical Equidistant (Lat-Lon) time-fixed grid at variable spatial resolution.

            set NbOutputTimeSteps [expr $NbOutputTimeSteps - 1]
            set NbLinesConc       [expr 6 + $Sim(NbVerticalLevels) - 1]

            set NbLinesConcFirst  [expr 8 + $Sim(NbVerticalLevels) - 1]
            set NbLinesPosFirst   $NbLinesPos
            set NbLinesOtherFirst 4

         } elseif { $Sim(GridType) == 0 } {

            #----- Polar Stereographic time-fixed grid at constant spatial resolution.
            set NbLinesConc [expr 6 + $Sim(NbVerticalLevels) - 1]

         } elseif { $Sim(GridType) == 1 } {

            #----- Cylindrical Equidistant (Lat-Lon) time-variable grid at constant spatial resolution.
            set NbLinesConc [expr 8 + $Sim(NbVerticalLevels) - 1]

         }

      } else {

         #----- Concentrations are not computed.
         set NbLinesConc 0

      }

      #----- Total number of output lines.
      set NbLines [expr $NbOutputTimeSteps * ($NbLinesConc + $NbLinesPos + $NbLinesOther) + \
                        ($NbLinesConcFirst + $NbLinesPosFirst + $NbLinesOtherFirst) + \
                        2 * ($NbModelTimeSteps - $NbOutputTimeStepsTrue) + $NbLinesBegin + $NbLinesEnd]

      set len [expr [string length $Sim(Path)] + 10]

      #----- Create script file to launch model.
      set file [open $ScriptFile w 0755]
      puts $file "#!/bin/ksh"
      puts $file ""
      puts $file ". ~/.profile > /dev/null 2>&1"
      puts $file ""
      puts $file "echo \"\""
      puts $file "date +\"Start date-time: %Y-%m-%d %T (%c %Z)\""
      puts $file "echo \"\""
      puts $file ""
      puts $file "[format "%-${len}s" "bin=\"$ModelBin\""] \# Binary executable file."
      puts $file "[format "%-${len}s" "dir=\"$Sim(Path)\""] \# Experiment directory."
      puts $file "[format "%-${len}s" "error=\"\${dir}/$ErrorFile\""] \# Output error file."
      puts $file "[format "%-${len}s" "outtime=\"\${dir}/$OutTimeFile\""] \# Output timing file."
      if { $Sim(IsDiag) } {
         puts $file "[format "%-${len}s" "diag=\"-diag \${dir}/$DiagFile\""] \# Output diagnostic file."
      } else {
         puts $file "[format "%-${len}s" "diag=\"\""] \# No output diagnostic file."
      }
      puts $file "[format "%-${len}s" "emission=\"\${dir}/$EmissionFile\""] \# Release input file."
      puts $file "[format "%-${len}s" "windfield=\"\${dir}/$WindFieldFile\""] \# Windfield input file."
      puts $file ""
      puts $file "[format "%-${len}s" "pos=\"\${dir}/$PositionsFile\""] \# Output position file."
      if { $Sim(IsConc) } {
         puts $file "[format "%-${len}s" "conc=\"\${dir}/$ConcentrationsFile\""] \# Output concentration file."
         puts $file "[format "%-${len}s" "grid=\"$Sim(GridType)\""] \# Type of grid."
         puts $file "[format "%-${len}s" "algo=\"$Sim(GridAlgo)\""] \# Type of algorithm (0: The nearest grid point, 1: The 4 nearest grid points)."
         puts $file "[format "%-${len}s" "domain=\"$Sim(GridDomain)\""] \# Grid domain for concentration calculations \[km\]."
         puts $file "conc=\"-conc \${conc} -grid \${grid} -algo \${algo} -domain \${domain}\""
      } else {
         puts $file "[format "%-${len}s" "conc=\"\""] \# No concentration calculations."
      }
      puts $file ""
      puts $file "[format "%-${len}s" "mode=\"$Sim(Mode)\""] \# Mode type (0: forward, 1: backward)."
      puts $file "[format "%-${len}s" "outputts=\"$Sim(OutputTimeStepMin)\""] \# Output time step \[min\]."
      puts $file "[format "%-${len}s" "modelts=\"$Sim(ModelTimeStepMin)\""] \# Model time step \[min\]."
      puts $file "[format "%-${len}s" "seed=\"$Sim(IsRanVar)\""] \# Type of seed (0: fixed seed, 1: variable seed)."
      puts $file ""
      puts $file "(date +\"Start date-time: %Y-%m-%d %T (%c %Z)\") > \${outtime}"
      puts $file ""
      puts $file "time \${bin} \\"
      puts $file "-mode \${mode} \\"
      puts $file "-emission \${emission} \\"
      puts $file "-windfield \${windfield} \\"
      puts $file "-pos \${pos} \\"
      puts $file "\${conc} \\"
      puts $file "-outputts \${outputts} \\"
      puts $file "-modelts \${modelts} \\"
      puts $file "-seed \${seed} \\"
      puts $file "-error \${error} \\"
      puts $file "\${diag}"
      puts $file ""
      puts $file "(date +\"End date-time: %Y-%m-%d %T (%c %Z)\") >> \${outtime}"
      puts $file ""
      puts $file "echo \"\""
      puts $file "date +\"End date-time: %Y-%m-%d %T (%c %Z)\""
      puts $file "echo \"\""

      close $file

      #----- Launch model.
      Debug::TraceProc "MLCD: Launching short-range dispersion model."
      Exp::Launch "$ScriptFile" "$pool" $NbLines $OutputFile

      #----- Relire les experiences
      Model::Check 0
   }
}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::SimSuppress>
# Creation : Octobre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Supprimer une simulation.
#
# Parametres  :
#   <Confirm> : Confirmation de la suppression
#   <Pool>    : Identificateur de la simulation
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc MLCD::SimSuppress { Confirm Pool } {
   global GDefs
   variable Msg
   variable Lbl
   variable Sim

   . config -cursor watch
   update idletasks

   set path "[Exp::Path]/[Info::Path $Sim(Info) $Pool]"

   if { $Confirm } {

      #----- Verifier la validite des parametres.

      set answer [Dialog::CreateDefault . 400 "Message" "[lindex $Msg(SuppressSim) $GDefs(Lang)]\n\n$path" \
         warning 0 [lindex $Lbl(Yes) $GDefs(Lang)] [lindex $Lbl(No) $GDefs(Lang)]]

      if { $answer == 1 } {
         return
      }
   }

   #----- Supprimer la simulation et ses descendants

   Debug::TraceProc "MLCD: Suppressing: $path"

   Exp::Kill    $Pool
   Info::Delete [Exp::Path]/MLCD.pool $Pool
   file delete -force $path

   #----- Relire les experiences

   . config -cursor left_ptr
   Model::Check 0
}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::SpeciesFormat>
# Creation : 16 March 2004 - A. Malo - CMC/CMOE
#
# But      : Format the returned ligne by the isotope selector module.
#
# Parametres :
#   <Line>   : Ligne de definiton d'un isotope
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc MLCD::SpeciesFormat { Line } {
   global GDefs
   variable Sim
   variable Warning
   variable Lbl

   if { [llength $Line]==7 } {

      set name        [lindex $Line 0]                 ; #----- Isotope Name.
      set halflife    [format "%.2E" [lindex $Line 2]] ; #----- Half-Life [s].
      set drydepvel   [lindex $Line 5]                 ; #----- Dry Deposition Velocity [m/s].
      #----- Here, we ignore the wet scavenging rate [s^-1] since
      #----- MLCD takes into account the wet scavenging coefficient (1.00E+05).

      if { $halflife > 0 } {
         #----- Verify that the isotope's radioactive half-life is positive.
         set Sim(EmIsoName)  $name
         set Sim(EmHalfLife) $halflife
         set Sim(EmDepVel)   $drydepvel

      } else {
         #----- Display warning message if radioactive half-life is negative or zero.
         Dialog::CreateDefault .mlcdnew 500 "[lindex $Lbl(Warning) $GDefs(Lang)]" "[lindex $Warning(HalfLife) $GDefs(Lang)] $name." warning 0 "OK"

         puts stderr ""
         puts stderr "WARNING: Isotope $name has a null or negative radioactive half-life value."
         puts stderr "         This isotope will be ignored."
         puts stderr "         Half-life: $halflife s."
      }
   }
}

#----------------------------------------------------------------------------
# Nom        : <MLCD::UpdateEmissionStartingTime>
# Creation   : 4 Octobre 2005 - A. Malo - CMC/CMOE
#
# But        : Update emission starting time for consistency with
#              model time step.
#
# Parametres :
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------
proc MLCD::UpdateEmissionStartingTime { } {
   variable Sim

   set sec [clock scan "$Sim(AccYear)$Sim(AccMonth)$Sim(AccDay) $Sim(AccHour):00" -gmt true]

   set min [string trimleft $Sim(AccMin) 0]
   if { $min == "" } {
      set min 0
   }
   set min [expr int(double($min)/double($Sim(ModelTimeStepMin))+0.5) * $Sim(ModelTimeStepMin)]

   set Sim(Second)      [expr $sec + $min*60]
   set Sim(AccYear)     [clock format $Sim(Second) -format "%Y" -gmt true]
   set Sim(AccMonth)    [clock format $Sim(Second) -format "%m" -gmt true]
   set Sim(AccDay)      [clock format $Sim(Second) -format "%d" -gmt true]
   set Sim(AccHour)     [clock format $Sim(Second) -format "%H" -gmt true]
   set Sim(AccMin)      [clock format $Sim(Second) -format "%M" -gmt true]
   set Sim(AccDateTime) "$Sim(AccYear)$Sim(AccMonth)$Sim(AccDay)$Sim(AccHour)$Sim(AccMin)"
   set Sim(AccStamp)    [GetDateTimeStamp $Sim(AccDateTime)]

   #----- Initialize old release date-time.
   set Sim(OldAccYear)     $Sim(AccYear)
   set Sim(OldAccMonth)    $Sim(AccMonth)
   set Sim(OldAccDay)      $Sim(AccDay)
   set Sim(OldAccHour)     $Sim(AccHour)
   set Sim(OldAccMin)      $Sim(AccMin)
   set Sim(OldSecond)      $Sim(Second)
   set Sim(OldAccStamp)    $Sim(AccStamp)
   set Sim(OldAccDateTime) $Sim(AccDateTime)

}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::UpdateObsInterface>
# Creation : 11 November 2003 - A. Malo - CMC/CMOE
#
# But      : Update observation in the editing interface.
#
# Parametres :
#      <Idx> : Index of observation time to update.
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc MLCD::UpdateObsInterface { Idx } {
   variable Sim
   variable Data

   #----- Refresh maximum number of observations within spibox.
   MLCD::RefreshMaxNbObsSpinBox

   #----- Get observation date-time according to selected observation.
   set time  [lindex $Sim(ObsTime) $Idx]

   set time  [MLCD::GetInfoDateTime $time]
   set year  [lindex $time 0]
   set month [lindex $time 1]
   set day   [lindex $time 2]
   set hour  [lindex $time 3]
   set min   [lindex $time 4]
   set sec   [lindex $time 5]

   #----- Update observation date.
   set Sim(ObsTimeDate)    [clock format $sec -format "%a %b %d %Y" -gmt true]
   set Sim(ObsTimeDateSec) $sec
   set Sim(ObsTimeYear)    $year
   set Sim(ObsTimeMonth)   $month
   set Sim(ObsTimeDay)     $day

   #----- Update observation time.
   set Sim(ObsTimeHour) $hour
   set Sim(ObsTimeMin)  $min

   #----- Update local parameters.
   set params          [lindex $Sim(LocalParameters) $Idx]
   set Sim(ObsRough)   [lindex $params 0]
   set Sim(ObsObukhov) [lindex $params 1]
   set Sim(ObsPrecip)  [lindex $params 2]

   #----- Erase wind profile interface.
   for { set i 0 } { $i < $Sim(ObsMaxNbLevels) } { incr i } {
      $Data(FrameWindProfile).l$i.z delete 0 end
      $Data(FrameWindProfile).l$i.v delete 0 end
      $Data(FrameWindProfile).l$i.d delete 0 end
   }

   #----- Update wind profile.
   set profiles [lindex $Sim(WindProfile) $Idx]

   for { set i 0 } { $i < $Sim(ObsMaxNbLevels) } { incr i } {
      set level [lindex $profiles $i]
      $Data(FrameWindProfile).l$i.z insert 0 [lindex $level 0]
      $Data(FrameWindProfile).l$i.v insert 0 [lindex $level 1]
      $Data(FrameWindProfile).l$i.d insert 0 [lindex $level 2]
   }

}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::ValidateCurrentObservation>
# Creation : 13 November 2003 - A. Malo - CMC/CMOE
#
# But      : Validate current observation.
#            - Local Parameters,
#            - Wind Profile.
# Parametres :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc MLCD::ValidateCurrentObservation { } {
   variable Sim

   #----- Validate local parameters.
   if { ![MLCD::ValidateLocalParameters] } {
      return 0
   }

   #----- Validate wind profile.
   if { ![MLCD::ValidateWindProfile] } {
      return 0
   }

   return 1
}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::ValidateEmissionTab>
# Creation : 19 November 2003 - A. Malo - CMC/CMOE
#
# But      : Validate emission tab parameters.
#            - Number of particles;
#            - Total release mass;
#            - Deposition velocity;
#            - Half-Life;
#            - Wet scavenging coefficient;
#            - Emission duration;
#            - Bottom of column;
#            - Top of column;
#            - Radius of column.
#
# Parametres :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc MLCD::ValidateEmissionTab { } {
   global GDefs
   variable Sim
   variable Data
   variable Error


   #----- Verify that the number of particles is an integer positive value.
   set idx ""
   set number [string is integer -strict -failindex idx $Sim(EmNumberParticles)]
   if { $number == 0 && $idx == -1 } {
      Dialog::CreateError .mlcdnew "[lindex $Error(EmNbPartRange) $GDefs(Lang)] $Sim(EmNumberParticles)." $GDefs(Lang) 600
      focus $Data(EmNbPartEnt)
      return 0
   } elseif { $number == 0 || ($number == 1 && $Sim(EmNumberParticles) <= 0) } {
      Dialog::CreateError .mlcdnew "[lindex $Error(EmNbPart) $GDefs(Lang)] $Sim(EmNumberParticles)." $GDefs(Lang) 600
      focus $Data(EmNbPartEnt)
      return 0
   }

   #----- Verify that the total release mass is a positive number.
   set idx ""
   set number [string is double -strict -failindex idx $Sim(EmTotMass)]
   if { $number == 0 && $idx == -1 } {
      Dialog::CreateError .mlcdnew "[lindex $Error(EmTotMassRange) $GDefs(Lang)] $Sim(EmTotMass) [lindex $Error(UnitMass) $GDefs(Lang)]" $GDefs(Lang) 600
      focus $Data(EmTotMassEnt)
      return 0
   } elseif { $number == 0 || ($number == 1 && $Sim(EmTotMass) <= 0) } {
      Dialog::CreateError .mlcdnew "[lindex $Error(EmTotMass) $GDefs(Lang)] $Sim(EmTotMass) [lindex $Error(UnitMass) $GDefs(Lang)]" $GDefs(Lang) 600
      focus $Data(EmTotMassEnt)
      return 0
   }

   #----- Verify that the deposition velocity is greater or equal to 0.
   set idx ""
   set number [string is double -strict -failindex idx $Sim(EmDepVel)]
   if { $number == 0 && $idx == -1 } {
      Dialog::CreateError .mlcdnew "[lindex $Error(EmDepVelRange) $GDefs(Lang)] $Sim(EmDepVel) $Error(UnitVelocity)" $GDefs(Lang) 600
      focus $Data(EmDepVelEnt)
      return 0
   } elseif { $number == 0 || ($number == 1 && $Sim(EmDepVel) < 0) } {
      Dialog::CreateError .mlcdnew "[lindex $Error(EmDepVel) $GDefs(Lang)] $Sim(EmDepVel) $Error(UnitVelocity)" $GDefs(Lang) 600
      focus $Data(EmDepVelEnt)
      return 0
   }

   #----- Verify that the radioactive half-life is a positive number.
   set idx ""
   set number [string is double -strict -failindex idx $Sim(EmHalfLife)]
   if { $number == 0 && $idx == -1 } {
      Dialog::CreateError .mlcdnew "[lindex $Error(EmHalfLifeRange) $GDefs(Lang)] $Sim(EmHalfLife) [lindex $Error(UnitDay) $GDefs(Lang)]" $GDefs(Lang) 600
      focus $Data(EmHalfLifeEnt)
      return 0
   } elseif { $number == 0 || ($number == 1 && $Sim(EmHalfLife) <= 0) } {
      Dialog::CreateError .mlcdnew "[lindex $Error(EmHalfLife) $GDefs(Lang)] $Sim(EmHalfLife) [lindex $Error(UnitDay) $GDefs(Lang)]" $GDefs(Lang) 600
      focus $Data(EmHalfLifeEnt)
      return 0
   }

   #----- Verify that the wet scavenging coefficient is greater or equal to 0.
   set idx ""
   set number [string is double -strict -failindex idx $Sim(EmWetScav)]
   if { $number == 0 && $idx == -1 } {
      Dialog::CreateError .mlcdnew "[lindex $Error(EmWetScavRange) $GDefs(Lang)] $Sim(EmWetScav)." $GDefs(Lang) 600
      focus $Data(EmWetScavEnt)
      return 0
   } elseif { $number == 0 || ($number == 1 && $Sim(EmWetScav) < 0) } {
      Dialog::CreateError .mlcdnew "[lindex $Error(EmWetScav) $GDefs(Lang)] $Sim(EmWetScav)." $GDefs(Lang) 600
      focus $Data(EmWetScavEnt)
      return 0
   }

   #----- Verify that the release duration is an integer positive value.
   set idx ""
   set number [string is integer -strict -failindex idx $Sim(EmDurationMin)]
   if { $number == 0 && $idx == -1 } {
      Dialog::CreateError .mlcdnew "[lindex $Error(EmDurationRange) $GDefs(Lang)] $Sim(EmDurationMin) $Error(UnitMinutes)" $GDefs(Lang) 600
      focus $Data(EmDurationEnt)
      return 0
   } elseif { $number == 0 || ($number == 1 && $Sim(EmDurationMin) <= 0) } {
      Dialog::CreateError .mlcdnew "[lindex $Error(EmDuration) $GDefs(Lang)] $Sim(EmDurationMin) $Error(UnitMinutes)" $GDefs(Lang) 600
      focus $Data(EmDurationEnt)
      return 0
   }

   #----- Verify that the release duration is greater or equal to model time step.
   if { $Sim(EmDurationMin) < $Sim(ModelTimeStepMin) } {
      Dialog::CreateError .mlcdnew "[lindex $Error(EmDuration2) $GDefs(Lang)][lindex $Error(EmDuration3) $GDefs(Lang)] $Sim(EmDurationMin) $Error(UnitMinutes)\n[lindex $Error(EmDuration4) $GDefs(Lang)] $Sim(ModelTimeStepMin) $Error(UnitMinutes)" $GDefs(Lang) 600
      focus $Data(EmDurationEnt)
      return 0
   }

   #----- Redefine release duration if greater than simulation duration.
   MLCD::SetEmissionDuration

   #----- Verify that bottom of release column is greater or equal to 0.
   set idx ""
   set number [string is double -strict -failindex idx $Sim(EmBottom)]
   if { $number == 0 && $idx == -1 } {
      Dialog::CreateError .mlcdnew "[lindex $Error(EmBottomRange) $GDefs(Lang)] $Sim(EmBottom) $Error(UnitMeters)" $GDefs(Lang) 600
      focus $Data(EmBottomEnt)
      return 0
   } elseif { $number == 0 || ($number == 1 && $Sim(EmBottom) < 0) } {
      Dialog::CreateError .mlcdnew "[lindex $Error(EmBottom) $GDefs(Lang)] $Sim(EmBottom) $Error(UnitMeters)" $GDefs(Lang) 600
      focus $Data(EmBottomEnt)
      return 0
   }

   #----- Verify that top of release column is greater than bottom of column.
   set idx ""
   set number [string is double -strict -failindex idx $Sim(EmTop)]
   if { $number == 0 && $idx == -1 } {
      Dialog::CreateError .mlcdnew "[lindex $Error(EmTopRange) $GDefs(Lang)] $Sim(EmTop) $Error(UnitMeters)" $GDefs(Lang) 600
      focus $Data(EmTopEnt)
      return 0
   } elseif { $number == 0 || ($number == 1 && $Sim(EmTop) <= $Sim(EmBottom)) } {
      Dialog::CreateError .mlcdnew "[lindex $Error(EmTop) $GDefs(Lang)] $Sim(EmTop) $Error(UnitMeters)" $GDefs(Lang) 600
      focus $Data(EmTopEnt)
      return 0
   }

   #----- Verify that release column radius is greater or euql to 0.
   set idx ""
   set number [string is double -strict -failindex idx $Sim(EmRadius)]
   if { $number == 0 && $idx == -1 } {
      Dialog::CreateError .mlcdnew "[lindex $Error(EmRadiusRange) $GDefs(Lang)] $Sim(EmRadius) $Error(UnitMeters)" $GDefs(Lang) 600
      focus $Data(EmRadiusEnt)
      return 0
   } elseif { $number == 0 || ($number == 1 && $Sim(EmRadius) < 0) } {
      Dialog::CreateError .mlcdnew "[lindex $Error(EmRadius) $GDefs(Lang)] $Sim(EmRadius) $Error(UnitMeters)" $GDefs(Lang) 600
      focus $Data(EmRadiusEnt)
      return 0
   }

   return 1
}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::ValidateInterface>
# Creation : 19 November 2003 - A. Malo - CMC/CMOE
#
# But      : Validate parameters inside interface of tabs when switching
#            between tabs.
#
# Parametres :
#      <Tab> : Tab.
#       <No> : No of tab.
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc MLCD::ValidateInterface { Tab No } {
   variable Sim
   variable Data

   set Data(TabFrameName) $Tab

   if { $No != 0 && $Sim(TabPrevNo) == 0 } {
      if { ![MLCD::ValidateModelTab] } {
         TabFrame::Select $Tab 0 ; #----- Select 'Model' tab.
         return 0
      }
   }

   if { $No != 1 && $Sim(TabPrevNo) == 1 } {
      if { ![MLCD::ValidateEmissionTab] } {
         TabFrame::Select $Tab 1 ; #----- Select 'Emission' tab.
         return 0
      }
   }

   if { $No != 2 && $Sim(TabPrevNo) == 2 } {
      if { ![MLCD::ValidateMeteoTab] } {
         TabFrame::Select $Tab 2 ; #----- Select 'Meteo' tab.
         return 0
      }
   }

   #----- Set previous tab no.
   set Sim(TabPrevNo) $No

}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::ValidateLocalParameters>
# Creation : 13 November 2003 - A. Malo - CMC/CMOE
#
# But      : Validate current local parameters:
#            - Roughness Length,
#            - Obukhov Length,
#            - Precipitation Rate.
#
# Parametres :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc MLCD::ValidateLocalParameters { } {
   global   GDefs
   variable Sim
   variable Error
   variable Data

   #----- Verify that roughness length is a positive value.
   set idx ""
   set number [string is double -strict -failindex idx $Sim(ObsRough)]
   if { $number == 0 && $idx == -1 } {
      Dialog::CreateError .mlcdnew "[lindex $Error(ObsRoughnessLengthRange) $GDefs(Lang)] $Sim(ObsRough) $Error(UnitMeters)" $GDefs(Lang) 600
      focus $Data(ObsRoughnessLengthEnt)
      return 0
   } elseif { $number == 0 || ($number == 1 && $Sim(ObsRough) <= 0) } {
      Dialog::CreateError .mlcdnew "[lindex $Error(ObsRoughnessLength) $GDefs(Lang)] $Sim(ObsRough) $Error(UnitMeters)" $GDefs(Lang) 600
      focus $Data(ObsRoughnessLengthEnt)
      return 0
   }

   #----- Verify that Obukhov length is a double value.
   set idx ""
   set number [string is double -strict -failindex idx $Sim(ObsObukhov)]
   if { $number == 0 && $idx == -1 } {
      Dialog::CreateError .mlcdnew "[lindex $Error(ObsObukhovLengthRange) $GDefs(Lang)] $Sim(ObsObukhov) $Error(UnitMeters)" $GDefs(Lang) 600
      focus $Data(ObsObukhovLengthEnt)
      return 0
   } elseif { $number == 0 } {
      Dialog::CreateError .mlcdnew "[lindex $Error(ObsObukhovLength) $GDefs(Lang)] $Sim(ObsObukhov) $Error(UnitMeters)" $GDefs(Lang) 600
      focus $Data(ObsObukhovLengthEnt)
      return 0
   }

   #----- Verify that precipitation rate is a value greater or equal to 0.
   set idx ""
   set number [string is double -strict -failindex idx $Sim(ObsPrecip)]
   if { $number == 0 && $idx == -1 } {
      Dialog::CreateError .mlcdnew "[lindex $Error(ObsPrecipRateRange) $GDefs(Lang)] $Sim(ObsPrecip) $Error(UnitPrecip)" $GDefs(Lang) 600
      focus $Data(ObsPrecipRateEnt)
      return 0
   } elseif { $number == 0 || ($number == 1 && $Sim(ObsPrecip) < 0) } {
      Dialog::CreateError .mlcdnew "[lindex $Error(ObsPrecipRate) $GDefs(Lang)] $Sim(ObsPrecip) $Error(UnitPrecip)" $GDefs(Lang) 600
      focus $Data(ObsPrecipRateEnt)
      return 0
   }

   return 1
}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::ValidateMeteoTab>
# Creation : 19 November 2003 - A. Malo - CMC/CMOE
#
# But      : Validate meteo tab parameters.
#
# Parametres :
#    <Check> : Flag indicating if
#              1. Exit function when SelectObs fails
#                 (saving last current observation).
#              2. Compute number of valid wind profiles.
#              3. Validate observation date-times.
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc MLCD::ValidateMeteoTab { { Check 0 } } {
   variable Sim

#   puts stderr "-->> Validate meteo tab parameters"

   #----- Validate current observation (local parameters + wind profile).
   if { ![MLCD::ValidateCurrentObservation] } {
      return 0
   }

   #----- Save last current observation.
   if { ![MLCD::SelectObs $Sim(ObsNo)] } {
      if { $Check } {
         return 0
      }
   }

   if { $Check } {

      #----- Compute number of valid wind profiles.
      if { ![MLCD::ComputeNbValidWindProfiles] } {
         return 0
      }

      #----- Validate observation date-times.
      if { ![MLCD::ValidateObsDateTime] } {
         return 0
      }

   }

   return 1
}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::ValidateModelTab>
# Creation : 19 November 2003 - A. Malo - CMC/CMOE
#
# But      : Validate model tab parameters.
#            - Simulation duration;
#            - Output time step;
#            - Model time step.
#
# Parametres :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc MLCD::ValidateModelTab { } {
   global GDefs
   variable Sim
   variable Data
   variable Error

#   puts stderr "-->> Validate model tab parameters"

   #----- Update release date-time.
   MLCD::SetReleaseDateTime

   #----- Verify that simulation duration is an integer positive value.
   set idx ""
   set number [string is integer -strict -failindex idx $Sim(DurMin)]
   if { $number == 0 && $idx == -1 } {
      Dialog::CreateError .mlcdnew "[lindex $Error(SimDurationRange) $GDefs(Lang)] $Sim(DurMin) $Error(UnitMinutes)" $GDefs(Lang) 600
      focus $Data(SimDurationEnt)
      return 0
   } elseif { $number == 0 || ($number == 1 && $Sim(DurMin) <= 0) } {
      Dialog::CreateError .mlcdnew "[lindex $Error(SimDuration) $GDefs(Lang)] $Sim(DurMin) $Error(UnitMinutes)" $GDefs(Lang) 600
      focus $Data(SimDurationEnt)
      return 0
   }

   set Sim(Duration) [expr int($Sim(DurMin)*60)] ; #----- Simulation duration [s].

   MLCD::CheckIfEmissionStartingTimeModified ; #----- Check if emission starting time has been modified.
   MLCD::CheckIfSimulationDurationModified   ; #----- Check if simulation duration has been modified.

   #----- Verify if emission starting time or simulation duration have been modified.
   if { $Sim(ReleaseDateTimeChanged) || $Sim(SimDurationChanged) } {

      #----- Verify if meteorological parameters have been modified or not.
      if { [MLCD::MeteoParametersModified] } {

         #----- Ask user if re-initializing meteo data when meteo parameters are modified.
         MLCD::AskIfInitMeteoData

      } else {

         #----- Re-initialize meteo data without asking user when meteo parameters are not modified.
         MLCD::InitMeteoData

      }

   }

   #----- Verify that output time step is an integer positive value.
   set idx ""
   set number [string is integer -strict -failindex idx $Sim(OutputTimeStepMin)]
   if { $number == 0 && $idx == -1 } {
      Dialog::CreateError .mlcdnew "[lindex $Error(OutputTimeStepRange) $GDefs(Lang)] $Sim(OutputTimeStepMin) $Error(UnitMinutes)" $GDefs(Lang) 600
      focus $Data(OutputTimeStepEnt)
      return 0
   } elseif { $number == 0 || ($number == 1 && $Sim(OutputTimeStepMin) <= 0) } {
      Dialog::CreateError .mlcdnew "[lindex $Error(OutputTimeStep1) $GDefs(Lang)] $Sim(OutputTimeStepMin) $Error(UnitMinutes)" $GDefs(Lang) 600
      focus $Data(OutputTimeStepEnt)
      return 0
   }

   #----- Verify that model time step is an integer positive value.
   set idx ""
   set number [string is integer -strict -failindex idx $Sim(ModelTimeStepMin)]
   if { $number == 0 && $idx == -1 } {
      Dialog::CreateError .mlcdnew "[lindex $Error(ModelTimeStepRange) $GDefs(Lang)] $Sim(ModelTimeStepMin) $Error(UnitMinutes)" $GDefs(Lang) 600
      focus $Data(ModelTimeStepEnt)
      return 0
   } elseif { $number == 0 || ($number == 1 && $Sim(ModelTimeStepMin) <= 0) } {
      Dialog::CreateError .mlcdnew "[lindex $Error(ModelTimeStep1) $GDefs(Lang)] $Sim(ModelTimeStepMin) $Error(UnitMinutes)" $GDefs(Lang) 600
      focus $Data(ModelTimeStepEnt)
      return 0
   }

   #----- Verify that output time step is lower (or equal to) simulation duration.
   if { $Sim(OutputTimeStepMin) > $Sim(DurMin) } {
      Dialog::CreateError .mlcdnew "[lindex $Error(OutputTimeStep2) $GDefs(Lang)][lindex $Error(OutputTimeStep3) $GDefs(Lang)] $Sim(OutputTimeStepMin) $Error(UnitMinutes)\n[lindex $Error(OutputTimeStep4) $GDefs(Lang)] $Sim(DurMin) $Error(UnitMinutes)" $GDefs(Lang) 600
      focus $Data(OutputTimeStepEnt)
      return 0
   }

   #----- Verify that model time step is lower (or equal to) release duration.
   if { $Sim(ModelTimeStepMin) > $Sim(EmDurationMin) } {
      Dialog::CreateError .mlcdnew "[lindex $Error(ModelTimeStep2) $GDefs(Lang)][lindex $Error(ModelTimeStep3) $GDefs(Lang)] $Sim(ModelTimeStepMin) $Error(UnitMinutes)\n[lindex $Error(ModelTimeStep4) $GDefs(Lang)] $Sim(EmDurationMin) $Error(UnitMinutes)" $GDefs(Lang) 600
      focus $Data(ModelTimeStepEnt)
      return 0
   }

   #----- Verify that model time step is lower (or equal) to simulation duration.
   if { $Sim(ModelTimeStepMin) > $Sim(DurMin) } {
      Dialog::CreateError .mlcdnew "[lindex $Error(ModelTimeStep5) $GDefs(Lang)][lindex $Error(ModelTimeStep6) $GDefs(Lang)] $Sim(ModelTimeStepMin) $Error(UnitMinutes)\n[lindex $Error(ModelTimeStep7) $GDefs(Lang)] $Sim(DurMin) $Error(UnitMinutes)" $GDefs(Lang) 600
      focus $Data(ModelTimeStepEnt)
      return 0
   }

   #----- Verify that model time step is greater than (or equal to) 1 minute and lower than (or equal to) 60 min.
   if { $Sim(ModelTimeStepMin) < 1 || $Sim(ModelTimeStepMin) > 60 } {
      Dialog::CreateError .mlcdnew "[lindex $Error(ModelTimeStep11) $GDefs(Lang)][lindex $Error(ModelTimeStep3) $GDefs(Lang)] $Sim(ModelTimeStepMin) $Error(UnitMinutes)" $GDefs(Lang) 600
      focus $Data(ModelTimeStepEnt)
      return 0
   }

   #----- Verify that model time step is an integer divisor of 60 minutes.
   if { [expr fmod(60, $Sim(ModelTimeStepMin))] > $Sim(Epsilon) } {
      Dialog::CreateError .mlcdnew "[lindex $Error(ModelTimeStep12) $GDefs(Lang)][lindex $Error(ModelTimeStep3) $GDefs(Lang)] $Sim(ModelTimeStepMin) $Error(UnitMinutes)" $GDefs(Lang) 600
      focus $Data(ModelTimeStepEnt)
      return 0
   }

   #----- Verify that model time step is a integer multiple of 1 minute.
   if { [expr fmod($Sim(ModelTimeStepMin), 1)] > $Sim(Epsilon) } {
      Dialog::CreateError .mlcdnew "[lindex $Error(ModelTimeStep13) $GDefs(Lang)][lindex $Error(ModelTimeStep3) $GDefs(Lang)] $Sim(ModelTimeStepMin) $Error(UnitMinutes)" $GDefs(Lang) 600
      focus $Data(ModelTimeStepEnt)
      return 0
   }

   #----- Verify that output time step is greater than (or equal to) 1 minute and lower than (or equal to) 1440 minutes (24 hrs).
   if { $Sim(OutputTimeStepMin) < 1 || $Sim(OutputTimeStepMin) > 1440 } {
      Dialog::CreateError .mlcdnew "[lindex $Error(OutputTimeStep6) $GDefs(Lang)][lindex $Error(OutputTimeStep3) $GDefs(Lang)] $Sim(OutputTimeStepMin) $Error(UnitMinutes)" $GDefs(Lang) 600
      focus $Data(OutputTimeStepEnt)
      return 0
   }

   if { $Sim(OutputTimeStepMin) < 60 } {

      #----- Verify that output time step is an integer divisor of 60 minutes.
      if { [expr fmod(60, $Sim(OutputTimeStepMin))] > $Sim(Epsilon) } {
         Dialog::CreateError .mlcdnew "[lindex $Error(OutputTimeStep7) $GDefs(Lang)][lindex $Error(OutputTimeStep3) $GDefs(Lang)] $Sim(OutputTimeStepMin) $Error(UnitMinutes)" $GDefs(Lang) 600
         focus $Data(OutputTimeStepEnt)
         return 0
      }

      #----- Verify that output time step is an integer multiple of 1 minute.
      if { [expr fmod($Sim(OutputTimeStepMin), 1)] > $Sim(Epsilon) } {
         Dialog::CreateError .mlcdnew "[lindex $Error(OutputTimeStep8) $GDefs(Lang)][lindex $Error(OutputTimeStep3) $GDefs(Lang)] $Sim(OutputTimeStepMin) $Error(UnitMinutes)" $GDefs(Lang) 600
         focus $Data(OutputTimeStepEnt)
         return 0
      }

   } else {

      #----- Verify that output time step is an integer divisor of 1440 minutes.
      if { [expr fmod(1440, $Sim(OutputTimeStepMin))] > $Sim(Epsilon) } {
         Dialog::CreateError .mlcdnew "[lindex $Error(OutputTimeStep9) $GDefs(Lang)][lindex $Error(OutputTimeStep3) $GDefs(Lang)] $Sim(OutputTimeStepMin) $Error(UnitMinutes)" $GDefs(Lang) 600
         focus $Data(OutputTimeStepEnt)
         return 0
      }

      #----- Verify that output time step is an integer multiple of 60 minutes.
      if { [expr fmod($Sim(OutputTimeStepMin), 60)] > $Sim(Epsilon) } {
         Dialog::CreateError .mlcdnew "[lindex $Error(OutputTimeStep10) $GDefs(Lang)][lindex $Error(OutputTimeStep3) $GDefs(Lang)] $Sim(OutputTimeStepMin) $Error(UnitMinutes)" $GDefs(Lang) 600
         focus $Data(OutputTimeStepEnt)
         return 0
      }

   }

   #----- Verify that model time step is lower (or equal to) output time step.
   if { $Sim(ModelTimeStepMin) > $Sim(OutputTimeStepMin) } {
      Dialog::CreateError .mlcdnew "[lindex $Error(ModelTimeStep8) $GDefs(Lang)][lindex $Error(ModelTimeStep9) $GDefs(Lang)] $Sim(ModelTimeStepMin) $Error(UnitMinutes)\n[lindex $Error(ModelTimeStep10) $GDefs(Lang)] $Sim(OutputTimeStepMin) $Error(UnitMinutes)" $GDefs(Lang) 600
      focus $Data(ModelTimeStepEnt)
      return 0
   }

   #----- Verify that output time step is an integer multiple of model time step.
   if { [expr fmod($Sim(OutputTimeStepMin), $Sim(ModelTimeStepMin))] > $Sim(Epsilon) } {
      Dialog::CreateError .mlcdnew "[lindex $Error(OutputTimeStep5) $GDefs(Lang)][lindex $Error(ModelTimeStep9) $GDefs(Lang)] $Sim(ModelTimeStepMin) $Error(UnitMinutes)\n[lindex $Error(ModelTimeStep10) $GDefs(Lang)] $Sim(OutputTimeStepMin) $Error(UnitMinutes)" $GDefs(Lang) 600
      focus $Data(ModelTimeStepEnt)
      return 0
   }

   #----- Validate grid domain for concentration calculations.
   set idx ""
   set number [string is double -strict -failindex idx $Sim(GridDomain)]
   if { $number == 0 && $idx == -1 } {
      Dialog::CreateError .mlcdnew "[lindex $Error(GridDomainRange) $GDefs(Lang)] $Sim(GridDomain) $Error(UnitKilometers)" $GDefs(Lang) 600
      focus $Data(ConcDomainEnt)
      return 0
   } elseif { $number == 0 || ($number == 1 && $Sim(GridDomain) <= 0.1) || ($number == 1 && $Sim(GridDomain) > 200) } {
      Dialog::CreateError .mlcdnew "[lindex $Error(GridDomain) $GDefs(Lang)] $Sim(GridDomain) $Error(UnitKilometers)" $GDefs(Lang) 600
      focus $Data(ConcDomainEnt)
      return 0
   }

   #----- Number of vertical levels for concentration calculations.
   set Sim(NbVerticalLevels) [llength $Sim(VerticalLevels)]

   #----- Verify that number of concentration vertical levels is greater than 1.
   if { $Sim(NbVerticalLevels) < 2 } {
      Dialog::CreateError .mlcdnew "[lindex $Error(VerticalLevels1) $GDefs(Lang)][lindex $Error(VerticalLevels2) $GDefs(Lang)] $Sim(NbVerticalLevels).\n[lindex $Error(VerticalLevels3) $GDefs(Lang)] $Sim(VerticalLevels)." $GDefs(Lang) 600
      focus $Data(ConcVerticalLevelsEnt)
      return 0
   }

   #----- Verify that all concentration vertical levels are positive and sorted in increasing order.
   for { set i 0 } { $i < $Sim(NbVerticalLevels) } { incr i } {
      set level [lindex $Sim(VerticalLevels) $i]

      set idx ""
      set number [string is double -strict -failindex idx $level]
      if { $number == 0 && $idx == -1 } {
         Dialog::CreateError .mlcdnew "[lindex $Error(VerticalLevelsRange) $GDefs(Lang)] $level $Error(UnitMeters)" $GDefs(Lang) 600
         focus $Data(ConcVerticalLevelsEnt)
         return 0
      } elseif { $number == 0 || ($number == 1 && $level < 0) } {
         Dialog::CreateError .mlcdnew "[lindex $Error(VerticalLevels4) $GDefs(Lang)] $level $Error(UnitMeters)" $GDefs(Lang) 600
         focus $Data(ConcVerticalLevelsEnt)
         return 0
      }

      if { $i > 0 } {
         set prevlevel [lindex $Sim(VerticalLevels) [expr $i - 1]]
         if { $level <= $prevlevel } {
            Dialog::CreateError .mlcdnew "[lindex $Error(VerticalLevels5) $GDefs(Lang)] $Sim(VerticalLevels)." $GDefs(Lang) 600
            focus $Data(ConcVerticalLevelsEnt)
            return 0
         }
      }

   }

   return 1
}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::ValidateObsDateTime>
# Creation : 14 November 2003 - A. Malo - CMC/CMOE
#
# But      : Validate observation date-times.
#
# Parametres :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc MLCD::ValidateObsDateTime { } {
   global   GDefs
   variable Sim
   variable Error
   variable Data

   for { set i 0 } { $i < [expr $Sim(ObsValidNb) - 1] } { incr i } {

      set Idx       [lindex $Sim(ObsValidIdx) $i]
      set NextIdx   [lindex $Sim(ObsValidIdx) [expr $i+1]]

      set Time      [lindex $Sim(ObsTime) $Idx]
      set NextTime  [lindex $Sim(ObsTime) $NextIdx]

      set Stamp     [lindex [MLCD::GetInfoDateTime $Time] 6]
      set NextStamp [lindex [MLCD::GetInfoDateTime $NextTime] 6]

      #----- Verify that all observations follow in time.
      if { $Stamp > $NextStamp } {
         set time  [MLCD::GetInfoDateTime [lindex $Sim(ObsTime) $Idx]]
         set year  [lindex $time 0]
         set month [lindex $time 1]
         set day   [lindex $time 2]
         set hour  [lindex $time 3]
         set min   [lindex $time 4]
         set time0 "Observation \# [expr $Idx + 1] : ${year}-${month}-${day} ${hour}:${min} UTC"

         set time  [MLCD::GetInfoDateTime [lindex $Sim(ObsTime) $NextIdx]]
         set year  [lindex $time 0]
         set month [lindex $time 1]
         set day   [lindex $time 2]
         set hour  [lindex $time 3]
         set min   [lindex $time 4]
         set time1 "Observation \# [expr $NextIdx + 1] : ${year}-${month}-${day} ${hour}:${min} UTC"

         Dialog::CreateError .mlcdnew "[lindex $Error(ObsTimes) $GDefs(Lang)]\t$time0\n\t$time1" $GDefs(Lang) 600

         #----- Select 'Meteo' tab.
         TabFrame::Select $Data(TabFrameName) 2

         #----- Select next observation according to date-time stamps comparison.
         MLCD::SelectObs [expr $NextIdx + 1]
         return 0
      }

   }

   #----- Validate release date-time according to valid observation date-times.
   set FirstIdx      [lindex $Sim(ObsValidIdx) 0]
   set LastIdx       [lindex $Sim(ObsValidIdx) [expr $Sim(ObsValidNb) - 1]]
   set FirstObsTime  [lindex $Sim(ObsTime) $FirstIdx]
   set LastObsTime   [lindex $Sim(ObsTime) $LastIdx]
   set FirstObsStamp [lindex [MLCD::GetInfoDateTime $FirstObsTime] 6]
   set LastObsStamp  [lindex [MLCD::GetInfoDateTime $LastObsTime] 6]

   if { $Sim(ObsValidNb) > 1 } { #----- Multiple valid observations.

      if { $Sim(AccStamp) < $FirstObsStamp || $Sim(AccStamp) >= $LastObsStamp } {

         puts stdout ""
         Debug::TraceProc "MLCD: Error! Inconsistency between emission starting time and observation times."
         puts stdout ""
         puts stdout "Emission date-time  (stamp)         : $Sim(AccDateTime) ($Sim(AccStamp))"
         puts stdout "First observation date-time (stamp) : $FirstObsTime ($FirstObsStamp)"
         puts stdout "Last observation date-time (stamp)  : $LastObsTime ($LastObsStamp)"

         set time  [MLCD::GetInfoDateTime $Sim(AccDateTime)]
         set year  [lindex $time 0]
         set month [lindex $time 1]
         set day   [lindex $time 2]
         set hour  [lindex $time 3]
         set min   [lindex $time 4]
         set acc   "Accident        : ${year}-${month}-${day} ${hour}:${min} UTC"

         set time  [MLCD::GetInfoDateTime $FirstObsTime]
         set year  [lindex $time 0]
         set month [lindex $time 1]
         set day   [lindex $time 2]
         set hour  [lindex $time 3]
         set min   [lindex $time 4]
         set obs0  "Observation \# [expr $FirstIdx + 1] : ${year}-${month}-${day} ${hour}:${min} UTC"

         set time  [MLCD::GetInfoDateTime $LastObsTime]
         set year  [lindex $time 0]
         set month [lindex $time 1]
         set day   [lindex $time 2]
         set hour  [lindex $time 3]
         set min   [lindex $time 4]
         set obs1  "Observation \# [expr $LastIdx + 1] : ${year}-${month}-${day} ${hour}:${min} UTC"

         Dialog::CreateError .mlcdnew "[lindex $Error(ObsTimes2) $GDefs(Lang)]\t$acc\n\t$obs0\n\t$obs1" $GDefs(Lang) 500

         #----- Select 'Meteo' tab.
         TabFrame::Select $Data(TabFrameName) 2

         #----- Select first observation.
         MLCD::SelectObs 1

         return 0

      }

   } elseif { $Sim(ObsValidNb) == 1 } { #----- One single valid observation.

      if { $Sim(AccStamp) < $FirstObsStamp } {

         puts stdout ""
         Debug::TraceProc "MLCD: Error! Inconsistency between emission starting time and observation time."
         puts stdout ""
         puts stdout "Emission date-time  (stamp)         : $Sim(AccDateTime) ($Sim(AccStamp))"
         puts stdout "First observation date-time (stamp) : $FirstObsTime ($FirstObsStamp)"

         set time  [MLCD::GetInfoDateTime $Sim(AccDateTime)]
         set year  [lindex $time 0]
         set month [lindex $time 1]
         set day   [lindex $time 2]
         set hour  [lindex $time 3]
         set min   [lindex $time 4]
         set acc   "Accident        : ${year}-${month}-${day} ${hour}:${min} UTC"

         set time  [MLCD::GetInfoDateTime $FirstObsTime]
         set year  [lindex $time 0]
         set month [lindex $time 1]
         set day   [lindex $time 2]
         set hour  [lindex $time 3]
         set min   [lindex $time 4]
         set obs0  "Observation \# [expr $FirstIdx + 1] : ${year}-${month}-${day} ${hour}:${min} UTC"

         Dialog::CreateError .mlcdnew "[lindex $Error(ObsTimes2) $GDefs(Lang)]\t$acc\n\t$obs0" $GDefs(Lang) 500

         #----- Select 'Meteo' tab.
         TabFrame::Select $Data(TabFrameName) 2

         #----- Select first observation.
         MLCD::SelectObs 1

         return 0

      }

   }

   return 1

}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::ValidateWindProfile>
# Creation : 13 November 2003 - A. Malo - CMC/CMOE
#
# But      : Validate current wind profile.
#
# Parametres :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc MLCD::ValidateWindProfile { } {
   global   GDefs
   variable Sim
   variable Error
   variable Data

   #----- Parse wind profile table.
   for { set i 0 } { $i < $MLCD::Sim(ObsMaxNbLevels) } { incr i } {

      set HeightEnt    $Data(FrameWindProfile).l$i.z
      set VelocityEnt  $Data(FrameWindProfile).l$i.v
      set DirectionEnt $Data(FrameWindProfile).l$i.d

      set height    [$HeightEnt    get]
      set velocity  [$VelocityEnt  get]
      set direction [$DirectionEnt get]

      #----- Verify that height is a positive value.
      set idx ""
      set number [string is double -failindex idx $height]
      if { $number == 0 && $idx == -1 } {
         Dialog::CreateError .mlcdnew "[lindex $Error(ObsHeightRange) $GDefs(Lang)] $height $Error(UnitMeters)" $GDefs(Lang) 600
         focus $HeightEnt
         return 0
      } elseif { $number == 0 || ($number == 1 && $height <= 0 && $height != "") } {
         Dialog::CreateError .mlcdnew "[lindex $Error(ObsHeight) $GDefs(Lang)] $height $Error(UnitMeters)" $GDefs(Lang) 600
         focus $HeightEnt
         return 0
      }

      #----- Verify that velocity is a value greater or equal to 0.
      set idx ""
      set number [string is double -failindex idx $velocity]
      if { $number == 0 && $idx == -1 } {
         Dialog::CreateError .mlcdnew "[lindex $Error(ObsVelocityRange) $GDefs(Lang)] $velocity $Error(UnitVelocity)" $GDefs(Lang) 600
         focus $VelocityEnt
         return 0
      } elseif { $number == 0 || ($number == 1 && $velocity < 0 && $velocity != "") } {
         Dialog::CreateError .mlcdnew "[lindex $Error(ObsVelocity) $GDefs(Lang)] $velocity $Error(UnitVelocity)" $GDefs(Lang) 600
         focus $VelocityEnt
         return 0
      }

      #----- Verify that direction is a positive value falling in the range [0.0, 360.0[.
      set idx ""
      set number [string is double -failindex idx $direction]
      if { $number == 0 && $idx == -1 } {
         Dialog::CreateError .mlcdnew "[lindex $Error(ObsDirectionRange) $GDefs(Lang)] $direction $Error(UnitDirection)" $GDefs(Lang) 600
         focus $DirectionEnt
         return 0
      } elseif { $number == 0 || ($number == 1 && $direction != "" && ($direction < 0.0 || $direction >= 360.0)) } {
         Dialog::CreateError .mlcdnew "[lindex $Error(ObsDirection) $GDefs(Lang)] $direction $Error(UnitDirection)" $GDefs(Lang) 600
         focus $DirectionEnt
         return 0
      }

      #----- Verify that height field is not empty for a specified velocity or direction.
      if { ($height == "" && ($velocity != "" || $direction != "")) } {
         Dialog::CreateError .mlcdnew "[lindex $Error(ObsEmptyField) $GDefs(Lang)]" $GDefs(Lang) 600
         focus $HeightEnt
         return 0
      }

      #----- Verify that velocity field is not empty for a specified height or direction.
      if { ($velocity == "" && ($height != "" || $direction != "")) } {
         Dialog::CreateError .mlcdnew "[lindex $Error(ObsEmptyField) $GDefs(Lang)]" $GDefs(Lang) 600
         focus $VelocityEnt
         return 0
      }

      #----- Verify that direction field is not empty for a specified height or velocity.
      if { ($direction == "" && ($height != "" || $velocity != "")) } {
         Dialog::CreateError .mlcdnew "[lindex $Error(ObsEmptyField) $GDefs(Lang)]" $GDefs(Lang) 600
         focus $DirectionEnt
         return 0
      }

   }

   return 1
}
