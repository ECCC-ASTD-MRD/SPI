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

source $GDefs(Dir)/tcl/Models/Types/MLCD.ctes
source $GDefs(Dir)/tcl/Models/Types/MLCD.txt
source $GDefs(Dir)/tcl/Models/Types/MLCD.int

#-------------------------------------------------------------------------------
# Nom      : <MLCD::ParamsCheck>
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

proc MLCD::ParamsCheck { Tab No } {
   variable Sim
   variable Data

   scan $Sim(Hour) "%02d" hour
   scan $Sim(Min)  "%02d" min

   set Sim(AccSecs)  [expr $Sim(Secs)+$hour*3600+$min*60]
   set Sim(RunStamp) [fstdstamp fromseconds $Sim(AccSecs)]

   set Sim(Name)         [lindex $Sim(GridSrc) 0]
   set Sim(Lat)          [lindex $Sim(GridSrc) 1]
   set Sim(Lon)          [lindex $Sim(GridSrc) 2]

   if { ![MLCD::ValidateModelTab] } {
      TabFrame::SelectPrevious $Tab
      return 0
   }

   #----- Check for last tab
   set nb [expr [TabFrame::NbFrame $Tab]-1]
   if { $No!=$nb } {
      return True
   }

   if { ![MLCD::ValidateEmissionTab] } {
      TabFrame::SelectPrevious $Tab
      return 0
   }

   if { !$Sim(Auto) && ![MLCD::ValidateMeteoTab] } {
      TabFrame::SelectPrevious $Tab
      return 0
   }

   if { $Sim(IsConc) } {
      if { $Sim(GridType) == 1 } {
         set Sim(GridDomain) NA
      }
   } else {
      set Sim(GridDomain)     NA
      set Sim(VerticalLevels) NA
   }
}

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

   if { $Sim(AccSecs)!=$Sim(OldSecs) && $Sim(DurMin)!=$Sim(OldDurMin) } {
      #----- Both release date-time and simulation duration have changed.
      set newdate [clock format $Sim(AccSecs) -format "%Y-%m-%d %H:%M UTC" -gmt True]
      set olddate [clock format $Sim(OldSecs) -format "%Y-%m-%d %H:%M UTC" -gmt True]
      set newdur "$Sim(DurMin) $Error(UnitMinutes)"
      set olddur "$Sim(OldDurMin) $Error(UnitMinutes)"
      set answer [Dialog::Default .modelnew 500 WARNING $Warning(ChangeReleaseDateTimeSimDur) "\n\n\t[lindex $Warning(ChangeReleaseDateTimeSimDur2) $GDefs(Lang)] $newdate\n\t[lindex $Warning(ChangeReleaseDateTimeSimDur3) $GDefs(Lang)] $olddate\n\n\t[lindex $Warning(ChangeReleaseDateTimeSimDur4) $GDefs(Lang)] $newdur\n\t[lindex $Warning(ChangeReleaseDateTimeSimDur5) $GDefs(Lang)] $olddur[lindex $Warning(ChangeReleaseDateTimeSimDur6) $GDefs(Lang)]" 0 $Lbl(Yes) $Lbl(No)]

      if { $answer == 1 } {
         #---- Answer is "No" : Do not re-initialize release date-time, simulation duration and meteo data.
         #----- Keep old release date-time as current date-time.
         set Sim(AccSecs) $Sim(OldSecs)

         #----- Keep old simulation duration as current simulation duration.
         set Sim(DurMin)   $Sim(OldDurMin)

      } else {
         #---- Answer is "Yes" : Re-initialize release date-time, simulation duration and meteo data
         MLCD::InitMeteoData
      }
   } else {

     #----- Release date-time has changed
     if { $Sim(AccSecs)!=$Sim(OldSecs) } {

         set newdate [clock format $Sim(AccSecs) -format "%Y-%m-%d %H:%M UTC" -gmt True]
         set olddate [clock format $Sim(OldSecs) -format "%Y-%m-%d %H:%M UTC" -gmt True]
         set answer [Dialog::Default .modelnew 500 WARNING $Warning(ChangeReleaseDateTime) "\n\n\t[lindex $Warning(ChangeReleaseDateTime2) $GDefs(Lang)] $newdate\n\t[lindex $Warning(ChangeReleaseDateTime3) $GDefs(Lang)] $olddate[lindex $Warning(ChangeReleaseDateTime4) $GDefs(Lang)]" 0 $Lbl(Yes) $Lbl(No)]

         if { $answer == 1 } {
            #----- Answer is "No" : Do not re-initialize release date-time and meteo data.
            #----- Re-initialize old release date-time.
            set Sim(OldSecs)  $Sim(AccSecs)
        } else {
            #---- Answer is "Yes" : Re-initialize release date-time and meteo data.
            MLCD::InitMeteoData
         }
      }

      #----- Simulation duration has changed
      if { $Sim(DurMin)!=$Sim(OldDurMin) } {

         set newdur "$Sim(DurMin) $Error(UnitMinutes)"
         set olddur "$Sim(OldDurMin) $Error(UnitMinutes)"
         set answer [Dialog::Default .modelnew 500 WARNING $Warning(ChangeSimDuration) "\n\n\t[lindex $Warning(ChangeSimDuration2) $GDefs(Lang)] $newdur\n\t[lindex $Warning(ChangeSimDuration3) $GDefs(Lang)] $olddur[lindex $Warning(ChangeSimDuration4) $GDefs(Lang)]" 0 $Lbl(Yes)$Lbl(No)]

         if { $answer == 1 } {
            #----- Answer is "No" : Do not re-initialize simulation duration and meteo data
            #----- Keep old simulation duration as current simulation duration
            set Sim(DurMin)   $Sim(OldDurMin)
         } else {
            #---- Answer is "Yes" : Re-initialize simulation duration and meteo data.
            MLCD::InitMeteoData
         }
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::ComputeNbValidWindProfiles>
# Creation : 14 November 2003 - A. Malo - CMC/CMOE
#
# But      : Compute list of valid wind profiles.
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

   set Sim(ObsValidIdx) {}

   #----- Loop over total number of observations
   for { set i 0 } { $i < $Sim(ObsMaxNb) } { incr i } {

      #----- Loop over vertical levels
      foreach level [lindex $Sim(WindProfile) $i] {

         set height    [lindex $level 0]
         set velocity  [lindex $level 1]
         set direction [lindex $level 2]

         if { $height != "" && $velocity != "" && $direction != "" } {
            #----- Keep valid wind profiles index.
            lappend Sim(ObsValidIdx) $i
            break
         }
      }
   }

   #----- Verify that number of wind profile is not zero.
   if { ![llength $Sim(ObsValidIdx)] } {
      Dialog::Error .modelnew $Error(ObsNbValidWindProfiles)
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

   #----- Loop over observation times
   for { set i 0 } { $i < $Sim(ObsNb) } { incr i } {

      set secs  [lindex $Sim(ObsTime)  $i]
      set stamp [fstdstamp fromseconds $secs]
      set file  [lindex [lindex $Sim(Data) $i] end]

      fstdfile open UNITFILE read $file

      #----- Get wind profile data for the specified date-time stamp and lat-lon coordinates of the source.
      set windprof [MetData::Profile $stamp UU UNITFILE $Sim(Lat) $Sim(Lon) 0 [expr $Sim(ObsNbLevels)-1]]

      #----- Verify if wind profile data has been found.
      if { $windprof == "" } {
         set msg " Observation \#[expr $i + 1] : [clock format $secs -format "%Y-%m-%d %H;%M UTC" -gmt True]"
         set answer [Dialog::Default .modelnew 400 WARNING $Warning(WindProfData) "$msg[lindex $Warning(WindProfData2) $GDefs(Lang)]" 0 $Lbl(Yes) $Lbl(No)]

         if { $answer == 1 } {
            fstdfile close UNITFILE
            return 0
         }
      }

      #----- Update Obukhov Length.
      set obukhov [MetData::Obukhov $stamp UNITFILE $Sim(Lat) $Sim(Lon)]

      #----- Update Roughness Length.
      set roughness [lindex [lindex $Sim(LocalParameters) $i] 0]

      set z0 [fstdfield find UNITFILE $stamp "" 1195 -1 -1 "" Z0]
      if { [llength $z0] } {
         fstdfield read TMP UNITFILE $z0

         set roughness [fstdfield stats TMP -coordvalue $Sim(Lat) $Sim(Lon)]
         if { $roughness == "-" } {
            Log::Print WARNING "Off grid localisation. Using default value for roughness length ($roughness \[m\])."
         }
      } else {
         Log::Print WARNING "Missing field 'Z0'. Using default value for roughness length ($roughness \[m\])."
      }

      #----- Update Precipitation Rate.
      set precip [lindex [lindex $Sim(LocalParameters) $i] 2]

      set rt [fstdfield find UNITFILE $stamp "" -1 -1 -1 "" RT]
      if { [llength $rt] } {
         fstdfield read TMP UNITFILE $rt

         set precip [fstdfield stats TMP -coordvalue $Sim(Lat) $Sim(Lon)]
         if { $precip == "-" } {
            Log::Print WARNING "Off grid localisation. Using default value for precipitation rate ($precip \[mm/h\])."
         } else {
            #----- Convert precipitation rate from [m/s] to [mm/h].
            set precip [expr $precip*3.6e+06]
         }
      } else {
         Log::Print WARNING "Missing field 'RT'. Using default value for precipitation rate ($precip \[mm/h\])."
      }

      #----- Set local parameters list.
      set params [list [format "%.7f" $roughness] [format "%.7f" $obukhov] [format "%.7f" $precip]]
      set j 0
      set prof {}

      if { $windprof != "" } {

         #----- Set wind profile according to number of specified vertical levels.
         for { } { $j < $Sim(ObsNbLevels) } { incr j } {
            set level     [lindex $windprof $j]
            set height    [format "%.7f" [lindex $level 0]]
            set velocity  [format "%.7f" [lindex $level 1]]
            set direction [format "%.7f" [lindex $level 2]]
            lappend prof  [list $height $velocity $direction]
         }
      }

      #----- Fill out rest of profile with blanks.
      for { } { $j < $Sim(ObsMaxNbLevels) } { incr j } {
         lappend prof [list "" "" ""]
      }

      fstdfile close UNITFILE

      #----- Set local parameters and wind profile to lists.
      lset Sim(LocalParameters) $i $params
      lset Sim(WindProfile)     $i $prof
   }

   #----- Initialize local parameters and wind profile for all other observations.
   for { set i $Sim(ObsNb) } { $i < $Sim(ObsMaxNb) } { incr i } {
      lset Sim(LocalParameters) $i [list 0.5 [format "%.7f" [MLCD::ObukhovFunc 0 0.5]] 0.0]
      lset Sim(WindProfile) $i [lrepeat Sim(ObsMaxNbLevels) [list "" "" ""]]
   }
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
   if { $Model::Param(DBaseDiag) == $Model::Param(DBaseProg) } {
      set Mixed -1 ; #----- Ignored the latest run.
   } else {
      set Mixed 1  ; #----- Take into account the latest run.
   }

   #----- Get list of available meteorological files according to release date-time.
   set Sim(Data) [MetData::File $Sim(RunStamp) $Model::Param(DBaseDiag) $Model::Param(DBaseProg) F $Mixed]

   #----- Verify that number of meteo data files is greater than 1.
   if { [llength $Sim(Data)] <= 1 } {
      set start [clock format $Sim(AccSecs) -format "%Y-%m-%d %H:%M UTC" -gmt True]
      set text "\n\n\t[lindex $Error(MetData2) $GDefs(Lang)] $start\n\t[lindex $Error(MetData3) $GDefs(Lang)] [clock format $MetData::Data(T0) -gmt True] - [clock format $MetData::Data(T1) -gmt True]"
      Dialog::Error .modelnew $Error(MetData) $text

      #----- Select first observation.
      MLCD::SelectObs 1

      return 0
   }

   #----- Get first available met file.
   set list       [lindex $Sim(Data) 0]
   set firststamp [lindex $list 0]             ; #----- Date-time stamp for first met file.

   #----- Get last available met file.
   set list       [lindex $Sim(Data) end]
   set laststamp  [lindex $list 0]             ; #----- Date-time stamp for last met file.

   #----- Compute simulation duration according to last available met data file.
   set SimDurHr  [fstdstamp diff $laststamp $Sim(RunStamp)] ; #----- [hr].
   set SimDurMin [expr round(double($SimDurHr) * 60.0)]     ; #----- [min].
   set SimDurSec [expr int($SimDurMin*60)]                  ; #----- [s].

   set Sim(Duration) [expr int($Sim(DurMin)*60)]

   if { $Sim(Duration) >= $SimDurSec } {

      #----- Here, simulation duration defined within interface is greater than (or equal to) simulation duration
      #----- computed according to available met files. Thus, simulation duration will be re-initialized.
      set OldSimDurMin  $Sim(DurMin)
      set Sim(Duration) $SimDurSec
      set Sim(DurMin)   $SimDurMin

      #----- Initialize old simulation duration.
      set Sim(OldDurMin) $Sim(DurMin)

      Dialog::Default .modelnew 500 WARNING $Warning(SimDuration) "\n\n\t[lindex $Warning(SimDuration2) $GDefs(Lang)] $Sim(DurMin) $Error(UnitMinutes)\n\t[lindex $Warning(SimDuration3) $GDefs(Lang)] $OldSimDurMin $Error(UnitMinutes)" 0 "OK"

      #----- Redefine emission duration since simulation duration has changed.
      MLCD::SetEmissionDuration

   } else {

      #----- Here, simulation duration defined within interface is less than simulation duration
      #----- computed according to available met files.

      #----- Compute new ending simulation date-time [s] according to release date-time and simulation duration.
      #----- Ending simulation date-time [s] := starting release date-time [s] + simulation duration [s].
      set stamp [fstdstamp fromseconds [expr $Sim(AccSecs) + $Sim(Duration)]]

      #----- Search list of met stamps for which ending simulation date-time
      #----- falls between two following met stamps.
      set idx 0
      foreach data $Sim(Data) {
         if { $stamp<=[lindex $data 0] } {
            break
         }
         incr idx
      }
      set Sim(Data) [lrange $Sim(Data) 0 $idx]
   }

   #----- List of met data files.
   set Sim(MeteoDataFiles) {}
   foreach data $Sim(Data) {
      lappend Sim(MeteoDataFiles) [lindex $data 2]
   }

   return 1
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
   set secs [lindex $Sim(ObsTime) $Sim(ObsIdx)]

   #----- Set observation date in seconds.
   set Sim(ObsDateSec)  [clock scan [clock format $secs -format "%Y%m%d" -gmt True]]

   #----- Set observation time.
   set Sim(ObsTimeHour) [clock format $secs -format "%H" -gmt True]
   set Sim(ObsTimeMin)  [clock format $secs -format "%M" -gmt True]

   #----- Save observation date and time for future verification.
   set Sim(OldObsTime) $Sim(ObsTime)
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
   set Sim(ObsNb)       1                           ;#----- Number of observations.
   set Sim(ObsNbLevels) 3                           ;#----- Number of vertical levels in wind profile.
   set Sim(ObsNo)       1                           ;#----- Current observation number.
   set Sim(ObsIdx)      [expr $Sim(ObsNo) - 1]      ;#----- Current observation index.
   set Sim(ObsPrevIdx)  $Sim(ObsIdx)                ;#----- Previous observation index.
   set Sim(Meteo)       [lindex $Sim(ListMeteo) 1]  ;#----- Type of meteorological model : 'REGIONAL'.

   #----- Save automation parameters for future verification.
   set Sim(OldObsNb)        $Sim(ObsNb)
   set Sim(OldObsNbLevels)  $Sim(ObsNbLevels)
   set Sim(OldObsMetModel)  $Sim(Meteo)
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

   #----- Initialize wind profile and local parameters and save for future verification.
   set Sim(OldLocalParameters) [set Sim(LocalParameters) [lrepeat $Sim(ObsMaxNb) [list $Sim(ObsRough) $Sim(ObsObukhov) $Sim(ObsPrecip)]]]
   set Sim(OldWindProfile)     [set Sim(WindProfile)     [lrepeat $Sim(ObsMaxNb) [lrepeat $Sim(ObsMaxNbLevels) [list "" "" ""]]]]
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

   set Sim(OldSecs)   $Sim(AccSecs)
   set Sim(OldDurMin) $Sim(DurMin)

   #----- Initialize meteo data.
   MLCD::InitAutomationParams
   MLCD::InitObservationTimes
   MLCD::InitLocalParameters

   #----- Update edition observation interface.
   if { $Flag } {
      MLCD::UpdateObsInterface
   }
}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::InitNew>
# Creation : Octobre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialisation des parametres.
#
# Parametres :
#   <Type>   : Type de source
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc MLCD::InitNew { Type } {
   global GDefs
   variable Sim
   variable Data

   set Sim(DurMin)            360                         ; #----- Simulation duration [min].
   set Sim(OutputTimeStepMin) 5                           ; #----- Output time step [min].
   set Sim(ModelTimeStepMin)  5                           ; #----- Model time step [min].

   #----- Initialize general and concentrations parameters.
   set Sim(Backward)       False    ; #----- Forward mode: 0; Backward mode: 1.
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
   set Sim(EmMass)            1.0             ; #----- Total mass released [arbitrary mass unit].
   set Sim(EmIsoName)         "Cs-137"        ; #----- Isotope name.
   set Sim(EmDepVel)          1.00e-03        ; #----- Deposition velocity [m/s].
   set Sim(EmHalfLife)        9.47e+08        ; #----- Radioactive half-life [s].
   set Sim(EmWetScav)         1.00e+05        ; #----- Wet scavenging coefficient [dimensionless].
   set Sim(EmBottom)          0.0             ; #----- Emission bottom [m].
   set Sim(EmTop)             1.0             ; #----- Emission top [m].
   set Sim(EmRadius)          1.0             ; #----- Emission radius [m].

   #----- Update emission starting time.
   Model::FitAccTime MLCD

   #----- Initialize meteorological parameters.
   MLCD::InitMeteoData 0
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

   #----- Define maximum number of observation according to release date-time and 48-hr simulation duration.
   set Sim(ObsNb)   [set Sim(ObsMaxNb) 49]
   set Sim(ObsTime) {}

   #----- Initialize observation times according to 1-hour time interval between following observations.
   set secs [clock scan [clock format $Sim(AccSecs) -format "%Y%m%d %H:00" -gmt True]]
   for { set i 0 } { $i < $Sim(ObsMaxNb) } { incr i } {
      lappend Sim(ObsTime) [expr $secs+$i*3600]
   }

   #----- Get current observation time according to (selected) observation index.
   MLCD::GetCurrentObservationTime
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
   if { $Sim(ObsNbLevels) != $Sim(OldObsNbLevels) || $Sim(Meteo) !=  $Sim(OldObsMetModel) } {
      return 1
   }

   #----- Verify if date-times have been modified for each observation.
   foreach time $Sim(ObsTime) origtime $Sim(OldObsTime) {
      if { [fstdstamp fromseconds $time]!=[fstdstamp fromseconds $origtime] } {
         return 1
      }
   }
   #----- I'm not even sure we can get down here but if so,

   #----- Verify if local parameters have been modified for each observation.
   if { $Sim(LocalParameters)!=$Sim(OldLocalParameters) } {
      return 1
   }

   #----- Verify if wind profiles have been modified for each observation.
   if { $Sim(WindProfile)!=$Sim(OldWindProfile) } {
      return 1
   }
   return 0
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

   set fs     [expr -4.0/(1.0 + 1.3 * pow(abs($Sc), 0.85))]
   set length [expr 1.0/(($Sc>=0?1.0:-1.0) * pow((0.216586 * log(1.2 + 10.0/$Z0)), 2) * pow(10, $fs))]

   return $length
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

   set path [Exp::Path]/[Info::Path $Exp::Data(SelectSim)]
   SPI::FileOpen NEW FieldBox "(MLCD) $Exp::Data(No) $Exp::Data(Name)" "" "$path/results/pos $path/results/conc"
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
      Log::Print ERROR "Selecting invalid observation \# $No. Observation number must be between 1 and $Sim(ObsMaxNb)."
      return 0
   }

   #----- Validate current observation (local parameters + wind profile).
   if { ![MLCD::ValidateLocalParameters] || ![MLCD::ValidateWindProfile] } {
      #----- Modify selected observation to last one.
      set Sim(ObsNo) [expr $Sim(ObsIdx) + 1]
      return 0
   }

   #----- Find index corresponding to selected observation label.
   set Sim(ObsPrevIdx) $Sim(ObsIdx)   ; #----- Set previous observation index.
   set Sim(ObsIdx)     [expr $No - 1] ; #----- Set current observation index.

   #----- Set current observation number.
   set Sim(ObsNo) $No

   MLCD::SetPreviousObs $Sim(ObsPrevIdx)
   MLCD::UpdateObsInterface

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
         Dialog::Default .modelnew 500 WARNING $Warning(EmDuration) "\n\n\t[lindex $Warning(EmDuration2) $GDefs(Lang)] $Sim(EmDurationMin) $Error(UnitMinutes)\n\t[lindex $Warning(EmDuration3) $GDefs(Lang)] $OldEmDurMin $Error(UnitMinutes)" 0 "OK"
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::GetMetData>
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

proc MLCD::GetMetData { } {
   global   GDefs
   variable Sim
   variable Data
   variable Msg

   #----- Find available meteorological data files.
   if { ![MLCD::FindMetData] } {
      return 0
   }

   Dialog::Wait . $Msg(MetGet)

   MLCD::SetObservationTimes
   MLCD::InitLocalParameters
   MLCD::ExtractMetData
   MLCD::ComputeNbValidWindProfiles

   Dialog::WaitDestroy

   return 1
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

proc MLCD::SetObsDate { } {
   variable Sim

   scan $Sim(ObsTimeHour) "%02d" hour
   scan $Sim(ObsTimeMin)  "%02d" min

   lset Sim(ObsTime) $Sim(ObsIdx) [expr $Sim(ObsDateSec)+$hour*3600+$min*60]]
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
   set Sim(ObsNb)    [llength $Sim(Data)]
   set Sim(ObsMaxNb) $Sim(ObsNb)

   #----- Set observation times, stamps, files.
   set Sim(ObsTime)  {}

   foreach data $Sim(Data) {
      lappend Sim(ObsTime) [fstdstamp toseconds [lindex $data 0]]
   }

   #----- Fill other observation time by 1 hour increment
   for { set i $Sim(ObsNb) } { $i < $Sim(ObsMaxNb) } { incr i } {
      #----- Increment observation time.
      lappend Sim(ObsTime) [expr [lindex $Sim(ObsTime) [expr $i-1]]+3600]
    }

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
   scan $Sim(ObsTimeHour) "%02d" hour
   scan $Sim(ObsTimeMin)  "%02d" min
   lset Sim(ObsTime) $Idx [expr $Sim(ObsDateSec)+$hour*3600+$min*60]

   #----- Set local parameters.
   lset Sim(LocalParameters) $Idx [list $Sim(ObsRough) $Sim(ObsObukhov) $Sim(ObsPrecip)]

   #----- Set wind profile.
   set profile {}
   for { set i 0 } { $i < $Sim(ObsMaxNbLevels) } { incr i } {
      set height      [$Data(FrameWindProfile).l$i.z get]
      set velocity    [$Data(FrameWindProfile).l$i.v get]
      set direction   [$Data(FrameWindProfile).l$i.d get]
      lappend profile [list $height $velocity $direction]
   }
   set Sim(WindProfile) [lreplace $Sim(WindProfile) $Idx $Idx $profile]
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

   set Sim(GridLat)  [format "%.6f" [lindex $Sim(GridSrc) 1]]
   set Sim(GridLon)  [format "%.6f" [lindex $Sim(GridSrc) 2]]
}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::CreateModelInput>
# Creation : Octobre 2001 - J.P. Gauthier - CMC/CMOE
#
# But       : Create MLCD model input file.
#
# Parametres :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc MLCD::CreateModelInput { } {
   global GDefs
   variable Sim
   variable Data
   variable Error

   #----- Create emission input file.
   set file [open $Sim(Path)/tmp/input.dat w 0644]

   #----- Convert half-life period from [s] to [days].
   set halflifedays [format "%13.7e" [expr double($Sim(EmHalfLife))/86400.0]]

   puts $file "$Sim(Name)"
   puts $file "H"
   puts $file "[expr int($Sim(EmDurationMin)*60)] $Sim(Duration) $Sim(IsSigma)"
   puts $file "$Sim(EmNumberParticles) $Sim(EmMass) $Sim(EmDepVel) $halflifedays $Sim(EmWetScav)"
   puts $file "$Sim(Lon) $Sim(Lat)"
   puts $file "$Sim(EmBottom) $Sim(EmTop) $Sim(EmRadius)"
   puts $file [clock format $Sim(AccSecs) -format "%Y %m %d %H %M" -gmt True]

   puts $file "[llength $Sim(VerticalLevels)]"
   foreach level $Sim(VerticalLevels) {
      puts $file [format "%.2f" $level]
   }
   close $file

   #---- Create met windfield input file.
   set file [open $Sim(Path)/tmp/winddata.dat w 0644]

   puts $file "$Sim(Name)"
   puts $file "[llength $Sim(ObsValidIdx)]"

   #----- Initialize variables.
   set NewObsTime         {}
   set NewLocalParameters {}
   set NewWindProfile     {}

   #----- Loop over valid observations.
   foreach idx $Sim(ObsValidIdx) {

      #----- Get observation time.
      set secs [lindex $Sim(ObsTime) $idx]
      lappend  NewObsTime $secs

      puts $file "\n[clock format $secs -format "%Y %m %d %H %M" -gmt True]"

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

      #----- Loop over levels in wind profile.
      foreach level $windprof {

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

   set Sim(ObsTime)         $NewObsTime
   set Sim(LocalParameters) $NewLocalParameters
   set Sim(WindProfile)     $NewWindProfile
}

#----------------------------------------------------------------------------
# Nom        : <MLCD::CreateScriptInput>
# Creation   : Juillet 2009 - J.P. Gauthier - CMC/CMOE
#
# But        : Create input file for launching script.
#
# Parametres :
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLCD::CreateScriptInput { } {
   variable Sim
   global   GDefs

   set file [open $Sim(Path)/tmp/Model_MLCD.in w 0644]
      puts $file "#----- Logger specific parameters"
      puts $file "LOG_MAILTO=\"$Model::Param(EMail)\""
      puts $file "LOG_MAILTITLE=\"$Sim(Model) ($Model::Param(App))\""
      puts $file "LOG_FILE=$Sim(PathRun)/tmp/Model_MLCD.out"
      puts $file "LOG_LEVEL=$Model::Param(LogLevel)"
      puts $file "LOG_TIME=$Model::Param(LogTime)"
      puts $file "LOG_JOBID=$Sim(Model)"

      if { !$Model::Param(Auto) } {
         puts $file "LOG_JOBCLASS=INTERACTIVE"
      }

      puts $file ""
      puts $file "#----- Job general parameters"
      puts $file "MODEL_SOFTWARE=SPI"
      puts $file "MODEL_NAME=$Sim(Model)"
      puts $file "MODEL_TYPE=\"\""
      puts $file "MODEL_USER=$GDefs(FrontEndUser)"
      puts $file ""
      puts $file "MODEL_LOCALHOST=$GDefs(Host)"
      puts $file "MODEL_LOCALDIR=$Sim(Path)"
      puts $file "MODEL_RUNDIR=$Sim(PathRun)"
      puts $file "MODEL_PRE=0"
      puts $file "MODEL_RUN=1"
      puts $file "MODEL_POST=1"
      puts $file "MODEL_POOL=$Model::Param(Pool)"
      puts $file "MODEL_CLEAN=1"
      puts $file "MODEL_TRACE=$Exp::Param(Path)/trace"
      puts $file ""
      puts $file "#----- Model specific parameters"
      if { $Sim(Backward) } {
         puts $file "MLCD_MODE=1"
      } else {
         puts $file "MLCD_MODE=0"
      }
      puts $file "MLCD_EMISSION=$Sim(PathRun)/tmp/input.dat"
      puts $file "MLCD_WIND=$Sim(PathRun)/tmp/winddata.dat"
      puts $file "MLCD_POS=$Sim(PathRun)/results/pos"
      if { $Sim(IsConc) } {
         puts $file "MLCD_CONC=$Sim(PathRun)/results/conc"
         puts $file "MLCD_GRID=$Sim(GridType)"
         puts $file "MLCD_ALGO=$Sim(GridAlgo)"
         puts $file "MLCD_DOMAIN=$Sim(GridDomain)"
      }
      puts $file "MLCD_OUTPUTTS=$Sim(OutputTimeStepMin)"
      puts $file "MLCD_MODELTS=$Sim(ModelTimeStepMin)"
      puts $file "MLCD_SEED=$Sim(IsRanVar)"
   close $file
}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::Launch>
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

proc MLCD::Launch { } {
   global GDefs
   global env
   variable Sim

   MLCD::CreateModelInput
   MLCD::CreateScriptInput

   if { $Model::Param(IsUsingSoumet) } {

      #----- Copy needed file to run host:directory.
      Model::ParamsCopy MLCD

      exec echo "#!/bin/sh\n\n$Model::Param(Submit) $env(EER_DIRSCRIPT)/Model.sh -args $Sim(PathRun)/tmp/Model_MLCD.in -mach $Model::Param(Host) \
         -t 3600 -cm 1G -listing $Model::Param(Listings) $Model::Param(Op) -queue $Model::Param(Queue)" >$Sim(Path)/tmp/Model_Launch.sh
      exec chmod 755 $Sim(Path)/tmp/Model_Launch.sh
      eval set err \[catch \{ exec $Model::Param(Submit) $env(EER_DIRSCRIPT)/Model.sh -args $Sim(PathRun)/tmp/Model_MLCD.in -mach $Model::Param(Host) \
         -t 3600 -cm 1G -listing $Model::Param(Listings) $Model::Param(Op) -queue $Model::Param(Queue) 2>@1 \} msg\]
      catch { exec echo "$msg" > $Sim(Path)/tmp/Model_Launch.out }

      if { $err } {
         Log::Print ERROR "Submitting the job on $Model::Param(Host) failed:\n\n\t$msg"
         return False
      }
      Log::Print INFO "Job has been submitted successfully on $Model::Param(Host)."
   } else {
      Log::Print INFO "Launching model on $Model::Param(Host)"
      exec echo "#!/bin/sh\n\n$env(EER_DIRSCRIPT)/Model.sh $Sim(Path)/tmp/Model_MLCD.in" >$Sim(Path)/tmp/Model_Launch.sh
      exec ssh -n $Model::Param(Host) $env(EER_DIRSCRIPT)/Model.sh $Sim(Path)/tmp/Model_MLCD.in &
   }
   return True
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
#    Here, we ignore the wet scavenging rate [s-1] since
#    MLCD takes into account the wet scavenging coefficient (1.00e+05).
#-------------------------------------------------------------------------------

proc MLCD::SpeciesFormat { Line } {
   global GDefs
   variable Sim
   variable Warning
   variable Lbl

   if { [llength $Line] == 11 } {

      set symbol      [lindex $Line 0] ; #----- Isotope Symbol.
      set halflife    [lindex $Line 5] ; #----- Half-Life [s].
      set drydepvel   [lindex $Line 8] ; #----- Dry Deposition Velocity [m/s].

      #----- Verify that the isotope's radioactive half-life is positive.
      if { $halflife > 0 } {
         set Sim(EmIsoName)  $symbol
         set Sim(EmHalfLife) $halflife
         set Sim(EmDepVel)   $drydepvel
      } else {
         Dialog::Default .modelnew 500 WARNING $Warning(HalfLife) " $symbol." 0 "OK"
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <MLCD::UpdateObsInterface>
# Creation : 11 November 2003 - A. Malo - CMC/CMOE
#
# But      : Update observation in the editing interface.
#
# Parametres :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc MLCD::UpdateObsInterface { } {
   variable Sim
   variable Data

   #----- Refresh maximum number of observations within spibox.
   $Data(ObsNoSpinBox) configure -to $Sim(ObsMaxNb)

   #----- Get observation date-time according to selected observation.
   set secs [lindex $Sim(ObsTime) $Sim(ObsIdx)]

   #----- Set observation date in seconds.
   set Sim(ObsDateSec)  [clock scan [clock format $secs -format "%Y%m%d" -gmt True]]

   #----- Set observation time.
   set Sim(ObsTimeHour) [clock format $secs -format "%H" -gmt True]
   set Sim(ObsTimeMin)  [clock format $secs -format "%M" -gmt True]

   #----- Update local parameters.
   set params          [lindex $Sim(LocalParameters) $Sim(ObsIdx)]
   set Sim(ObsRough)   [lindex $params 0]
   set Sim(ObsObukhov) [lindex $params 1]
   set Sim(ObsPrecip)  [lindex $params 2]

   #----- Update wind profile.
   set profiles [lindex $Sim(WindProfile) $Sim(ObsIdx)]

   for { set i 0 } { $i < $Sim(ObsMaxNbLevels) } { incr i } {
      set level [lindex $profiles $i]
      $Data(FrameWindProfile).l$i.z delete 0 end
      $Data(FrameWindProfile).l$i.v delete 0 end
      $Data(FrameWindProfile).l$i.d delete 0 end
      $Data(FrameWindProfile).l$i.z insert 0 [lindex $level 0]
      $Data(FrameWindProfile).l$i.v insert 0 [lindex $level 1]
      $Data(FrameWindProfile).l$i.d insert 0 [lindex $level 2]
   }
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
      Dialog::Error .modelnew $Error(EmNbPartRange) " $Sim(EmNumberParticles)."
      focus $Data(EmNbPartEnt)
      return 0
   } elseif { $number == 0 || ($number == 1 && $Sim(EmNumberParticles) <= 0) } {
      Dialog::Error .modelnew $Error(EmNbPart) $GDefs(Lang) " $Sim(EmNumberParticles)."
      focus $Data(EmNbPartEnt)
      return 0
   }

   #----- Verify that the total release mass is a positive number.
   set idx ""
   set number [string is double -strict -failindex idx $Sim(EmMass)]
   if { $number == 0 && $idx == -1 } {
      Dialog::Error .modelnew $Error(EmMassRange) " $Sim(EmMass) [lindex $Error(UnitMass) $GDefs(Lang)]"
      focus $Data(EmMassEnt)
      return 0
   } elseif { $number == 0 || ($number == 1 && $Sim(EmMass) <= 0) } {
      Dialog::Error .modelnew $Error(EmMass) " $Sim(EmMass) [lindex $Error(UnitMass) $GDefs(Lang)]"
      focus $Data(EmMassEnt)
      return 0
   }

   #----- Verify that the deposition velocity is greater or equal to 0.
   set idx ""
   set number [string is double -strict -failindex idx $Sim(EmDepVel)]
   if { $number == 0 && $idx == -1 } {
      Dialog::Error .modelnew $Error(EmDepVelRange) " $Sim(EmDepVel) $Error(UnitVelocity)"
      focus $Data(EmDepVelEnt)
      return 0
   } elseif { $number == 0 || ($number == 1 && $Sim(EmDepVel) < 0) } {
      Dialog::Error .modelnew $Error(EmDepVel) " $Sim(EmDepVel) $Error(UnitVelocity)"
      focus $Data(EmDepVelEnt)
      return 0
   }

   #----- Verify that the radioactive half-life is a positive number.
   set idx ""
   set number [string is double -strict -failindex idx $Sim(EmHalfLife)]
   if { $number == 0 && $idx == -1 } {
      Dialog::Error .modelnew $Error(EmHalfLifeRange) " $Sim(EmHalfLife) [lindex $Error(UnitDay) $GDefs(Lang)]"
      focus $Data(EmHalfLifeEnt)
      return 0
   } elseif { $number == 0 || ($number == 1 && $Sim(EmHalfLife) <= 0) } {
      Dialog::Error .modelnew $Error(EmHalfLife) " $Sim(EmHalfLife) [lindex $Error(UnitDay) $GDefs(Lang)]"
      focus $Data(EmHalfLifeEnt)
      return 0
   }

   #----- Verify that the wet scavenging coefficient is greater or equal to 0.
   set idx ""
   set number [string is double -strict -failindex idx $Sim(EmWetScav)]
   if { $number == 0 && $idx == -1 } {
      Dialog::Error .modelnew $Error(EmWetScavRange) " $Sim(EmWetScav)."
      focus $Data(EmWetScavEnt)
      return 0
   } elseif { $number == 0 || ($number == 1 && $Sim(EmWetScav) < 0) } {
      Dialog::Error .modelnew $Error(EmWetScav) " $Sim(EmWetScav)."
      focus $Data(EmWetScavEnt)
      return 0
   }

   #----- Verify that the release duration is an integer positive value.
   set idx ""
   set number [string is integer -strict -failindex idx $Sim(EmDurationMin)]
   if { $number == 0 && $idx == -1 } {
      Dialog::Error .modelnew $Error(EmDurationRange) " $Sim(EmDurationMin) $Error(UnitMinutes)"
      focus $Data(EmDurationEnt)
      return 0
   } elseif { $number == 0 || ($number == 1 && $Sim(EmDurationMin) <= 0) } {
      Dialog::Error .modelnew $Error(EmDuration) " $Sim(EmDurationMin) $Error(UnitMinutes)"
      focus $Data(EmDurationEnt)
      return 0
   }

   #----- Verify that the release duration is greater or equal to model time step.
   if { $Sim(EmDurationMin) < $Sim(ModelTimeStepMin) } {
      Dialog::Error .modelnew $Error(EmDuration2) " [lindex $Error(EmDuration3) $GDefs(Lang)] $Sim(EmDurationMin) $Error(UnitMinutes)\n[lindex $Error(EmDuration4) $GDefs(Lang)] $Sim(ModelTimeStepMin) $Error(UnitMinutes)"
      focus $Data(EmDurationEnt)
      return 0
   }

   #----- Redefine release duration if greater than simulation duration.
   MLCD::SetEmissionDuration

   #----- Verify that bottom of release column is greater or equal to 0.
   set idx ""
   set number [string is double -strict -failindex idx $Sim(EmBottom)]
   if { $number == 0 && $idx == -1 } {
      Dialog::Error .modelnew $Error(EmBottomRange) " $Sim(EmBottom) $Error(UnitMeters)"
      focus $Data(EmBottomEnt)
      return 0
   } elseif { $number == 0 || ($number == 1 && $Sim(EmBottom) < 0) } {
      Dialog::Error .modelnew $Error(EmBottom) " $Sim(EmBottom) $Error(UnitMeters)"
      focus $Data(EmBottomEnt)
      return 0
   }

   #----- Verify that top of release column is greater than bottom of column.
   set idx ""
   set number [string is double -strict -failindex idx $Sim(EmTop)]
   if { $number == 0 && $idx == -1 } {
      Dialog::Error .modelnew $Error(EmTopRange) " $Sim(EmTop) $Error(UnitMeters)"
      focus $Data(EmTopEnt)
      return 0
   } elseif { $number == 0 || ($number == 1 && $Sim(EmTop) <= $Sim(EmBottom)) } {
      Dialog::Error .modelnew $Error(EmTop) " $Sim(EmTop) $Error(UnitMeters)"
      focus $Data(EmTopEnt)
      return 0
   }
   set Sim(EmTopOld) $Sim(EmTop)

   #----- Verify that release column radius is greater or equal to 0.
   set idx ""
   set number [string is double -strict -failindex idx $Sim(EmRadius)]
   if { $number == 0 && $idx == -1 } {
      Dialog::Error .modelnew $Error(EmRadiusRange) " $Sim(EmRadius) $Error(UnitMeters)"
      focus $Data(EmRadiusEnt)
      return 0
   } elseif { $number == 0 || ($number == 1 && $Sim(EmRadius) < 0) } {
      Dialog::Error .modelnew $Error(EmRadius) " $Sim(EmRadius) $Error(UnitMeters)"
      focus $Data(EmRadiusEnt)
      return 0
   }

   return 1
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
      Dialog::Error .modelnew $Error(ObsRoughnessLengthRange) " $Sim(ObsRough) $Error(UnitMeters)"
      focus $Data(ObsRoughnessLengthEnt)
      return 0
   } elseif { $number == 0 || ($number == 1 && $Sim(ObsRough) <= 0) } {
      Dialog::Error .modelnew $Error(ObsRoughnessLength) " $Sim(ObsRough) $Error(UnitMeters)"
      focus $Data(ObsRoughnessLengthEnt)
      return 0
   }

   #----- Verify that Obukhov length is a double value.
   set idx ""
   set number [string is double -strict -failindex idx $Sim(ObsObukhov)]
   if { $number == 0 && $idx == -1 } {
      Dialog::Error .modelnew $Error(ObsObukhovLengthRange) " $Sim(ObsObukhov) $Error(UnitMeters)"
      focus $Data(ObsObukhovLengthEnt)
      return 0
   } elseif { $number == 0 } {
      Dialog::Error .modelnew $Error(ObsObukhovLength) " $Sim(ObsObukhov) $Error(UnitMeters)"
      focus $Data(ObsObukhovLengthEnt)
      return 0
   }

   #----- Verify that precipitation rate is a value greater or equal to 0.
   set idx ""
   set number [string is double -strict -failindex idx $Sim(ObsPrecip)]
   if { $number == 0 && $idx == -1 } {
      Dialog::Error .modelnew $Error(ObsPrecipRateRange) " $Sim(ObsPrecip) $Error(UnitPrecip)"
      focus $Data(ObsPrecipRateEnt)
      return 0
   } elseif { $number == 0 || ($number == 1 && $Sim(ObsPrecip) < 0) } {
      Dialog::Error .modelnew $Error(ObsPrecipRate) " $Sim(ObsPrecip) $Error(UnitPrecip)"
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
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc MLCD::ValidateMeteoTab { } {
   variable Sim

   #----- Validate current observation (local parameters + wind profile).
   if { ![MLCD::ValidateLocalParameters] || ![MLCD::ValidateWindProfile] } {
      return 0
   }

   #----- Save last current observation.
   if { ![MLCD::SelectObs $Sim(ObsNo)] } {
      return 0
   }

   #----- Compute number of valid wind profiles.
   if { ![MLCD::ComputeNbValidWindProfiles] } {
      return 0
   }

   #----- Validate observation date-times.
   if { ![MLCD::ValidateObsDateTime] } {
      return 0
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

   #----- Update emission starting time.
   Model::FitAccTime MLCD

   #----- Verify that simulation duration is an integer positive value.
   set idx ""
   set number [string is integer -strict -failindex idx $Sim(DurMin)]
   if { $number == 0 && $idx == -1 } {
      Dialog::Error .modelnew $Error(SimDurationRange) " $Sim(DurMin) $Error(UnitMinutes)"
      focus $Data(SimDurationEnt)
      return 0
   } elseif { $number == 0 || ($number == 1 && $Sim(DurMin) <= 0) } {
      Dialog::Error .modelnew $Error(SimDuration) " $Sim(DurMin) $Error(UnitMinutes)"
      focus $Data(SimDurationEnt)
      return 0
   }

   set Sim(Duration) [expr int($Sim(DurMin)*60)] ; #----- Simulation duration [s].

   #----- Verify if emission starting time or simulation duration have been modified.
   if { !$Sim(Auto) } {
      if { $Sim(AccSecs)!=$Sim(OldSecs) || $Sim(DurMin)!=$Sim(OldDurMin) } {

         #----- Verify if meteorological parameters have been modified or not.
         if { [MLCD::MeteoParametersModified] } {

            #----- Ask user if re-initializing meteo data when meteo parameters are modified.
            MLCD::AskIfInitMeteoData
         } else {
            #----- Re-initialize meteo data without asking user when meteo parameters are not modified.
            MLCD::InitMeteoData
         }
      }
   }

   #----- Verify that output time step is an integer positive value.
   set idx ""
   set number [string is integer -strict -failindex idx $Sim(OutputTimeStepMin)]
   if { $number == 0 && $idx == -1 } {
      Dialog::Error .modelnew $Error(OutputTimeStepRange) " $Sim(OutputTimeStepMin) $Error(UnitMinutes)"
      focus $Data(OutputTimeStepEnt)
      return 0
   } elseif { $number == 0 || ($number == 1 && $Sim(OutputTimeStepMin) <= 0) } {
      Dialog::Error .modelnew $Error(OutputTimeStep1) " $Sim(OutputTimeStepMin) $Error(UnitMinutes)"
      focus $Data(OutputTimeStepEnt)
      return 0
   }

   #----- Verify that model time step is an integer positive value.
   set idx ""
   set number [string is integer -strict -failindex idx $Sim(ModelTimeStepMin)]
   if { $number == 0 && $idx == -1 } {
      Dialog::Error .modelnew $Error(ModelTimeStepRange) " $Sim(ModelTimeStepMin) $Error(UnitMinutes)"
      focus $Data(ModelTimeStepEnt)
      return 0
   } elseif { $number == 0 || ($number == 1 && $Sim(ModelTimeStepMin) <= 0) } {
      Dialog::Error .modelnew $Error(ModelTimeStep1) " $Sim(ModelTimeStepMin) $Error(UnitMinutes)"
      focus $Data(ModelTimeStepEnt)
      return 0
   }

   #----- Verify that output time step is lower (or equal to) simulation duration.
   if { $Sim(OutputTimeStepMin) > $Sim(DurMin) } {
      Dialog::Error .modelnew $Error(OutputTimeStep2) " [lindex $Error(OutputTimeStep3) $GDefs(Lang)] $Sim(OutputTimeStepMin) $Error(UnitMinutes)\n[lindex $Error(OutputTimeStep4) $GDefs(Lang)] $Sim(DurMin) $Error(UnitMinutes)"
      focus $Data(OutputTimeStepEnt)
      return 0
   }

   #----- Verify that model time step is lower (or equal to) release duration.
   if { $Sim(ModelTimeStepMin) > $Sim(EmDurationMin) } {
      Dialog::Error .modelnew $Error(ModelTimeStep2) " [lindex $Error(ModelTimeStep3) $GDefs(Lang)] $Sim(ModelTimeStepMin) $Error(UnitMinutes)\n[lindex $Error(ModelTimeStep4) $GDefs(Lang)] $Sim(EmDurationMin) $Error(UnitMinutes)"
      focus $Data(ModelTimeStepEnt)
      return 0
   }

   #----- Verify that model time step is lower (or equal) to simulation duration.
   if { $Sim(ModelTimeStepMin) > $Sim(DurMin) } {
      Dialog::Error .modelnew $Error(ModelTimeStep5) " [lindex $Error(ModelTimeStep6) $GDefs(Lang)] $Sim(ModelTimeStepMin) $Error(UnitMinutes)\n[lindex $Error(ModelTimeStep7) $GDefs(Lang)] $Sim(DurMin) $Error(UnitMinutes)"
      focus $Data(ModelTimeStepEnt)
      return 0
   }

   #----- Verify that model time step is greater than (or equal to) 1 minute and lower than (or equal to) 60 min.
   if { $Sim(ModelTimeStepMin) < 1 || $Sim(ModelTimeStepMin) > 60 } {
      Dialog::Error .modelnew $Error(ModelTimeStep11) " [lindex $Error(ModelTimeStep3) $GDefs(Lang)] $Sim(ModelTimeStepMin) $Error(UnitMinutes)"
      focus $Data(ModelTimeStepEnt)
      return 0
   }

   #----- Verify that model time step is an integer divisor of 60 minutes.
   if { [expr fmod(60, $Sim(ModelTimeStepMin))] > $Sim(Epsilon) } {
      Dialog::Error .modelnew $Error(ModelTimeStep12) " [lindex $Error(ModelTimeStep3) $GDefs(Lang)] $Sim(ModelTimeStepMin) $Error(UnitMinutes)"
      focus $Data(ModelTimeStepEnt)
      return 0
   }

   #----- Verify that model time step is a integer multiple of 1 minute.
   if { [expr fmod($Sim(ModelTimeStepMin), 1)] > $Sim(Epsilon) } {
      Dialog::Error .modelnew $Error(ModelTimeStep13) " [lindex $Error(ModelTimeStep3) $GDefs(Lang)] $Sim(ModelTimeStepMin) $Error(UnitMinutes)"
      focus $Data(ModelTimeStepEnt)
      return 0
   }

   #----- Verify that output time step is greater than (or equal to) 1 minute and lower than (or equal to) 1440 minutes (24 hrs).
   if { $Sim(OutputTimeStepMin) < 1 || $Sim(OutputTimeStepMin) > 1440 } {
      Dialog::Error .modelnew $Error(OutputTimeStep6) " [lindex $Error(OutputTimeStep3) $GDefs(Lang)] $Sim(OutputTimeStepMin) $Error(UnitMinutes)"
      focus $Data(OutputTimeStepEnt)
      return 0
   }

   if { $Sim(OutputTimeStepMin) < 60 } {

      #----- Verify that output time step is an integer divisor of 60 minutes.
      if { [expr fmod(60, $Sim(OutputTimeStepMin))] > $Sim(Epsilon) } {
         Dialog::Error .modelnew $Error(OutputTimeStep7) " [lindex $Error(OutputTimeStep3) $GDefs(Lang)] $Sim(OutputTimeStepMin) $Error(UnitMinutes)"
         focus $Data(OutputTimeStepEnt)
         return 0
      }

      #----- Verify that output time step is an integer multiple of 1 minute.
      if { [expr fmod($Sim(OutputTimeStepMin), 1)] > $Sim(Epsilon) } {
         Dialog::Error .modelnew $Error(OutputTimeStep8) " [lindex $Error(OutputTimeStep3) $GDefs(Lang)] $Sim(OutputTimeStepMin) $Error(UnitMinutes)"
         focus $Data(OutputTimeStepEnt)
         return 0
      }

   } else {

      #----- Verify that output time step is an integer divisor of 1440 minutes.
      if { [expr fmod(1440, $Sim(OutputTimeStepMin))] > $Sim(Epsilon) } {
         Dialog::Error .modelnew $Error(OutputTimeStep9) " [lindex $Error(OutputTimeStep3) $GDefs(Lang)] $Sim(OutputTimeStepMin) $Error(UnitMinutes)"
         focus $Data(OutputTimeStepEnt)
         return 0
      }

      #----- Verify that output time step is an integer multiple of 60 minutes.
      if { [expr fmod($Sim(OutputTimeStepMin), 60)] > $Sim(Epsilon) } {
         Dialog::Error .modelnew $Error(OutputTimeStep10) " [lindex $Error(OutputTimeStep3) $GDefs(Lang)] $Sim(OutputTimeStepMin) $Error(UnitMinutes)"
         focus $Data(OutputTimeStepEnt)
         return 0
      }

   }

   #----- Verify that model time step is lower (or equal to) output time step.
   if { $Sim(ModelTimeStepMin) > $Sim(OutputTimeStepMin) } {
      Dialog::Error .modelnew $Error(ModelTimeStep8) " [lindex $Error(ModelTimeStep9) $GDefs(Lang)] $Sim(ModelTimeStepMin) $Error(UnitMinutes)\n[lindex $Error(ModelTimeStep10) $GDefs(Lang)] $Sim(OutputTimeStepMin) $Error(UnitMinutes)"
      focus $Data(ModelTimeStepEnt)
      return 0
   }

   #----- Verify that output time step is an integer multiple of model time step.
   if { [expr fmod($Sim(OutputTimeStepMin), $Sim(ModelTimeStepMin))] > $Sim(Epsilon) } {
      Dialog::Error .modelnew $Error(OutputTimeStep5) " [lindex $Error(ModelTimeStep9) $GDefs(Lang)] $Sim(ModelTimeStepMin) $Error(UnitMinutes)\n[lindex $Error(ModelTimeStep10) $GDefs(Lang)] $Sim(OutputTimeStepMin) $Error(UnitMinutes)"
      focus $Data(ModelTimeStepEnt)
      return 0
   }

   #----- Validate grid domain for concentration calculations.
   set idx ""
   set number [string is double -strict -failindex idx $Sim(GridDomain)]
   if { $number == 0 && $idx == -1 } {
      Dialog::Error .modelnew $Error(GridDomainRange) " $Sim(GridDomain) $Error(UnitKilometers)"
      focus $Data(ConcDomainEnt)
      return 0
   } elseif { $number == 0 || ($number == 1 && $Sim(GridDomain) <= 0.1) || ($number == 1 && $Sim(GridDomain) > 500) } {
      Dialog::Error .modelnew $Error(GridDomain) " $Sim(GridDomain) $Error(UnitKilometers)"
      focus $Data(ConcDomainEnt)
      return 0
   }

   #----- Verify that number of concentration vertical levels is greater than 1.
   if { [llength $Sim(VerticalLevels)]<2 } {
      Dialog::Error .modelnew $Error(VerticalLevels1) " [lindex $Error(VerticalLevels2) $GDefs(Lang)] $Sim(NbVerticalLevels).\n[lindex $Error(VerticalLevels3) $GDefs(Lang)] $Sim(VerticalLevels)."
      focus $Data(ConcVerticalLevelsEnt)
      return 0
   }

   #----- Verify that all concentration vertical levels are positive and sorted in increasing order.
   for { set i 0 } { $i < [llength $Sim(VerticalLevels)] } { incr i } {
      set level [lindex $Sim(VerticalLevels) $i]

      set idx ""
      set number [string is double -strict -failindex idx $level]
      if { $number == 0 && $idx == -1 } {
         Dialog::Error .modelnew $Error(VerticalLevelsRange) " $level $Error(UnitMeters)"
         focus $Data(ConcVerticalLevelsEnt)
         return 0
      } elseif { $number == 0 || ($number == 1 && $level < 0) } {
         Dialog::Error .modelnew $Error(VerticalLevels4) " $level $Error(UnitMeters)"
         focus $Data(ConcVerticalLevelsEnt)
         return 0
      }

      if { $i > 0 } {
         set prevlevel [lindex $Sim(VerticalLevels) [expr $i - 1]]
         if { $level <= $prevlevel } {
            Dialog::Error .modelnew $Error(VerticalLevels5) " $Sim(VerticalLevels)."
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

   for { set i 0 } { $i < [expr [llength $Sim(ObsValidIdx)]-1] } { incr i } {

      set idx       [lindex $Sim(ObsValidIdx) $i]
      set nextidx   [lindex $Sim(ObsValidIdx) [expr $i+1]]

      set time      [lindex $Sim(ObsTime) $idx]
      set nexttime  [lindex $Sim(ObsTime) $nextidx]

      #----- Verify that all observations follow in time.
      if { $time > $nexttime } {
         set time0 "Observation \# [expr $idx + 1] : [clock format $time -format "%Y-%m-%d %H:%M UTC" -gmt True]"
         set time1 "Observation \# [expr $nextidx + 1] : [clock format $nexttime -format "%Y-%m-%d %H:%M UTC" -gmt True]"

         Dialog::Error .modelnew $Error(ObsTimes) "\t$time0\n\t$time1"

         #----- Select next observation according to date-time stamps comparison.
         MLCD::SelectObs [expr $nextidx + 1]
         return 0
      }
   }

   #----- Validate release date-time according to valid observation date-times.
   set FirstIdx      [lindex $Sim(ObsValidIdx) 0]
   set LastIdx       [lindex $Sim(ObsValidIdx) end]
   set FirstObsTime  [lindex $Sim(ObsTime) $FirstIdx]
   set LastObsTime   [lindex $Sim(ObsTime) $LastIdx]

   if { [llength $Sim(ObsValidIdx)]>1 } { #----- Multiple valid observations.

      if { $Sim(AccSecs) < $FirstObsTime || $Sim(AccSecs) >= $LastObsTime } {
         set acc   "Accident        : [clock format $Sim(AccSecs) -format "%Y-%m-%d %H:%M UTC" -gmt True]"
         set obs0  "Observation \# [expr $FirstIdx + 1] : [clock format $FirstObsTime -format "%Y-%m-%d %H:%M UTC" -gmt True]"
         set obs1  "Observation \# [expr $LastIdx + 1] : [clock format $LastObsTime -format "%Y-%m-%d %H:%M UTC" -gmt True]"

         Dialog::Error .modelnew $Error(ObsTimes2) "\t$acc\n\t$obs0\n\t$obs1"

         #----- Select first observation.
         MLCD::SelectObs 1
         return 0
      }
   } elseif { [llength $Sim(ObsValidIdx)]==1 } { #----- One single valid observation.

      if { $Sim(AccSecs) < $FirstObsTime } {
         set acc   "Accident        : [clock format $Sim(AccSecs) -format "%Y-%m-%d %H:%M UTC" -gmt True]"
         set obs0  "Observation \# [expr $FirstIdx + 1] : [clock format $FirstObsTime -format "%Y-%m-%d %H:%M UTC" -gmt True]"

         Dialog::Error .modelnew $Error(ObsTimes2) "\t$acc\n\t$obs0"

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
         Dialog::Error .modelnew $Error(ObsHeightRange) " $height $Error(UnitMeters)"
         focus $HeightEnt
         return 0
      } elseif { $number == 0 || ($number == 1 && $height <= 0 && $height != "") } {
         Dialog::Error .modelnew $Error(ObsHeight) " $height $Error(UnitMeters)"
         focus $HeightEnt
         return 0
      }

      #----- Verify that velocity is a value greater or equal to 0.
      set idx ""
      set number [string is double -failindex idx $velocity]
      if { $number == 0 && $idx == -1 } {
         Dialog::Error .modelnew $Error(ObsVelocityRange) $GDefs(Lang)] " $velocity $Error(UnitVelocity)"
         focus $VelocityEnt
         return 0
      } elseif { $number == 0 || ($number == 1 && $velocity < 0 && $velocity != "") } {
         Dialog::Error .modelnew $Error(ObsVelocity) " $velocity $Error(UnitVelocity)"
         focus $VelocityEnt
         return 0
      }

      #----- Verify that direction is a positive value falling in the range [0.0, 360.0[.
      set idx ""
      set number [string is double -failindex idx $direction]
      if { $number == 0 && $idx == -1 } {
         Dialog::Error .modelnew $Error(ObsDirectionRange) " $direction $Error(UnitDirection)"
         focus $DirectionEnt
         return 0
      } elseif { $number == 0 || ($number == 1 && $direction != "" && ($direction < 0.0 || $direction >= 360.0)) } {
         Dialog::Error .modelnew $Error(ObsDirection) " $direction $Error(UnitDirection)"
         focus $DirectionEnt
         return 0
      }

      #----- Verify that height field is not empty for a specified velocity or direction.
      if { ($height == "" && ($velocity != "" || $direction != "")) } {
         Dialog::Error .modelnew $Error(ObsEmptyField)
         focus $HeightEnt
         return 0
      }

      #----- Verify that velocity field is not empty for a specified height or direction.
      if { ($velocity == "" && ($height != "" || $direction != "")) } {
         Dialog::Error .modelnew $Error(ObsEmptyField)
         focus $VelocityEnt
         return 0
      }

      #----- Verify that direction field is not empty for a specified height or velocity.
      if { ($direction == "" && ($height != "" || $velocity != "")) } {
         Dialog::Error .modelnew $Error(ObsEmptyField)
         focus $DirectionEnt
         return 0
      }
   }

   return 1
}
