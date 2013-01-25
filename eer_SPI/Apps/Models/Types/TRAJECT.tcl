#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet    : Interface pour la gestion des experiences.
# Fichier   : TRAJECT.tcl
# Creation  : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But       : Description des procedures relatives au module TRAJECTOIRE.
#
#===============================================================================

#----- Fichiers complementaires

source $GDefs(Dir)/Apps/Models/Types/TRAJECT.ctes
source $GDefs(Dir)/Apps/Models/Types/TRAJECT.txt
source $GDefs(Dir)/Apps/Models/Types/TRAJECT.int

#-------------------------------------------------------------------------------
# Nom      : <TRAJECT::InitNew>
# Creation : Octobre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialisation des parametres de la trajectoire.
#
# Parametres :
#   <Type>   : Type de source
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc TRAJECT::InitNew { Type } {
   global GDefs
   variable Sim

   set Sim(Method)     [lindex [lindex $Sim(ListMethod) $GDefs(Lang)] 0]
   set Sim(Backward)   False
   set Sim(Mode)       "prog"
   set Sim(TimeStep)   "3600.0"
   set Sim(BatchStart) 0
   set Sim(Duration)   72
   set Sim(MultiLevel) False

   if { $Type==0 } {

      #----- Ajuster les fonctions de niveau selon le type volcan.
      set Sim(LevelUnit)   "MILLIBARS"
      set Sim(Level1)        700.0
      set Sim(Level2)        500.0
      set Sim(Level3)        250.0
   } else {

      #----- Ajuster les fonctions de niveau selon tout autre type.
      set Sim(LevelUnit)   "METERS"
      set Sim(Level1)       500.0
      set Sim(Level2)       1500.0
      set Sim(Level3)       3000.0
   }

   for { set i 4 } { $i <= 25 } { incr i } {
       set Sim(Level$i)     ""
   }
}

#---------------------------------------------------------------------------
# Nom      : <TRAJECT::Result>
# Creation : Novembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche la trajectoire sur la projection.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc TRAJECT::Result { } {
   variable Sim

   #----- Extraire le nom du fichier de la trajectoire.
   SPI::FileOpen NEW TrajBox "$Exp::Data(No) $Exp::Data(Name)" "" [glob [Exp::Path]/[Info::Path $Exp::Data(SelectSim)]/results/*.points]
}

#-------------------------------------------------------------------------------
# Nom        : <TRAJECT::GetMetData>
# Creation   : Octobre 1999 - J.P. Gauthier - CMC/CMOE
#
# But        : Creation de la liste des fichiers standards pour la meteo.
#
# Parametres :
#
# Retour     :
#   <Bool>   : True ou False.
#
# Remarques  :
#
#-------------------------------------------------------------------------------

proc TRAJECT::GetMetData { } {
   global   GDefs
   variable Error
   variable Sim
   variable Msg

   Dialog::Wait . $Msg(MetGet)

   set Sim(RunStamp) [fstdstamp fromseconds $Sim(AccSecs)]
   if { $Sim(Backward) } {
      set Sim(Data)     [MetData::File $Sim(RunStamp) $Model::Param(DBaseDiag) $Model::Param(DBaseProg) B 1 $Sim(Delta)]
   } else {
      set Sim(Data)     [MetData::File $Sim(RunStamp) $Model::Param(DBaseDiag) $Model::Param(DBaseProg) F 1 $Sim(Delta)]
   }
   Dialog::WaitDestroy

   #----- Extract relevant met files according to available meteorological data files and simulation duration.
   if { [set ok [Model::ParamsMetData TRAJECT]] } {
      set Sim(SimSecs) $Sim(MetSecs)
   }
   return $ok
}

#-------------------------------------------------------------------------------
# Nom      : <TRAJECT::ParamsCheck>
# Creation : Avril 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialiser le lancement en recuperant les dates limites et en verifiant
#            les selections de l'usager.
#
# Parametres :
#   <Tab>    : Frame parent de l'onglet
#   <No>     : Numero de l'onglet selectionne
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc TRAJECT::ParamsCheck { Tab No } {
   global   GDefs
   variable Lbl
   variable Msg
   variable Sim

   #----- Check for last tab
   set nb [expr [TabFrame::NbFrame $Tab]-1]
   if { $No!=$nb } {
      return True
   }

   scan $Sim(Min)  "%02d" min
   scan $Sim(Hour) "%02d" hour
   set Sim(AccSecs) [expr $Sim(Secs)+$hour*3600+$min*60]

   #----- Get the levels
   set Sim(Level) ""
   for { set i 1 } { $i<=25 } { incr i } {
      if { $Sim(Level$i)!="" } {
         lappend Sim(Level) "$Sim(Level$i)"
      } else {
         break
      }
   }

   #----- Get meteorological data according to met database, time interval between files, release accident date-time.
   if { $Sim(ReNewMeteo)=="" } {
      Model::ParamsMetDataDir TRAJECT
      if { ![GetMetData] } {
         return False
      }
   }
   return True
}

#-------------------------------------------------------------------------------
# Nom        : <TRAJECT::CreateModelInput>
# Creation   : Octobre 1999 - J.P. Gauthier - CMC/CMOE
#
# But        : Creer le fichier d'entree du modele.
#
# Parametres :
#
# Retour     :
#
# Remarques  :
#
#-------------------------------------------------------------------------------

proc TRAJECT::CreateModelInput { } {
   variable Sim
   variable Tmp

   if { $Sim(LevelUnit)=="METERS" } {
      set unit MAGL
   } else {
      set unit PRESSURE
   }
   set Sim(Particles) {}

   #----- Creation du fichier de directives
   set f [open  $Sim(Path)/tmp/TRAJECT.in w 0644]
   puts $f "\n#----- Model parameters\n"
   puts $f [format "%-20s= %-12.1f # Internal model time step \[s\]" MDL_DT_INT $Sim(TimeStep)]
   puts $f [format "%-20s= %-12s # Backward simulation" MDL_RETRO $Sim(Backward)]
   puts $f [format "%-20s= %-12s # Type of vertical coordinates (PRESSURE or MAGL)" MDL_ZTYPE $unit]
   puts $f [format "%-20s= %-12s # Emission date-time \[UTC\]: YearMonthDayHourMinutes" MDL_TIME [clock format $Sim(AccSecs) -format "%Y%m%d%H%M" -gmt True]]
   puts $f [format "%-20s= %-12.1f # Batch mode sart time increment \[hours\]" MDL_BATCH_INC $Sim(BatchStart)]
   puts $f [format "%-20s= %-12.1f # Batch mode trajectory duration \[hours\]" MDL_BATCH_LEN $Sim(Duration)]

   puts $f "\n#----- Source parameters\n"
   if { $Sim(MultiLevel) } {
      set levelslist $Sim(Level)
   } else {
      foreach name $Sim(Name) {
         lappend levelslist $Sim(Level)
      }
   }
   foreach name $Sim(Name) lat $Sim(Lat) lon $Sim(Lon) levels $levelslist {
      foreach level $levels {
         lappend Sim(Particles) [list $lon $lat $level $name]
      }
      puts $f [format "%-20s= %-22s # Source name" SRC_NAME $name]
      puts $f [format "%-20s= %-9.7f %-10.7f # Latitude and longitude coordinate of the sources \[degrees\]" SRC_COORD $lat $lon]
      puts $f [format "%-20s= %-22s # Levels of starting parcel" SRC_LEVEL $levels]
   }

   puts $f "\n#----- Output parameters\n"
   puts $f [format "%-20s= %-12s # Output interval (INTERN or EXTERN)" OUT_STEP EXTERN]
   puts $f [format "%-20s= %-12s # Split into files per trajectory id" OUT_SPLIT TRUE]

   puts $f "\n#----- Meteorological input standard file\n"
   puts $f [format "%-20s= %s" MET_FILES $Sim(MeteoDataFiles)]

   close $f
}

#----------------------------------------------------------------------------
# Nom        : <TRAJECT::CreateScriptInput>
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

proc TRAJECT::CreateScriptInput { } {
   variable Sim
   global   GDefs

   set file [open $Sim(Path)/tmp/Model_TRAJECT.in w 0644]

   puts $file "\n#----- Logger specific parameters\n"
   puts $file "LOG_MAILTO=\"$Model::Param(EMail)\""
   puts $file "LOG_MAILTITLE=\"$Sim(Model) ($Model::Param(App))\""
   puts $file "LOG_FILE=$Sim(PathRun)/tmp/Model_TRAJECT.out"
   puts $file "LOG_LEVEL=$Model::Param(LogLevel)"
   puts $file "LOG_TIME=$Model::Param(LogTime)"
   puts $file "LOG_JOBID=$Sim(Model)"

   if { !$Model::Param(Auto) } {
      puts $file "LOG_JOBCLASS=INTERACTIVE"
   }

   puts $file "\n#----- Job general parameters\n"
   puts $file "MODEL_SOFTWARE=SPI"
   puts $file "MODEL_NAME=$Sim(Model)"
   puts $file "MODEL_TYPE=\"\""
   puts $file "MODEL_USER=$GDefs(FrontEndUser)"
   puts $file "MODEL_LOCALHOST=$GDefs(Host)"
   puts $file "MODEL_LOCALDIR=$Sim(Path)"
   puts $file "MODEL_RUNDIR=$Sim(PathRun)"
   puts $file "MODEL_PRE=0"
   puts $file "MODEL_RUN=1"
   puts $file "MODEL_POST=0"
   puts $file "MODEL_POOL=$Model::Param(Pool)"
   puts $file "MODEL_CLEAN=1"
   puts $file "MODEL_TRACE=$Exp::Param(Path)/trace"

   puts $file "\n#----- Model specific parameters\n"
   puts $file "TRAJECT_INPUT=tmp/$Sim(Model).in"
   puts $file "TRAJECT_RESULT=results/traject.points"

   close $file
}

#-------------------------------------------------------------------------------
# Nom      : <TRAJECT::Launch>
# Creation : Octobre 1999 - J.P. Gauthier - CMC/CMOE
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

proc TRAJECT::Launch { } {
   global   GDefs
   global   env
   variable Sim

   Model::ParamsMeteoInput TRAJECT

   TRAJECT::CreateModelInput
   TRAJECT::CreateScriptInput

   if { $Sim(Backward) } {
      set mode BACKWARD
   } else {
      set mode FORWARD
   }

   if { $Sim(LevelUnit)=="METERS" } {
      set unit MAGL
   } else {
      set unit PRESSURE
   }

   exec echo "#!/bin/sh\n\n$Model::Param(Submit) $env(EER_DIRSCRIPT)/Model.sh -args $Sim(PathRun)/tmp/Model_TRAJECT.in -mach $Model::Param(Host) \
      -t 3600 -cm 1G -listing $Model::Param(Listings) $Model::Param(Op) -queue $Model::Param(Queue)" >$Sim(Path)/tmp/Model_Launch.sh
   exec chmod 755 $Sim(Path)/tmp/Model_Launch.sh

   if { $Model::Param(IsUsingSoumet) } {

      #----- Copy needed file to run host:directory.
      Model::ParamsCopy TRAJECT

      eval set err \[catch \{ exec $Model::Param(Submit) $env(EER_DIRSCRIPT)/Model.sh -args $Sim(PathRun)/tmp/Model_TRAJECT.in -mach $Model::Param(Host) \
         -t 3600 -cm 1G -listing $Model::Param(Listings) $Model::Param(Op) -queue $Model::Param(Queue) 2>@1 \} msg\]
      catch { exec echo "$msg" > $Sim(Path)/tmp/Model_Launch.out }

      if { $err } {
         Log::Print ERROR "Submitting the job on $Model::Param(Host) failed:\n\n\t$msg"
         return False
      }
      Log::Print INFO "Job has been submitted successfully on $Model::Param(Host)."
  } else {
      set info [Info::Code ::TRAJECT::Sim]
      set id [Exp::Id $info]
      simulation create $id -type Trajectory
      simulation param $id -title $Sim(NameExp) -timestep $Sim(TimeStep) \
         -mode $mode -unit $unit -date $Sim(AccSecs) -particles $Sim(Particles) -data $Sim(MeteoDataFiles) -output $Sim(Path)/results/traject.points \
         -tinc $Sim(BatchStart) -tlen $Sim(Duration) -split 1
      simulation define $id -tag $info -loglevel $Model::Param(LogLevel) -logfile $Sim(Path)/tmp/traject.log

      #----- Launch simulation within a new thread
      eval set tid1 \[thread::create -stack 1073741824 \{ load $GDefs(Dir)/Lib/$env(ARCH)/libTclSim[info sharedlibextension] TclSim\; simulation run $id\}\]

      Exp::ThreadUpdate $id $Exp::Param(Path)/$Sim(NoExp)_$Sim(NameExp)/TRAJECT.pool [simulation param $id -result]
   }

   return True
}

#-------------------------------------------------------------------------------
# Nom      : <TRAJECT::SwitchElev>
# Creation : Ocotbre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Faire la conversion en metre ou en millibar.
#
# Parametres :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc TRAJECT::SwitchElev { } {
   variable Sim

   if { [string compare $Sim(LevelUnit) "MILLIBARS"] == 0 } {

      set Sim(LevelUnit) "METERS"

      for { set i 1 } { $i <= 25 } { incr i } {
         if { $Sim(Level$i)!="" } {
            set Sim(Level$i) [format "%0.1f" [Convert::Millibar2Meter $Sim(Level$i)]]
         }
      }
   } else {

      set Sim(LevelUnit) "MILLIBARS"

      for { set i 1 } { $i <= 25 } { incr i } {
         if { $Sim(Level$i)!="" } {
            set Sim(Level$i) [format "%0.1f" [Convert::Meter2Millibar $Sim(Level$i)]]
         }
      }
   }
}
