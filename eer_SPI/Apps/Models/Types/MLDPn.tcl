#============================================================================
# Environnement Canada - Service meteorologique du Canada
# Centre meteorologique canadien
# 2121 Route Trans-canadienne
# Dorval, Quebec
# H9P 1J3
#
# Projet     : Interface pour la gestion des experiences.
# Fichier    : <MLDPn.tcl>
# Creation   : Octobre 1999 - J.P. Gauthier - CMC/CMOE
#
# Description: Description des procedures relatives au module MLDP.
#
# Remarques  :
#
#============================================================================

#----- Fichiers complementaires

source $GDefs(Dir)/Apps/Models/Types/MLDPn.txt
source $GDefs(Dir)/Apps/Models/Types/MLDPn.ctes
source $GDefs(Dir)/Apps/Models/Types/MLDPn.int
source $GDefs(Dir)/Apps/Models/Types/MLDPn_Validate.tcl

#----------------------------------------------------------------------------
# Nom        : <MLDPn::CheckFileSize>
# Creation   : 28 August 2007 - A. Malo - CMC/CMOE
#
# But        : Check if simulation duration and output time step have
#              changed.
#
# Parametres :
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDPn::CheckFileSize { } {
   variable Sim
   variable Tmp

   if { $Sim(Duration) != $Tmp(Duration) } {
      set Sim(IsResFileSizeChecked) 0
      set Sim(IsMetFileSizeChecked) 0
      set Tmp(Duration)             $Sim(Duration)
      set Tmp(Delta)                $Sim(Delta)
   }

   if { $Sim(OutputTimeStepMin) != $Tmp(OutputTimeStepMin) } {
      set Sim(IsResFileSizeChecked) 0
      set Tmp(OutputTimeStepMin)    $Sim(OutputTimeStepMin)
   }

   if { $Sim(Delta) != $Tmp(Delta) } {
      set Sim(IsMetFileSizeChecked) 0
      set Tmp(Delta)                $Sim(Delta)
   }
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::Launch>
# Creation   : 5 September 2007 - A. Malo - CMC/CMOE
#
# But        : Launch entire job (meteorological preprocessing and model).
#
# Parametres :
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDPn::Launch { } {
   global   GDefs
   global   env
   variable Sim
   variable Lbl
   variable Msg
   variable Warning

   #----- Create input files for meteorological preprocessing script, model script, launch script.
   Model::ParamsMeteoInput MLDPn
   MLDPn::CreateModelInput
   MLDPn::CreateScriptInput

   #----- Launch meteorological fields script for RSMC response.
   if { $Sim(SrcType) == "ACCIDENT" && [file exists $Sim(Path)/tmp/data_std_pres.in] } {
      Log::Print INFO "Launching RSMC meteorological fields script on local host ($GDefs(Host))."
      set ErrorCode [catch { exec $env(EER_DIRSCRIPT)/GenerateMetfields.tcl $Sim(Path)/tmp $Sim(SimYear)$Sim(SimMonth)$Sim(SimDay)$Sim(SimHour) $Sim(SimYear)$Sim(SimMonth)$Sim(SimDay)$Sim(SimHour) $Sim(Path)/tmp/data_std_pres.in >& $Sim(Path)/tmp/GenerateMetfields.out & } Message]
   }

   if { $Model::Param(IsUsingSoumet) } {

      #----- Meteo is local, launch it's processing and wait for it.
      if { $Model::Param(DBaseLocal) } {
         if { ![Dialog::Default .modelnew 400 WARNING $Warning(MetLocal) "" 0 $Lbl(No) $Lbl(Yes)] } {
            return False
         }

         Dialog::Wait . $Msg(MetProcess)
         exec $env(EER_DIRSCRIPT)/Model_Meteo$Sim(Model).sh $Sim(Path)/tmp $Sim(Meteo) 1 $Sim(NI)x$Sim(NJ)x$Sim(NK) low
         Dialog::WaitDestroy
      }

      #----- Copy needed file to run host:directory.
      Model::ParamsCopy MLDP

      set cpus "-cpus $Model::Param(NbMPItasks)x$Model::Param(NbOMPthreads) -mpi"
      set mem 20G

      exec echo "#!/bin/sh\n\n$Model::Param(Submit) $env(EER_DIRSCRIPT)/Model.sh -args $Sim(PathRun)/tmp/Model_MLDPn.in -mach $Model::Param(Host) \
         -t $Sim(RunningTimeCPU) -cm $mem -waste $cpus -listing $Model::Param(Listings) $Model::Param(Op) -queue $Model::Param(Queue)" >$Sim(Path)/tmp/Model_Launch.sh
      exec chmod 755 $Sim(Path)/tmp/Model_Launch.sh
      eval set err \[catch \{ exec $Model::Param(Submit) $env(EER_DIRSCRIPT)/Model.sh -args $Sim(PathRun)/tmp/Model_MLDPn.in -mach $Model::Param(Host) \
         -t $Sim(RunningTimeCPU) -cm $mem -waste $cpus -listing $Model::Param(Listings) $Model::Param(Op) -queue $Model::Param(Queue) 2>@1 \} msg\]
      catch { exec echo "$msg" > $Sim(Path)/tmp/Model_Launch.out }

      if { $err } {
         Log::Print ERROR "Submitting the job on $Model::Param(Host) failed:\n\n\t$msg"
         return False
      }
      Log::Print INFO "Job has been submitted successfully on $Model::Param(Host)."

   } else {
      Log::Print INFO "Launching model on $Model::Param(Host)"
      exec echo "#!/bin/sh\n\n$env(EER_DIRSCRIPT)/Model.sh $Sim(Path)/tmp/Model_MLDPn.in" >$Sim(Path)/tmp/Model_Launch.sh
      exec ssh -n $Model::Param(Host) $env(EER_DIRSCRIPT)/Model.sh $Sim(Path)/tmp/Model_MLDP.in &
   }
   exec chmod 755 $Sim(Path)/tmp/Model_Launch.sh

   return True
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::CreateScriptInput>
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

proc MLDPn::CreateScriptInput { } {
   variable Sim
   global   GDefs

   #----- Create ASCII file containing directives for launching entire job.
   set file [open $Sim(Path)/tmp/Model_MLDPn.in w 0644]

   puts $file "\n#----- Logger specific parameters\n"
   puts $file "LOG_MAILTO=\"$Model::Param(EMail)\""
   puts $file "LOG_MAILTITLE=\"$Sim(Model) ($Model::Param(App))\""
   puts $file "LOG_FILE=$Sim(PathRun)/tmp/Model_MLDPn.out"
   puts $file "LOG_LEVEL=$Model::Param(LogLevel)"
   puts $file "LOG_TIME=$Model::Param(LogTime)"
   puts $file "LOG_JOBID=$Sim(Model)"

   if { !$Model::Param(Auto) } {
      puts $file "LOG_JOBCLASS=INTERACTIVE"
   }

   puts $file "\n#----- Job general parameters\n"
   puts $file "MODEL_SOFTWARE=SPI"
   puts $file "MODEL_NAME=MLDPn"
   puts $file "MODEL_USER=$GDefs(FrontEndUser)"
   puts $file "MODEL_LOCALHOST=$GDefs(Host)"
   puts $file "MODEL_LOCALDIR=$Sim(Path)"
   puts $file "MODEL_RUNDIR=$Sim(PathRun)"
   puts $file "MODEL_PRE=$Model::Param(NbCPUsMeteo)"
   puts $file "MODEL_RUN=1"
   puts $file "MODEL_POST=1"
   puts $file "MODEL_POOL=$Model::Param(Pool)"
   puts $file "MODEL_CLEAN=1"
   puts $file "MODEL_TRACE=$Exp::Param(Path)/trace"
   puts $file "MODEL_NBMPITASKS=$Model::Param(NbMPItasks)"
   puts $file "MODEL_NBOMPTHREADS=$Model::Param(NbOMPthreads)"
   puts $file "MODEL_SEED=$Sim(Seed)"

   puts $file "MODEL_RESTARTABLE=1"

   puts $file "\n#----- Model specific parameters\n"

   if { $Sim(ReNewMeteo)!="" } {
      puts $file "MLDP_METEO=$Sim(ReNewMeteo)"
   } elseif { $Model::Param(DBaseLocal) } {
      puts $file "MLDP_METEO=$Sim(Path)/meteo"
   } else {
      puts $file "MLDP_METEO=$Sim(Meteo)"
   }
   puts $file "MLDP_GRIDDEF=$Sim(NI)x$Sim(NJ)x$Sim(NK)"
   puts $file "MLDP_INPUT=$Sim(PathRun)/tmp/$Sim(Model).in"
   puts $file "MLDP_RESULT=$Sim(PathRun)/results"

   close $file
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::ParamsCheck>
# Creation   : Juin 2001 - J.P.Gauthier - CMC/CMOE
#
# But        : Effectuer toutes les verifications de parametres et recuperer
#              les donnees meteorologiques disponibles pour la simulation.
#
# Parametres :
#   <Tab>    : Frame parent de l'onglet
#   <No>     : Numero de l'onglet
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDPn::ParamsCheck { Tab No } {
   global   GDefs
   variable Error
   variable Sim

   if { $No == $Sim(ScenarioTabNo) } {
      if { [TabFrame::Previous $Tab]==0 && $Sim(ReNewMeteo)=="" } {
         MLDPn::UpdateEmissionStartingTime
         if { ![MLDPn::GetMetData] } {
            return False
         }
      }
      MLDPn::GraphUpdate
   }

   #----- Check for last tab
   set nb [expr [TabFrame::NbFrame $Tab]-1]
   if { $No!=$nb } {
      return True
   }


   #----- Validate output and model time steps but not if we are relaunching.
   if { $Sim(ReNewMeteo)=="" } {
      if { ![MLDPn::ValidateTimeSteps] || ![MLDPn::ValidateSimulationDuration] || ![MLDPn::ValidateOtherParams] } {
         TabFrame::Select $Tab 0
         return False
      }
   }

   #----- Make sure there is an emission
   if { $Sim(EmNbIntervals)<=$Sim(EmInterStart) && !$Sim(IsRestart)} {
      Dialog::Error .modelnew $Error(NoEmission)
      TabFrame::Select $Tab $Sim(ScenarioTabNo)
      return False
   }

   #----- Validate emission
   if { ![MLDPn::ValidateScenarioDuration] || ![MLDPn::ValidateEmissionQuantity] || ![MLDPn::ValidateLullPeriods] || ![MLDPn::ValidateDurationsVsModelTimeStep] } {
      TabFrame::Select $Tab $Sim(ScenarioTabNo)
      return False
   }

   #----- Check if we need to save if we are leaving the scenario tab
   if { !$Sim(IsRestart) } {
      MLDPn::ScenarioPromptSave
   }

   #----- Format useful information for pool encoding
   MLDPn::UpdatePoolLists

   #----- Get meteorological data according to met database, time interval between files, release accident date-time.
   if { $Sim(ReNewMeteo)=="" } {
      if { ![MLDPn::GetMetData] } {
         return False
      }
   }

   return True
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::CreateModelInput>
# Creation   : 22 March 2004 - A. Malo - CMC/CMOE
#
# But        : Create MLDP model input file.
#
# Parametres :
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDPn::CreateModelInput { } {
   global   GDefs
   variable Sim
   variable Tmp

   set file [open $Sim(Path)/tmp/$Sim(Model).in w]

   puts $file "\n#----- Model parameters\n"
   puts $file [format "%-21s= %-10.1f # Internal model time step \[s\]" MDL_DT_INT [expr $MLDPn::Sim(ModelTimeStepMin)*60].0]]
   puts $file [format "%-21s= %-10.1f # Ratio of diffusion time step over Lagrangian time scale \[dimensionless\]" MDL_DT_SUR_TL $Sim(DtOverTl)]
   puts $file [format "%-21s= %-10.1f # Diffusion time step minimum value \[s\]" MDL_DT_BAS $Sim(DtMin)]
   puts $file [format "%-21s= %-10.2f # Horizontal wind velocity variance for mesoscale fluctuations \[m2/s2\]" MDL_SIG2_V $Sim(VarMesoscale)]
   puts $file [format "%-21s= %-10.1f # Lagrangian time scale \[s\]" MDL_TL_V $Sim(Timescale)]
   puts $file [format "%-21s= %-10s # Flag indicating if including horizontal diffusion in free atmosphere" MDL_INCLUDEHORIZDIFF $Sim(IsIncludeHorizDiff)]
   puts $file [format "%-21s= %-10s # Flag indicating if including horizontal wind speed variances in diffusion calculations" MDL_INCLUDESUV $Sim(IsIncludeSUV)]]
   puts $file [format "%-21s= %-10s # Diffusion kernel selection method (VARIABLE or kernel name)" MDL_KERNEL $Sim(DiffKernel)]
   puts $file [format "%-21s= %-10s # Backward simulation" MDL_RETRO $Sim(Backward)]

   puts $file "\n#----- Emission parameters\n\n#----- Chemical symbol of radionuclide, Radioactive half-life period \[s\] , Dry deposition velocity \[m/s\], Wet scavenging rate \[s-1\]"
   foreach iso $Sim(EmIsoInfo) {
      puts $file [format "%-8s= %-15s %15.2e %15.2e %15.2e" EMI_ISO [lindex $iso 0] [lindex $iso 1] [lindex $iso 2] [lindex $iso 3]]
   }

   if { $Sim(RestartTrialDate) } {
      puts $file [format "%-21s= %-25s # Time(s) at which to produce a restart \[UTC\]: YearMonthDayHourMinute" MDL_RESTART_TIME [clock format $Sim(RestartTrialDate) -format "%Y%m%d%H%M" -timezone :UTC]]
   }

   puts $file "\n#----- Source parameters\n"
   foreach name $Sim(Name) lat $Sim(Lat) lon $Sim(Lon) {
      puts $file [format "%-21s= %-25s # Source name" SRC_NAME $name]
      puts $file [format "%-21s= %-25s # Source type" SRC_TYPE $Sim(SrcType)]
      puts $file [format "%-21s= %-12.6f %-12.6f # Latitude and longitude coordinate of the sources \[degrees\]" SRC_COORD $lat $lon]
      puts $file [format "%-21s= %-25s # Emission date-time \[UTC\]: YearMonthDayHourMinute" SRC_TIME $Sim(AccYear)$Sim(AccMonth)$Sim(AccDay)$Sim(AccHour)$Sim(AccMin)]

      if { $Sim(SrcType) == "VOLCANO" } {
         puts $file [format "%-21s= %-25s # Total released mass for volcanic eruption \[micrograms\]" SRC_EMI_MASS_VOLCANO $Sim(EmMass)]
      }

      if { [set vert [lsearch -exact [lindex $MLDPn::Sim(ListEmVerticalDist) 0] $Sim(EmVerticalDist)]]==-1 } {
         set vert [lsearch -exact [lindex $MLDPn::Sim(ListEmVerticalDist) 1] $Sim(EmVerticalDist)]
      }
      switch $vert {
         0 { set distr "0.000 0.100 0.200 0.300 0.400 0.500 0.600 0.700 0.800 0.900 1.000" }
         1 { set distr "0.000 0.010 0.030 0.060 0.100 0.150 0.260 0.410 0.700 0.900 1.000" }
         2 { set distr "0.000 0.010 0.020 0.040 0.070 0.120 0.190 0.290 0.430 0.650 1.000" }
         3 { set distr "0.000 0.010 0.030 0.060 0.100 0.150 0.300 0.550 0.800 0.950 1.000" }
         4 { set distr "0.000 0.100 0.300 0.590 0.740 0.850 0.900 0.940 0.970 0.990 1.000" }
      }
      puts $file [format "%-21s= %s # Cumulative fraction \[0,1\] of number of particles within vertical emission plume column" SRC_COLUMN $distr]

      for { set i $Sim(EmInterStart) } { $i < $Sim(EmNbIntervals) } { incr i } {
         set emti [expr $Sim(EmInter.$i)*3600]

         if { $Sim(EmIsEm.$i) } {
            set height  $Sim(EmHeight.$i)
            set radius  $Sim(EmRadius.$i)
            set np      [expr $Sim(EmIsAutoNP) ? 0 : $Sim(EmNumberParticles.$i)]
            if { $Sim(EmMassMode.$i) == 0 } {
               set mass SPARKS
            } elseif { $Sim(EmMassMode.$i) == 1 } {
               set mass MASTIN
            } else {
               set mass [format "%-9e" $Sim(EmMass.$i)]
            }
         } else {
            set height  0.0
            set radius  0.0
            set np      0
            set mass    0.0
         }

         set rates {}
         switch $Sim(SrcType) {
             VOLCANO  { set rates $mass }
             ACCIDENT {
                for {set j 0} {$j < $Sim(EmNbIso)} {incr j} {
                   lappend rates [format "%-9e" $Sim(EmRate.$i.$j)]
                }
             }
             default { set rates $Sim(EmRate.$i) }
         }

         if { $i==$Sim(EmInterStart) } {
            puts $file [format "%-21s= %-7i %7i %8.1f %6.1f %s # Nb Parcel, Duration, Height, Radius, Mass/Rate for each isotope" SRC_EMI_INTERVAL $np $emti $height $radius $rates]
         } else {
            puts $file [format "%-21s= %-7i %7i %8.1f %6.1f %s" SRC_EMI_INTERVAL $np $emti $height $radius $rates]
         }
      }
   }

   puts $file "\n#----- Parcel parameters\n"
   #----- Particle size distribution (gravitational settling velocities).
   if { [set sizeIdx [lsearch -exact [lindex $MLDPn::Sim(ListEmSizeDist) 0] $Sim(EmSizeDist)]]==-1 } {
     set sizeIdx [lsearch -exact [lindex $MLDPn::Sim(ListEmSizeDist) 1] $Sim(EmSizeDist)]
   }
   set sizeLast [expr [llength [lindex $MLDPn::Sim(ListEmSizeDist) $GDefs(Lang)]] - 1]
   set IsComputeSV "FALSE"
   if { $Sim(SrcType) == "VOLCANO" && $sizeIdx != $sizeLast } {
      set IsComputeSV "TRUE"
   }
   puts $file [format "%-20s= %-10s # Flag indicating if computing gravitational settling velocities" PRC_COMPUTESV $IsComputeSV]
   puts $file [format "%-20s= %-10.3e # " PRC_DENSITY $Sim(EmDensity)]
   puts $file [format "%-20s= %-10s # Particle diameter size boundaries \[microns\], and Fraction \[0,1\] of total number of particles for each size bin" PRC_BINS ""]

   switch $Sim(SrcType) {
      "VOLCANO" {
         #----- Number of particle diameter intervals.
         switch $sizeIdx {
            0 { set dist $Sim(DistSpurrSept1992) ;#----- Spurr September 1992 size distribution. }
            1 { set dist $Sim(DistSpurrAug1992)  ;#----- Spurr August 1992 size distribution. }
            2 { set dist $Sim(DistRedoubt1989)   ;#----- Redoubt 1989-1990 empirical size distribution. }
            3 { set dist $Sim(DistNAME)          ;#----- Size distribution used in NAME (London VAAC, UK Met Office). }
            4 { set dist $Sim(DistFine)          ;#----- Fine size distribution. }
            default { set dist $Sim(DistRedoubt1989) }
         }
      }
      "ACCIDENT" -
      "VIRUS" { set dist $Sim(DistRedoubt1989) }
   }

   foreach { diam frac } $dist {
      puts $file [format "%-10.1f %-10.3f" $diam $frac]
   }

   puts $file "\n#----- Output parameters\n"
   puts $file [format "%-15s= %-10.1f # Output time step \[s\]" OUT_DT [expr $Sim(OutputTimeStepMin)*60]]
   puts $file [format "%-15s= %s # Output grid definition" OUT_GRID $Sim(PathRun)/tmp/grid.in]
   puts $file [format "%-15s= %s # Concentration vertical levels \[m\]" OUT_CVLEVELS $Sim(OutCV)]
   if { $Sim(SrcType) == "VOLCANO" } {
      puts $file [format "%-15s= %s # Aviation vertical levels \[feet\]" OUT_AVLEVELS $Sim(OutAV)]
   }
   puts $file [format "%-15s= %s # Variables to be saved in output" OUT_VARS $Sim(OutVar)]

   puts $file "\n#----- Meteorological parameters\n"
   puts $file [format "%-11s= %-10.4f # Bottom reflection level of particles in the atmosphere \[hybrid|eta|sigma\]" MET_BOTTOM $Sim(ReflectionLevel)]
   puts $file "MET_FILES  =            # Meteorological input files"
   for { set i 0 } { $i < [llength $Sim(MeteoDataFiles)] } { incr i } {
      puts $file "$Sim(PathRun)/meteo/[file tail [lindex $Sim(MeteoDataFiles) $i]].std"
   }

   close $file

   exec ln -s $Sim(PathRun)/meteo/[file tail [lindex $Sim(MeteoDataFiles) 0]].std $Sim(PathRun)/tmp/grid.in
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::GetMetData>
# Creation   : Octobre 1999 - J.P. Gauthier - CMC/CMOE
#
# But        : Creation de la liste des fichiers standards pour la meteo.
#
# Parametres :
#
# Retour     :
#  <Bool>    : True ou False.
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDPn::GetMetData { } {
   global GDefs
   variable Msg
   variable Sim

   #----- Skip if this is a relaunch since we use the same meteo
   if { $Sim(ReNewMeteo)!="" } {
      return True
   }

   Dialog::Wait . $Msg(MetGet)

   #----- Define mixed mode.
   if { $Model::Param(DBaseDiag) == $Model::Param(DBaseProg) } {
      set LatestRun -1 ; #----- Ignored the latest run.
   } else {
      set LatestRun 1  ; #----- Take into account the latest run.
   }

   #----- Get available meteorological files.
   if { $Sim(IsRestart) } {
      set Sim(RunStamp) [fstdstamp fromseconds $Sim(RestartDate)]
   } else {
      set Sim(RunStamp) [fstdstamp fromdate $Sim(AccYear)$Sim(AccMonth)$Sim(AccDay) $Sim(AccHour)$Sim(AccMin)0000]
   }
   set Sim(Data)     [MetData::File $Sim(RunStamp) $Model::Param(DBaseDiag) $Model::Param(DBaseProg) F $LatestRun $Sim(Delta)]
   set Sim(Mode)     [MetData::GetMode $Sim(Data)]

   Dialog::WaitDestroy

   #----- Check for restart date at last trial
   set Sim(RestartTrialDate) 0

   if { $MetData::Data(TA)!=-1 } {

      #----- If the date of the last trial date is earlierr than the end of the simulation
      if { $MetData::Data(TA) < [expr [fstdstamp toseconds $Sim(RunStamp)] + $Sim(Duration)*3600] } {
         set Sim(RestartTrialDate) $MetData::Data(TA)
      }
   }

   #----- Extract relevant met files according to available meteorological data files and simulation duration.
   return [Model::ParamsMetData MLDPn]
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::ScenarioSaveAs>
# Creation   : Juillet 2010 - E. Legault-Ouellet - CMC/CMOE
#
# But        : Save changes made to a scenario under a new name.
#
# Parametres :
#
# Retour     : 1 if save was successful, 0 if it wasn't.
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDPn::ScenarioSaveAs { } {
   variable Warning
   variable Error
   variable Lbl
   variable Sim
   variable Tmp
   variable Msg

   #----- Get scenario name from user
   set Tmp(EmScenario) [Dialog::Get .modelnew $Lbl(ScenarioName) $Msg(ScenarioName)]

   #----- Make sure a name was given
   if { $Tmp(EmScenario) == "" } {
      Dialog::Error .modelnew $Error(ScenarioName)
      return 0
   }

   #----- Substitute all spaces, semicolon and colon by underscore.
   regsub -all "\[^a-zA-Z0-9-\]" $Tmp(EmScenario) "_" Tmp(EmScenario)

   #----- Check if scenario is the "default" scenario, which can't be overwritten
   if { $Tmp(EmScenario) == "default" } {
      Dialog::Error .modelnew $Warning(OverwriteDefault)
      return 0
   }

   #----- If file already exists, ask user for overwrite
   if { [file isfile $Sim(EmDirScenario)/$Tmp(EmScenario).txt] } {
      if { ![Dialog::Default .modelnew 400 WARNING $Warning(Overwrite) " $Tmp(EmScenario)" 0 $Lbl(Cancel) $Lbl(Overwrite)] } {
         return 0
      } else {
         set Sim(EmScenario) $Tmp(EmScenario)
         MLDPn::ScenarioWrite
         return 1
      }
   } else {
      set Sim(EmScenario) $Tmp(EmScenario)
      MLDPn::ScenarioWrite

      #----- Add new scenario name to scenario list
      lappend Sim(EmList) $Sim(EmScenario)
      return 1
   }
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::ScenarioSave>
# Creation   : Juillet 2010 - E. Legault-Ouellet - CMC/CMOE
#
# But        : Save changes made to a scenario.
#
# Parametres :
#
# Retour     : 1 if save was successful, 0 if it wasn't.
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDPn::ScenarioSave { } {
   variable Sim
   variable Warning

   #----- Check if scenario is the "default" scenario, which can't be overwritten
   if { $Sim(EmScenario) == "default" } {
      Dialog::Error .modelnew $Warning(OverwriteDefault)
      return 0
   }

   MLDPn::ScenarioWrite
   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::ScenarioPromptSave>
# Creation   : Juillet 2010 - E. Legault-Ouellet - CMC/CMOE
#
# But        : Check for changes to emission scenario and prompt user if needed.
#
# Parametres :
#     <Force>  : (Optionnal) set to True to force the prompt. Default is False.
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDPn::ScenarioPromptSave { {Force False} } {
   variable Warning
   variable Error
   variable Lbl
   variable Sim
   variable Tmp
   variable Msg

   #----- Don't prompt if there is nothing new to save.
   if { !$Sim(ScenarioHasChanged) && !$Force } {
      return
   }

   set ok 0
   while { True } {
      set Tmp(EmScenario) $Sim(EmScenario)

      #----- Ask user if he wants to : (0) cancel, (1) save, (2) save as
      set choice [Dialog::Default .modelnew 400 WARNING $Warning(Save) $Tmp(EmScenario) 0 $Lbl(Ignore) $Lbl(Save) $Lbl(SaveAs)]
      switch $choice {
         0 { break }
         1 { set ok [MLDPn::ScenarioSave] }
         2 { set ok [MLDPn::ScenarioSaveAs] }
      }

      if { $ok } {
         break
      }
   }

   #----- Reset flag
   if { !$Force } {
      set Sim(ScenarioHasChanged) 0
   }
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::ScenarioList>
# Creation   : Juillet 2010 - E. Legault-Ouellet - CMC/CMOE
#
# But        : Returns the list of all scenario names available.
#
# Parametres :
#
# Retour     :
#  <Idx>     : Flag indicating if reading of emission scenario file
#              has been read successfully (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDPn::ScenarioList { } {
   global GDefs
   variable Sim
   variable Error

   #----- Initialize list of emission scenario.
   set Sim(EmList) {}

   #----- Initialize scenario directory.
   switch $Sim(SrcType) {
      "ACCIDENT" { set Sim(EmDirScenario) "$Sim(EmDir)/ACCIDENT" }
      "VOLCANO"  { set Sim(EmDirScenario) "$Sim(EmDir)/VOLCANO" }
      "VIRUS"    { set Sim(EmDirScenario) "$Sim(EmDir)/VIRUS" }
   }

   if { ![file isdirectory $Sim(EmDirScenario)] } {
      Dialog::Error .modelnew $Error(ScenarioDirectory)
      return 0
   }

   #----- Add names of the emission scenarios to the list of scenarios.
   foreach path [glob -nocomplain $Sim(EmDirScenario)/*.txt] {
      lappend Sim(EmList) [file tail [file rootname $path]]
   }
   return [expr [llength $Sim(EmList)] ? 1 : 0]
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::ScenarioClear>
# Creation   : Juillet 2010 - E. Legault-Ouellet - CMC/CMOE
#
# But        : Remet le scenario d'emission aux valeurs initiales.
#
# Parametres :
#
# Retour     :
#
# Remarques  :
#     - En reinitialisant les compteurs a 0, l'usager doit a nouveau passer
#       par la fonction "EmissionIntervalAdd" qui fait reellement la remise
#       aux valeurs par defaut.
#
#----------------------------------------------------------------------------

proc MLDPn::ScenarioClear { } {
   variable Sim

   if { $Sim(IsRestart) } {
      return
   }

   #----- Reset counters
   set Sim(EmNbIntervals)  $Sim(EmInterStart)

   if { $Sim(SrcType) == "ACCIDENT" } {
      set Sim(EmNbIso)     0
      set Sim(EmIsoInfo)   {}
   }

   #----- Reset global parameters
   set Sim(EmSizeDist)     $Sim(EmSizeDistDefault)
   set Sim(EmDensity)      $Sim(EmDensityDefault)
   set Sim(EmVerticalDist) $Sim(EmVerticalDistDefault)
   set Sim(EmIsAutoNP)     $Sim(EmIsAutoNPDefault)
   set Sim(EmGlobalNP)     $Sim(EmNumberParticles)

   #----- Set flag to indicate that scenario has been changed
   set Sim(ScenarioHasChanged) 1

   MLDPn::GraphUpdate
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::ScenarioDelete>
# Creation   : 22 March 2004 - A. Malo - CMC/CMOE
#
# But        : Delete an emission scenario
#
# Parametres :
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDPn::ScenarioDelete { } {
   global GDefs
   variable Lbl
   variable Sim
   variable Warning

   #----- Do not erase default emission scenario.
   if { $Sim(EmScenario) == "default" } {
      Dialog::Error .modelnew $Warning(DeleteDefault)
      return
   }

   #----- Ask user if deleting emission scenario.
   if { ![Dialog::Default .modelnew 400 WARNING $Warning(DeleteScenario) "$Sim(EmScenario)." 0 $Lbl(No) $Lbl(Yes)] } {
      return
   }

   #----- Delete scenario file.
   file delete -force $Sim(EmDirScenario)/$Sim(EmScenario).txt

   #----- Delete scenario name from the combo box.
   ComboBox::Del $MLDPn::Sim(ScenarioFrame).name.ent $Sim(EmScenario)

   set idx [lsearch -exact $Sim(EmList) $Sim(EmScenario)]
   set Sim(EmList) [lreplace $Sim(EmList) $idx $idx]

   #----- Set current scenario name to the first one in the list.
   set Sim(EmScenario) [lindex $Sim(EmList) 0]

   #----- Since scenario has been deleted, changes that were made to it are now irrelevant
   set Sim(ScenarioHasChanged) 0

   #----- Select emission scenario.
   MLDPn::ScenarioSelect
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::ScenarioSelect>
# Creation   : Juillet 2010 - E. Legault-Ouellet - CMC/CMOE
#
# But        : Select a release scenario.
#
# Parametres :
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDPn::ScenarioSelect { } {
   variable Error
   variable Sim
   variable Tmp

   #----- Does directory exists?
   if { ![file isdirectory $Sim(EmDirScenario)] } {
      Dialog::Error .modelnew $Error(ScenarioDirectory)
      return 0
   }

   #----- Does scenario has a file?
   if { ![file isfile $Sim(EmDirScenario)/$Sim(EmScenario).txt] } {
      Dialog::Error .modelnew $Error(ScenarioNotAvailable)
      return 0
   }

   set f [open $Sim(EmDirScenario)/$Sim(EmScenario).txt r]

   #----- Read global parameters

   if { $Sim(SrcType) == "VOLCANO" } {
      gets $f Sim(EmSizeDist)
      gets $f Sim(EmDensity)
   }
   gets $f Sim(EmVerticalDist)
   gets $f Sim(EmIsAutoNP)
   if { $Sim(EmIsAutoNP) } {
      gets $f Sim(EmGlobalNP)
   } else {
      set Sim(EmGlobalNP) $Sim(EmNumberParticles)
   }

   #----- Read Isotopes

   set Sim(EmIsoInfo) ""
   gets $f Sim(EmNbIso)

   for {set i 0} {$i<$Sim(EmNbIso)} {incr i} {
      gets $f line
      lappend Sim(EmIsoInfo) $line
      set Sim(EmIso$i)   [lindex $line 0]
   }

   #----- Read intervals' params

   gets $f Sim(EmNbIntervals)

   for {set i 0} {$i<$Sim(EmNbIntervals)} {incr i} {
      gets $f Sim(EmInter.$i)
      gets $f Sim(EmIsEm.$i)

      if { $Sim(EmIsEm.$i) } {
         if { !$Sim(EmIsAutoNP) } {
            gets $f Sim(EmNumberParticles.$i)
         } else {
            set Sim(EmNumberParticles.$i) $Sim(EmNumberParticles)
         }
         gets $f Sim(EmHeight.$i)
         gets $f Sim(EmRadius.$i)

         switch $Sim(SrcType) {
            "VOLCANO" {
               gets $f Sim(EmMassMode.$i)
               if { $Sim(EmMassMode.$i) } {
                  gets $f Sim(EmMass.$i)
               } else {
                  #----- Compute mass using Sparks et al. formula
                  set Tmp(EmDensity)      $Sim(EmDensity)
                  set Tmp(EmInter.$i)     $Sim(EmInter.$i)
                  set Tmp(EmIsEm.$i)      $Sim(EmIsEm.$i)
                  set Tmp(EmHeight.$i)    $Sim(EmHeight.$i)
                  set Tmp(EmMassMode.$i)  $Sim(EmMassMode.$i)
                  if { [MLDPn::ComputeMass $i] } {
                     set Sim(EmMass.$i)   $Tmp(EmMass.$i)
                  }
               }
            }
            "VIRUS" {
               gets $f Sim(EmRate.$i)
            }
            "ACCIDENT" {
               for {set j 0} {$j<$Sim(EmNbIso)} {incr j} {
                  gets $f Sim(EmRate.$i.$j)
               }
            }
         }
      } else {
         set Sim(EmNumberParticles.$i) $Sim(EmNumberParticles)
         set Sim(EmHeight.$i)          $Sim(EmHeight)
         set Sim(EmRadius.$i)          $Sim(EmRadius)
         switch $Sim(SrcType) {
            "VOLCANO" {
               set Sim(EmMassMode.$i)  0
               set Sim(EmMass.$i)      0
            }
            "VIRUS" {
               set Sim(EmRate.$i)      0.0
            }
            "ACCIDENT" {
               for {set j 0} {$j<$Sim(EmNbIso)} {incr j} {
                  set Sim(EmRate.$i.$j) 0.0
               }
            }
         }
      }
   }
   close $f

   set Sim(ScenarioHasChanged) 0

   MLDPn::GraphUpdate
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::ScenarioWrite>
# Creation   : Juillet 2010 - E. Legault-Ouellet - CMC/CMOE
#
# But        : Write emission scenario to ascii file.
#
# Parametres :
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDPn::ScenarioWrite { } {
   variable Sim

   set f [open $Sim(EmDirScenario)/$Sim(EmScenario).txt w]

   #----- Write global parameters

   if { $Sim(SrcType) == "VOLCANO" } {
      puts $f $Sim(EmSizeDist)
      puts $f $Sim(EmDensity)
   }
   puts $f $Sim(EmVerticalDist)
   puts $f $Sim(EmIsAutoNP)
   if { $Sim(EmIsAutoNP) } {
      puts $f $Sim(EmGlobalNP)
   }

   #----- Write Isotopes

   puts $f $Sim(EmNbIso)

   foreach line $Sim(EmIsoInfo) {
      puts $f $line
   }

   #----- Write intervals' params

   puts $f $Sim(EmNbIntervals)

   for {set i 0} {$i<$Sim(EmNbIntervals)} {incr i} {
      puts $f $Sim(EmInter.$i)
      puts $f $Sim(EmIsEm.$i)

      if { $Sim(EmIsEm.$i) } {
         if { !$Sim(EmIsAutoNP) } {
            puts $f $Sim(EmNumberParticles.$i)
         }
         puts $f $Sim(EmHeight.$i)
         puts $f $Sim(EmRadius.$i)

         switch $Sim(SrcType) {
            "VOLCANO" {
               puts $f $Sim(EmMassMode.$i)
               if { $Sim(EmMassMode.$i) } {
                  puts $f $Sim(EmMass.$i)
               }
            }
            "VIRUS" {
               puts $f $Sim(EmRate.$i)
            }
            "ACCIDENT" {
               for { set j 0 } { $j<$Sim(EmNbIso) } { incr j } {
                  puts $f $Sim(EmRate.$i.$j)
               }
            }
         }
      }
   }
   close $f

   MLDPn::ScenarioList
   ComboBox::Add $MLDPn::Sim(ScenarioFrame).name.ent $Sim(EmScenario)
   set Sim(ScenarioHasChanged) 0
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::File>
# Creation   : Octobre 1999 - J.P. Gauthier - CMC/CMOE
#
# But        : Creer le nom des fichier resultats.
#
# Parametres :
#    <Info>  : Ligne d'info
#    <Path>  : Path de l'experience
#    <Type>  : Type de fichier resultats
#    <Back>  : Recuperation des fichiers des simulations precedente
#
# Retour     :
#   <Files>  : Liste des path complets des fichiers resultats
#              dans l'ordre croissant
#
# Remarques  :
#   - La procedure boucle sur toutes les simulations precedentes
#     en remontant l'arbre.
#
#----------------------------------------------------------------------------

proc MLDPn::File { Info Path Type Back } {
   variable Sim
   variable Tmp

   set files      ""
   set prevsecs   -1
   set hasNext    1

   while { $hasNext } {
      Info::Decode ::MLDPn::Tmp $Info

      set simpath $Path/[Info::Path $Info]
      set simdate "$Tmp(SimYear)$Tmp(SimMonth)$Tmp(SimDay)$Tmp(SimHour)"
      set simsecs [clock scan $simdate -format "%Y%m%d%H" -timezone :UTC]
      set file    ${simdate}_000

      set pos        [glob $simpath/results/${file}_???]  ;#----- Particle positions result output file.
      set metfields  $simpath/results/${file}_000m        ;#----- Meteorological fields for RSMC response.
      set metfiles   [glob -nocomplain $simpath/meteo/*]  ;#----- Meteorological files required for launching model.

      switch $Type {
         "all"     { set files [concat $files $pos $metfields $metfiles] }
         "result"  { set files [concat $files $pos] }
         "meteo"   { set files [concat $files $metfiles] }
         "metf"    { lappend files $metfields }
      }

      if { $Back && $Tmp(NoPrev)!=-1 } {
         set Info [lindex [Info::Find $Path/$Tmp(Model).pool $Tmp(Model) NoSim $Tmp(NoPrev)] 0]
      } else {
         set hasNext 0
      }
   }

   return $files
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::Move>
# Creation   : Octobre 2000 - J.P. Gauthier - CMC/CMOE
#
# But        : Effectuer la fonction de deplacement de la selection
#              sur la projection.
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <VP>     : Identificateur du Viewport
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDPn::Move { Frame VP } {
   variable Sim
   variable Data

   set Sim(GridLat) $Viewport::Map(LatCursor)
   set Sim(GridLon) $Viewport::Map(LonCursor)

   Model::ParamsGridDefine MLDP
}

proc MLDPn::MoveDone { Canvas VP } { }
proc MLDPn::MoveInit { Canvas VP } { }
proc MLDPn::DrawDone { Canvas VP } { }
proc MLDPn::Draw     { Canvas VP } { }
proc MLDPn::DrawInit { Canvas VP } { }

#----------------------------------------------------------------------------
# Nom        : <MLDPn::Result>
# Creation   : Juillet 1998 - J.P. Gauthier - CMC/CMOE
#
# But        : Recuperation des resultats et affichage par SPI.
#
# Parametres :
#    <Type>  : Type de fichier (standard ou post)
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDPn::Result { Type } {
   variable Sim
   variable Tmp

   #----- Recuperer les noms de fichiers resultats avec retour sur les precedentes
   set files [File $Exp::Data(SelectSim) [Exp::Path] $Type True]

   Info::Decode ::MLDPn::Tmp $Exp::Data(SelectSim)
   SPI::FileOpen NEW FieldBox "(MLDP) $Tmp(NoExp) $Tmp(Name) ($Type)" "" $files
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::SetAccidentDate>
# Creation   : 27 August 2007 - J.P. Gauthier - CMC/CMOE
#
# But        : Set accident release date (year, month and day).
#
# Parametres :
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDPn::SetAccidentDate { } {
   variable Sim

   set Sim(AccYear)  [clock format $Sim(AccSeconds) -format "%Y" -gmt true] ; #----- Year of accident date.
   set Sim(AccMonth) [clock format $Sim(AccSeconds) -format "%m" -gmt true] ; #----- Month of accident date.
   set Sim(AccDay)   [clock format $Sim(AccSeconds) -format "%d" -gmt true] ; #----- Day of accident date.
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::GetDistributionNP>
# Creation   : Aout 2010 - E. Legault-Ouellet - CMC/CMOE
#
# But        : Retourne la liste de la repartition des particules pour les
#              differtentes intervalles tel que le ferait le mecanisme de
#              distribution des particules du modele.
#
# Parametres :
#
# Retour     :
#     <nplst>: La liste du nombre de particules par intervalle.
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDPn::GetDistributionNP { } {
   variable Sim

   set nplst   {}
   set masslst {}

   if { !$Sim(EmIsAutoNP) } {
      for {set i $Sim(EmInterStart)} {$i < $Sim(EmNbIntervals)} {incr i} {
         lappend nplst $Sim(EmNumberParticles.$i)
      }
      return $nplst
   }

   #----- Calculate intervals and total released mass
   set totmass 0
   for {set i $Sim(EmInterStart)} {$i < $Sim(EmNbIntervals)} {incr i} {
      set mass 0
      if { $Sim(EmIsEm.$i) } {
         switch $Sim(SrcType) {
            "VOLCANO" {
               set mass $Sim(EmMass.$i)
            }
            "VIRUS" {
               set mass [expr $Sim(EmRate.$i) * $Sim(EmInter.$i)]
            }
            "ACCIDENT" {
               for { set j 0 } { $j < $Sim(EmNbIso) } { incr j } {
                  set mass [expr $mass + $Sim(EmRate.$i.$j)*$Sim(EmInter.$i)]
               }
            }
         }
      }
      lappend masslst $mass
      set totmass [expr $totmass + $mass]
   }

   #----- Calculate particles distribution
   set totnp 0
   set idx -1
   set i $Sim(EmInterStart)
   set massp [expr $totmass/double($Sim(EmGlobalNP))] ;#----- Mass of one particle
   foreach mass $masslst {
      if { $Sim(EmIsEm.$i) } {
         lappend nplst [expr int($mass/$massp)]
         set totnp [expr $totnp + [lindex $nplst end]]
         set idx [expr $i - $Sim(EmInterStart)]
      } else {
         lappend nplst 0
      }
      incr i
   }

   #----- Since values are rounded, this make sure we don't "forget" particles
   if { $idx != -1 } {
      lset nplst $idx [expr [lindex $nplst $idx] + $Sim(EmGlobalNP) - $totnp]
   }

   return $nplst
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::UpdatePoolLists>
# Created    : Juillet 2010 - E. Legault-Ouellet - CMC/CMOE
#
# But        : Encode emission information for pool simulation.
#
# Parametres :
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDPn::UpdatePoolLists { } {
   variable Sim

   #----- Empty lists
   set Sim(EmInterList)             {}
   set Sim(EmIsEmList)              {}
   set Sim(EmNumberParticlesList)   [MLDPn::GetDistributionNP]
   set Sim(EmHeightList)            {}
   set Sim(EmRadiusList)            {}

   set Sim(EmMassModeList)          {}
   set Sim(EmMassList)              {}
   set Sim(EmRateList)              {}

   if { !$Sim(EmIsAutoNP) } {
      set $Sim(EmGlobalNP) 0
   }

   #----- Fill lists with current emission scenario data
   for { set i $Sim(EmInterStart) } { $i < $Sim(EmNbIntervals) } { incr i } {
      lappend Sim(EmInterList)            $Sim(EmInter.$i)
      lappend Sim(EmIsEmList)             $Sim(EmIsEm.$i)
      lappend Sim(EmHeightList)           $Sim(EmHeight.$i)
      lappend Sim(EmRadiusList)           $Sim(EmRadius.$i)

      switch $Sim(SrcType) {
         "VOLCANO" {
            lappend Sim(EmMassModeList)   $Sim(EmMassMode.$i)
            lappend Sim(EmMassList)       $Sim(EmMass.$i)
         }
         "ACCIDENT" {
            for { set j 0 } {$j < $Sim(EmNbIso) } { incr j } {
               lappend Sim(EmRateList)    $Sim(EmRate.$i.$j)
            }
         }
         "VIRUS" {
            lappend Sim(EmRateList)       $Sim(EmRate.$i)
         }
      }
   }

   #----- Build list of isotopes.
   if { $Sim(SrcType) == "ACCIDENT" } {
      set Sim(EmIsoSymbol) {}
      foreach iso $Sim(EmIsoInfo) {
         lappend Sim(EmIsoSymbol) [lindex $iso 0]
      }
   }
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::InitNew>
# Creation   : Octobre 1999 - J.P. Gauthier - CMC/CMOE
#
# But        : Initialise un tableau de defintions de simulation pour une$Sim(ESCanvas)
#              nouvelle simulation.
#
# Parametres :
#   <Type>   : Type de source
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDPn::InitNew { Type } {
   global   GDefs
   variable Sim
   variable Tmp

   set Sim(GridSrc)              [lindex $Sim(Name) 0]               ;#----- Name of first source.
   set Sim(Mode)                 prog                                ;#----- Type of meteorological data.
   set Sim(Backward)             False                               ;#----- Inverse mode.
   set Sim(DiffKernel)           ORDER0                              ;#----- Diffusion kernel.
   set Sim(Duration)             72                                  ;#----- Simulation duration [hr].
   set Sim(OutputTimeStepMin)    60                                  ;#----- Output time step [min].
   set Sim(ModelTimeStepMin)     10                                  ;#----- Internal model time step [min].
   set Sim(Scale)                MESO                                ;#----- Grid resolution string.
   set Sim(Meteo)                glb                                 ;#----- Meteorological model.
   set Sim(Delta)                1                                   ;#----- Time interval for meteorological data files [hr].
   set Sim(EmNumberParticles)    100000                              ;#----- Number of particles.
   set Sim(ListScale)           { "HEMI    (50 km, 687x687)" "HEMI-1  (50 km, 334x334)" "HEMI-2  (50 km, 400x400)" "HEMI-3  (50 km, 477x477)" "SHEMI-1 (33 km, 505x505)" "SHEMI-2 (33 km, 606x606)" "SHEMI-3 (33 km, 722x722)" "LMESO   (33 km, 400x400)" "MESO    (33 km, 229x229)" "FINE    (15 km, 503x503)" "SFINE   (15 km, 251x251)" "VFINE   (10 km, 229x229)" "EFINE   (5 km,  457x457)" "UFINE   (2 km,  300x300)"} ; #----- List of grid resolutions [km].
   set Sim(ListMeteoModel)      { glb reg glb100 reg24 }
   set Sim(RestartDates)        { }

   #----- Set source type according to experiment data type.
   if { $Type==0 || $Type==3 } {
      #----- Volcano (0) or fire (3) source.
      set Sim(SrcType)           VOLCANO
      set Sim(OutVar)            { AG HCL CVNF CVI CVBL FM DD WD DI DWI TTCV TCAV WI DW IT MF }
      set Sim(EmHeight)          10000.0
      set Sim(EmRadius)          1000.0
      set Sim(EmIsoSymbol)       TRACER
      set Sim(EmNbIso)           1
      set Sim(EmIsoInfo)         { "VOLCAN 1.00e+38 1.00e-03 3.00e-05" }
   } elseif { $Type== 4 } {
      #----- Virus (4) source.
      set Sim(SrcType)           VIRUS
      set Sim(OutVar)            { AG HCL CVNF CVI CVBL FM DD WD DI DWI TTCV TCAV WI DW IT MF TA TDA TTA TOU }
      set Sim(Duration)          48
      set Sim(Meteo)             reg
      set Sim(VirusType)         [lindex $Sim(ListVirusType) 0]
      set Sim(VirusSymbol)       [lindex $Sim(ListVirusSymbol) 0]
      set Sim(EmHeight)          100.0
      set Sim(EmRadius)          100.0
      set Sim(EmIsoSymbol)       [lindex [lindex $Sim(ListVirusName) $GDefs(Lang)] 0]
      set Sim(EmNbIso)           1
      set Sim(VirusType)         [lindex $Sim(ListVirusType) 0]
      set Sim(VirusSymbol)       [lindex $Sim(ListVirusSymbol) 0]
      set Sim(Scale)             EFINE
      set Sim(EmIsoInfo)         "$Sim(VirusSymbol) 1.00e+38 1.00e-03 3.00e-05"
   } else {
      #----- Nuclear accident (1), CTBT (2), pollutant spill (5), or other (6) sources.
      set Sim(SrcType)           ACCIDENT
      set Sim(OutVar)            { AG HCL CVNF CVI CVBL FM DD WD DI DWI TTCV TCAV WI DW IT MF TA TDA TTA TOU }
      set Sim(OutputTimeStepMin) 180
      set Sim(Duration)          12                                  ; #----- Simulation duration [hr].
      set Sim(OutputTimeStepMin) 30                                  ; #----- Output time step [min].
      set Sim(ModelTimeStepMin)  5                                   ; #----- Internal model time step [min].
      set Sim(Scale)             VFINE                               ; #----- Grid resolution string.
      set Sim(Meteo)             reg                                 ; #----- Meteorological model.
      set Sim(EmHeight)          500.0
      set Sim(EmRadius)          100.0
      set Sim(EmNbIso)           0
      set Sim(EmIsoInfo)         {}
   }

   switch $Sim(DiffKernel) {
      ORDER0 {
         set Sim(VarMesoscale)         1.00                                ;#----- Horizontal wind velocity variance for mesoscale fluctuations [m2/s2].
         set Sim(Timescale)            10800                               ;#----- Lagrangian time scale [s].
         set Sim(ReflectionLevel)      0.9999                              ;#----- Reflection level [hyb|eta|sig].
         set Sim(DtOverTl)             1.0
         set Sim(DtMin)                1.0
         set Sim(ListReflectionLevel) { 0.9990 0.9995 0.9996 0.9997 0.9998 0.9999 1.0000 }
         set Sim(ListOutCV)           { { 1 200 400 600 2000 4000 }  \
                                        { 1 500 1000 1500 2000 3000 4000 5000 } \
                                        { 1 500 1000 1500 2000 2500 3000 3500 4000 4500 5000 } \
                                        { 1 500 1000 } \
                                        { 1 1000 } \
                                        { 1 1000 2000 5000 10000 } \
                                        { 1 1000 2000 5000 10000 15000 } \
                                        { 1 1000 2000 3000 4000 5000 6000 7000 8000 9000 10000 } \
                                        { 1 2000 4000 6000 8000 10000 } \
                                        { 1 100 200 300 400 500 600 700 800 900 1000 } \
                                        { 1 100 1000 } \
                                        { 1 100 500 1000 } \
                                        { 1 50 100 } \
                                        { 1 50 100 500 1000 } \
                                        { 1 50 100 1000 } \
                                        { 1 10 50 100 } \
                                        { 1 10 50 100 1000 } \
                                        { 1 10 100 1000 } }
      }
      ORDER1 -
      VARIABLE {
         set Sim(VarMesoscale)        0.10                                ; #----- Horizontal wind velocity variance for mesoscale fluctuations [m2/s2].
         set Sim(Timescale)           2700                                ; #----- Lagrangian time scale [s].
         set Sim(ReflectionLevel)     0.9990                              ; #----- Reflection level [hyb|eta|sig].
         set Sim(DtOverTl)            0.1
         set Sim(DtMin)               0.1
         set Sim(ListReflectionLevel) { 0.9990 0.9995 0.9996 0.9997 0.9998 0.9999 }
         set Sim(ListOutCV)           { { 0 200 400 600 800 1000 } \
                                        { 0 500 1000 1500 2000 3000 4000 5000 } \
                                        { 0 500 1000 1500 2000 2500 3000 3500 4000 4500 5000 } \
                                        { 0 500 1000 } \
                                        { 0 1000 } \
                                        { 0 1000 2000 5000 10000 } \
                                        { 0 1000 2000 5000 10000 15000 } \
                                        { 0 1000 2000 3000 4000 5000 6000 7000 8000 9000 10000 } \
                                        { 0 100 200 300 400 500 600 700 800 900 1000 } \
                                        { 0 100 1000 } \
                                        { 0 100 500 1000 } \
                                        { 0 50 100 } \
                                        { 0 50 100 500 1000 } \
                                        { 0 50 100 1000 } \
                                        { 0 10 50 100 } \
                                        { 0 10 50 100 1000 } \
                                        { 0 10 100 1000 } }
      }
   }

   set Sim(IsResFileSizeChecked) 0                                   ; #----- Flag indicating if results file size has been checked (1) or not (0).
   set Sim(IsMetFileSizeChecked) 0                                   ; #----- Flag indicating if met data file size has been checked (1) or not (0).
   set Sim(Event)                [lindex $Sim(ListEvent) 0]          ; #----- Type of event.
   set Sim(OutCV)                [lindex $Sim(ListOutCV) 0]          ; #----- CV Vertical levels [m].
   set Sim(OutAV)                { 0 20000 35000 60000 85000 110000 135000 }  ; #----- AVVertical levels [m].
   set Sim(VarMesoscale)         1.00                                ; #----- Horizontal wind velocity variance for mesoscale fluctuations [m2/s2].
   set Sim(PrevReflectionLevel)  $Sim(ReflectionLevel)               ; #----- Previous reflection level [hyb|eta|sig].

   set Sim(EmScenario)           "default"                           ; #----- Scenario name.
   set Sim(EmMass)               0.0                                 ; #----- Total mass released.
   set Sim(EmList)               {}                                  ; #----- List of emission scenarios.
   set Sim(EmNbIntervals)        0                                   ; #----- Number of emission intervals.
   set Sim(EmInterStart)         0                                   ; #----- Starting index for emission intervals.
   set Sim(EmEffectiveDuration)  0.0                                 ; #----- Effective emission duration, only release periods [s].
   set Sim(EmTotalDuration)      0.0                                 ; #----- Total emission duration, including release and lull periods [s].
   set Sim(EmIsoSymbol)          ""                                  ; #----- List of isotopes.
   set Sim(EmIsoQuantity)        ""                                  ; #----- Total release quantity for each isotope.
   set Sim(EmGlobalNP)           $Sim(EmNumberParticles)             ; #----- Number of particules to distribute amongst emission intervals.
   set Sim(EmRateMode)           0                                   ; #----- Mode of the release rate. 0 is "unit/h", 1 is "unit"
   set Sim(EmInterMode)          0                                   ; #----- Mode of the interval duration. 0 is "(h)", 1 is "(s)"
   set Sim(RestartTrialDate)     0
   set Sim(BubbleId)             -1
   set Sim(BubbleShowingId)      -1

   set NA [lindex $Sim(NotAvailable) $GDefs(Lang)]

   #----- Initialize unused variables to "not available" for pool information.
   if { $Sim(SrcType) == "ACCIDENT" || $Sim(SrcType) == "VIRUS" } {
      set Sim(EmDensityDefault)  2.500e+12
      set Sim(EmMass)     $NA
      set Sim(EmSizeDistDefault) $NA
   }

   if { $Sim(SrcType) == "VOLCANO" } {
      set Sim(EmDensityDefault)       2.500e+12 ; #----- Particle density [microgram/m3].
      set Sim(EmSizeDistDefault)      [lindex [lindex $Sim(ListEmSizeDist) $GDefs(Lang)] end] ; #----- Particle size distribution.
      set Sim(EmIsoQuantity)   $NA
   }

   set Sim(EmVerticalDistDefault) [lindex [lindex $Sim(ListEmVerticalDist) $GDefs(Lang)] 0] ; #----- Plume vertical distribution.

   set Sim(EmSizeDist)     $Sim(EmSizeDistDefault)
   set Sim(EmDensity)      $Sim(EmDensityDefault)
   set Sim(EmVerticalDist) $Sim(EmVerticalDistDefault)
   set Sim(EmIsAutoNP)     $Sim(EmIsAutoNPDefault)

   set Tmp(Duration)          $Sim(Duration)          ; #----- Temporary variable for simulation duration.
   set Tmp(OutputTimeStepMin) $Sim(OutputTimeStepMin) ; #----- Temporary variable for output time step.
   set Tmp(Delta)             $Sim(Delta)             ; #----- Temporary variable for time interval between met data files.

   #----- Restart stuff
   set Sim(IsRestart)   0
   set Sim(RestartFile) ""

   MLDPn::UpdateEmissionStartingTime
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::InitCont>
# Creation   : Juillet 2010 - E. Legault-Ouellet - CMC/CMOE
#
# But        : Initialise les param�tres n�cessaire au lancement de
#              l'interface de continuation d'une simulation.
#
# Parametres :
#     <Type> : Le type de la source.
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDPn::InitCont { Type } {
   global GDefs
   variable Sim
   variable Error

   #----- Initialize variables needed to the graphical interface

   set Sim(ListScale)         { "HEMI    (50 km, 687x687)" "HEMI-1  (50 km, 334x334)" "HEMI-2  (50 km, 400x400)" "HEMI-3  (50 km, 477x477)" "SHEMI-1 (33 km, 505x505)" "SHEMI-2 (33 km, 606x606)" "SHEMI-3 (33 km, 722x722)" "LMESO   (33 km, 400x400)" "MESO    (33 km, 229x229)" "FINE    (15 km, 503x503)" "SFINE   (15 km, 251x251)" "VFINE   (10 km, 229x229)" "EFINE   (5 km,  457x457)" "UFINE   (2 km,  300x300)"} ; #----- List of grid resolutions [km].
   set Sim(EmNumberParticles) 100000

   set Sim(ListReflectionLevel)     $Sim(ReflectionLevel)
   set Sim(PrevReflectionLevel)     $Sim(ReflectionLevel)
   set Sim(ListOutCV)               $Sim(OutCV)
   set Sim(ListMeteoModel)          $Sim(Meteo)
   set Sim(IsResFileSizeChecked)    0
   set Sim(IsMetFileSizeChecked)    0
   set Sim(EmVerticalDistDefault)   [lindex [lindex $Sim(ListEmVerticalDist) $GDefs(Lang)] 0]
   set Sim(BubbleId)                -1
   set Sim(BubbleShowingId)         -1
   set Sim(RestartTrialDate)        0

   #----- Initialize default values

   switch $Sim(SrcType) {
      "VOLCANO" {
         set Sim(EmHeight)          10000.0
         set Sim(EmRadius)          1000.0
         set Sim(EmIsoInfo)         { "VOLCAN 1.00e+38 1.00e-03 3.00e-05" }
         set Sim(EmSizeDistDefault) [lindex [lindex $Sim(ListEmSizeDist) $GDefs(Lang)] 3]
         set Sim(EmDensityDefault)  2.500e+12
      }
      "VIRUS" {
         set Sim(EmHeight)          100.0
         set Sim(EmRadius)          100.0
         set Sim(EmIsoInfo)         {}
         lappend Sim(EmIsoInfo)     "[lindex $Sim(ListVirusSymbol) [lsearch -exact [lindex $Sim(ListVirusName) $GDefs(Lang)] $Sim(EmIsoSymbol)]] 1.00e+38 1.00e-03 3.00e-05"
      }
      "ACCIDENT" {
         set Sim(EmHeight)          500.0
         set Sim(EmRadius)          100.0

         set Sim(EmIsoInfo)         {}
         set i 0
         foreach iso $Sim(EmIsoSymbol) {
            set Sim(EmIso$i) $iso
            set desc [IsoBox::Get $iso]
            lappend Sim(EmIsoInfo) "[lindex $desc 0] [lindex $desc 2] [lindex $desc 5] [lindex $desc 4]"
            incr i
         }
      }
   }
   set Sim(ScenarioHasChanged) 0

   set Sim(AccSeconds) [clock scan "$Sim(AccYear)$Sim(AccMonth)$Sim(AccDay)$Sim(AccHour)$Sim(AccMin)" -format "%Y%m%d%H%M" -timezone :UTC]

   #----- Restart stuff

   set Sim(IsRestart)      1
   set Sim(NoPrev)         $Sim(NoSim)
   set Sim(RestartFile)    $Sim(SimYear)$Sim(SimMonth)$Sim(SimDay)$Sim(SimHour)_000.rst
   set Sim(EmInterStart)   $Sim(EmNbIntervals)

   #----- Get restart valid date from restart file

   set pathbase $Exp::Param(Path)/$Sim(NoExp)_$Sim(NameExp)
   set pathprev $Sim(Model).$Sim(NoSim).$Sim(AccYear)$Sim(AccMonth)$Sim(AccDay).$Sim(AccHour)$Sim(AccMin)
   if { [fstdfile open RSTFILE read $pathbase/$pathprev/results/$Sim(RestartFile)] == "" } {
      fstdfile close RSTFILE
      Dialog::Error . $Error(NoRestartFile)
      return
   }

   set Sim(RestartDates) [fstdfile info RSTFILE DATEV]

   for { set i 0 } { $i < [llength $Sim(RestartDates)] } { incr i } {
      lset Sim(RestartDates) $i [expr [lindex $Sim(RestartDates) $i]-$Sim(AccSeconds)]
   }
   set Sim(RestartDate)  [lindex $Sim(RestartDates) end]

   fstdfile close RSTFILE
   if { [llength $Sim(RestartDates)] != 1 } {
      Dialog::Error . $Error(NoRestartFile)
      return
   }
#   set Sim(EmScenario)     "Rst [clock format $Sim(RestartSeconds) -format "%Y%m%d %H%M UTC" -timezone :UTC]"

#   set Sim(EmNbIntervals) 0
#   set Sim(RestartPrevDates) [MLDPn::ReconstructPrevSim $pathbase/$Sim(Model).pool -1 $Sim(RestartSeconds) $Sim(NoPrev)]
#   lappend Sim(RestartPrevDates) [expr ($Sim(RestartSeconds)-$Sim(AccSeconds))/3600.0]
#   set Sim(EmInterStart) $Sim(EmNbIntervals)

   #----- Reconstruct from restart date to end date.
#   MLDPn::ReconstructPrevSim $pathbase/$Sim(Model).pool $Sim(RestartSeconds) -1 $Sim(NoPrev)
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::IsoDelete>
# Creation   : Novembre 1999 - J.P. Gauthier - CMC/CMOE
# Modified   : Juillet 2010 - E. Legault-Ouellet - CMC/CMOE
#
# But        : Retire une espece de la liste selectionne.
#
# Parametres :
#    <Idx>   : Index dans la liste a supprimer
#    <Id>    : Id de l'interval courrant.
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDPn::IsoDelete { Idx Id } {
   variable Tmp
   variable Sim

   if { $Sim(IsRestart) } {
      return
   }

   #----- Si l'index est valide

   if { $Idx < [llength $Tmp(EmIsoInfo)] } {

      set Tmp(EmIsoInfo) [lreplace $Tmp(EmIsoInfo) $Idx $Idx]

      #----- Reinitialiser les valeurs aux valeurs du precendent
      #      pour faire un scrolldown des valeurs

      for { set i $Sim(EmInterStart) } { $i < $Tmp(EmNbIntervals) } { incr i } {
         set j0 $Idx
         for { set j [expr $Idx+1] } { $j < $Tmp(EmNbIso) } { incr j } {
            set Tmp(EmRate.$i.$j0)  $Tmp(EmRate.$i.$j)
            set Tmp(EmIso$j0)       $Tmp(EmIso$j)
            incr j0
         }
      }

      #----- Ajuste le mode du taux d'emission
      set j0 $Idx
      for {set j [expr $Idx+1]} {$j < $Tmp(EmNbIso)} {incr j} {
         set Tmp(EmRateMode.$Id.$j0) $Tmp(EmRateMode.$Id.$j)
         incr j0
      }

      incr Tmp(EmNbIso) -1
   }
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::IsoUpdate>
# Creation   : Juillet 2010 - E. Legault-Ouellet - CMC/CMOE
#
# But        : Change l'isotope a l'index indique par le nouvel isotope
#              specifie.
#
# Parametres :
#    <Idx>   : Index de l'isotope a remplacer.
#    <Line>  : Ligne decrivant le nouvel isotope.
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDPn::IsoUpdate { Idx Line } {
   variable Tmp
   variable Sim

   if { $Sim(IsRestart) || $Idx>$Tmp(EmNbIso) || [llength $Line]!=11 } {
      return
   }

   set symbol      [lindex $Line 0] ; #----- Isotope Symbol.
   set halflife    [lindex $Line 5] ; #----- Half-Life [s].
   set wetscavrate [lindex $Line 7] ; #----- Wet Scavenging Rate [s-1].
   set drydepvel   [lindex $Line 8] ; #----- Dry Deposition Velocity [m/s].

   if { $halflife >= 900 } {
      set Tmp(EmIsoInfo) [lreplace $Tmp(EmIsoInfo) $Idx $Idx "$symbol $halflife $drydepvel $wetscavrate"]
      set Tmp(EmIso$Idx) $symbol
   } else {
      #----- Display warning message if radioactive half-life is less than 15 minutes.
      Dialog::Error .modelnew $Warning(HalfLife) " $symbol."
   }
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::IsoAdd>
# Creation   : Aout 1997 - J.P. Gauthier - CMC/CMOE
# Modified   : Juillet 2010 - E. Legault-Ouellet - CMC/CMOE
#
# But        : Met en format la ligne retourne par le module de selection
#              d'especes.
#
# Parametres :
#   <Line>   : Ligne de definiton d'un isotope
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDPn::IsoAdd { Line } {
   global GDefs
   variable Sim
   variable Tmp
   variable Warning
   variable Lbl

   if { $Sim(IsRestart) } {
      return
   }

   if { [llength $Line] == 11 } {

      set symbol      [lindex $Line 0] ; #----- Isotope Symbol.
      set halflife    [lindex $Line 5] ; #----- Half-Life [s].
      set wetscavrate [lindex $Line 7] ; #----- Wet Scavenging Rate [s-1].
      set drydepvel   [lindex $Line 8] ; #----- Dry Deposition Velocity [m/s].

      if { [llength $Tmp(EmIsoInfo)] < $Sim(EmMaxIso) && [lsearchsub $Tmp(EmIsoInfo) $symbol 0] == -1 } {

         if { $halflife >= 900 } {
            #----- Verify if isotope's radioactive half-life is long enough
            #----- ( >= 15 minutes ) to generate relevant simulation results.
            set Tmp(EmIso$Tmp(EmNbIso)) $symbol
            lappend Tmp(EmIsoInfo) "$symbol $halflife $drydepvel $wetscavrate"

            #----- Set to 0 the release rate of all emission intervals for this isotope
            for {set i $Sim(EmInterStart)} {$i < $Tmp(EmNbIntervals)} {incr i} {
               set Tmp(EmRate.$i.$Tmp(EmNbIso))       0.0
               set Tmp(EmRateMode.$i.$Tmp(EmNbIso))   0
            }

            incr Tmp(EmNbIso)
         } else {
            #----- Display warning message if radioactive half-life is less than 15 minutes.
            Dialog::Error .modelnew $Warning(HalfLife) " $symbol."
         }
      }
   }
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::UpdateListVerticalLevels>
# Creation   : 6 July 2006 - A. Malo - CMC/CMOE
#
# But        : Update first vertical level for entire lists of levels and
#              current list of levels if reflection level was modified.
#
# Parametres :
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDPn::UpdateListVerticalLevels { } {
   variable Sim

   #----- Validate reflection level.
   if { ![MLDPn::ValidateReflectionLevel] } {
      return
   }

   if { $Sim(ReflectionLevel) == $Sim(PrevReflectionLevel) } {
      return ; #----- Exit this procedure!
   }

   set Reflection $Sim(ReflectionLevel) ; #----- Reflection level [hyb|eta|sig].
   set Rair       287.04                ; #----- Gas constant for dry air [J/K/kg].
   set Temp       273.15                ; #----- Temperature [K].
   set grav       9.81                  ; #----- Gravitational acceleration [m/2].
   set firstlevel [expr int(round((1.0 - $Reflection)*$Rair*$Temp/$grav))]

   #----- List of vertical levels.
   set newoutcv {}
   foreach list $Sim(ListOutCV) {
      lappend newoutcv [lreplace $list 0 0 $firstlevel]
   }

   set Sim(ListOutCV)  $newoutcv                              ; #----- Update list of vertical levels.
   Option::Set $Sim(OutCVFrm) $Sim(ListOutCV)
   set Sim(OutCV)      [lreplace $Sim(OutCV) 0 0 $firstlevel] ; #----- Update vertical levels.
   set Sim(PrevReflectionLevel) $Sim(ReflectionLevel)         ; #----- Reset previous reflection level.
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::ComputeMass>
# Creation   : 22 March 2004 - A. Malo - CMC/CMOE
#
# But        : Validate input parameters for the total released mass
#              calculation. Compute total mass according to empirical
#              formula of Sparks et al. (1997).
#
# Parametres :
#     <Id>  : The id of the emission interval.
#
# Retour     : 1 if mass was calulated with success, 0 if not.
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDPn::ComputeMass { Id } {
   variable Sim
   variable Tmp

   #----- Verify if source is a volcano type.
   if { $Sim(SrcType)!="VOLCANO" } {
      return 0
   }

   #------ If there is no emission, no mass will be released
   if { !$Tmp(EmIsEm.$Id) } {
      return 0
   }

   #----- Verify if mass mode is 0 (Empirical formula of Sparks et al. 1997).
   if { $Tmp(EmMassMode.$Id) == 2 } {
      #----- Mass can be edited.
      return 0
   }

   #----- Calculate the total released mass calculation
   #----- according to empirical formula of Sparks et al. (1997).
   if { [MLDPn::ValidateMassInputParams $Id] } {
      if { $Tmp(EmMassMode.$Id) == 0 } {
         #----- Compute mass according to empirical formula of Sparks et al. (1997).
         set Tmp(EmMass.$Id) [format "%.6e" [expr 0.1 * $Tmp(EmDensity) * $Tmp(EmInter.$Id) * 3600 * pow(double($Tmp(EmHeight.$Id)/1.670e+03),double(1.0/0.259))]]
      } elseif { $Tmp(EmMassMode.$Id) == 1 } {
         #----- Compute mass according to empirical formula of Mastin et al. (2009).
         set Tmp(EmMass.$Id) [format "%.6e" [expr 0.1 * $Tmp(EmDensity) * $Tmp(EmInter.$Id) * 3600 * pow(double($Tmp(EmHeight.$Id)/2.000e+03),double(1.0/0.241))]]
      }
      set Tmp(EmMassOld) $Tmp(EmMass.$Id);
      return 1
   }
   return 0
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::EmissionIntervalApply>
# Creation   : Juillet 2010 - E. Legault-Ouellet - CMC/CMOE
#
# But        : Make changes to an emission interval permanent.
#
# Parametres :
#     <Id>  : The id of the emission interval.
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDPn::EmissionIntervalApply { Id } {
   variable Sim
   variable Tmp

   if { $Id < $Sim(EmInterStart) } {
      return
   }

   #----- Will prompt to save changes made to scenario

   set Sim(ScenarioHasChanged) 1

   #----- Update emission interval's count

   set Sim(EmNbIntervals) $Tmp(EmNbIntervals)

   #----- Save global parameters

   set Sim(EmSizeDist)        $Tmp(EmSizeDist)
   set Sim(EmDensity)         $Tmp(EmDensity)
   set Sim(EmVerticalDist)    $Tmp(EmVerticalDist)
   set Sim(EmIsAutoNP)        $Tmp(EmIsAutoNP)
   set Sim(EmGlobalNP)        $Tmp(EmGlobalNP)

   #----- Save emission interval details

   set Sim(EmInter.$Id)             $Tmp(EmInter.$Id)
   set Sim(EmIsEm.$Id)              $Tmp(EmIsEm.$Id)
   set Sim(EmNumberParticles.$Id)   $Tmp(EmNumberParticles.$Id)
   set Sim(EmHeight.$Id)            $Tmp(EmHeight.$Id)
   set Sim(EmRadius.$Id)            $Tmp(EmRadius.$Id)

   switch $Sim(SrcType) {
      "VOLCANO" {
         set Sim(EmMass.$Id)        $Tmp(EmMass.$Id)
         set Sim(EmMassMode.$Id)    $Tmp(EmMassMode.$Id)
      }
      "ACCIDENT" {
         set Sim(EmNbIso)           $Tmp(EmNbIso)
         set Sim(EmIsoInfo)         $Tmp(EmIsoInfo)

         for {set i 0} {$i < $Tmp(EmNbIso)} {incr i} {
            set Sim(EmIso$i)        $Tmp(EmIso$i)

            for {set j $Sim(EmInterStart)} {$j < $Tmp(EmNbIntervals)} {incr j} {
               set Sim(EmRate.$j.$i)  $Tmp(EmRate.$j.$i)
            }
         }
      }
      "VIRUS" {
         set Sim(EmRate.$Id)        $Tmp(EmRate.$Id)
      }
   }

   MLDPn::GraphUpdate
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::EmissionIntervalEdit>
# Creation   : Juillet 2010 - E. Legault-Ouellet - CMC/CMOE
#
# But        : Make a copy of an emission interval's parameters for the
#              editor to work on. This way, changes can be reverted.
#
# Parametres :
#     <Id>  : The id of the emission interval.
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDPn::EmissionIntervalEdit { Id } {
   variable Sim
   variable Tmp

   if { $Id < $Sim(EmInterStart) } {
      return
   }

   set Tmp(EmNbIntervals)     $Sim(EmNbIntervals)

   #----- Save global parameters

   set Tmp(EmSizeDist)        $Sim(EmSizeDist)
   set Tmp(EmDensity)         $Sim(EmDensity)
   set Tmp(EmVerticalDist)    $Sim(EmVerticalDist)
   set Tmp(EmIsAutoNP)        $Sim(EmIsAutoNP)
   set Tmp(EmGlobalNP)        $Sim(EmGlobalNP)

   #----- Save emission interval details

   set Tmp(EmInter.$Id)             $Sim(EmInter.$Id)
   set Tmp(EmInterMode.$Id)         0
   set Tmp(EmIsEm.$Id)              $Sim(EmIsEm.$Id)
   set Tmp(EmNumberParticles.$Id)   $Sim(EmNumberParticles.$Id)
   set Tmp(EmHeight.$Id)            $Sim(EmHeight.$Id)
   set Tmp(EmRadius.$Id)            $Sim(EmRadius.$Id)

   switch $Sim(SrcType) {
      "VOLCANO" {
         set Tmp(EmMass.$Id)        $Sim(EmMass.$Id)
         set Tmp(EmMassMode.$Id)    $Sim(EmMassMode.$Id)
      }
      "ACCIDENT" {
         set Tmp(EmNbIso)           $Sim(EmNbIso)
         set Tmp(EmIsoInfo)         $Sim(EmIsoInfo)

         for {set i 0} {$i < $Sim(EmNbIso)} {incr i} {
            set Tmp(EmIso$i)           $Sim(EmIso$i)
            set Tmp(EmRateMode.$Id.$i) 0

            for {set j $Sim(EmInterStart)} {$j < $Sim(EmNbIntervals)} {incr j} {
               set Tmp(EmRate.$j.$i)   $Sim(EmRate.$j.$i)
            }
         }
      }
      "VIRUS" {
         set Tmp(EmRate.$Id)        $Sim(EmRate.$Id)
         set Tmp(EmRateMode.$Id)    0
      }
   }

   MLDPn::EmissionDetails .modelnew $Id Edit
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::EmissionIntervalDelete>
# Creation   : Juillet 2010 - E. Legault-Ouellet - CMC/CMOE
#
# But        : Delete an emission interval.
#
# Parametres :
#     <Id>  : The id of the emission interval.
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDPn::EmissionIntervalDelete { Id } {
   variable Sim

   if { $Id < $Sim(EmInterStart) } {
      return
   }

   #----- Delete emission interval

   set i0 $Id

   for {set i [expr $Id + 1]} {$i < $Sim(EmNbIntervals)} {incr i} {
      set Sim(EmInter.$i0)             $Sim(EmInter.$i)
      set Sim(EmIsEm.$i0)              $Sim(EmIsEm.$i)
      set Sim(EmNumberParticles.$i0)   $Sim(EmNumberParticles.$i)
      set Sim(EmHeight.$i0)            $Sim(EmHeight.$i)
      set Sim(EmRadius.$i0)            $Sim(EmRadius.$i)

      switch $Sim(SrcType) {
         "VOLCANO" {
            set Sim(EmMass.$i0)        $Sim(EmMass.$i)
            set Sim(EmMassMode.$i0)    $Sim(EmMassMode.$i)
         }
         "ACCIDENT" {
            for {set j 0} {$j < $Sim(EmNbIso)} {incr j} {
               set Sim(EmRate.$i0.$j)  $Sim(EmRate.$i.$j)
            }
         }
         "VIRUS" {
            set Sim(EmRate.$i0)        $Sim(EmRate.$i)
         }
      }

      incr i0
   }

   incr Sim(EmNbIntervals) -1
   set Sim(ScenarioHasChanged) 1
   MLDPn::GraphUpdate
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::EmissionIntervalAdd>
# Creation   : Juillet 2010 - E. Legault-Ouellet - CMC/CMOE
#
# But        : Add a new emission interval.
#
# Parametres :
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDPn::EmissionIntervalAdd { } {
   variable Sim
   variable Tmp

   set id                  $Sim(EmNbIntervals)
   set Tmp(EmNbIntervals)  [expr $Sim(EmNbIntervals) + 1]

   #----- Init global parameters

   set Tmp(EmSizeDist)        $Sim(EmSizeDist)
   set Tmp(EmDensity)         $Sim(EmDensity)
   set Tmp(EmVerticalDist)    $Sim(EmVerticalDist)
   set Tmp(EmIsAutoNP)        $Sim(EmIsAutoNP)
   set Tmp(EmGlobalNP)        $Sim(EmGlobalNP)

   #----- Init emission interval details

   set Tmp(EmInterMode.$id) 0
   if { $Sim(EmNbIntervals) > 0 } {
      set Tmp(EmInter.$id)             $Sim(EmInter.0)
      set Tmp(EmNumberParticles.$id)   $Sim(EmNumberParticles.0)
      set Tmp(EmIsEm.$id)              1
      set Tmp(EmHeight.$id)            $Sim(EmHeight.0)
      set Tmp(EmRadius.$id)            $Sim(EmRadius.0)
   } else {
      set Tmp(EmInter.$id)             1
      set Tmp(EmIsEm.$id)              1
      set Tmp(EmNumberParticles.$id)   $Sim(EmNumberParticles)
      set Tmp(EmHeight.$id)            $Sim(EmHeight)
      set Tmp(EmRadius.$id)            $Sim(EmRadius)
   }

   switch $Sim(SrcType) {
      "VOLCANO" {
         if { $Sim(EmNbIntervals) > 0 } {
            set Tmp(EmMassMode.$id)    $Sim(EmMassMode.0)
            if { $Sim(EmMassMode.0) } {
               set Tmp(EmMass.$id)     $Sim(EmMass.0)
            } else {
               ComputeMass $id
            }
         } else {
            set Tmp(EmMassMode.$id)    0
            ComputeMass $id
         }
      }
      "ACCIDENT" {
         set Tmp(EmNbIso)     $Sim(EmNbIso)
         set Tmp(EmIsoInfo)   $Sim(EmIsoInfo)

         for {set i 0} {$i < $Sim(EmNbIso)} {incr i} {
            set Tmp(EmIso$i)           $Sim(EmIso$i)
            set Tmp(EmRate.$id.$i)     0.0
            set Tmp(EmRateMode.$id.$i) 0

            for {set j $Sim(EmInterStart)} {$j < $Sim(EmNbIntervals)} {incr j} {
               set Tmp(EmRate.$j.$i) $Sim(EmRate.$j.$i)
            }
         }
      }
      "VIRUS" {
         set Tmp(EmRate.$id)     0.0
         set Tmp(EmRateMode.$id) 0
      }
   }

   MLDPn::EmissionDetails .modelnew $id
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::GetIntervalFractionNP>
# Creation   : Aout 2010 - E. Legault-Ouellet - CMC/CMOE
#
# But        : Retourne le nombre de particules qui ont ete emises dans un
#              interval incomplet (arrive lors des restarts).
#
# Parametres :
#  <From>      : L'heure du debut (dans le referentiel de l'intervalle).
#  <To>        : L'heure de fin (dans le referentiel de l'intervalle).
#  <NP>        : Le nombre de aprticules de l'intervalle.
#  <Duration>  : La duree de l'intervalle (en heures).
#  <IMTS>      : La duree du pas de temps interne du model (Internal Model
#                Time Step) en heures.
#
# Retour     : La date du dernier trial au format YYYYMMDDhhmmss.
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDPn::GetIntervalFractionNP { From To NP Duration IMTS } {
   set nbts    [expr int($Duration/$IMTS)]            ;#----- Number of time steps
   set idxfrom [expr $From<0 ? 0 : int($From/$IMTS)]  ;#----- Index from
   set idxto   [expr $To<0 ? $nbts : int($To/$IMTS)]  ;#----- Index to
   set np      [expr int(double($NP)/$nbts)]          ;#----- Number of particles per steps
   set lastnp  [expr $NP - $np*$nbts]                 ;#----- Number of particles not included in steps

   set totnp [expr ($idxto-$idxfrom)*$np]
   if { $idxto == $nbts } {
      incr totnp $lastnp
   }

   return $totnp
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::ReconstructPrevSim>
# Creation   : Aout 2010 - E. Legault-Ouellet - CMC/CMOE
#
# But        : Reconstruit la partie de la simulation precedante qui n'est
#              pas inclue dans le restart et ajoute cette partie au scenario
#              d'emission courant.
#
# Parametres :
#  <File>    : Path complet vers le fichier contenant les lignes de pool.
#  <DateFrom>: Date (en secondes) a partir de laquelle la reconstruction doit
#              se faire. (-1 = la date d'accident)
#  <DateTo>  : Date (en secondes) jusqu'a laquelle la reconstruction doit se
#              se rendre (-1 = fin de la derni�re simulation).
#  <NoPrev>  : Numero de la simulation precedante.
#
# Retour     :
#  <restarts>: Liste des dates de restart, en heures, relatives a la date de
#              l'accident.
#
# Remarques  :
#     - Dans le cas de resultats superposes (frequent avec les restarts), les
#       intervalles seront pris de la simlation la plus recente.
#
#----------------------------------------------------------------------------

proc MLDPn::ReconstructPrevSim { File DateFrom DateTo NoPrev } {
   variable Tmp
   variable Sim

   set rstlst ""

   #----- Get sim pool line

   set line [Info::Find $File $Sim(Model) NoSim $NoPrev]
   if { $line=="" || [llength $line]>1 } { ;#----- this should never happen
      return
   }
   Info::Decode ::MLDPn::Tmp [lindex $line 0]

   #----- Get acc and sim date in hours

   set dateAcc $Tmp(AccYear)$Tmp(AccMonth)$Tmp(AccDay)$Tmp(AccHour)$Tmp(AccMin)
   set dateAcc [expr [clock scan $dateAcc -format "%Y%m%d%H%M" -timezone :UTC] / 3600.0]
   set dateSim $Tmp(SimYear)$Tmp(SimMonth)$Tmp(SimDay)$Tmp(SimHour)
   set dateSim [expr [clock scan $dateSim -format "%Y%m%d%H" -timezone :UTC] / 3600.0]

   set dateStart [expr $dateSim>$dateAcc ? $dateSim : $dateAcc]

   #----- Get reconstruction time interval

   set DateFrom [expr $DateFrom<0 ? $dateAcc : $DateFrom/3600.0]
   set DateTo   [expr $DateTo<0 ? $dateSim+$Tmp(Duration) : $DateTo/3600.0]

   #----- Make sure this sim has data useful to the reconstruction

   if { $dateSim+$Tmp(Duration)<$DateFrom || $dateStart>=$DateTo } {
      return
   }

   #----- If reconstruction starting date is anterior to this sim's starting date
   #----- we add previous sim's reconstruction before this one's reconstruction.

   if { $DateFrom<$dateSim && $Tmp(NoPrev)!=-1 } {
      set rstlst [MLDPn::ReconstructPrevSim $File [expr $DateFrom*3600] [expr $dateStart*3600] $Tmp(NoPrev)]
      Info::Decode ::MLDPn::Tmp [lindex $line 0] ;#----- previous call unpacked its sim on top of ours
   }
   if { $dateSim > $dateAcc } {
      lappend rstlst [expr $dateSim - $dateAcc]
   }

   #----- Add pertinent intervals from this sim

   set idx     -1
   set i       $Sim(EmNbIntervals)
   set totTime $dateStart

   foreach inter $Tmp(EmInterList) np $Tmp(EmNumberParticlesList) {
      incr idx
      set totTime [expr $totTime + $inter]
      set dur     $inter
      set np      [lindex $Tmp(EmNumberParticlesList) $idx]

      #----- Skip intervals that are not part of asked timeframe
      if { $totTime <= $DateFrom } {
         continue
      }
      #----- If we were in the middle of an interval, only add part of it
      set from -1
      if { $totTime-$DateFrom < $inter } {
         set dur [expr $totTime - $DateFrom]
         set from [expr $inter - $dur]
      }
      #----- If we are passed end time, only add the part until end time.
      set to -1
      if { $totTime > $DateTo } {
         set dur [expr $dur - $totTime + $DateTo]
         set to [expr $inter - $totTime + $DateTo]
      }
      if { $dur <= 0 } {
         continue
      }
      #----- Adjust number of particles to represent modifications to interval's duration
      if { $from!=-1 || $to!=-1 } {
         set np [MLDPn::GetIntervalFractionNP $from $to $np $inter [expr $Tmp(ModelTimeStepMin)/60.0]]
      }

      set Sim(EmInter.$i)              $dur
      set Sim(EmIsEm.$i)               [lindex $Tmp(EmIsEmList)            $idx]
      set Sim(EmNumberParticles.$i)    $np
      set Sim(EmHeight.$i)             [lindex $Tmp(EmHeightList)          $idx]
      set Sim(EmRadius.$i)             [lindex $Tmp(EmRadiusList)          $idx]
      switch $Sim(SrcType) {
         "VOLCANO" {
            set Sim(EmMassMode.$i)     [lindex $Tmp(EmMassModeList)        $idx]
            set Sim(EmMass.$i)         [lindex $Tmp(EmMassList)            $idx]
         }
         "VIRUS" {
            set Sim(EmRate.$i)         [lindex $Tmp(EmRateList)            $idx]
         }
         "ACCIDENT" {
            for {set j 0} {$j < $Tmp(EmNbIso)} {incr j} {
               set Sim(EmRate.$i.$j)   [lindex $Tmp(EmRateList) [expr $idx*$Tmp(EmNbIso) + $j]]
            }
         }
      }

      incr i

      if { $totTime >= $DateTo } {
         break
      }
   }
   set Sim(EmNbIntervals) $i

   #----- Pad remaining time with a lull interval

   set padFrom [expr $DateFrom>$totTime ? $DateFrom : $totTime]
   if { $padFrom < $DateTo } {
      set Sim(EmInter.$i)           [expr $DateTo - $padFrom]
      set Sim(EmIsEm.$i)            0
      set Sim(EmNumberParticles.$i) $Sim(EmNumberParticles)
      set Sim(EmHeight.$i)          $Sim(EmHeight)
      set Sim(EmRadius.$i)          $Sim(EmRadius)
      switch $Sim(SrcType) {
         "VOLCANO" {
            set Sim(EmMassMode.$i)  0
            set Sim(EmMass.$i)      0
         }
         "VIRUS" {
            set Sim(EmRate.$i)      0.0
         }
         "ACCIDENT" {
            for {set j 0} {$j < $Tmp(EmNbIso)} {incr j} {
               set Sim(EmRate.$i.$j) 0.0
            }
         }
      }

      incr Sim(EmNbIntervals)
   }

   return [lsort -real -unique $rstlst]
}