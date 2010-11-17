#============================================================================
# Environnement Canada - Service meteorologique du Canada
# Centre meteorologique canadien
# 2121 Route Trans-canadienne
# Dorval, Quebec
# H9P 1J3
#
# Projet     : Interface pour la gestion des experiences.
# Fichier    : <MLDP.tcl>
# Creation   : Octobre 1999 - J.P. Gauthier - CMC/CMOE
#
# Description: Description des procedures relatives au module MLDP.
#
# Remarques  :
#
#============================================================================

#----- Fichiers complementaires

source $GDefs(Dir)/Apps/Models/Types/MLDP.txt
source $GDefs(Dir)/Apps/Models/Types/MLDP.ctes
source $GDefs(Dir)/Apps/Models/Types/MLDP.int
source $GDefs(Dir)/Apps/Models/Types/MLDP_Scenario.tcl

#----------------------------------------------------------------------------
# Nom        : <MLDP::CheckFileSize>
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

proc MLDP::CheckFileSize { } {
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
# Nom        : <MLDP::Launch>
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

proc MLDP::Launch { } {
   global   GDefs
   global   env
   variable Sim
   variable Lbl
   variable Msg
   variable Warning

   #----- Create input files for meteorological preprocessing script, model script, launch script.
   Model::ParamsMeteoInput MLDP
   MLDP::CreateModelInput
   MLDP::CreateScriptInput

   #----- Launch meteorological fields script for RSMC response.
   if { $Sim(SrcType) == "accident" && [file exists $Sim(Path)/tmp/data_std_pres.in] } {
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

      if { $Sim(Model)=="MLDP1" } {
         set cpus "-cpus $Model::Param(NbMPItasks)x$Model::Param(NbOMPthreads) -mpi"
      } else {
         set cpus "-cpus $Model::Param(NbCPUsMeteo)"
      }
      set mem 40G

      exec echo "#!/bin/sh\n\n$Model::Param(Submit) $env(EER_DIRSCRIPT)/Model.sh -args $Sim(PathRun)/tmp/Model_MLDP.in -mach $Model::Param(Host) \
         -t $Sim(RunningTimeCPU) -cm $mem -waste $cpus -listing $Model::Param(Listings) $Model::Param(Op) -queue $Model::Param(Queue)" >$Sim(Path)/tmp/Model_Launch.sh
      exec chmod 755 $Sim(Path)/tmp/Model_Launch.sh
      eval set err \[catch \{ exec $Model::Param(Submit) $env(EER_DIRSCRIPT)/Model.sh -args $Sim(PathRun)/tmp/Model_MLDP.in -mach $Model::Param(Host) \
         -t $Sim(RunningTimeCPU) -cm $mem -waste $cpus -listing $Model::Param(Listings) $Model::Param(Op) -queue $Model::Param(Queue) 2>@1 \} msg\]
      catch { exec echo "$msg" > $Sim(Path)/tmp/Model_Launch.out }

      if { $err } {
         Log::Print ERROR "Submitting the job on $Model::Param(Host) failed:\n\n\t$msg"
         return False
      }
      Log::Print INFO "Job has been submitted successfully on $Model::Param(Host)."

   } else {
      Log::Print INFO "Launching model on $Model::Param(Host)"
      exec echo "#!/bin/sh\n\n$env(EER_DIRSCRIPT)/Model.sh $Sim(Path)/tmp/Model_MLDP.in" >$Sim(Path)/tmp/Model_Launch.sh
      exec ssh -n $Model::Param(Host) $env(EER_DIRSCRIPT)/Model.sh $Sim(Path)/tmp/Model_MLDP.in &
   }
   return True
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::CreateScriptInput>
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

proc MLDP::CreateScriptInput { } {
   variable Sim
   global   GDefs

   #----- Create ASCII file containing directives for launching entire job.
   set file [open $Sim(Path)/tmp/Model_MLDP.in w 0644]

   puts $file "#----- Logger specific parameters"
   puts $file "LOG_MAILTO=\"$Model::Param(EMail)\""
   puts $file "LOG_MAILTITLE=\"$Sim(Model) ($Model::Param(App))\""
   puts $file "LOG_FILE=$Sim(PathRun)/tmp/Model_MLDP.out"
   puts $file "LOG_LEVEL=$Model::Param(LogLevel)"
   puts $file "LOG_TIME=$Model::Param(LogTime)"
   puts $file "LOG_JOBID=$Sim(Model)"

   if { !$Model::Param(Auto) } {
      puts $file "LOG_JOBCLASS=INTERACTIVE"
   }

   puts $file ""
   puts $file "#----- Job general parameters"
   puts $file "MODEL_SOFTWARE=SPI"
   puts $file "MODEL_NAME=MLDP"
   puts $file "MODEL_TYPE=[string index $Sim(Model) end]"
   puts $file "MODEL_USER=$GDefs(FrontEndUser)"
   puts $file ""
   puts $file "MODEL_LOCALHOST=$GDefs(Host)"
   puts $file "MODEL_LOCALDIR=$Sim(Path)"
   puts $file "MODEL_RUNDIR=$Sim(PathRun)"
   puts $file "MODEL_PRE=$Model::Param(NbCPUsMeteo)"
   puts $file "MODEL_RUN=1"
   puts $file "MODEL_POST=1"
   puts $file "MODEL_POOL=$Model::Param(Pool)"
   puts $file "MODEL_CLEAN=1"
   puts $file "MODEL_TRACE=$Exp::Param(Path)/trace"
   puts $file ""
   puts $file "MODEL_NBMPITASKS=$Model::Param(NbMPItasks)     #\[1, 2, ..., 16\]"
   if { $Sim(Model)=="MLDP1" } {
      puts $file "MODEL_NBOMPTHREADS=$Model::Param(NbOMPthreads)   #\[1, 2, ..., 16\]"
      puts $file "MODEL_OMPTHREADFACT=$Model::Param(OMPthreadFact)  #\[1, 2\]"
   }
   puts $file ""
   puts $file "#----- Model specific parameters"

   if { $Sim(ReNewMeteo)!="" } {
      puts $file "MLDP_METEO=$Sim(ReNewMeteo)"
   } elseif { $Model::Param(DBaseLocal) } {
      puts $file "MLDP_METEO=$Sim(Path)/meteo"
   } else {
      puts $file "MLDP_METEO=$Sim(Meteo)"
   }
   puts $file "MLDP_GRIDDEF=$Sim(NI)x$Sim(NJ)x$Sim(NK)"
   puts $file "MLDP_INPUT=$Sim(PathRun)/tmp/$Sim(Model).in"
   puts $file "MLDP_LOGLEVEL=$Sim(LogLevel)"
   puts $file "MLDP_SEED=$Sim(Seed)"

   #----- Type of source.
   if { $Sim(SrcType)=="virus" } {
      puts $file "MLDP_SOURCE=$Sim(VirusType)"
   } else {
      if { $Sim(Model)=="MLDP1" } {
         puts $file "MLDP_SOURCE=accident"
      } else {
         puts $file "MLDP_SOURCE=$Sim(SrcType)"
      }
   }
   puts $file "MLDP_OUTMODE=$Sim(OutMode)"

   close $file
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::ParamsCheck>
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

proc MLDP::ParamsCheck { Tab No } {
   global   GDefs
   variable Sim

   #----- Check for last tab
   set nb [expr [TabFrame::NbFrame $Tab]-1]
   if { $No!=$nb } {
      return True
   }

   #----- Validate output and model time steps but not if we are relaunching.
   if { $Sim(ReNewMeteo)=="" } {
      if { ![MLDP::ValidateNbSrc] || ![MLDP::ValidateTimeSteps] || ![MLDP::ValidateSimulationDuration] || ![MLDP::ValidateOtherParams] } {
         TabFrame::Select $Tab 0
         return False
      }
   }

   #----- Validate emission column parameters.
   if { ![MLDP::ValidateEmissionColumn] } {
      TabFrame::Select $Tab [expr $nb==2?1:0]
      return False
   }

   #----- Validate emission scenario if not validated yet.
   if { !$Sim(IsScenarioValid) } {
      if { ![MLDP::ValidateDurationsVsModelTimeStep] } {
         TabFrame::Select $Tab [expr $nb==2?1:0]
         return False
      }
   }

   #----- Set isotopes information for pool of simulation.
   MLDP::SetIsotopesInfo

   #----- Get meteorological data according to met database, time interval between files, release accident date-time.
   if { $Sim(ReNewMeteo)=="" } {
        Model::ParamsMetDataDir MLDP
       if { ![MLDP::GetMetData] } {
         return False
      }
   }
   return True
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::CreateModelInput>
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

proc MLDP::CreateModelInput { } {
   global   GDefs
   variable Sim
   variable Tmp

   set Sim(NI) [lindex $Sim(Grid) 1]
   set Sim(NJ) [lindex $Sim(Grid) 2]
   set Sim(NK) 25

   set file [open $Sim(Path)/tmp/$Sim(Model).in w]

   #----- Output files.
   set len [expr [string length "$Sim(PathRun)/results/$Sim(SimYear)$Sim(SimMonth)$Sim(SimDay)$Sim(SimHour)_000.pos"] + 10]
   puts $file "Output files:"
   puts $file "[format "%-${len}s" '$Sim(PathRun)/results/$Sim(SimYear)$Sim(SimMonth)$Sim(SimDay)$Sim(SimHour)_000.pos']     outfile_pos      Positions output standard file (256 characters)."
   puts $file "[format "%-${len}s" '$Sim(PathRun)/results/$Sim(SimYear)$Sim(SimMonth)$Sim(SimDay)$Sim(SimHour)_000.con']     outfile_conc     Concentrations output standard file (256 characters)."
   puts $file "[format "%-${len}s" '$Sim(PathRun)/results/$Sim(SimYear)$Sim(SimMonth)$Sim(SimDay)$Sim(SimHour)_000.sv' ]     outfile_sv       Settling velocities output file (256 characters)."

   #----- Input files.
   set len [expr [string length $Sim(PathRun)/meteo] + [string length [file tail [lindex $Sim(MeteoDataFiles) 0]]] + 10]
   puts $file "\nInput files:"
   puts $file "[format "%-${len}s" [llength $Sim(MeteoDataFiles)]]     nb_metfiles       Number of meteorological input standard files."
   for { set i 0 } { $i < [llength $Sim(MeteoDataFiles)] } { incr i } {
      set filename [lindex $Sim(MeteoDataFiles) $i]
      set metfile  "$Sim(PathRun)/meteo/[file tail $filename].std"
      if { $i > 0 } {
         set string ""
      } else {
         set string "     infile_met(i)     Meteorological input standard file (256 characters)."
      }
      puts $file "[format "%-${len}s" '$metfile']$string"
   }

   #----- Grid parameters.
   if { $Sim(Model)=="MLDP0" } {
      puts $file "\nGrid parameters:"
      puts $file "[format "%-20s" $Sim(NI)] [format "%-20s" GNI] Number of X-grid points in meteorological input standard files."
      puts $file "[format "%-20s" $Sim(NJ)] [format "%-20s" GNJ] Number of Y-grid points in meteorological input standard files."
      puts $file "[format "%-20s" $Sim(NK)] [format "%-20s" GNK] Number of vertical levels in meteorological input standard files."
   }

   #----- Model parameters.
   puts $file "\nModel parameters:"
   puts $file "[format "%-20s" [expr $MLDP::Sim(ModelTimeStepMin)*60].0] [format "%-20s" dt_int] Internal model time step \[s\]."
   puts $file "[format "%-20s" [expr $MLDP::Sim(OutputTimeStepMin)*60].0] [format "%-20s" dt_out] Output time step \[s\]."
   puts $file "[format "%-20s" $Sim(EmNumberParticles)] [format "%-20s" NP] Number of particles."
   puts $file "[format "%-20s" $Sim(DtOverTl)] [format "%-20s" dt_sur_tl] Ratio of diffusion time step over Lagrangian time scale \[dimensionless\]."
   puts $file "[format "%-20s" $Sim(DtMin)] [format "%-20s" dt_bas] Diffusion time step minimum value \[s\]."
   puts $file "[format "%-20s" [format "%.4f" $Sim(ReflectionLevel)]] [format "%-20s" hybb] Bottom reflection level of particles in the atmosphere \[hybrid|eta|sigma\]."
   puts $file "[format "%-20s" [format "%.2f" $Sim(VarMesoscale)]] [format "%-20s" sig2_v] Horizontal wind velocity variance for mesoscale fluctuations \[m2/s2\]."
   puts $file "[format "%-20s" [format "%.1f" $Sim(Timescale)]] [format "%-20s" tl_v] Lagrangian time scale \[s\]."
   if { $Sim(Model)=="MLDP0" } {
      puts $file "[format "%-20s" $Sim(IsIncludeHorizDiff)] [format "%-20s" isIncludeHorizDiff] Flag indicating if including horizontal diffusion in free atmosphere."
   }
   puts $file "[format "%-20s" $Sim(IsIncludeSUV)] [format "%-20s" isIncludeSUV] Flag indicating if including horizontal wind speed variances in diffusion calculations."
   puts $file "[format "%-20s" $Model::Param(OMPthreadFact)] [format "%-20s" ompthreads_fact] Integer multiplicative factor to apply to number of OpenMP threads \[1|2\]."

   #----- Source parameters.
   set nbsrc [expr [llength $Sim(Name)]>$Sim(MaxNbSrc)?$Sim(MaxNbSrc):[llength $Sim(Name)]]

   puts $file "\nSource parameters:"
   puts $file "[format "%-25s" '[string range $Sim(NameExp) 0 11]'] [format "%-25s" src_name] Source name (12 characters)."
   puts $file "[format "%-25s" "$Sim(AccYear), $Sim(AccMonth), $Sim(AccDay), $Sim(AccHour), $Sim(AccMin)"] [format "%-25s" etime(i)] Emission date-time \[UTC\]: Year, Month, Day, Hour, Minutes."
   puts $file "[format "%-25s" $nbsrc] [format "%-25s" nbsrc] Number of sources."
   puts $file "[format "%-25s" $Sim(EmHeight)] [format "%-25s" z_src] Maximum plume height \[m\]."
   puts $file "[format "%-25s" $Sim(EmRadius)] [format "%-25s" rad_src] Horizontal dispersion source radius \[m\]."
   for { set i 0 } { $i < $nbsrc } { incr i } {
      set coord "[lindex $Sim(Lat) $i], [lindex $Sim(Lon) $i]"
      if { $i == 0 } {
         puts $file "[format "%-25s" $coord] [format "%-25s" "lat_src(i), lon_src(i)"] Latitude and longitude coordinate of i-th source \[degrees\]."
      } else {
         puts $file "[format "%-25s" $coord]"
      }
   }

   puts $file "[format "%-25s" 11] [format "%-25s" nblevcol] Number of levels in the cumulative distribution of particles within vertical emission plume column."
   set strg   "[format "%-25s" fnp_column(i)] Cumulative fraction \[0,1\] of number of particles within vertical emission plume column."

   if { [set vert [lsearch -exact [lindex $MLDP::Sim(ListEmVerticalDist) 0] $Sim(EmVerticalDist)]]==-1 } {
      set vert [lsearch -exact [lindex $MLDP::Sim(ListEmVerticalDist) 1] $Sim(EmVerticalDist)] } {
   }
   switch $vert {
      0 { set distr [list "[format "%-25s" 0.000] $strg" 0.100 0.200 0.300 0.400 0.500 0.600 0.700 0.800 0.900 1.000] }
      1 { set distr [list "[format "%-25s" 0.000] $strg" 0.010 0.030 0.060 0.100 0.150 0.260 0.410 0.700 0.900 1.000] }
      2 { set distr [list "[format "%-25s" 0.000] $strg" 0.010 0.020 0.040 0.070 0.120 0.190 0.290 0.430 0.650 1.000] }
      3 { set distr [list "[format "%-25s" 0.000] $strg" 0.010 0.030 0.060 0.100 0.150 0.300 0.550 0.800 0.950 1.000] }
      4 { set distr [list "[format "%-25s" 0.000] $strg" 0.100 0.300 0.590 0.740 0.850 0.900 0.940 0.970 0.990 1.000] }
   }
   puts $file [join $distr "\n"]

   #----- Particle size distribution (gravitational settling velocities).
   if { [set sizeIdx [lsearch -exact [lindex $MLDP::Sim(ListEmSizeDist) 0] $Sim(EmSizeDist)]]==-1 } {
     set sizeIdx [lsearch -exact [lindex $MLDP::Sim(ListEmSizeDist) 1] $Sim(EmSizeDist)]
   }
   set sizeLast [expr [llength [lindex $MLDP::Sim(ListEmSizeDist) $GDefs(Lang)]] - 1]

   puts $file "\nParticle size distribution (gravitational settling velocities):"
   set IsComputeSV ".FALSE."
   if { $Sim(SrcType) == "volcano" && $sizeIdx != $sizeLast } {
      set IsComputeSV ".TRUE."
   }
   set Density $Sim(EmDensity)

   puts $file "[format "%-25s" $IsComputeSV] [format "%-25s" isComputeSV] Flag indicating if computing gravitational settling velocities."
   puts $file "[format "%-25s" .FALSE.] [format "%-25s" isWriteSV] Flag indicating if writing gravitational settling velocities to output file (Debugging purposes)."
   puts $file "[format "%-25s" $Sim(EmDensity)] [format "%-25s" density] Density of a particle \[micrograms/m3\]."

   if { $Sim(SrcType) == "volcano" } {

      #----- Number of particle diameter intervals.
      if { $sizeIdx == 0 } {

         #----- Spurr September 1992 size distribution.
         puts $file "[format "%-25s" 18] [format "%-25s" nbbinsSD] Number of bins in particle size distribution."
         puts $file "[format "%-25s" "First column  :"] [format "%-25s" diam_size(i+1)] Particle diameter size boundaries \[microns\]."
         puts $file "[format "%-25s" "Second column :"] [format "%-25s" fnp_size(i)] Fraction \[0,1\] of total number of particles for each particle size bin (3-digits precision)."
         puts $file "0.5"
         puts $file "1.0       0.001"
         puts $file "2.0       0.003"
         puts $file "4.0       0.024"
         puts $file "8.0       0.038"
         puts $file "16.0      0.065   Ex.: 6.5% of particles have a size (diameter) in the range 8-16 microns."
         puts $file "31.0      0.102"
         puts $file "62.5      0.170"
         puts $file "125.0     0.235"
         puts $file "250.0     0.160"
         puts $file "500.0     0.020"
         puts $file "1000.0    0.025"
         puts $file "2000.0    0.055"
         puts $file "4000.0    0.030"
         puts $file "8000.0    0.018"
         puts $file "16000.0   0.025"
         puts $file "32000.0   0.020"
         puts $file "64000.0   0.006"
         puts $file "128000.0  0.003"

      } elseif { $sizeIdx == 1 } {

         #----- Spurr August 1992 size distribution.
         puts $file "[format "%-25s" 17] [format "%-25s" nbbinsSD] Number of bins in particle size distribution."
         puts $file "[format "%-25s" "First column  :"] [format "%-25s" diam_size(i+1)] Particle diameter size boundaries \[microns\]."
         puts $file "[format "%-25s" "Second column :"] [format "%-25s" fnp_size(i)] Fraction \[0,1\] of total number of particles for each particle size bin (3-digits precision)."
         puts $file "0.5"
         puts $file "1.0       0.001"
         puts $file "2.0       0.002"
         puts $file "4.0       0.016"
         puts $file "8.0       0.025"
         puts $file "16.0      0.055   Ex.: 5.5% of particles have a size (diameter) in the range 8-16 microns."
         puts $file "31.0      0.080"
         puts $file "62.5      0.110"
         puts $file "125.0     0.115"
         puts $file "250.0     0.230"
         puts $file "500.0     0.060"
         puts $file "1000.0    0.040"
         puts $file "2000.0    0.056"
         puts $file "4000.0    0.070"
         puts $file "8000.0    0.070"
         puts $file "16000.0   0.050"
         puts $file "32000.0   0.018"
         puts $file "64000.0   0.002"

      } elseif { $sizeIdx == 2 || $sizeIdx == $sizeLast } {

         #----- Redoubt 1989-1990 empirical size distribution.
         puts $file "[format "%-25s" 10] [format "%-25s" nbbinsSD] Number of bins in particle size distribution."
         puts $file "[format "%-25s" "First column  :"] [format "%-25s" diam_size(i+1)] Particle diameter size boundaries \[microns\]."
         puts $file "[format "%-25s" "Second column :"] [format "%-25s" fnp_size(i)] Fraction \[0,1\] of total number of particles for each particle size bin (3-digits precision)."
         puts $file "0.0"
         puts $file "2.0       0.000"
         puts $file "4.0       0.025"
         puts $file "8.0       0.038"
         puts $file "16.0      0.056   Ex.: 5.6% of particles have a size (diameter) in the range 8-16 microns."
         puts $file "32.0      0.102"
         puts $file "62.0      0.143"
         puts $file "125.0     0.224"
         puts $file "250.0     0.193"
         puts $file "500.0     0.153"
         puts $file "1000.0    0.066"

      } elseif { $sizeIdx == 3 } {

         #----- Fine size distribution.
         puts $file "[format "%-25s" 4] [format "%-25s" nbbinsSD] Number of bins in particle size distribution."
         puts $file "[format "%-25s" "First column  :"] [format "%-25s" diam_size(i+1)] Particle diameter size boundaries \[microns\]."
         puts $file "[format "%-25s" "Second column :"] [format "%-25s" fnp_size(i)] Fraction \[0,1\] of total number of particles for each particle size bin (3-digits precision)."
         puts $file "0.0"
         puts $file "2.0       0.250"
         puts $file "4.0       0.250"
         puts $file "8.0       0.250"
         puts $file "16.0      0.250   Ex.: 25.0% of particles have a size (diameter) in the range 8-16 microns."

      }

   } elseif { $Sim(SrcType) == "accident" || $Sim(SrcType) == "virus" } {

      #----- Redoubt 1989-1990 empirical size distribution.
      puts $file "[format "%-25s" 10] [format "%-25s" nbbinsSD] Number of bins in particle size distribution."
      puts $file "[format "%-25s" "First column  :"] [format "%-25s" diam_size(i+1)] Particle diameter size boundaries \[microns\]."
      puts $file "[format "%-25s" "Second column :"] [format "%-25s" fnp_size(i)] Fraction \[0,1\] of total number of particles for each particle size bin (3-digits precision)."
      puts $file "0.0"
      puts $file "2.0       0.000"
      puts $file "4.0       0.025"
      puts $file "8.0       0.038"
      puts $file "16.0      0.056   Ex.: 5.6% of particles have a size (diameter) in the range 8-16 microns."
      puts $file "32.0      0.102"
      puts $file "62.0      0.143"
      puts $file "125.0     0.224"
      puts $file "250.0     0.193"
      puts $file "500.0     0.153"
      puts $file "1000.0    0.066"

   }

   #----- Concentration vertical levels.
   puts $file "\nConcentration vertical levels:"
   puts $file "[format "%-15s" [llength $Sim(VerticalLevels)]] [format "%-15s" nbcvlevel] Number of vertical levels for volumic concentration calculations."
   for { set i 0 } { $i<[llength $Sim(VerticalLevels)] } { incr i } {
      set level [lindex $Sim(VerticalLevels) $i]
      if { $i==0 } {
         puts $file "[format "%-15s" [format "%.1f" $level]] [format "%-15s" cvlevels(i)] Vertical levels \[m\] for defining volumic concentration layers."
      } else {
         puts $file [format "%.1f" $level]
      }
   }

   #----- Emission parameters.
   puts $file "\nEmission parameters:"
   puts $file "[format "%-25s" $Sim(EmNbIso)] [format "%-20s" nbiso] Number of radionuclides (isotopes)."
   puts $file "[format "%-25s" "1st column    :"] [format "%-20s" symbol(j)] Chemical symbol of radionuclide (12 characters)."
   puts $file "[format "%-25s" "2nd column    :"] [format "%-20s" halflife(j)] Radioactive half-life period \[s\]."
   puts $file "[format "%-25s" "3rd column    :"] [format "%-20s" depovelo(j)] Dry deposition velocity      \[m/s\]."
   puts $file "[format "%-25s" "4th column    :"] [format "%-20s" wetscav(j)] Wet scavenging rate          \[s -1\]."
   foreach iso $Sim(EmIso.$Sim(EmScenario)) {
      puts $file "[format "%-15s" "'[string range [lindex $iso 0] 0 11]'"] [format "%-15s" [lindex $iso 1]] [format "%-15s" [lindex $iso 2]] [format "%-15s" [lindex $iso 3]]"
   }
   if { $Sim(SrcType) == "volcano" } {
      puts $file "[format "%-25s" $Sim(EmMass)] [format "%-20s" mass_volcano] Total released mass for volcanic eruption \[micrograms\]."
      if { $Sim(EmMassMode) == 0 } {
         set IsComputeMass ".TRUE."
      } else {
         set IsComputeMass ".FALSE."
      }
      puts $file "[format "%-25s" $IsComputeMass] [format "%-20s" isComputeMass] Flag indicating if computing total released mass \[micrograms\] according to empirical formula from Sparks et al. (1997) for volcanic eruption."
   }
   puts $file "[format "%-25s" $Sim(EmNbIntervals)] [format "%-20s" nbemti] Number of emission time intervals."
   puts $file "[format "%-25s" "1st column    :"] [format "%-20s" emti(i)] Emission time interval \[s\]."
   if { $Sim(SrcType) == "volcano" } {
      set string "\[0: Lull period (no release), 1: Release period\]."
   } else {
      if { $Sim(SrcType) == "accident" } {
         set unit "\[Bq/h\]"
      } elseif { $Sim(SrcType) == "virus" } {
         set unit "\[TCID/h\]"
      }
      set string "and j-th radionuclide $unit."
   }
   puts $file "[format "%-25s" "Other columns :"] [format "%-20s" emrate(i,j)] Emission release rate for i-th emission time interval $string"
   foreach inter $Sim(EmInter.$Sim(EmScenario)) {
      set duration [lindex $inter 0]
      set rates    [lrange $inter 1 end]
      set string   "[format "%-15.1f" $duration]"
      for { set j 0 } { $j < $Sim(EmNbIso) } { incr j } {
         set rate [lindex $rates $j]
         if { $Sim(SrcType) == "accident" || $Sim(SrcType) == "virus" } {
            set rate "[format "%-15s" $rate]"
         }
         append string " $rate"
      }
      set string [string trim $string]
      puts $file $string
   }
   close $file;
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::GetMetData>
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

proc MLDP::GetMetData { } {
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
   set Sim(RunStamp) [fstdstamp fromdate $Sim(AccYear)$Sim(AccMonth)$Sim(AccDay) $Sim(AccHour)$Sim(AccMin)0000]
   set Sim(Data)     [MetData::File $Sim(RunStamp) $Model::Param(DBaseDiag) $Model::Param(DBaseProg) F $LatestRun $Sim(Delta)]

   Dialog::WaitDestroy

   #----- Extract relevant met files according to available meteorological data files and simulation duration.
   return [Model::ParamsMetData MLDP]
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::EmissionRead>
# Creation   : 22 March 2004 - A. Malo - CMC/CMOE
#
# But        : Read emission scenario files.
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

proc MLDP::EmissionRead { } {
   global GDefs
   variable Sim
   variable Error

   set Sim(IsScenarioValid)  0 ; #----- Flag indicating if emission scenario has been validated successfully (1) or not (0).
   set Sim(EmMassMode) 0 ;#----- Total released mass mode
                          #----- 0: Empirical Formula of Sparks et al. (1997). For this mode, mass cannot be modified manually.
                          #---- 1: Edition. For this mode, mass can be modified manually for specific purposes.

   #----- Initialize list of emission scenario.
   set Sim(EmList) {}

   #----- Initialize scenario directory.
   switch $Sim(SrcType) {
      "accident" { set Sim(EmDirScenario) "$Sim(EmDir)/accident" }
      "volcano"  { set Sim(EmDirScenario) "$Sim(EmDir)/volcan" }
      "virus"    { set Sim(EmDirScenario) "$Sim(EmDir)/virus" }
   }

   if { ![file isdirectory $Sim(EmDirScenario)] } {
      Dialog::Error .modelnew $Error(ScenarioDirectory)
      return 0
   }

   foreach path [glob $Sim(EmDirScenario)/*.txt] {

      #----- Open emission scenario file.
      set file [open $path r]

      #----- Set name of the emission scenario.
      set name [file tail [file rootname $path]]
      lappend Sim(EmList) $name

      #----- Read number of release intervals.
      gets $file nb

      #----- Read number of isotopes.
      if { $Sim(SrcType) == "accident" } {
         gets $file iso
      }

      gets $file Sim(EmTotal.$name)     ; #----- Read total duration [s].
      gets $file Sim(EmEffective.$name) ; #----- Read effective duration [s].

      #----- Read release intervals (duration intervals and release rates).
      set Sim(EmInter.$name) {}
      for { set i 0 } { $i < $nb } { incr i } {
         gets $file Line
         lappend Sim(EmInter.$name) $Line
      }

      #----- Read isotopes properties.
      if { $Sim(SrcType) == "accident" } {

         set Sim(EmIso.$name) {}
         for { set i 0 } { $i < $iso } { incr i } {
            gets $file Line
            lappend Sim(EmIso.$name) $Line
         }

      } elseif { $Sim(SrcType) == "volcano" } {

         set Sim(EmIso.$name) { "VOLCAN 1.00e+38 1.00e-03 3.00e-05" }

      } elseif { $Sim(SrcType) == "virus" } {

         set Sim(EmIso.$name) {}
         lappend Sim(EmIso.$name) [list $Sim(VirusSymbol) 1.00e+38 1.00e-03 3.00e-05]

      }
      close $file
   }
   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::EmissionDelete>
# Creation   : 22 March 2004 - A. Malo - CMC/CMOE
#
# But        : Delete emission scenario.
#
# Parametres :
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP::EmissionDelete { } {
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
   ComboBox::Del $MLDP::Sim(ScenarioFrame).name.ent $Sim(EmScenario)

   set idx [lsearch -exact $Sim(EmList) $Sim(EmScenario)]
   set Sim(EmList) [lreplace $Sim(EmList) $idx $idx]

   unset Sim(EmInter.$Sim(EmScenario))
   unset Sim(EmIso.$Sim(EmScenario))
   unset Sim(EmTotal.$Sim(EmScenario))
   unset Sim(EmEffective.$Sim(EmScenario))

   #----- Set current scenario name to the first one in the list.
   set Sim(EmScenario) [lindex $Sim(EmList) 0]

   #----- Select emission scenario.
   MLDP::EmissionSelect
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::EmissionSelect>
# Creation   : 22 March 2004 - A. Malo - CMC/CMOE
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

proc MLDP::EmissionSelect { } {
   variable Sim

   set Sim(EmNbIntervals)       [llength $Sim(EmInter.$Sim(EmScenario))]
   set Sim(EmNbIso)             [llength $Sim(EmIso.$Sim(EmScenario))]
   set Sim(EmTotalDuration)     $Sim(EmTotal.$Sim(EmScenario))
   set Sim(EmEffectiveDuration) $Sim(EmEffective.$Sim(EmScenario))

   #----- Compute total mass released.
   if { $Sim(SrcType) == "volcano" } {
      MLDP::ComputeMass
   }
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::EmissionUpdate>
# Creation   : 22 March 2004 - A. Malo - CMC/CMOE
#
# But        : Update modifications associated to the release scenario.
#
# Parametres :
#
# Retour     :
#  <Idx>     : Flag indicating if modifications have been applied
#              successfully (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP::EmissionUpdate { } {
   global   GDefs
   variable Lbl
   variable Warning
   variable Error
   variable Sim
   variable Tmp

   #----- Validate the release scenario.
   if { ![set Sim(IsScenarioValid) [MLDP::ScenarioValidate]] } {
      return 0
   }

   #----- Substitute all spaces, semicolon and colon by underscore.
   regsub -all "\[^a-zA-Z0-9-\]" $Tmp(Scenario) "_" Tmp(Scenario)

   if { $Tmp(Scenario) == "default" } {
      Dialog::Error .newscenario $Warning(OverwriteDefault)
      focus $Sim(ScenarioNameEntry)
      return 0
   }

   set idx [lsearch -exact $Sim(EmList) $Tmp(Scenario)]
   set save [Dialog::Default .newscenario 400 WARNING $Warning(Save) " $Tmp(Scenario)" 0 $Lbl(No) $Lbl(Yes)]

   #----- Verify if release scenario name does not already exists.
   if { $save && $idx != -1 } {
      if { ![Dialog::Default .newscenario 400 WARNING $Warning(Overwrite) " $Tmp(Scenario)" 0 $Lbl(Cancel) $Lbl(Overwrite)] } {
         focus $Sim(ScenarioNameEntry)
         return 0
      }
   }

   #----- Update durations and emission values.
   set Sim(EmScenario)                   $Tmp(Scenario)
   set Sim(EmNbIntervals)                $Tmp(NbIntervals)
   set Sim(EmIso.$Sim(EmScenario))       $Tmp(Iso)
   set Sim(EmNbIso)                      [llength $Tmp(Iso)]
   set Sim(EmTotal.$Sim(EmScenario))     [set Sim(EmTotalDuration) $Tmp(TotalDuration)]
   set Sim(EmEffective.$Sim(EmScenario)) [set Sim(EmEffectiveDuration) $Tmp(EffectiveDuration)]
   set Sim(EmInter.$Sim(EmScenario))     {}

   if { $Sim(SrcType) == "accident" } {

      for { set i 0 } { $i < $Sim(EmMaxInterval) } { incr i } {
         if { $Tmp(Duration$i) != "" } {
            set inter "$Tmp(Duration$i)"
            for { set j 0 } { $j < [llength $Tmp(Iso)] } { incr j } {
               append inter " $Tmp(ReleaseRate$i.$j)"
            }
            lappend Sim(EmInter.$Sim(EmScenario)) $inter
         }
      }

   } elseif { $Sim(SrcType) == "volcano" } {

      for { set i 0 } { $i < $Sim(EmMaxInterval) } { incr i } {
         if { $Tmp(Duration$i) != "" && $Tmp(Value$i) != -1 } {
            lappend Sim(EmInter.$Sim(EmScenario)) "$Tmp(Duration$i) $Tmp(Value$i)"
         }
      }
      MLDP::ComputeMass

   } elseif { $Sim(SrcType) == "virus" } {

      for { set i 0 } { $i < $Sim(EmMaxInterval) } { incr i } {
         if { $Tmp(Duration$i) != "" } {
            lappend Sim(EmInter.$Sim(EmScenario)) "$Tmp(Duration$i) $Tmp(ReleaseRate$i)"
         }
      }
   }

   #----- Save scenario to file.
   if { $save } {
      MLDP::EmissionWrite
   }

   #----- Add release scenario to list and combo box.
   if { $idx == -1 } {
      lappend Sim(EmList) $Sim(EmScenario)
      ComboBox::Add $MLDP::Sim(ScenarioFrame).name.ent $Sim(EmScenario)
   }

   destroy .newscenario
   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::EmissionWrite>
# Creation   : 22 March 2004 - A. Malo - CMC/CMOE
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

proc MLDP::EmissionWrite { } {
   variable Sim

   set file [open $Sim(EmDirScenario)/$Sim(EmScenario).txt w]

   puts $file "[llength $Sim(EmInter.$Sim(EmScenario))]"

   if { $Sim(SrcType) == "accident" } {
      puts $file "[llength $Sim(EmIso.$Sim(EmScenario))]"
   }

   puts $file "$Sim(EmTotal.$Sim(EmScenario))"
   puts $file "$Sim(EmEffective.$Sim(EmScenario))"

   foreach inter $Sim(EmInter.$Sim(EmScenario)) {
      puts $file "$inter"
   }

   if { $Sim(SrcType) == "accident" } {
      foreach iso  $Sim(EmIso.$Sim(EmScenario)) {
         puts $file "$iso"
      }
   }
   close $file
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::File>
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

proc MLDP::File { Info Path Type Back } {
   variable Sim
   variable Tmp

   Info::Decode ::MLDP::Tmp $Info

   set simpath $Path/[Info::Path $Info]
   set file "$Tmp(SimYear)$Tmp(SimMonth)$Tmp(SimDay)$Tmp(SimHour)_000"
   set std  ""

   set pos       "$simpath/results/${file}.pos"      ; #----- Particle positions result output file.
   set con       "$simpath/results/${file}.con"      ; #----- Concentrations/Depositions result output file.
   set metfields "$simpath/results/${file}m"         ; #----- Meteorological fields for RSMC response.
   set metfiles  [glob -nocomplain $simpath/meteo/*] ; #----- Meteorological files required for launching model.

   switch $Type {
      "all"     { set std "$pos $con $metfields $metfiles" }
      "result"  { set std "$pos $con" }
      "meteo"   { set std "$metfiles" }
      "metf"    { set std "$metfields" }
   }

   return $std
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::Move>
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

proc MLDP::Move { Frame VP } {
   variable Sim
   variable Data

   set Sim(GridLat) $Viewport::Map(LatCursor)
   set Sim(GridLon) $Viewport::Map(LonCursor)

   Model::ParamsGridDefine MLDP
}

proc MLDP::MoveDone { Canvas VP } { }
proc MLDP::MoveInit { Canvas VP } { }
proc MLDP::DrawDone { Canvas VP } { }
proc MLDP::Draw     { Canvas VP } { }
proc MLDP::DrawInit { Canvas VP } { }

#----------------------------------------------------------------------------
# Nom        : <MLDP::Result>
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

proc MLDP::Result { Type } {
   variable Sim
   variable Tmp

   #----- Recuperer les noms de fichiers resultats avec retour sur les precedentes
   set files [File $Exp::Data(SelectSim) [Exp::Path] $Type True]

   Info::Decode ::MLDP::Tmp $Exp::Data(SelectSim)
   SPI::FileOpen NEW FieldBox "(MLDP) $Tmp(NoExp) $Tmp(Name) ($Type)" "" $files
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::SetAccidentDate>
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

proc MLDP::SetAccidentDate { } {
   variable Sim

   set Sim(AccYear)  [clock format $Sim(AccSeconds) -format "%Y" -gmt true] ; #----- Year of accident date.
   set Sim(AccMonth) [clock format $Sim(AccSeconds) -format "%m" -gmt true] ; #----- Month of accident date.
   set Sim(AccDay)   [clock format $Sim(AccSeconds) -format "%d" -gmt true] ; #----- Day of accident date.
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::SetIsotopesInfo>
# Creation   : 9 March 2009 - A. Malo - CMC/CMOE
#
# But        : Set isotopes information for pool simulation.
#
# Parametres :
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP::SetIsotopesInfo { } {
   variable Sim
   variable Duration
   variable ReleaseRate
   variable Quantity

   if { $Sim(SrcType) == "volcano" } {
      return
   }

   #----- Build list of isotopes.
   set Sim(EmIsoSymbol) {}
   foreach iso $Sim(EmIso.$Sim(EmScenario)) {
      lappend Sim(EmIsoSymbol) [lindex $iso 0]
   }

   for { set i 0 } { $i < $Sim(EmNbIntervals) } { incr i } { #----- Loop over number of release time intervals.

      set interval [lindex $Sim(EmInter.$Sim(EmScenario)) $i]
      set Duration($i) [lindex $interval 0]

      for { set j 0 } { $j < $Sim(EmNbIso) } { incr j } { #----- Loop over isotopes.
         set k [expr $j + 1]
         set ReleaseRate($i.$j) [lindex $interval $k]
      }
   }

   #----- Initialize total release quantity.
   for { set i 0 } { $i < $Sim(EmNbIso) } { incr i } { #----- Loop over isotopes.
      set Quantity($i) 0
   }

   for { set i 0 } { $i < $Sim(EmNbIntervals) } { incr i } { #----- Loop over number of release time intervals.

      for { set j 0 } { $j < $Sim(EmNbIso) } { incr j } {    #----- Loop over isotopes.

         #----- Compute total release quantity for each isotope.
         set Quantity($j) [expr $Quantity($j) + double($Duration($i))/3600.0 * double($ReleaseRate($i.$j))]
      }
   }

   #----- Build list of total release quantity for each isotope.
   set Sim(EmIsoQuantity) {}
   for { set j 0 } { $j < $Sim(EmNbIso) } { incr j } { #----- Loop over isotopes.
      lappend Sim(EmIsoQuantity) [format "%.7e" $Quantity($j)]
   }
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::InitNew>
# Creation   : Octobre 1999 - J.P. Gauthier - CMC/CMOE
#
# But        : Initialise un tableau de defintions de simulation pour une
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

proc MLDP::InitNew { Type } {
   global   GDefs
   variable Sim
   variable Tmp

   set Sim(GridSrc) [lindex $Sim(Name) 0] ;#----- Name of first source.
   set Sim(Mode)    prog                  ;#----- Type of meteorological data.

   if { $Sim(Model)=="MLDP0" } {
      set Sim(Duration)             72                                  ; #----- Simulation duration [hr].
      set Sim(OutputTimeStepMin)    60                                  ; #----- Output time step [min].
      set Sim(ModelTimeStepMin)     10                                  ; #----- Internal model time step [min].
      set Sim(Scale)                "MESO"                              ; #----- Grid resolution string.
      set Sim(Meteo)                glb                                 ; #----- Meteorological model.
      set Sim(Delta)                3                                   ; #----- Time interval for meteorological data files [hr].
      set Sim(VarMesoscale)         1.00                                ; #----- Horizontal wind velocity variance for mesoscale fluctuations [m2/s2].
      set Sim(Timescale)            10800                               ; #----- Lagrangian time scale [s].
      set Sim(ReflectionLevel)      0.9999                              ; #----- Reflection level [hyb|eta|sig].
      set Sim(EmNumberParticles)    50000                               ; #----- Number of particles.
      set Sim(ListScale)           { "HEMI  (50 km, 687x687)" "MESO  (33 km, 229x229)" "FINE  (15 km, 503x503)" "VFINE (10 km, 229x229)" "EFINE (5 km,  457x457)" } ; #----- List of grid resolutions [km].
      set Sim(ListMeteoModel)      { glb reg glb100 reg24 }
      set Sim(ListReflectionLevel) { 0.9990 0.9995 0.9996 0.9997 0.9998 0.9999 1.0000 }
      set Sim(DtOverTl)             1.0
      set Sim(DtMin)                1.0
      set Sim(ListVerticalLevels)  { "1 200 400 600 2000 4000" \
                                     "1 500 1000 1500 2000 3000 4000 5000" \
                                     "1 500 1000 1500 2000 2500 3000 3500 4000 4500 5000" \
                                     "1 500 1000" \
                                     "1 1000" \
                                     "1 1000 2000 5000 10000" \
                                     "1 1000 2000 5000 10000 15000" \
                                     "1 1000 2000 3000 4000 5000 6000 7000 8000 9000 10000" \
                                     "1 100 200 300 400 500 600 700 800 900 1000" \
                                     "1 100 1000" \
                                     "1 100 500 1000" \
                                     "1 50 100" \
                                     "1 50 100 500 1000" \
                                     "1 50 100 1000" \
                                     "1 10 50 100" \
                                     "1 10 50 100 1000" \
                                     "1 10 100 1000" }

      #----- Set source type according to experiment data type.
      if { $Type==0 || $Type==3 } {
         #----- Volcano (0) or fire (3) source.
         set Sim(SrcType) "volcano"
      } elseif { $Type== 4 } {
         #----- Virus (4) source.
         set Sim(SrcType)  "virus"
         set Sim(Duration) 48
         set Sim(Meteo)    reg
      } else {
         #----- Nuclear accident (1), CTBT (2), pollutant spill (5), or other (6) sources.
         set Sim(SrcType) "accident"
         set Sim(OutputTimeStepMin) 180
      }
   } else {
      set Sim(Duration)             12                                  ; #----- Simulation duration [hr].
      set Sim(OutputTimeStepMin)    30                                  ; #----- Output time step [min].
      set Sim(ModelTimeStepMin)     5                                   ; #----- Internal model time step [min].
      set Sim(Scale)                "VFINE"                             ; #----- Grid resolution string.
      set Sim(Meteo)                reg                                 ; #----- Meteorological model.
      set Sim(Delta)                1                                   ; #----- Time interval for meteorological data files [hr].
      set Sim(VarMesoscale)         0.10                                ; #----- Horizontal wind velocity variance for mesoscale fluctuations [m2/s2].
      set Sim(Timescale)            2700                                ; #----- Lagrangian time scale [s].
      set Sim(ReflectionLevel)      0.9990                              ; #----- Reflection level [hyb|eta|sig].
      set Sim(EmNumberParticles)    100000                              ; #----- Number of particles.
      set Sim(ListScale)             { "MESO  (33 km,  229x229)" "FINE  (15 km,  229x229)" "VFINE (5 km,   229x229)" "EFINE (1 km,   229x229)" "UFINE (0.1 km, 229x229)" } ; #----- List of grid resolutions [km].
      set Sim(ListMeteoModel)        { glb reg }
      set Sim(ListReflectionLevel)   { 0.9990 0.9995 0.9996 0.9997 0.9998 0.9999 }
      set Sim(DtOverTl)               0.1
      set Sim(DtMin)                  0.1
      set Sim(ListVerticalLevels) { "0 200 400 600 800 1000" \
                                    "0 500 1000 1500 2000 3000 4000 5000" \
                                    "0 500 1000 1500 2000 2500 3000 3500 4000 4500 5000" \
                                    "0 500 1000" \
                                    "0 1000" \
                                    "0 1000 2000 5000 10000" \
                                    "0 1000 2000 5000 10000 15000" \
                                    "0 1000 2000 3000 4000 5000 6000 7000 8000 9000 10000" \
                                    "0 100 200 300 400 500 600 700 800 900 1000" \
                                    "0 100 1000" \
                                    "0 100 500 1000" \
                                    "0 50 100" \
                                    "0 50 100 500 1000" \
                                    "0 50 100 1000" \
                                    "0 10 50 100" \
                                    "0 10 50 100 1000" \
                                    "0 10 100 1000" }

      #----- Set source type according to experiment data type.
      if { $Type== 4 } {
         #----- Virus (4) source.
         set Sim(SrcType) "virus"
      } else {
         #----- Voilcano (0), Nuclear accident (1), CTBT (2), pollutant spill (5), or other (6) sources.
         set Sim(SrcType) "accident"
      }
   }

   set Sim(IsResFileSizeChecked) 0                                   ; #----- Flag indicating if results file size has been checked (1) or not (0).
   set Sim(IsMetFileSizeChecked) 0                                   ; #----- Flag indicating if met data file size has been checked (1) or not (0).
   set Sim(Event)                [lindex $Sim(ListEvent) 0]          ; #----- Type of event.
   set Sim(VerticalLevels)       [lindex $Sim(ListVerticalLevels) 0] ; #----- Vertical levels [m].
   set Sim(PrevReflectionLevel)  $Sim(ReflectionLevel)               ; #----- Previous reflection level [hyb|eta|sig].

   set Sim(EmScenario)           "default"                           ; #----- Scenario name.
   set Sim(EmMass)               0.0                                 ; #----- Total mass released.
   set Sim(EmList)               {}                                  ; #----- List of emission scenarios.
   set Sim(EmNbIntervals)        0                                   ; #----- Number of emission intervals.
   set Sim(EmEffectiveDuration)  0.0                                 ; #----- Effective emission duration, only release periods [s].
   set Sim(EmTotalDuration)      0.0                                 ; #----- Total emission duration, including release and lull periods [s].
   set Sim(EmNbIso)              0                                   ; #----- Number of isotopes.
   set Sim(EmIsoSymbol)          ""                                  ; #----- List of isotopes.
   set Sim(EmIsoQuantity)        ""                                  ; #----- Total release quantity for each isotope.

   #----- Initialize maximum plume height [m] and column radius [m].
   if { $Sim(SrcType) == "volcano" } {        #----- Volcano source type.
      set Sim(EmHeight)    10000.0
      set Sim(EmRadius)    1000.0
      set Sim(EmIsoSymbol) TRACER
      set Sim(EmNbIso)     1
   } elseif { $Sim(SrcType) == "accident" } { #----- Accident source type.
      set Sim(EmHeight)    500.0
      set Sim(EmRadius)    100.0
   } elseif { $Sim(SrcType) == "virus" } {    #----- Virus source type.
      set Sim(EmHeight)    100.0
      set Sim(EmRadius)    100.0
      set Sim(EmIsoSymbol) [lindex [lindex $Sim(ListVirusName) $GDefs(Lang)] 0]
      set Sim(EmNbIso)     1
      set Sim(VirusType)   [lindex $Sim(ListVirusType) 0]
      set Sim(VirusSymbol) [lindex $Sim(ListVirusSymbol) 0]
      set Sim(Scale)       "EFINE"
   }

   set NA [lindex $Sim(NotAvailable) $GDefs(Lang)]

   #----- Initialize unused variables to "not available" for pool information.
   if { $Sim(SrcType) == "accident" || $Sim(SrcType) == "virus" } {
      set Sim(EmDensity)  2.500e+12
      set Sim(EmMass)     $NA
      set Sim(EmSizeDist) $NA
   }

   if { $Sim(SrcType) == "volcano" } {
      set Sim(EmDensity)       2.500e+12 ; #----- Particle density [microgram/m3].
      set Sim(EmSizeDist)      [lindex [lindex $Sim(ListEmSizeDist) $GDefs(Lang)] end] ; #----- Particle size distribution.
      set Sim(EmIsoQuantity)   $NA
   }

   set Sim(EmVerticalDist) [lindex [lindex $Sim(ListEmVerticalDist) $GDefs(Lang)] 0] ; #----- Plume vertical distribution.

   set Tmp(Duration)          $Sim(Duration)          ; #----- Temporary variable for simulation duration.
   set Tmp(OutputTimeStepMin) $Sim(OutputTimeStepMin) ; #----- Temporary variable for output time step.
   set Tmp(Delta)             $Sim(Delta)             ; #----- Temporary variable for time interval between met data files.

   Model::FitAccTime MLDP
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::SpeciesDelete>
# Creation   : Novembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But        : Retire une espece de la liste selectionne.
#
# Parametres :
#    <Idx>   : Index dans la liste a supprimer
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP::SpeciesDelete { Idx } {
   variable Sim
   variable Tmp

   #----- Si l'index est valide

   if { $Idx < [llength $Tmp(Iso)] } {

      set Tmp(Iso) [lreplace $Tmp(Iso) $Idx $Idx]
      set Tmp(Iso$Idx) ""

      #----- Reinitialiser les valeurs aux valeurs du precendent
      #      pour faire un scrolldown des valeurs

      for { set i 0 } { $i < $Sim(EmMaxInterval) } { incr i } {
         set j0 $Idx
         for { set j [expr $Idx+1] } { $j < $Sim(EmMaxIso) } { incr j } {
            set Tmp(ReleaseRate$i.$j0)  $Tmp(ReleaseRate$i.$j)
            set Tmp(ReleaseQuantity$j0) $Tmp(ReleaseQuantity$j)
            set Tmp(Iso$j) ""
            incr j0
         }
      }

      set j 0
      foreach iso $Tmp(Iso) {
         set Tmp(Iso$j) [lindex $iso 0]
         incr j
      }

      #----- Forcer le dernier a vide puisque l'on est sur qu'il l'est
      set Tmp(Iso[expr $Sim(EmMaxIso)-1]) ""

      MLDP::ScenarioAccidentUpdateEmission
   }
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::SpeciesFormat>
# Creation   : Aout 1997 - J.P. Gauthier - CMC/CMOE
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

proc MLDP::SpeciesFormat { Line } {
   global GDefs
   variable Sim
   variable Tmp
   variable Warning
   variable Lbl

   if { [llength $Line] == 11 } {

      set symbol      [lindex $Line 0] ; #----- Isotope Symbol.
      set halflife    [lindex $Line 5] ; #----- Half-Life [s].
      set wetscavrate [lindex $Line 7] ; #----- Wet Scavenging Rate [s-1].
      set drydepvel   [lindex $Line 8] ; #----- Dry Deposition Velocity [m/s].

      if { [llength $Tmp(Iso)] < $Sim(EmMaxIso) && [lsearchsub $Tmp(Iso) $symbol 0] == -1 } {

         if { $halflife >= 900 } {
            #----- Verify if isotope's radioactive half-life is long enough
            #----- ( >= 15 minutes ) to generate relevant simulation results.
            set Tmp(Iso[llength $Tmp(Iso)]) $symbol
            lappend Tmp(Iso) "$symbol $halflife $drydepvel $wetscavrate"

            MLDP::ScenarioAccidentUpdateEmission
         } else {
            #----- Display warning message if radioactive half-life is less than 15 minutes.
            Dialog::Error .modelnew $Warning(HalfLife) " $symbol."
         }
      }
   }
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::UpdateListVerticalLevels>
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

proc MLDP::UpdateListVerticalLevels { } {
   variable Sim

   #----- Validate reflection level.
   if { ![MLDP::ValidateReflectionLevel] } {
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
   set NewListVerticalLevels {}
   foreach list $Sim(ListVerticalLevels) {
      set newlist [lreplace $list 0 0 $firstlevel]
      lappend NewListVerticalLevels $newlist
   }

   set Sim(ListVerticalLevels)  $NewListVerticalLevels                          ; #----- Update list of vertical levels.
   Option::Set $Sim(VerticalLevelsFrm) $Sim(ListVerticalLevels)
   set Sim(VerticalLevels)      [lreplace $Sim(VerticalLevels) 0 0 $firstlevel] ; #----- Update vertical levels.
   set Sim(PrevReflectionLevel) $Sim(ReflectionLevel)                           ; #----- Reset previous reflection level.
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::UpdateVarMesoscale>
# Creation   : 16 July 2010 - A. Malo - CMC/CMOE
#
# But        : Update horizontal wind velocity variance for mesoscale
#              fluctuations [m2/s2] according to selected grid resolution.
#
# Parametres :
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP::UpdateVarMesoscale { } {
   variable Sim

   set Sim(VarMesoscale) 1.00

   if { $Sim(Model)=="MLDP0" } {

      if { $Sim(GridRes) <= 15000 } {
         set Sim(VarMesoscale) 0.10
      }

   } elseif { $Sim(Model)=="MLDP1" } {

      if { $Sim(GridRes) <= 5000 } {
         set Sim(VarMesoscale) 0.10
      }

   }

}

#----------------------------------------------------------------------------
# Nom        : <MLDP::ValidateDensity>
# Creation   : 22 March 2004 - A. Malo - CMC/CMOE
#
# But        : Validate density.
#
# Parametres :
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP::ValidateDensity { } {
   global GDefs
   variable Error
   variable Sim

   #----- Verify if density is positive.

   set number [string is double -strict -failindex idx $Sim(EmDensity)]

   if { $number==0 && $idx==-1 } {
      Dialog::Error .modelnew $Error(EmDensityOutRange) " $Sim(EmDensity) [lindex $Error(UnitDensity) $GDefs(Lang)]."
      return 0
   } elseif { $number== 0 || $Sim(EmDensity)<=0 } {
      Dialog::Error .modelnew $Error(EmDensity) " $Sim(EmDensity) [lindex $Error(UnitDensity) $GDefs(Lang)]"
      return 0
   }

   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::ValidateEmissionColumn>
# Creation   : 22 March 2004 - A. Malo - CMC/CMOE
#
# But        : Validate emission column parameters.
#
# Parametres :
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#    Validate :
#    - Number of particles.
#    - Density and total mass released for volcano.
#    - Maximum plume height.
#    - Column radius.
#
#----------------------------------------------------------------------------

proc MLDP::ValidateEmissionColumn { } {
   global GDefs
   variable Sim
   variable Error

   #----- Validate number of particles.
   if { ![MLDP::ValidateNumberParticles] } {
      focus $Sim(EmissionColumnFrame).nbpart.ent
      return 0
   }

   #----- If source is a volcano type.
   if { $Sim(SrcType) == "volcano" } {

      #----- Validate density.
      if { ![MLDP::ValidateDensity] } {
         focus $Sim(EmissionColumnFrame).density.ent
         return 0
      }

      #----- Validate total mass released.
      if { ![MLDP::ValidateMass] } {
         focus $Sim(EmissionColumnFrame).mass.e
         return 0
      }
   }

   #----- Validate maximum plume height.
   if { ![MLDP::ValidatePlumeHeight] } {
      focus $Sim(EmissionColumnFrame).height.e
      return 0
   }

   #----- Validate column radius.
   if { ![MLDP::ValidateRadius] } {
      focus $Sim(EmissionColumnFrame).radius.ent
      return 0
   }

   #----- Compute total mass released.
   if { $Sim(SrcType) == "volcano" } {
      MLDP::ComputeMass
   }

   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::ValidateMass>
# Creation   : 22 March 2004 - A. Malo - CMC/CMOE
#
# But        : Validate total mass released.
#
# Parametres :
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP::ValidateMass { } {
   global   GDefs
   variable Error
   variable Sim

   #----- Verify if total mass released is positive.
   set number [string is double -strict -failindex idx $Sim(EmMass)]

   if { $number == 0 && $idx == -1 } {
      Dialog::Error .modelnew $Error(MassRange) " $Sim(EmMass) [lindex $Error(UnitMass) $GDefs(Lang)]"
      return 0
   } elseif { $number == 0 || $Sim(EmMass) <= 0 } {
      Dialog::Error .modelnew $Error(Mass) " $Sim(EmMass) [lindex $Error(UnitMass) $GDefs(Lang)]"
      return 0
   }

   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::ValidateMassInputParams>
# Creation   : 22 March 2004 - A. Malo - CMC/CMOE
#
# But        : Validate input parameters for total release mass
#              calculation for volcano eruption.
#
# Parametres :
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#    Validate :
#    - particle density.
#    - Maximum plume height.
#    - Emission durations according to model time step.
#
#----------------------------------------------------------------------------

proc MLDP::ValidateMassInputParams { } {
   global GDefs
   variable Sim
   variable Error

   #----- Validate particle density.
   if { ![MLDP::ValidateDensity] } {
      focus $Sim(EmissionColumnFrame).density.ent
      return 0
   }

   #----- Validate maximum plume height.
   if { ![MLDP::ValidatePlumeHeight] } {
      focus $Sim(EmissionColumnFrame).height.e
      return 0
   }

   #----- Validate emission durations according to model time step
   #----- if scenario has not been validated yet.
   if { !$Sim(IsScenarioValid) } {
      if { ![MLDP::ValidateDurationsVsModelTimeStep] } {
         return 0
      }
   }

   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::ValidateNbSrc>
# Creation   : 7 July 2006 - A. Malo - CMC/CMOE
#
# But        : Validate number of sources.
#
# Parametres :
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP::ValidateNbSrc { } {
   global GDefs
   variable Warning
   variable Lbl
   variable Sim

   #----- Verify if number of sources is less than (or equal to) maximum number of sources.
   if { [llength $Sim(Name)] > $Sim(MaxNbSrc) } {
      Dialog::Error .modelnew $Warning(NbSrc1) " [llength $Sim(Name)].\n[lindex $Warning(NbSrc2) $GDefs(Lang)] $Sim(MaxNbSrc).\n[lindex $Warning(NbSrc3) $GDefs(Lang)] $Sim(MaxNbSrc) [lindex $Warning(NbSrc4) $GDefs(Lang)]"
   }
   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::ValidateNumberParticles>
# Creation   : 22 March 2004 - A. Malo - CMC/CMOE
#
# But        : Validate number of particles.
#
# Parametres :
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP::ValidateNumberParticles { } {
   global GDefs
   variable Error
   variable Sim

   #----- Verify if number of particles is positive and greater or equal to 1000.
   set number [string is integer -strict -failindex idx $Sim(EmNumberParticles)]

   if { $number == 0 && $idx == -1 } {
      Dialog::Error .modelnew $Error(EmNumberParticlesOutRange) " $Sim(EmNumberParticles)."
      return 0
   } elseif { $number == 0 || $Sim(EmNumberParticles) < 1000 } {
      Dialog::Error .modelnew $Error(EmNumberParticles) " $Sim(EmNumberParticles)."
      return 0
   }

   #----- Verify if number of particles is an integer multiple number of 1000.
   if { [expr fmod($Sim(EmNumberParticles),1000)] > $Sim(EmEpsilon) } {
      Dialog::Error .modelnew $Error(EmNumberParticles2) " $Sim(EmNumberParticles)."
      return 0
   }

   #----- Verify if number of particles is less than (or equal to) maximum number of particles.
   if { $Sim(EmNumberParticles) > $Sim(EmMaxNumberParticles) } {
      Dialog::Error .modelnew $Error(EmNumberParticles3) " $Sim(EmNumberParticles).\n[lindex $Error(EmNumberParticles4) $GDefs(Lang)] $Sim(EmMaxNumberParticles)."
      return 0
   }

   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::ValidateOtherParams>
# Creation   : 29 June 2006 - A. Malo - CMC/CMOE
#
# But        : Validate other parameters:
#              - Vertical levels for concentration calculations [m].
#              - Horizontal wind velocity variance for mesoscale fluctuations [m2/s2].
#              - Lagrangian time scale for mesoscale fluctuations [s].
#              - Bottom reflection level of particles in the atmosphere [hyb|eta|sig].
#
# Parametres :
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP::ValidateOtherParams { } {
   global GDefs
   variable Error
   variable Sim

   #----- Validate vertical levels for concentration calculations.
   if { ![MLDP::ValidateVerticalLevels] } {
      focus $Sim(VerticalLevelsEnt)
      return 0
   }

   #----- Validate horizontal wind velocity variance for mesoscale fluctuations.
   if { ![MLDP::ValidateVarianceMesoscale] } {
      focus $Sim(VarMesoscaleEnt)
      return 0
   }

   #----- Validate Lagrangian time scale for mesoscale fluctuations.
   if { ![MLDP::ValidateTimescale] } {
      focus $Sim(TimescaleEnt)
      return 0
   }

   #----- Validate bottom reflection level of particles in the atmosphere.
   if { ![MLDP::ValidateReflectionLevel] } {
      focus $Sim(ReflectionLevelEnt)
      return 0
   }

   #----- Update list of vertical levels according to reflection level.
   MLDP::UpdateListVerticalLevels

   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::ValidatePlumeHeight>
# Creation   : 22 March 2004 - A. Malo - CMC/CMOE
#
# But        : Validate maximum plume height.
#
# Parametres :
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP::ValidatePlumeHeight { } {
   global GDefs
   variable Error
   variable Sim

   #----- Verify if maximum plume height is positive.
   set number [string is double -strict -failindex idx $Sim(EmHeight)]

   if { $number == 0 && $idx == -1 } {
      Dialog::Error .modelnew $Error(HeightRange) " $Sim(EmHeight) $Error(UnitMeters)"
      return 0
   } elseif { $number == 0 || $Sim(EmHeight) <= 0 } {
      Dialog::Error .modelnew $Error(Height) " $Sim(EmHeight) $Error(UnitMeters)"
      return 0
   }

   #----- Verify if maximum plume height is lower or equal to 30000 meters.
   if { $Sim(EmHeight) > 30000.0 } {
      Dialog::Error .modelnew $Error(Height2) " $Sim(EmHeight) $Error(UnitMeters)"
      return 0
   }

   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::ValidateRadius>
# Creation   : 22 March 2004 - A. Malo - CMC/CMOE
#
# But        : Validate radius.
#
# Parametres :
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP::ValidateRadius { } {
   global GDefs
   variable Error
   variable Sim

   #----- Verify if column radius is positive.
   set number [string is double -strict -failindex idx $Sim(EmRadius)]

   if { $number == 0 && $idx == -1 } {
      Dialog::Error .modelnew $Error(RadiusRange) " $Sim(EmRadius) $Error(UnitMeters)"
      return 0
   } elseif { $number == 0 || $Sim(EmRadius) < 0 } {
      Dialog::Error .modelnew $Error(Radius) " $Sim(EmRadius) $Error(UnitMeters)"
      return 0
   }

   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::ValidateReflectionLevel>
# Creation   : 30 June 2006 - A. Malo - CMC/CMOE
#
# But        : Validate bottom reflection level [hyb|eta|sig] of particles
#              in the atmosphere.
#
# Parametres :
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP::ValidateReflectionLevel { } {
   global GDefs
   variable Error
   variable Sim

   #----- Verify if reflection level is positive.
   set number [string is double -strict -failindex idx $Sim(ReflectionLevel)]

   if { $number == 0 && $idx == -1 } {
      Dialog::Error .modelnew $Error(ReflectionLevelRange) " $Sim(ReflectionLevel) $Error(UnitHybEtaSig)"
      return 0
   } elseif { $number == 0 || $Sim(ReflectionLevel) <= 0.0 } {
      Dialog::Error .modelnew $Error(ReflectionLevel) " $Sim(ReflectionLevel) $Error(UnitHybEtaSig)"
      return 0
   }

   #----- Verify if reflection level falls within the range [0.9900, 1.0000].
   if { $Sim(ReflectionLevel) > 1.0 || $Sim(ReflectionLevel) < 0.9900 } {
      Dialog::Error .modelnew $Error(ReflectionLevel2) " $Sim(ReflectionLevel) $Error(UnitHybEtaSig)"
      return 0
   }

   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::ValidateTimescale>
# Creation   : 16 November 2007 - A. Malo - CMC/CMOE
#
# But        : Validate Lagrangian time scale (s) for
#              mesoscale fluctuations.
#
# Parametres :
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP::ValidateTimescale { } {
   global GDefs
   variable Error
   variable Sim

   #----- Verify if time scale is positive.
   set number [string is double -strict -failindex idx $Sim(Timescale)]

   if { $number == 0 && $idx == -1 } {
      Dialog::Error .modelnew $Error(TimescaleRange) " $Sim(Timescale) $Error(UnitSeconds)"
      return 0
   } elseif { $number == 0 || $Sim(Timescale) <= 0.0 } {
      Dialog::Error .modelnew $Error(Timescale) " $Sim(Timescale) $Error(UnitSeconds)"
      return 0
   }

   #----- Verify if timescale is lower or equal to 21600 s.
   if { $Sim(Timescale) > 21600.0 } {
      Dialog::Error .modelnew $Error(Timescale2) " $Sim(Timescale) $Error(UnitSeconds)"
      return 0
   }

   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::ValidateVarianceMesoscale>
# Creation   : 29 June 2006 - A. Malo - CMC/CMOE
#
# But        : Validate horizontal wind velocity variance (m2/s2) for
#              mesoscale fluctuations.
#
# Parametres :
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP::ValidateVarianceMesoscale { } {
   global GDefs
   variable Error
   variable Sim

   #----- Verify if variance is positive.
   set number [string is double -strict -failindex idx $Sim(VarMesoscale)]

   if { $number == 0 && $idx == -1 } {
      Dialog::Error .modelnew $Error(VarMesoscaleRange) " $Sim(VarMesoscale) $Error(UnitM2PS2)"
      return 0
   } elseif { $number == 0 || $Sim(VarMesoscale) < 0.0 } {
      Dialog::Error .modelnew $Error(VarMesoscale) " $Sim(VarMesoscale) $Error(UnitM2PS2)"
      return 0
   }

   #----- Verify if variance is lower or equal to 10.0 m2/s2.
   if { $Sim(VarMesoscale) > 10.0 } {
      Dialog::Error .modelnew $Error(VarMesoscale2) " $Sim(VarMesoscale) $Error(UnitM2PS2)"
      return 0
   }

   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::ValidateVerticalLevels>
# Creation   : 29 June 2006 - A. Malo - CMC/CMOE
#
# But        : Validate vertical levels (m) for concentration calculations.
#
# Parametres :
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP::ValidateVerticalLevels { } {
   global GDefs
   variable Error
   variable Lbl
   variable Warning
   variable Sim

   #----- Number of vertical levels for concentration calculations.
   set nb [llength $Sim(VerticalLevels)]

   #----- Verify if number of concentration vertical levels is greater than 1 and
   #----- less than (or equal to) maximum number of vertical levels.
   if { $nb<2 || $nb>$Sim(MaxNbVerticalLevels) } {
      Dialog::Error .modelnew $Error(VerticalLevels1) "[lindex $Error(VerticalLevels2) $GDefs(Lang)] $nb.\n[lindex $Error(VerticalLevels3) $GDefs(Lang)] $Sim(MaxNbVerticalLevels).\n[lindex $Error(VerticalLevels4) $GDefs(Lang)] $Sim(VerticalLevels) $Error(UnitMeters)"
      return 0
   }

   #----- Verify if all concentration vertical levels are positive and sorted in increasing order.
   for { set i 0 } { $i < $nb } { incr i } {
      set level [lindex $Sim(VerticalLevels) $i]

      set idx ""
      set number [string is double -strict -failindex idx $level]
      if { $number == 0 && $idx == -1 } {
         Dialog::Error .modelnew $Error(VerticalLevelsRange) " $level $Error(UnitMeters)"
         return 0
      } elseif { $number == 0 || ($number == 1 && $level < 0) } {
         Dialog::Error .modelnew $Error(VerticalLevels5) " $level $Error(UnitMeters)"
         return 0
      }

      if { $i > 0 } {
         set prevlevel [lindex $Sim(VerticalLevels) [expr $i - 1]]
         if { $level <= $prevlevel } {
            Dialog::Error .modelnew $Error(VerticalLevels6) " $Sim(VerticalLevels) $Error(UnitMeters)"
            return 0
         }
      } else {
         set firstlevel [lindex $Sim(VerticalLevels) 0]

         if { $Sim(Model)=="MLDP1" && $firstlevel!=0.0 } {
            #----- Replace first level.
            set oldlist $Sim(VerticalLevels)
            set firstlevel 0
            set Sim(VerticalLevels) [lreplace $Sim(VerticalLevels) 0 0 $firstlevel]
            Dialog::Default .modelnew 800 WARNING $Warning(VerticalLevels1) "\n\n[lindex $Warning(VerticalLevels2) $GDefs(Lang)] $oldlist $Error(UnitMeters)\n[lindex $Warning(VerticalLevels3) $GDefs(Lang)] $Sim(VerticalLevels) $Error(UnitMeters)" 0 "OK"
         }
         if { $Sim(Model)=="MLDP0" && $firstlevel!=1.0 } {
            #----- Replace first level.
            set oldlist $Sim(VerticalLevels)
            set firstlevel 1
            set Sim(VerticalLevels) [lreplace $Sim(VerticalLevels) 0 0 $firstlevel]
            Dialog::Default .modelnew 800 WARNING $Warning(VerticalLevels1) "\n\n[lindex $Warning(VerticalLevels2) $GDefs(Lang)] $oldlist $Error(UnitMeters)\n[lindex $Warning(VerticalLevels3) $GDefs(Lang)] $Sim(VerticalLevels) $Error(UnitMeters)" 0 "OK"
         }
      }
   }
   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::ComputeMass>
# Creation   : 22 March 2004 - A. Malo - CMC/CMOE
#
# But        : Validate input parameters for the total released mass
#              calculation. Compute total mass according to empirical
#              formula of Sparks et al. (1997).
#
# Parametres :
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP::ComputeMass { } {
   variable Sim

   #----- Verify if source is a volcano type.
   if { $Sim(SrcType)!="volcano" } {
      return
   }

   set Sim(EmMassOld) $Sim(EmMass)

   #----- Verify if mass mode is 0 (Empirical formula of Sparks et al. 1997).
   if { $Sim(EmMassMode) == 1 } {
      #----- Mass can be edited.
      return
   }

   #----- Validate input parameters for the total released mass calculation
   #----- according to empirical formula of Sparks et al. (1997).
   if { [MLDP::ValidateMassInputParams] } {
      set Sim(EmMass) [format "%.6e" [expr 0.1 * $Sim(EmDensity) * $Sim(EmEffectiveDuration) * pow(double($Sim(EmHeight)/1.670e3),double(1.0/0.259))]]
      set Sim(EmMassOld) $Sim(EmMass)
   }
}
