#============================================================================
# Environnement Canada - Service meteorologique du Canada
# Centre meteorologique canadien
# 2121 Route Trans-canadienne
# Dorval, Quebec
# H9P 1J3
#
# Projet     : Interface pour la gestion des experiences.
# Fichier    : <MLDP0.tcl>
# Creation   : Octobre 1999 - J.P. Gauthier - CMC/CMOE
#
# Description: Description des procedures relatives au module MLDP0.
#
# Remarques  :
#
#============================================================================

#----- Fichiers complementaires

source $GDefs(Dir)/Apps/Models/Types/MLDP0.txt
source $GDefs(Dir)/Apps/Models/Types/MLDP0.ctes
source $GDefs(Dir)/Apps/Models/Types/MLDP0.int

package require IsoBox

#----------------------------------------------------------------------------
# Nom        : <MLDP0::CheckAvailableDiskSpace>
# Creation   : 16 January 2008 - A. Malo - CMC/CMOE
#
# But        : Display warning message if available disk space if lower
#              than critical value.
#
# Parametres :
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP0::CheckAvailableDiskSpace { } {

   global   GDefs
   variable Sim
   variable Warning
   variable Lbl

   set info ""
   set Sim(InfoDisk) ""

   #----- Get disk space information.
   set dirdata $GDefs(DirData)

   if { $Sim(Username) == "afseeer" } {
      set dirdata [lrange [split $GDefs(DirData) "/"] 1 2]
      if { [lindex $dirdata 0] != "data" } {
         Debug::TraceProc "MLDP0: Warning! Unable to set experiment directory for disk space information."
         return 1
      }
      set dirdata "/[join $dirdata "/"]"
   }

   if { $GDefs(Arch) == "Linux" } {
      if { $Sim(Username) == "afseeer" } {
         set ErrorCode [catch { set info [exec df $dirdata | grep "/" | grep "%"] } Message]
      } else {
         set ErrorCode [catch { set info [exec df -k $dirdata | grep "/" | grep "%"] } Message]
      }
   } elseif { $GDefs(Arch) == "IRIX64" } {
      if { $Sim(Username) == "afseeer" } {
         set ErrorCode [catch { set info [exec df $dirdata | grep "/"] } Message]
      } else {
         set ErrorCode [catch { set info [exec df -k $dirdata | grep "/"] } Message]
      }
   }

   if { $ErrorCode != 0 } {
      Debug::TraceProc "MLDP0: Warning! Unable to get disk space information for directory $GDefs(DirData).\n\n$Message"
      return 1
   }

   if { $GDefs(Arch) == "Linux" } {

      set last [expr [llength $info] - 1]
      set filesys "?"

      if { $last == 5 } {
         set filesys [lindex $info 0]                ; #----- File system.
      }
      set total      [lindex $info [expr $last - 4]] ; #----- Total disk space.
      set used       [lindex $info [expr $last - 3]] ; #----- Used disk space.
      set available  [lindex $info [expr $last - 2]] ; #----- Available disk space.
      set mounted    [lindex $info $last]            ; #----- Mounted disk on.

   } elseif { $GDefs(Arch) == "IRIX64" } {

      set filesys   [lindex $info 0] ; #----- File system.
      set total     [lindex $info 2] ; #----- Total disk space.
      set used      [lindex $info 3] ; #----- Used disk space.
      set available [lindex $info 4] ; #----- Available disk space.
      set mounted   [lindex $info 6] ; #----- Mounted disk on.

   }

   #----- Compute fraction [%] of available and used disk space.
   if { [expr double($total)] > 0.0 } {
      set f_available [format "%.2f" [expr double($available)/double($total)*100.0]]
      set f_used      [format "%.2f" [expr double($used)/double($total)*100.0]]
   }

   #----- Compute available disk space in GBytes.
   set availableGB [expr double($available)/pow(double(1024.0),2.0)]

   set crit  "[format "%9s" "$Sim(CriticalDiskSpace) GB"]"
   set avail "[format "%9s" [MLDP0::KBytes2Human $available]] (${f_available} %)"
   set used  "[format "%9s" [MLDP0::KBytes2Human $used]] (${f_used} %)"
   set total "[format "%9s" [MLDP0::KBytes2Human $total]]"

   set    Sim(InfoDisk) "\nMain experiment directory : $GDefs(DirData)"
   append Sim(InfoDisk) "\nFile system               : $filesys"
   append Sim(InfoDisk) "\nDisk mounted on           : $mounted"
   append Sim(InfoDisk) "\nCritical disk space       : $crit"
   append Sim(InfoDisk) "\nAvailable disk space      : $avail"
   append Sim(InfoDisk) "\nUsed disk space           : $used"
   append Sim(InfoDisk) "\nTotal disk space          : $total"

   puts stdout $Sim(InfoDisk)

   if { $availableGB < $Sim(CriticalDiskSpace) } {

      set    Info "\n[lindex $Warning(DiskSpace2) $GDefs(Lang)] : $GDefs(DirData)"
      append Info "\n[lindex $Warning(DiskSpace3) $GDefs(Lang)] : $filesys"
      append Info "\n[lindex $Warning(DiskSpace4) $GDefs(Lang)] : $mounted"
      append Info "\n"
      append Info "\n[lindex $Warning(DiskSpace5) $GDefs(Lang)] : $crit"
      append Info "\n[lindex $Warning(DiskSpace6) $GDefs(Lang)] : $avail"
      append Info "\n[lindex $Warning(DiskSpace7) $GDefs(Lang)] : $used"
      append Info "\n[lindex $Warning(DiskSpace8) $GDefs(Lang)] : $total"

      set answer [Dialog::CreateDefault .mldp0new 700 "[lindex $Lbl(Warning) $GDefs(Lang)]" "[lindex $Warning(DiskSpace) $GDefs(Lang)]\n$Info" warning 1 [lindex $Lbl(Yes) $GDefs(Lang)] [lindex $Lbl(No) $GDefs(Lang)]]

      if { $answer } {
         #----- Answer is No (1).
         return 0
      }

   }

   return 1

}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::CheckFileSize>
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

proc MLDP0::CheckFileSize { } {
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
# Nom        : <MLDP0::ComputeMass>
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

proc MLDP0::ComputeMass { } {
   variable Sim

   #----- Verify if source is a volcano type.
   if { $Sim(SrcType) != "volcano" } {
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
   if { [MLDP0::ValidateMassInputParams] } {
      set Sim(EmMass) [expr 0.1 * $Sim(EmDensity) * $Sim(EmEffectiveDuration) * pow(double($Sim(EmHeight)/1.670e3), double(1.0/0.259))]
      set Sim(EmMass) [format "%.6e" $Sim(EmMass)]
      set Sim(EmMassOld) $Sim(EmMass)
   }
}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::CreateDirectories>
# Creation   : 29 August 2007 - A. Malo - CMC/CMOE
#
# But        : Create directories on host.
#
# Parametres :
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP0::CreateDirectories { } {
   global   GDefs
   variable Sim

   #----- Delete directories.
   if { ![MLDP0::DeleteDirectories] } {
      return 0
   }

   Debug::TraceProc "MLDP0: Creating simulation directories."

   #----- Create simulation directories on local host.
   file mkdir $Sim(Path) $Sim(LocalMetDir) $Sim(LocalResDir) $Sim(LocalTmpDir)

   if { $Sim(IsUsingSoumet) } { #----- Remote host.

      #----- Create listing directory.
      if { ![file isdirectory $Sim(Listing)] } {
         exec mkdir -p $Sim(Listing)
      }

      #----- Create simulation directories on remote host.
      set ErrorCode [catch { exec ssh -l $GDefs(FrontEndUser) -n -x $Sim(Host) mkdir -p $Sim(RemotePath) $Sim(RemoteMetDir) $Sim(RemoteResDir) $Sim(RemoteTmpDir) } Message]
      if { $ErrorCode != 0 } {
         Debug::TraceProc "MLDP0: Error! Unable to create simulation directories on $Sim(HostType) host $Sim(Host).\n\n$Message"
         return 0
      }

   }

   Debug::TraceProc "MLDP0: Simulation directories have been created successfully."

   return 1

}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::CreateLaunchInputFile>
# Creation   : 1 November 2007 - A. Malo - CMC/CMOE
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

proc MLDP0::CreateLaunchInputFile { } {
   variable Sim
   global   GDefs

   Debug::TraceProc "MLDP0: Creating directives input file for launching script."

   #----- Create ASCII file containing directives for launching entire job.
   set file [open $Sim(LaunchInputFile) w 0644]

   puts $file "software      : [format "%-15s" SPI] \[SPI, ARGOS\]"
   puts $file "model         : [format "%-15s" $Sim(ModelName)] \[mldp0, mldp1\]"
   puts $file "username      : [format "%-15s" $GDefs(FrontEndUser)]"
   puts $file ""
   puts $file "localhost     : [format "%-15s" $GDefs(Host)] \[pollux, castor, linux_workstation\]"
   puts $file "localdir      : [format "%-15s" $Sim(Path)]"
   if { $Sim(IsUsingSoumet) } {
      puts $file "remotehost    : [format "%-15s" $Sim(Host)] \[maia, saiph, pollux, castor\]"
      puts $file "remotedir     : [format "%-15s" $Sim(RemotePath)]"
   }
   puts $file ""
   puts $file "nbmetproc     : [format "%-15s" $Sim(NbCPUsMeteo)] \[1, 2, ..., 16\]"
   puts $file "nbmpitasks    : [format "%-15s" $Sim(NbMPItasks)] \[1, 2, ..., 16\]"
   if { $Sim(IsEmailAddress) } {
      puts $file "email         : [format "%-15s" $Sim(EmailAddress)]"
   }
   puts $file "issubmit      : [format "%-15s" $Sim(IsUsingSoumet)] \[Using soumet to launch job on remote host           : 0: No; 1: Yes\]"
   puts $file "isrunmeteo    : [format "%-15s" 1] \[Running meteorological preprocessing                : 0: No; 1: Yes\]"
   puts $file "isrunmodel    : [format "%-15s" 1] \[Running atmospheric transport/dispersion model      : 0: No; 1: Yes\]"
   puts $file "isrunencode   : [format "%-15s" 1] \[Encoding pool information within model result files : 0: No; 1: Yes\]"
   if { $Sim(IsUsingSoumet) } {
      puts $file "isdelete      : [format "%-15s" 1] \[Deleting files located on remote host               : 0: No; 1: Yes\]"
      puts $file "isoutmeteo    : [format "%-15s" 1] \[Redirecting meteo preprocessing job to output file  : 0: No; 1: Yes\]"
      puts $file "isoutmodel    : [format "%-15s" 1] \[Redirecting model job to output file                : 0: No; 1: Yes\]"
      puts $file "isoutencode   : [format "%-15s" 1] \[Redirecting encoding pool info job to output file   : 0: No; 1: Yes\]"
      puts $file "iscopymeteo   : [format "%-15s" 1] \[Copying meteorological data to local host           : 0: No; 1: Yes\]"
      puts $file "iscopymodel   : [format "%-15s" 1] \[Copying model results to local host                 : 0: No; 1: Yes\]"
   } else {
      puts $file "isoutmeteo    : [format "%-15s" 0] \[Redirecting meteo preprocessing job to output file  : 0: No; 1: Yes\]"
      puts $file "isoutmodel    : [format "%-15s" 0] \[Redirecting model job to output file                : 0: No; 1: Yes\]"
      puts $file "isoutencode   : [format "%-15s" 0] \[Redirecting encoding pool info job to output file   : 0: No; 1: Yes\]"
      puts $file "iscopymeteo   : [format "%-15s" 0] \[Copying meteorological data to local host           : 0: No; 1: Yes\]"
      puts $file "iscopymodel   : [format "%-15s" 0] \[Copying model results to local host                 : 0: No; 1: Yes\]"
   }

   close $file

   Debug::TraceProc "MLDP0: Directives input file for launching script has been created successfully."

   return 1

}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::CreateMeteoInputFiles>
# Creation   : 30 August 2007 - A. Malo - CMC/CMOE
#
# But        : Create meteorological input files.
#                - Input file containing list of meteorological files.
#                - Trace information output file containing list of meteorological standard files for simulation.
#                - Input file containing grid parameters.
#
# Parametres :
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP0::CreateMeteoInputFiles { } {
   variable Sim

   Debug::TraceProc "MLDP0: Creating meteorological input files."

   #----- Create ASCII file containing list of meteorological files.
   set file [open $Sim(MetInputFile) w 0644]
   puts $file $Sim(MeteoDataFiles)
   close $file

   #----- Create ASCII file containing list of meteorological files for RSMC response.
   if { $Sim(SrcType) == "accident" } {

      if { $Sim(Meteo) == "reg" } {

         if { $Sim(DBaseProg) == "$Sim(Host):/fs/ops/cmo/gridpt/dbase/prog/regeta" } {
            regsub -all "/fs/ops/cmo" $Sim(MeteoDataFiles)     "/data"     Sim(MeteoDataFilesRSMC)
            regsub -all "/regeta/"    $Sim(MeteoDataFilesRSMC) "/regpres/" Sim(MeteoDataFilesRSMC)
         } elseif { $Sim(DBaseProg) == "$Sim(Host):/fs/ops/cmo/eer/afse/mldp/dbase/prog/regeta" } {
            regsub -all "/fs/ops/cmo/eer/afse" $Sim(MeteoDataFiles) "/data/cmod8/afseeer" Sim(MeteoDataFilesRSMC)
         } elseif { $Sim(DBaseProg) == "/data/gridpt/dbase/prog/regeta" } {
            regsub -all "/regeta/" $Sim(MeteoDataFiles) "/regpres/" Sim(MeteoDataFilesRSMC)
         } else {
            set CurrentDir [pwd]
            cd $Sim(LocalTmpDir)
            file link [file tail $Sim(MetInputFileRSMC)] $Sim(MetInputFile)
            cd $CurrentDir
         }

      } elseif { $Sim(Meteo) == "glb" } {

         if { $Sim(DBaseProg) == "$Sim(Host):/fs/ops/cmo/gridpt/dbase/prog/glbeta" } {
            regsub -all "/fs/ops/cmo" $Sim(MeteoDataFiles)     "/data"     Sim(MeteoDataFilesRSMC)
            regsub -all "/glbeta/"    $Sim(MeteoDataFilesRSMC) "/glbpres/" Sim(MeteoDataFilesRSMC)
         } elseif { $Sim(DBaseProg) == "$Sim(Host):/fs/ops/cmo/eer/afse/mldp/dbase/prog/glbeta" } {
            regsub -all "/fs/ops/cmo/eer/afse" $Sim(MeteoDataFiles) "/data/cmod8/afseeer" Sim(MeteoDataFilesRSMC)
         } elseif { $Sim(DBaseProg) == "/data/gridpt/dbase/prog/glbeta" } {
            regsub -all "/glbeta/" $Sim(MeteoDataFilesRSMC) "/glbpres/" Sim(MeteoDataFilesRSMC)
         } else {
            set CurrentDir [pwd]
            cd $Sim(LocalTmpDir)
            file link [file tail $Sim(MetInputFileRSMC)] $Sim(MetInputFile)
            cd $CurrentDir
         }

      }

      set file [open $Sim(MetInputFileRSMC) w 0644]
      puts $file $Sim(MeteoDataFilesRSMC)
      close $file

   }

   #----- Create trace information output file.
   set file [open $Sim(TraceInfoFile) w 0644]
   set    TraceInfo "$Sim(InfoMet)"
   append TraceInfo "$Sim(InfoLaunch)"
   append TraceInfo "\n$Sim(InfoVar)"
   append TraceInfo "\n$Sim(InfoDisk)"
   puts $file $TraceInfo
   close $file

   #----- Create ASCII file containing grid parameters.
   set ErrorCode [catch { exec echo [format "%.0f,%.0f,%.1f,%.1f,%.1f,%.1f,%s" \
                                         [lindex $Sim(Grid) 1] [lindex $Sim(Grid) 2] [lindex $Sim(Grid) 3] [lindex $Sim(Grid) 4] \
                                         [lindex $Sim(Grid) 5] [lindex $Sim(Grid) 6] [lindex $Sim(Grid) 0]] > $Sim(GridInputFile) } Message]
   if { $ErrorCode != 0 } {
      Debug::TraceProc "MLDP0: Error! Unable to create grid parameters input file.\n\n$Message"
      return 0
   }

   #----- Save simulation pool information.
   set ErrorCode [catch { exec echo "[Info::Code ::MLDP0::Sim $Sim(Info) :]" > $Sim(PoolFile) } Message]
   if { $ErrorCode != 0 } {
      Debug::TraceProc "MLDP0: Error! Unable to save pool information.\n\n$Message"
      return 0
   }

   Debug::TraceProc "MLDP0: Meteorological input files have been created successfully."

   return 1

}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::CreateModelInputFile>
# Creation   : 22 March 2004 - A. Malo - CMC/CMOE
#
# But        : Create MLDP0 model input file.
#
# Parametres :
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP0::CreateModelInputFile { } {
   variable Sim
   variable Tmp

   Debug::TraceProc "MLDP0: Creating MLDP0 model input file."

   #----- Open the model input file for writing.
   set file [open $Sim(ModelInputFile) w]

   #----- Set output files.
   set name "$Sim(SimDateTime)_000"
   set Sim(PositionsOutputFile)        "$Sim(ResDir)/$name.pos"
   set Sim(ConcentrationsOutputFile)   "$Sim(ResDir)/$name.con"
   set Sim(SettlingVelocityOutputFile) "$Sim(ResDir)/$name.sv"

   #----- Output files.
   set len [expr [string length $Sim(ConcentrationsOutputFile)] + 10]
   puts $file "Output files:"
   puts $file "[format "%-${len}s" '$Sim(PositionsOutputFile)'       ]     outfile_pos      Positions output standard file (256 characters)."
   puts $file "[format "%-${len}s" '$Sim(ConcentrationsOutputFile)'  ]     outfile_conc     Concentrations output standard file (256 characters)."
   puts $file "[format "%-${len}s" '$Sim(SettlingVelocityOutputFile)']     outfile_sv       Settling velocities output file (256 characters)."

   #----- Input files.
   set len [expr [string length $Sim(MetDir)] + [string length [file tail [lindex $Sim(MeteoDataFiles) 0]]] + 10]
   puts $file "\nInput files:"
   puts $file "[format "%-${len}s" $Sim(NbMetFiles)]     nb_metfiles       Number of meteorological input standard files."
   for { set i 0 } { $i < $Sim(NbMetFiles) } { incr i } {
      set filename [lindex $Sim(MeteoDataFiles) $i]
      set metfile  "$Sim(MetDir)/[file tail $filename].std"
      if { $i > 0 } {
         set string ""
      } else {
         set string "     infile_met(i)     Meteorological input standard file (256 characters)."
      }
      puts $file "[format "%-${len}s" '$metfile']$string"
   }

   #----- Grid parameters.
   puts $file "\nGrid parameters:"
   puts $file "[format "%-20s" $Sim(NI)] [format "%-20s" GNI] Number of X-grid points in meteorological input standard files."
   puts $file "[format "%-20s" $Sim(NJ)] [format "%-20s" GNJ] Number of Y-grid points in meteorological input standard files."
   puts $file "[format "%-20s" $Sim(NK)] [format "%-20s" GNK] Number of vertical levels in meteorological input standard files."

   #----- Model parameters.
   puts $file "\nModel parameters:"
   puts $file "[format "%-20s" $Sim(ModelTimeStepSec).0] [format "%-20s" dt_int] Internal model time step \[s\]."
   puts $file "[format "%-20s" $Sim(OutputTimeStepSec).0] [format "%-20s" dt_out] Output time step \[s\]."
   puts $file "[format "%-20s" $Sim(EmNumberParticles)] [format "%-20s" NP] Number of particles."
   puts $file "[format "%-20s" $Sim(DtOverTl)] [format "%-20s" dt_sur_tl] Ratio of diffusion time step over Lagrangian time scale \[dimensionless\]."
   puts $file "[format "%-20s" $Sim(DtMin)] [format "%-20s" dt_bas] Diffusion time step minimum value \[s\]."
   puts $file "[format "%-20s" [format "%.4f" $Sim(ReflectionLevel)]] [format "%-20s" hybb] Bottom reflection level of particles in the atmosphere \[hybrid|eta|sigma\]."
   puts $file "[format "%-20s" [format "%.2f" $Sim(VarMesoscale)]] [format "%-20s" sig2_v] Horizontal wind velocity variance for mesoscale fluctuations \[m2/s2\]."
   puts $file "[format "%-20s" [format "%.1f" $Sim(Timescale)]] [format "%-20s" tl_v] Lagrangian time scale \[s\]."
   puts $file "[format "%-20s" $Sim(IsIncludeHorizDiff)] [format "%-20s" isIncludeHorizDiff] Flag indicating if including horizontal diffusion in free atmosphere."
   puts $file "[format "%-20s" $Sim(IsIncludeSUV)] [format "%-20s" isIncludeSUV] Flag indicating if including horizontal wind speed variances in diffusion calculations."
   puts $file "[format "%-20s" $Sim(OMPthreadFact)] [format "%-20s" ompthreads_fact] Integer multiplicative factor to apply to number of OpenMP threads \[1|2\]."

   #----- Source parameters.
   puts $file "\nSource parameters:"
   puts $file "[format "%-25s" '[string range $Sim(Name) 0 11]'] [format "%-25s" src_name] Source name (12 characters)."
   puts $file "[format "%-25s" "$Sim(AccYear), $Sim(AccMonth), $Sim(AccDay), $Sim(AccHour), $Sim(AccMin)"] [format "%-25s" etime(i)] Emission date-time \[UTC\]: Year, Month, Day, Hour, Minutes."
   puts $file "[format "%-25s" $Sim(NbSrc)] [format "%-25s" nbsrc] Number of sources."
   puts $file "[format "%-25s" $Sim(EmHeight)] [format "%-25s" z_src] Maximum plume height \[m\]."
   puts $file "[format "%-25s" $Sim(EmRadius)] [format "%-25s" rad_src] Horizontal dispersion source radius \[m\]."
   for { set i 0 } { $i < $Sim(NbSrc) } { incr i } {
      set coord [lindex $Sim(CoordSrc) $i]
      if { $i == 0 } {
         puts $file "[format "%-25s" $coord] [format "%-25s" "lat_src(i), lon_src(i)"] Latitude and longitude coordinate of i-th source \[degrees\]."
      } else {
         puts $file "[format "%-25s" $coord]"
      }
   }

   puts $file "[format "%-25s" 11] [format "%-25s" nblevcol] Number of levels in the cumulative distribution of particles within vertical emission plume column."
   set strg   "[format "%-25s" fnp_column(i)] Cumulative fraction \[0,1\] of number of particles within vertical emission plume column."
   switch $Sim(EmVerticalDistValue) {
      0 { set distr [list "[format "%-25s" 0.000] $strg" 0.100 0.200 0.300 0.400 0.500 0.600 0.700 0.800 0.900 1.000] }
      1 { set distr [list "[format "%-25s" 0.000] $strg" 0.010 0.030 0.060 0.100 0.150 0.260 0.410 0.700 0.900 1.000] }
      2 { set distr [list "[format "%-25s" 0.000] $strg" 0.010 0.020 0.040 0.070 0.120 0.190 0.290 0.430 0.650 1.000] }
      3 { set distr [list "[format "%-25s" 0.000] $strg" 0.010 0.030 0.060 0.100 0.150 0.300 0.550 0.800 0.950 1.000] }
      4 { set distr [list "[format "%-25s" 0.000] $strg" 0.100 0.300 0.590 0.740 0.850 0.900 0.940 0.970 0.990 1.000] }
   }
   puts $file [join $distr "\n"]

   #----- Particle size distribution (settling velocities).
   puts $file "\nParticle size distribution (settling velocities):"
   if { $Sim(SrcType) == "accident" || $Sim(SrcType) == "virus" } {
      set IsComputeSV ".FALSE."
      set Density $Tmp(EmDensity)
   } elseif { $Sim(SrcType) == "volcano" } {
      if { $Sim(EmSizeDistValue) == 3 } {
         set IsComputeSV ".FALSE."
      } else {
         set IsComputeSV ".TRUE."
      }
      set Density $Sim(EmDensity)
   }
   puts $file "[format "%-25s" $IsComputeSV] [format "%-25s" isComputeSV] Flag indicating if computing settling velocities."
   puts $file "[format "%-25s" .FALSE.] [format "%-25s" isWriteSV] Flag indicating if writing settling velocities to output file (Debugging purposes)."
   puts $file "[format "%-25s" $Density] [format "%-25s" density] Density of a particle \[micrograms/m3\]."
   if { $Sim(SrcType) == "volcano" } {

      #----- Number of particle diameter intervals.
      if { $Sim(EmSizeDistValue) == 0 } {

         #----- Spurr 1992 empirical size distribution (Default distribution).
         puts $file "[format "%-25s" 5] [format "%-25s" nbbinsSD] Number of bins in particle size distribution."
         puts $file "[format "%-25s" "First column  :"] [format "%-25s" diam_size(i+1)] Particle diameter size boundaries \[microns\]."
         puts $file "[format "%-25s" "Second column :"] [format "%-25s" fnp_size(i)] Fraction \[0,1\] of total number of particles for each particle size bin (3-digits precision)."
         puts $file "4.0"
         puts $file "8.0       0.100"
         puts $file "16.0      0.200"
         puts $file "31.0      0.400   Ex.: 40.0% of particles have a size (diameter) in the range 16-31 microns."
         puts $file "62.5      0.200"
         puts $file "125.0     0.100"

      } elseif { $Sim(EmSizeDistValue) == 1 || $Sim(EmSizeDistValue) == 3 } {

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

      } elseif { $Sim(EmSizeDistValue) == 2 } {

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

      #----- Empirical size distribution.
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
   puts $file "[format "%-15s" $Sim(NbVerticalLevels)] [format "%-15s" nbcvlevel] Number of vertical levels for volumic concentration calculations."
   for { set i 0 } { $i < $Sim(NbVerticalLevels) } { incr i } {
      set level [lindex $Sim(VerticalLevels) $i]
      if { $i == 0 } {
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

   close $file ; #----- Close file.

   Debug::TraceProc "MLDP0: Model input file has been created successfully."

   return 1

}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::CreateScriptsLaunchJob>
# Creation   : 31 August 2007 - A. Malo - CMC/CMOE
#
# But        : Create ksh scripts for launching whole job :
#                - meteorological preprocessing ;
#                - atmospheric transport/dispersion model ;
#                - encoding pool information.
#
# Parametres :
#
# Retour     :
#   <>       : Flag indicating if procedure has terminated
#              successfully.
#
# Remarques  :
#   - If Flag = 1 : procedure succeeded.
#   - If Flag = 0 : procedure failed.
#
#----------------------------------------------------------------------------

proc MLDP0::CreateScriptsLaunchJob { } {
   global   GDefs
   variable Sim

   Debug::TraceProc "MLDP0: Creating script for launching entire job."

   if { $Sim(IsUsingSoumet) } {

      set file [open $Sim(SubmitLaunchScript) w 0755] ; #----- Open file.

      puts $file "#!/bin/ksh"
      puts $file ""
      puts $file ". ~/.profile > /dev/null 2>&1"
      puts $file ""
      puts $file "echo \"\""
      puts $file "date +\"Start date-time: %Y-%m-%d %T (%c %Z)\""
      puts $file "echo \"\""
      puts $file ""
      puts $file "soumet=soumet+++"
      puts $file "mach=$Sim(Host)"
      puts $file "tcpu=$Sim(RunningTimeCPU)"
      set cl ""
      if { $Sim(Queue) != "none" } {
         puts $file "class=$Sim(Queue)"
         set cl     " -cl \${class}"
      }
      puts $file "cpu=$Sim(NbCPUsMeteo)"
      set memory ""
      set mem    ""
      if { $Sim(Arch) == "IRIX64" } {
         set mem "2G"
      } elseif { $Sim(Arch) == "AIX" && $Sim(Queue) == "production" } {
         set mem "2G"
         if { $Sim(NI) == 687 && $Sim(NJ) == 687 } {
            set mem "9G"
         } elseif { $Sim(NI) == 503 && $Sim(NJ) == 503 } {
            set mem "5G"
         } elseif { $Sim(NI) == 457 && $Sim(NJ) == 457 } {
            set mem "4G"
         } elseif { $Sim(NI) == 229 && $Sim(NJ) == 229 } {
            set mem "1280M"
         }
      }
      if { $mem != "" } {
         puts $file "memory=$mem"
         set memory " -cm \${memory}"
      }
      puts $file "script=$Sim(LaunchScript)"
      puts $file "input=$Sim(TmpDir)/[file tail $Sim(LaunchInputFile)]"
      puts $file "listing=$Sim(Listing)"
      puts $file ""
      puts $file "echo \"Version of \${soumet} : `which \${soumet}`\""
      puts $file ""
      puts $file "time \${soumet} \${script} -args \${input} -mach \${mach} -t \${tcpu}$memory -cpus \${cpu} -listing \${listing}$cl"
      puts $file ""
      puts $file "echo \"\""
      puts $file "date +\"End date-time: %Y-%m-%d %T (%c %Z)\""
      puts $file "echo \"\""

      close $file ; #----- Close file.

      Debug::TraceProc "MLDP0: Script for submitting the launching of entire job on $Sim(HostType) host ($Sim(Host)) has been created successfully."

      #----- Copy input file and script from temporary directory on local host to temporary directory on remote host.
      Debug::TraceProc "MLDP0: Copying launching input file and script on $Sim(HostType) host ($Sim(Host))."

      set ErrorCode [catch { exec scp -p $Sim(LaunchInputFile) $Sim(SubmitLaunchScript) $GDefs(FrontEndUser)@$Sim(Host):$Sim(TmpDir) } Message]

      if { $ErrorCode != 0 } {
         Debug::TraceProc "MLDP0: Error! Copying launching input file and script on $Sim(HostType) host ($Sim(Host)) has failed.\n\n$Message"
         return 0
      }

      Debug::TraceProc "MLDP0: Launching input file and script have been copied on $Sim(HostType) host ($Sim(Host)) successfully."

   } else {

      set file [open $Sim(JobScript) w 0755] ; #----- Open file.

      puts $file "#!/bin/ksh"
      puts $file ""
      puts $file ". ~/.profile > /dev/null 2>&1"
      puts $file ""
      puts $file "echo \"\""
      puts $file "date +\"Start date-time: %Y-%m-%d %T (%c %Z)\""
      puts $file "echo \"\""
      puts $file ""
      puts $file "script=$Sim(LaunchScript)"
      puts $file "input=$Sim(LaunchInputFile)"
      puts $file ""
      puts $file "$Sim(Timing) \${script} \${input}"
      puts $file ""
      puts $file "echo \"\""
      puts $file "date +\"End date-time: %Y-%m-%d %T (%c %Z)\""
      puts $file "echo \"\""

      close $file ; #----- Close file.

      Debug::TraceProc "MLDP0: Script for launching entire job on $Sim(HostType) host ($Sim(Host)) has been created successfully."

   }

   return 1

}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::CreateScriptLaunchEncode>
# Creation   : 1 November 2007 - A. Malo - CMC/CMOE
#
# But        : Create ksh script for launching pool info encoding.
#
# Parametres :
#
# Retour     :
#   <>       : Flag indicating if procedure has terminated
#              successfully.
#
# Remarques  :
#   - If Flag = 1 : procedure succeeded.
#   - If Flag = 0 : procedure failed.
#
#----------------------------------------------------------------------------

proc MLDP0::CreateScriptLaunchEncode { } {
   variable Sim

   #----- Create script for encoding information.
   Debug::TraceProc "MLDP0: Creating script for encoding pool information."

   set file [open $Sim(EncodeScript) w 0755] ; #----- Open file.

   puts $file "#!/bin/ksh"
   puts $file ""
   puts $file ". ~/.profile > /dev/null 2>&1"
   puts $file ""
   puts $file "echo \"\""
   puts $file "date +\"Start date-time: %Y-%m-%d %T (%c %Z)\""
   puts $file "echo \"\""
   puts $file ""
   puts $file "time ("
   puts $file ""
   puts $file "arch=`uname -s`"
   puts $file ""
   puts $file "binr=$Sim(BinDir)/\${arch}/[file tail $Sim(CodeInfoBin)]"
   puts $file "done=$Sim(StateSimDoneScript)"
   puts $file "pexp=$Sim(PoolFileExp)"
   puts $file "pool=$Sim(PoolFile)"
   puts $file "posi=$Sim(LocalResDir)/[file tail $Sim(PositionsOutputFile)]"
   puts $file "conc=$Sim(LocalResDir)/[file tail $Sim(ConcentrationsOutputFile)]"
   puts $file ""
   puts $file "#----- Encode pool info."
   puts $file "\$binr -INFO \$pool -FSTD \$posi -CKEY codef -NOMVAR INFO"
   puts $file "\$binr -INFO \$pool -FSTD \$conc -CKEY codef -NOMVAR INFO"
   puts $file ""
   puts $file "\$done \$pool \$pexp"
   puts $file ""
   puts $file ")"
   puts $file ""
   puts $file "echo \"\""
   puts $file "date +\"End date-time: %Y-%m-%d %T (%c %Z)\""
   puts $file "echo \"\""

   close $file ; #----- Close file.

   Debug::TraceProc "MLDP0: Script for encoding pool information has been created successfully."

   return 1

}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::CreateScriptLaunchMeteo>
# Creation   : 30 August 2007 - A. Malo - CMC/CMOE
#
# But        : Create ksh script for launching meteorological preprocessing.
#
# Parametres :
#
# Retour     :
#   <>       : Flag indicating if procedure has terminated
#              successfully.
#
# Remarques  :
#   - If Flag = 1 : procedure succeeded.
#   - If Flag = 0 : procedure failed.
#
#----------------------------------------------------------------------------

proc MLDP0::CreateScriptLaunchMeteo { } {
   global   GDefs
   variable Sim

   Debug::TraceProc "MLDP0: Creating script for launching meteorological preprocessing."

   set file [open $Sim(MeteoScript) w 0755]
   puts $file "#!/bin/ksh"
   puts $file ""
   puts $file ". ~/.profile > /dev/null 2>&1"
   puts $file ""
   puts $file "echo \"\""
   puts $file "date +\"Start date-time: %Y-%m-%d %T (%c %Z)\""
   puts $file "echo \"\""
   puts $file ""
   puts $file "arch=`uname -s`"
   puts $file "script_metfields=$Sim(InterpMeteoScript)"
   puts $file "bin_metfields=$Sim(BinDir)/\${arch}/[file tail $Sim(MeteoPreprocBin)]"
   puts $file "tmpdir=$Sim(TmpDir)"
   puts $file "metmodel=$Sim(Meteo)"
   puts $file "nbproc=$Sim(NbCPUsMeteo)"
   puts $file "gridsize=$Sim(NI)x$Sim(NJ)x$Sim(NK)"
   puts $file "debug=$Sim(PrintDebugLevel)"
   puts $file ""
   puts $file "$Sim(Timing) \${script_metfields} \${bin_metfields} \${tmpdir} \${metmodel} \${nbproc} \${gridsize} \${debug}"
   puts $file ""
   puts $file "echo \"\""
   puts $file "date +\"End date-time: %Y-%m-%d %T (%c %Z)\""
   puts $file "echo \"\""

   close $file ; #----- Close file.

   Debug::TraceProc "MLDP0: Script for launching meteorological preprocessing has been created successfully."

   if { $Sim(IsUsingSoumet) } { #----- Using soumet for remote host.

      #----- Copy grid file, met data file and script from temporary directory on local host to temporary directory on remote host.
      Debug::TraceProc "MLDP0: Copying meteorological preprocessing input files and script on $Sim(HostType) host ($Sim(Host))."

      set ErrorCode [catch { exec scp -p $Sim(GridInputFile) $Sim(MetInputFile) $Sim(MeteoScript) $GDefs(FrontEndUser)@$Sim(Host):$Sim(TmpDir) } Message]

      if { $ErrorCode != 0 } {
         Debug::TraceProc "MLDP0: Error! Copying meteorological preprocessing input file and script on $Sim(HostType) host ($Sim(Host)) has failed.\n\n$Message"
         return 0
      }

      Debug::TraceProc "MLDP0: Meteorological preprocessing input files and script have been copied on $Sim(HostType) host ($Sim(Host)) successfully."

   }

   return 1

}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::CreateScriptLaunchModel>
# Creation   : 30 August 2007 - A. Malo - CMC/CMOE
#
# But        : Create ksh script for launching atmospheric
#              transport/dispersion model.
#
# Parametres :
#
# Retour     :
#   <>       : Flag indicating if procedure has terminated
#              successfully.
#
# Remarques  :
#   - If Flag = 1 : procedure succeeded.
#   - If Flag = 0 : procedure failed.
#
#----------------------------------------------------------------------------

proc MLDP0::CreateScriptLaunchModel { } {
   global   GDefs
   variable Sim

   Debug::TraceProc "MLDP0: Creating script for launching model."

   set file [open $Sim(ModelScript) w 0755]
   puts $file "#!/bin/ksh"
   puts $file ""
   puts $file ". ~/.profile > /dev/null 2>&1"
   puts $file ""
   puts $file "echo \"\""
   puts $file "date +\"Start date-time: %Y-%m-%d %T (%c %Z)\""
   puts $file "echo \"\""
   puts $file ""
   puts $file "arch=`uname -s`"
   puts $file "bin=$Sim(BinDir)/\${arch}/[file tail $Sim(ModelBin)]"
   puts $file "input=$Sim(TmpDir)/[file tail $Sim(ModelInputFile)]"
   puts $file "debug=$Sim(PrintDebugLevel)"
   puts $file "seed=$Sim(InitialSeed)"

   #----- Type of source.
   set srctype "$Sim(SrcType)" ; #----- Source type: accident or volcano.
   if { $Sim(SrcType) == "virus" } {
      set srctype "$Sim(VirusType)"
   }

   puts $file "source=$srctype"
   puts $file "outmode=$Sim(OutputMode)"
   puts $file ""
   puts $file "export MLDP0_PARAMS=\"\""
   puts $file ""
   puts $file "echo \"MLDP0_PARAMS : \${MLDP0_PARAMS}\""
   puts $file ""
   puts $file "$Sim(Timing) \${bin} -input \${input} -print \${debug} -seed \${seed} -source \${source} -outmode \${outmode}"
   puts $file ""
   puts $file "echo \"\""
   puts $file "date +\"End date-time: %Y-%m-%d %T (%c %Z)\""
   puts $file "echo \"\""

   close $file ; #----- Close file.

   Debug::TraceProc "MLDP0: Script for launching model has been created successfully."

   if { $Sim(IsUsingSoumet) } { #----- Using soumet for remote host.

      #----- Copy model input file and script from temporary directory on local host to temporary directory on remote host.
      Debug::TraceProc "MLDP0: Copying model input file and script on $Sim(HostType) host ($Sim(Host))."

      set ErrorCode [catch { exec scp -p $Sim(ModelInputFile) $Sim(ModelScript) $GDefs(FrontEndUser)@$Sim(Host):$Sim(TmpDir) } Message]

      if { $ErrorCode != 0 } {
         Debug::TraceProc "MLDP0: Error! Copying model input file and script on $Sim(HostType) host ($Sim(Host)) has failed.\n\n$Message"
         return 0
      }

      Debug::TraceProc "MLDP0: Model input files and script have been copied on $Sim(HostType) host ($Sim(Host)) successfully."

   }

   return 1

}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::DefineDirFiles>
# Creation   : 30 August 2007 - A. Malo - CMC/CMOE
#
# But        : Define directorie and files.
#
# Parametres :
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP0::DefineDirFiles { } {
   global   GDefs
   global   env
   variable Sim

   #----- Define variables.
   set Sim(ModelName)   [string tolower $Sim(Model)]              ; #----- Model name.
   set ExpName          "$Sim(NoExp)_$Sim(Name)"                  ; #----- Experiment name.
   set ExpDir           "$GDefs(DirData)/$ExpName"                ; #----- Experiment directory.
   set Sim(PoolFileExp) "$ExpDir/$Sim(Model).pool"                ; #----- Pool information file of experiment.
   set Sim(NoSim)       [Info::Request $Sim(PoolFileExp)]         ; #----- Simulation no.
   set Date             "$Sim(AccYear)$Sim(AccMonth)$Sim(AccDay)" ; #----- Accident date: YYYYMMDD
   set Time             "$Sim(AccHour)$Sim(AccMin)"               ; #----- Accident time: HHmm
   set SimName          "$Sim(Model).$Sim(NoSim).$Date.$Time"     ; #----- Simulation name: Model.No.YYYYMMDD.HHmm.
   set Sim(Path)        "$ExpDir/$SimName"                        ; #----- Simulation directory on local host.

   set Sim(LocalMetDir)         "$Sim(Path)/meteo"                                        ; #----- Meteo directory of simulation on local host.
   set Sim(LocalResDir)         "$Sim(Path)/results"                                      ; #----- Results directory of simulation on local host.
   set Sim(LocalTmpDir)         "$Sim(Path)/tmp"                                          ; #----- Temporary directory of simulation on local host.
   set Sim(PoolFile)            "$Sim(LocalTmpDir)/sim.pool"                              ; #----- Pool information file of simulation.
   set Sim(MetInputFile)        "$Sim(LocalTmpDir)/data_std_sim.eta"                      ; #----- Input file containing list of met files for simulation.
   set Sim(MetInputFileRSMC)    "$Sim(LocalTmpDir)/data_std_sim.pres"                     ; #----- Input file containing list of met files for simulation for RSMC response.
   set Sim(TraceInfoFile)       "$Sim(LocalTmpDir)/out.trace_info.txt"                    ; #----- Trace information output file.
   set Sim(GridInputFile)       "$Sim(LocalTmpDir)/griddef"                               ; #----- Input file containing grid parameters for simulation.
   set Sim(ModelInputFile)      "$Sim(LocalTmpDir)/input_$Sim(ModelName).txt"             ; #----- Model input file.
   set Sim(LaunchInputFile)     "$Sim(LocalTmpDir)/input_launch_$Sim(ModelName).txt"      ; #----- Launch input file.

   if { $Sim(IsUsingSoumet) } {
      set Sim(SubmitLaunchScript)  "$Sim(LocalTmpDir)/soumet_launch_$Sim(ModelName).ksh"     ; #----- Submit launch script.
      set Sim(SubmitLaunchOutFile) "$Sim(LocalTmpDir)/out.soumet_launch_$Sim(ModelName).txt" ; #----- Std output error file for submit launch script.
   } else {
      set Sim(JobScript)           "$Sim(LocalTmpDir)/exec_job_$Sim(ModelName).ksh"       ; #----- Job script for launching entire job on local host.
      set Sim(JobOutFile)          "$Sim(LocalTmpDir)/out.exec_job_$Sim(ModelName).txt"   ; #----- Std output error file for job script.
   }

   set Sim(MeteoScript)         "$Sim(LocalTmpDir)/exec_meteo_$Sim(ModelName).ksh"        ; #----- Meteo script.
   set Sim(ModelScript)         "$Sim(LocalTmpDir)/exec_$Sim(ModelName).ksh"              ; #----- Model script.
   set Sim(EncodeScript)        "$Sim(LocalTmpDir)/exec_encode_info.ksh"                  ; #----- Encode information script.

   set Sim(ScriptDir)           "$GDefs(Dir)/Script"                                      ; #----- Main script directory.
   set Sim(BinDir)              "$GDefs(Dir)/Bin"                                         ; #----- Main bin directory.

   set Sim(LaunchScript)        "$Sim(ScriptDir)/launch_mldp.ksh"                      ; #----- Launch script.
   set Sim(StateSimDoneScript)  "$Sim(ScriptDir)/SimDone.sh"                           ; #----- Simulation state script.
   set Sim(InterpMeteoScript)   "$Sim(ScriptDir)/InterpolateMeteoFields$Sim(Model).sh" ; #----- Interpolation meteorological preprocessing script.
   set Sim(MetfieldsScript)     "$Sim(ScriptDir)/GenerateMetfields.tcl"                ; #----- RSMC meteorological fields script.
   set Sim(MetfieldsOutFile)    "$Sim(LocalTmpDir)/out.GenerateMetfields.txt"          ; #----- Std output error file for RSMC metfields script.
   set Sim(MeteoPreprocBin)     "$Sim(BinDir)/$Sim(Arch)/metfields_$Sim(ModelName)"    ; #----- Meteo preprocessing binary.
   set Sim(ModelBin)            "$Sim(BinDir)/$Sim(Arch)/$Sim(ModelName)"              ; #----- Model binary.
   set Sim(CodeInfoBin)         "$Sim(BinDir)/$GDefs(Arch)/CodeInfo"                   ; #----- Encode pool information binary.

   if { $Sim(IsUsingSoumet) } { #----- Remote host.

      if { $Sim(Arch) == "AIX" } {
         set maindir [lindex $GDefs(BackEnd$Sim(Host)) 1]
      } elseif { $Sim(Arch) == "IRIX64" } {
         set maindir /tmp/$GDefs(FrontEndUser)
      }
      set token             "$Sim(Host)_${ExpName}_${SimName}_[clock seconds]" ; #----- Token string.
      set Sim(RemotePath)   "$maindir/eer_Experiment/$token"                   ; #----- Simulation directory on remote host.
      set Sim(Listing)      "$env(HOME)/listings/eer_Experiment"               ; #----- Listing directory.

      set Sim(RemoteMetDir) "$Sim(RemotePath)/meteo"
      set Sim(RemoteResDir) "$Sim(RemotePath)/results"
      set Sim(RemoteTmpDir) "$Sim(RemotePath)/tmp"

      #----- Define general directories.
      set Sim(MetDir) $Sim(RemoteMetDir)
      set Sim(ResDir) $Sim(RemoteResDir)
      set Sim(TmpDir) $Sim(RemoteTmpDir)

   } else { #----- Local host.

      #----- Define general directories.
      set Sim(MetDir) $Sim(LocalMetDir)
      set Sim(ResDir) $Sim(LocalResDir)
      set Sim(TmpDir) $Sim(LocalTmpDir)

   }

   #----- Print variables.
   set    Sim(InfoLaunch) "\nHost name                            : $Sim(Host)"
   append Sim(InfoLaunch) "\nHost architecture                    : $Sim(Arch)"
   append Sim(InfoLaunch) "\nHost queue                           : $Sim(Queue)"
   append Sim(InfoLaunch) "\nNumber of CPUs for met preprocessing : $Sim(NbCPUsMeteo)"
   if { $Sim(IsEmailAddress) } {
      append Sim(InfoLaunch) "\nEmail address for monitoring job     : $Sim(EmailAddress)"
   }

   set    Sim(InfoVar) "\nSim(ModelName)           : $Sim(ModelName)"
   append Sim(InfoVar) "\nGDefs(DirData)           : $GDefs(DirData)"
   append Sim(InfoVar) "\nSim(PoolFileExp)         : $Sim(PoolFileExp)"
   append Sim(InfoVar) "\nSim(NoSim)               : $Sim(NoSim)"
   append Sim(InfoVar) "\nSim(Path)                : $Sim(Path)"
   append Sim(InfoVar) "\nSim(LocalMetDir)         : $Sim(LocalMetDir)"
   append Sim(InfoVar) "\nSim(LocalResDir)         : $Sim(LocalResDir)"
   append Sim(InfoVar) "\nSim(LocalTmpDir)         : $Sim(LocalTmpDir)"

   if { $Sim(IsUsingSoumet) } {
      append Sim(InfoVar) "\nSim(RemotePath)          : $Sim(RemotePath)"
      append Sim(InfoVar) "\nSim(RemoteMetDir)        : $Sim(RemoteMetDir)"
      append Sim(InfoVar) "\nSim(RemoteResDir)        : $Sim(RemoteResDir)"
      append Sim(InfoVar) "\nSim(RemoteTmpDir)        : $Sim(RemoteTmpDir)"
      append Sim(InfoVar) "\nSim(Listing)             : $Sim(Listing)"
   }

   append Sim(InfoVar) "\nSim(MetDir)              : $Sim(MetDir)"
   append Sim(InfoVar) "\nSim(ResDir)              : $Sim(ResDir)"
   append Sim(InfoVar) "\nSim(TmpDir)              : $Sim(TmpDir)"
   append Sim(InfoVar) "\nSim(PoolFile)            : $Sim(PoolFile)"
   append Sim(InfoVar) "\nSim(MetInputFile)        : $Sim(MetInputFile)"
   append Sim(InfoVar) "\nSim(MetInputFileRSMC)    : $Sim(MetInputFileRSMC)"
   append Sim(InfoVar) "\nSim(TraceInfoFile)       : $Sim(TraceInfoFile)"
   append Sim(InfoVar) "\nSim(GridInputFile)       : $Sim(GridInputFile)"
   append Sim(InfoVar) "\nSim(ModelInputFile)      : $Sim(ModelInputFile)"
   append Sim(InfoVar) "\nSim(LaunchInputFile)     : $Sim(LaunchInputFile)"
   append Sim(InfoVar) "\nSim(MeteoScript)         : $Sim(MeteoScript)"
   append Sim(InfoVar) "\nSim(ModelScript)         : $Sim(ModelScript)"
   append Sim(InfoVar) "\nSim(EncodeScript)        : $Sim(EncodeScript)"

   if { $Sim(IsUsingSoumet) } {
      append Sim(InfoVar) "\nSim(SubmitLaunchScript)  : $Sim(SubmitLaunchScript)"
      append Sim(InfoVar) "\nSim(SubmitLaunchOutFile) : $Sim(SubmitLaunchOutFile)"
   } else {
      append Sim(InfoVar) "\nSim(JobScript)           : $Sim(JobScript)"
      append Sim(InfoVar) "\nSim(JobOutFile)          : $Sim(JobOutFile)"
   }

   append Sim(InfoVar) "\nSim(ScriptDir)           : $Sim(ScriptDir)"
   append Sim(InfoVar) "\nSim(BinDir)              : $Sim(BinDir)"
   append Sim(InfoVar) "\nSim(LaunchScript)        : $Sim(LaunchScript)"
   append Sim(InfoVar) "\nSim(StateSimDoneScript)  : $Sim(StateSimDoneScript)"
   append Sim(InfoVar) "\nSim(InterpMeteoScript)   : $Sim(InterpMeteoScript)"
   append Sim(InfoVar) "\nSim(MetfieldsScript)     : $Sim(MetfieldsScript)"
   append Sim(InfoVar) "\nSim(MetfieldsOutFile)    : $Sim(MetfieldsOutFile)"
   append Sim(InfoVar) "\nSim(MeteoPreprocBin)     : $Sim(MeteoPreprocBin)"
   append Sim(InfoVar) "\nSim(ModelBin)            : $Sim(ModelBin)"
   append Sim(InfoVar) "\nSim(CodeInfoBin)         : $Sim(CodeInfoBin)"

   puts stdout $Sim(InfoLaunch)
   puts stdout $Sim(InfoVar)

}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::DeleteDirectories>
# Creation   : 30 August 2007 - A. Malo - CMC/CMOE
#
# But        : Delete directories on remote host.
#
# Parametres :
#
# Retour     :
#   <>       : Flag indicating if procedure has terminated
#              successfully.
#
# Remarques  :
#   - If Flag = 1 : procedure succeeded.
#   - If Flag = 0 : procedure failed.
#
#----------------------------------------------------------------------------

proc MLDP0::DeleteDirectories { } {
   global   GDefs
   variable Sim

   puts stdout ""
   Debug::TraceProc "MLDP0: Removing simulation directories."

   #----- Delete directories on local host.
   file delete -force $Sim(Path)

   if { $Sim(IsUsingSoumet) } { #----- Remote host.

      #----- Delete directories on remote host.
      set ErrorCode [catch { exec ssh -l $GDefs(FrontEndUser) -n -x $Sim(Host) rm -rf $Sim(RemotePath) } Message]
      if { $ErrorCode != 0 } {
         Debug::TraceProc "MLDP0: Error! Unable to remove simulation directory on $Sim(HostType) host $Sim(Host).\n\n$Message"
         return 0
      }

   }

   Debug::TraceProc "MLDP0: Simulation directories have been removed successfully."

   return 1

}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::EmissionDelete>
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

proc MLDP0::EmissionDelete { } {
   global GDefs
   variable Lbl
   variable Sim
   variable Warning

   #----- Do not erase default emission scenario.
   if { $Sim(EmScenario) == "default" } {
      Dialog::CreateDefault .mldp0new 400 "[lindex $Lbl(Warning) $GDefs(Lang)]" "[lindex $Warning(DeleteDefault) $GDefs(Lang)]" warning 0 "OK"
      return
   }

   #----- Ask user if deleting emission scenario.
   set erase [Dialog::CreateDefault .mldp0new 400 "[lindex $Lbl(Warning) $GDefs(Lang)]" "[lindex $Warning(DeleteScenario) $GDefs(Lang)] $Sim(EmScenario)." warning 0 [lindex $Lbl(No) $GDefs(Lang)] [lindex $Lbl(Yes) $GDefs(Lang)]]

   if { !$erase } {
      return
   }

   #----- Delete scenario file.

   file delete -force $Sim(EmDirScenario)/$Sim(EmScenario).txt

   #----- Delete scenario name from the combo box.

   ComboBox::Del $MLDP0::Sim(ScenarioFrame).name.ent $Sim(EmScenario)

   set idx [lsearch -exact $Sim(EmList) $Sim(EmScenario)]
   set Sim(EmList) [lreplace $Sim(EmList) $idx $idx]

   unset Sim(EmInter.$Sim(EmScenario))
   unset Sim(EmIso.$Sim(EmScenario))
   unset Sim(EmTotal.$Sim(EmScenario))
   unset Sim(EmEffective.$Sim(EmScenario))

   #----- Set current scenario name to the first one in the list.

   set Sim(EmScenario) [lindex $Sim(EmList) 0]


   #----- Select emission scenario.
   MLDP0::EmissionSelect
}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::EmissionRead>
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

proc MLDP0::EmissionRead { } {
   global GDefs
   variable Sim
   variable Error

   #----- Initialize list of emission scenario.
   set Sim(EmList) {}

   #----- Initialize scenario directory.
   switch $Sim(SrcType) {
      "accident" { set Sim(EmDirScenario) "$Sim(EmDir)/accident" }
      "volcano"  { set Sim(EmDirScenario) "$Sim(EmDir)/volcan" }
      "virus"    { set Sim(EmDirScenario) "$Sim(EmDir)/virus" }
   }

   if { ![file isdirectory $Sim(EmDirScenario)] } {
      Dialog::CreateError .mldp0new "[lindex $Error(ScenarioDirectory) $GDefs(Lang)]" $GDefs(Lang)
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
# Nom        : <MLDP0::EmissionSelect>
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

proc MLDP0::EmissionSelect { } {
   variable Sim

   set Sim(EmNbIntervals)       [llength $Sim(EmInter.$Sim(EmScenario))]
   set Sim(EmNbIso)             [llength $Sim(EmIso.$Sim(EmScenario))]
   set Sim(EmTotalDuration)     $Sim(EmTotal.$Sim(EmScenario))
   set Sim(EmEffectiveDuration) $Sim(EmEffective.$Sim(EmScenario))

   #----- Compute total mass released.
   if { $Sim(SrcType) == "volcano" } {
      MLDP0::ComputeMass
   }
}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::EmissionUpdate>
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

proc MLDP0::EmissionUpdate { } {
   global   GDefs
   variable Lbl
   variable Warning
   variable Error
   variable Sim
   variable Tmp

   #----- Validate the release scenario.
   if { ![MLDP0::ValidateScenario] } {
      return 0
   }

   #----- Substitute all spaces, semicolon and colon by underscore.
   regsub -all "\[^a-zA-Z0-9-\]" $Tmp(Scenario) "_" Tmp(Scenario)

   if { $Tmp(Scenario) == "default" } {
      Dialog::CreateDefault .newscenario 400 "[lindex $Lbl(Warning) $GDefs(Lang)]" "[lindex $Warning(OverwriteDefault) $GDefs(Lang)]" warning 0 "OK"
      focus $Sim(ScenarioNameEntry)
      grab .newscenario
      return 0
   }

   set idx [lsearch -exact $Sim(EmList) $Tmp(Scenario)]
   set save [Dialog::CreateDefault .newscenario 400 "[lindex $Lbl(Warning) $GDefs(Lang)]" "[lindex $Warning(Save) $GDefs(Lang)] $Tmp(Scenario)." \
                 warning 0 [lindex $Lbl(No) $GDefs(Lang)] [lindex $Lbl(Yes) $GDefs(Lang)]]

   #----- Verify if release scenario name does not already exists.
   if { $save && $idx != -1 } {
      set ok [Dialog::CreateDefault .newscenario 400 "[lindex $Lbl(Warning) $GDefs(Lang)]" "[lindex $Warning(Overwrite) $GDefs(Lang)] $Tmp(Scenario)." \
                  warning 0 [lindex $Lbl(Cancel) $GDefs(Lang)] [lindex $Lbl(Overwrite) $GDefs(Lang)]]

      if { !$ok } {
         focus $Sim(ScenarioNameEntry)
         grab .newscenario
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

      MLDP0::ComputeMass ; #----- Compute total mass released.

   } elseif { $Sim(SrcType) == "virus" } {

      for { set i 0 } { $i < $Sim(EmMaxInterval) } { incr i } {
         if { $Tmp(Duration$i) != "" } {
            lappend Sim(EmInter.$Sim(EmScenario)) "$Tmp(Duration$i) $Tmp(ReleaseRate$i)"
         }
      }

   }

   #----- Save scenario to file.
   if { $save } {
      MLDP0::EmissionWrite
   }

   #----- Add release scenario to list and combo box.
   if { $idx == -1 } {
      lappend Sim(EmList) $Sim(EmScenario)
      ComboBox::Add $MLDP0::Sim(ScenarioFrame).name.ent $Sim(EmScenario)
   }

   destroy .newscenario
   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::EmissionWrite>
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

proc MLDP0::EmissionWrite { } {
   variable Sim

   #----- Write emission scenario.

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

#---------------------------------------------------------------------------
# Nom      : <MLDP0::ExpandTime>
# Creation : 27 August 2007 - A. Malo - CMC/CMOE
#
# But      : Expand time in hours, minutes and seconds.
#
# Parametres :
#   <Seconds> : Number of seconds to be converted.
#
# Retour :
#   <time>    : Expanded time.
#
# Remarques :
#
#---------------------------------------------------------------------------
proc MLDP0::ExpandTime { Seconds } {

   set seconds $Seconds
   set minutes 0
   set hours   0

   if { $seconds >= 60.0 } {

      set minutes [expr int($seconds/60.0)]
      set seconds [expr fmod($seconds , 60.0)]
      set seconds [string trimleft $seconds "-"]

      if { $minutes >= 60 } {
         set hours   [expr int($minutes/60.0)]
         set minutes [expr int(fmod($minutes, 60.0))]
      }

   }

   set seconds [format "%.3f" $seconds]

   if { $hours > 0 } {

      set time "$hours hr, $minutes min, $seconds s. ($Seconds s.)"

   } else {

      if { $minutes > 0 } {
         set time "$minutes min, $seconds s. ($Seconds s.)"
      } else {
         set time "$seconds s."
      }

   }

   return $time

}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::ExtractMetFiles>
# Creation   : 27 August 2007 - A. Malo - CMC/CMOE
#
# But        : Extract relevant met files according to available
#              meteorological data files and simulation duration.
#
# Parametres :
#
# Retour     :
#  <Bool>    : True ou False.
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP0::ExtractMetFiles { } {
   global GDefs
   variable Sim
   variable Error
   variable Warning
   variable Lbl

   #----- Set starting simulation date-time associated to date-time of
   #----- first meteorological file available in the database.
   set data             [lindex $Sim(Data) 0]
   set SimDateTimeStamp [lindex $data 0] ; #----- CMC date-time stamp.
   set SimDateTime      [lindex $data 1] ; #----- Date-time.

   #----- Get last meteorological file.
   set data      [lindex $Sim(Data) end]
   set laststamp [lindex $data 0]        ; #----- Date-time stamp for last met file.
   set lastdate  [lindex $data 1]        ; #----- Date-time for last met file.

   #----- Compute simulation duration [hr].
   set simdur [expr int([fstdstamp diff $laststamp $SimDateTimeStamp] + 0.5)]

   if { $simdur <= 0 } {
      puts stdout ""
      Debug::TraceProc "MLDP0: Error! Not enough available meteorological data files in database according to emission date-time to run the model."
      Debug::TraceProc "MLDP0:        - Emission date-time (stamp)       : $Sim(AccDateTime) ($Sim(AccDateTimeStamp))"
      Debug::TraceProc "MLDP0:        - Simulation date-time (stamp)     : $SimDateTime ($SimDateTimeStamp)"
      Debug::TraceProc "MLDP0:        - Last available date-time (stamp) : $lastdate ($laststamp)"

      set emission [FormatDateTime $Sim(AccDateTime)]
      set first    [FormatDateTime $SimDateTime]
      set last     [FormatDateTime $lastdate]
      Dialog::CreateError .mldp0new "[lindex $Error(MetFiles) $GDefs(Lang)]\n\n[lindex $Error(DateTimeEmission) $GDefs(Lang)] $emission.\n[lindex $Error(FirstMetDateTime) $GDefs(Lang)] $first.\n[lindex $Error(LastMetDateTime) $GDefs(Lang)] $last." $GDefs(Lang) 600
      return False
   }

   if { $Sim(Duration) == 0 } {

      #----- Define simulation duration [hr] according to available met files.
      set Sim(Duration) $simdur

   } else {

      if { $Sim(Duration) > $simdur } {

         #----- Here, simulation duration set as input parameter is greater than (or equal to) simulation duration
         #----- computed according to available met files. Thus, simulation duration will be re-initialized.
         set oldsimdur     $Sim(Duration)
         set Sim(Duration) $simdur
         puts stdout ""
         Debug::TraceProc "MLDP0: Warning: Re-initializing simulation duration according to available met files in database."
         Debug::TraceProc "MLDP0:          - Old simulation duration : $oldsimdur hr."
         Debug::TraceProc "MLDP0:          - New simulation duration : $Sim(Duration) hr."
         Dialog::CreateDefault .mldp0new 400 "[lindex $Lbl(Warning) $GDefs(Lang)]" "[lindex $Warning(SimDuration1) $GDefs(Lang)]\n\n[lindex $Warning(SimDuration2) $GDefs(Lang)] $oldsimdur $Error(UnitHours)\n[lindex $Warning(SimDuration3) $GDefs(Lang)] $Sim(Duration) $Error(UnitHours)" warning 0 "OK"

      } else {

         #----- Here, simulation duration set as input parameter is less than simulation duration
         #----- computed according to available met files.

         #----- Compute new ending simulation date-time [s] according to starting simulation date-time and simulation duration.
         set date  [string range $SimDateTime 0 7]
         set hour  [string range $SimDateTime 8 9]
         set min   [string range $SimDateTime 10 11]
         set start [clock scan "$date $hour:$min" -gmt true] ; #----- Starting simulation date-time [s].
         set end   [expr $start + $Sim(Duration)*3600]       ; #----- Ending simulation date-time [s] := starting simulation date-time [s] + simulation duration [s].

         #----- Convert new ending simulation date-time from [s] to human readable format.
         set year  [clock format $end -format "%Y" -gmt true] ; #----- YYYY.
         set month [clock format $end -format "%m" -gmt true] ; #----- MM.
         set day   [clock format $end -format "%d" -gmt true] ; #----- DD.
         set hour  [clock format $end -format "%H" -gmt true] ; #----- HH.
         set min   [clock format $end -format "%M" -gmt true] ; #----- mm.

         #----- Convert new ending simulation date-time from human readable format to CMC date-time stamp.
         set laststamp [fstdstamp fromdate ${year}${month}${day} ${hour}${min}0000]

         #----- Build temporary list of met stamps.
         set MeteoStamp {}
         foreach data $Sim(Data) {
            lappend MeteoStamp [lindex $data 0]
         }

         #----- Redefine list of available meteorological data files according to simulation duration set as input parameter.
         set idx [lsearch -exact $MeteoStamp $laststamp]
         if { $idx != -1 } {
            set Sim(Data) [lrange $Sim(Data) 0 $idx]
         }

      }

   }

   #----- Compute effective simulation duration.
   #----- Effective simulation duration starts from the beginning of release scenario (corresponding to the
   #----- accident date-time) and ends at the date-time of last available met data file.
   set Sim(EffectiveDurationMin) [expr int([fstdstamp diff $laststamp $Sim(AccDateTimeStamp)]*60 + 0.5)] ; #----- [min].
   set Sim(EffectiveDurationSec) [expr $Sim(EffectiveDurationMin)*60]                                    ; #----- [s].

   set Sim(NbMetFiles)     [llength $Sim(Data)] ; #----- Number of meteorological files.
   set MeteoDateTime       {}
   set Sim(MeteoDataFiles) {}

   set Sim(InfoMet) "\nList of available meteorological files for '$Sim(Meteo)' meteorological model:"
   foreach data $Sim(Data) {
      lappend MeteoDateTime       [lindex $data 1] ; #----- List of met date-times.
      lappend Sim(MeteoDataFiles) [lindex $data 2] ; #----- List of met data files.
      append Sim(InfoMet) "\n$data"
   }

   set Sim(Mode) [MetData::GetMode $Sim(Data)]

   append Sim(InfoMet) "\n"
   append Sim(InfoMet) "\nEmission date-time (stamp)           : $Sim(AccDateTime) ($Sim(AccDateTimeStamp))"
   append Sim(InfoMet) "\nSimulation date-time (stamp)         : $SimDateTime ($SimDateTimeStamp)"
   append Sim(InfoMet) "\nDiagnostics meteorological database  : $Sim(DBaseDiag)"
   append Sim(InfoMet) "\nPrognostics meteorological database  : $Sim(DBaseProg)"
   append Sim(InfoMet) "\nMeteorological model                 : $Sim(Meteo)"
   append Sim(InfoMet) "\nMeteorological data mode             : $Sim(Mode)"
   append Sim(InfoMet) "\nMeteorological data time interval    : $Sim(Delta) hr"
   append Sim(InfoMet) "\nNumber of meteorological files       : $Sim(NbMetFiles)"
   append Sim(InfoMet) "\nSimulation duration                  : [MLDP0::ExpandTime [expr $Sim(Duration)*3600]]"
   append Sim(InfoMet) "\nEffective simulation duration        : [MLDP0::ExpandTime [expr $Sim(EffectiveDurationSec)]]"

   puts stdout $Sim(InfoMet)

   #----- Verify if number of meteo data files is greater than 1.
   if { [llength $Sim(Data)] < 2 } {
      puts stdout ""
      Debug::TraceProc "MLDP0: Error! Not enough available meteorological data files."
      Dialog::CreateError .mldp0new "[lindex $Error(MetFiles) $GDefs(Lang)]" $GDefs(Lang) 600
      return False
   }

   #----- Set simulation date-time.
   set Sim(SimYear)     [string range $SimDateTime 0 3] ; #----- Year.
   set Sim(SimMonth)    [string range $SimDateTime 4 5] ; #----- Month.
   set Sim(SimDay)      [string range $SimDateTime 6 7] ; #----- Day.
   set Sim(SimHour)     [string range $SimDateTime 8 9] ; #----- Hour.
   set Sim(SimDateTime) "$Sim(SimYear)$Sim(SimMonth)$Sim(SimDay)$Sim(SimHour)"

   #----- Validate emission time according to available meteorological data files.
   set first [lindex [lindex $Sim(Data) 0] 0]
   set last  [lindex [lindex $Sim(Data) end] 0]

   if { $Sim(AccDateTimeStamp) < $first || $Sim(AccDateTimeStamp) > $last } {

      puts stdout ""
      Debug::TraceProc "MLDP0: Error! Emission date-time is not consistent according to available meteorological data files."
      Debug::TraceProc "MLDP0:        - Emission date-time (stamp) : $Sim(AccDateTime) ($Sim(AccDateTimeStamp))"
      Debug::TraceProc "MLDP0:        - Available meteo times      : $MeteoDateTime"
      Dialog::CreateError .mldp0new "[lindex $Error(DateTimeMetFiles) $GDefs(Lang)]" $GDefs(Lang) 600
      return False

   }

   puts stdout ""
   Debug::TraceProc "MLDP0: Meteorological data files have been extracted successfully."

   return True

}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::File>
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

proc MLDP0::File { Info Path Type Back } {
   variable Sim
   variable Tmp

   Info::Decode ::MLDP0::Tmp $Sim(Info) $Info

   set simpath $Path/[Info::Path $Sim(Info) $Info]
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
# Nom        : <MLDP0::FormatDateTime>
# Creation   : 27 August 2007 - A. Malo - CMC/CMOE
#
# But        : Format date-time.
#
# Parametres :
#
# Retour     :
#  <Date-Time>   : Date-Time in the format YYYY-MM-DD HH:mm:SS UTC.
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP0::FormatDateTime { DateTime } {

   set year  [string range $DateTime 0 3]
   set month [string range $DateTime 4 5]
   set day   [string range $DateTime 6 7]
   set hour  [string range $DateTime 8 9]
   set min   [string range $DateTime 10 11]
   set sec   [string range $DateTime 12 13]

   return "${year}-${month}-${day} ${hour}:${min}:${sec} UTC"

}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::GetMetData>
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

proc MLDP0::GetMetData { } {
   global GDefs
   variable Error
   variable Sim

   #----- Set flag indicating if meteorological data files were retrieve successfully (1) or not (0).
   set Sim(IsMetDataOK) 0

   #----- Set accident release date-time.
   set Sim(AccDateTime)      "$Sim(AccYear)$Sim(AccMonth)$Sim(AccDay)$Sim(AccHour)$Sim(AccMin)00"
   set Sim(AccDateTimeStamp) [fstdstamp fromdate $Sim(AccYear)$Sim(AccMonth)$Sim(AccDay) $Sim(AccHour)$Sim(AccMin)0000]

   #----- Define mixed mode.
   if { $Sim(DBaseDiag) == $Sim(DBaseProg) } {
      set LatestRun -1 ; #----- Ignored the latest run.
   } else {
      set LatestRun 1  ; #----- Take into account the latest run.
   }

   #----- Verify if diagnostic and prognostic databases are defined.
   if { $Sim(DBaseDiag) == "" && $Sim(DBaseProg) == "" } {
      puts stdout ""
      Debug::TraceProc "MLDP0: Error! Diagnostic and prognostic databases are undefined."

      Dialog::CreateError .mldp0new "[lindex $Error(MetDBase) $GDefs(Lang)]" $GDefs(Lang) 600
      return False
   }

   #----- Get available meteorological files.
   set Sim(Data) [MetData::File $Sim(AccDateTimeStamp) $Sim(DBaseDiag) $Sim(DBaseProg) F $LatestRun $Sim(Delta)]

   #----- Verify if number of meteo data files is greater than 0.
   if { [llength $Sim(Data)] < 1 } {
      puts stdout ""
      Debug::TraceProc "MLDP0: Error! Not enough available meteorological data files in database (on host $Sim(Host)) according to emission date-time."
      Debug::TraceProc "MLDP0:        - Emission date-time (stamp) : $Sim(AccDateTime) ($Sim(AccDateTimeStamp))"

      Dialog::CreateError .mldp0new "[lindex $Error(MetFiles) $GDefs(Lang)]" $GDefs(Lang) 600
      return False
   }

   puts stdout ""
   Debug::TraceProc "MLDP0: Available meteorological data files have been found successfully."

   #----- Extract relevant met files according to available meteorological data files and simulation duration.
   if { ![MLDP0::ExtractMetFiles] } {
      return False
   }

   set Sim(IsMetDataOK) 1

   return True
}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::GridDef>
# Creation   : Mai 2000 - J.P. Gauthier - CMC/CMOE
#
# But        : Debuter le mode MLDP0.
#
# Parametres :
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP0::GridDef { } {
   variable Sim
   variable Data

   set Data(Frame) $Page::Data(Frame)
   set Data(VP)    $Viewport::Data(VP)

   fstdfield free GRID

   set Sim(Grid)   [MetData::GridDefinePS [list $Sim(Scale) $Sim(GridRes)] $Sim(NI) $Sim(NJ) $Sim(GridLat) $Sim(GridLon) GRID]

   fstdfield configure GRID -rendergrid 1 -colormap FLDMAPDefault -color black -font XFont10

   Viewport::Assign $Data(Frame) $Data(VP) GRID
   Viewport::UpdateData $Data(Frame)
}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::KBytes2Human>
# Creation   : 16 January 2008 - A. Malo - CMC/CMOE
#
# But        : Convert KBytes to human readable format.
#
# Parametres :
#   <Size>   : Size in KBytes.
#
# Retour     :
#   <Size>   : New size [KBytes|MBytes|GBytes|TBytes].
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP0::KBytes2Human { Size } {

   if { [string is double -strict $Size] == 0 } {
      return "$Size"
   }

   #----- KBytes.
   if { [expr double($Size)] < 1024.0 } {
      set unit "KB"
      set size "$Size"
   }

   #----- Mbytes.
   if { [expr double($Size)] >= 1024.0 && [expr double($Size)] < [expr pow(double(1024.0),2.0)] } {
      set unit "MB"
      set size [expr double($Size)/1024.0]
   }

   #----- Gbytes.
   if { [expr double($Size)] >= [expr pow(double(1024.0),2.0)] && [expr double($Size)] < [expr pow(double(1024.0),3.0)] } {
      set unit "GB"
      set size [expr double($Size)/pow(double(1024.0),2.0)]
   }

   #----- Tbytes.
   if { [expr double($Size)] >= [expr pow(double(1024.0),3.0)] && [expr double($Size)] < [expr pow(double(1024.0),4.0)] } {
      set unit "TB"
      set size [expr double($Size)/pow(double(1024.0),3.0)]
   }

   #----- Format size.
   set size [format "%.1f" $size]

   return "$size $unit"

}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::LaunchJob>
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

proc MLDP0::LaunchJob { } {
   global   GDefs
   variable Sim

   . config -cursor watch
   update idletasks

   set Sim(State) 2
   Info::Set $Sim(PoolFileExp) [Info::Code ::MLDP0::Sim $Sim(Info) :]

   if { $Sim(SrcType) == "accident" } {
      #----- Launch meteorological fields script for RSMC response.
      Debug::TraceProc "MLDP0: Launching RSMC meteorological fields script on local host ($GDefs(Host))."

      set ErrorCode [catch { exec $Sim(MetfieldsScript) $Sim(LocalTmpDir) $Sim(SimDateTime) $Sim(SimDateTime) $Sim(MetInputFileRSMC) >& $Sim(MetfieldsOutFile) & } Message]
   }

   if { $Sim(IsUsingSoumet) } {

      #----- Submit the launching of entire job.
      Debug::TraceProc "MLDP0: Submitting the launching of entire job on $Sim(HostType) host ($Sim(Host))."

      set script "$Sim(TmpDir)/[file tail $Sim(SubmitLaunchScript)]"
      set ErrorCode [catch { exec ssh -l $GDefs(FrontEndUser) -n -x $Sim(Host) $script >& $Sim(SubmitLaunchOutFile) } Message]

      if { $ErrorCode != 0 } {
         Debug::TraceProc "MLDP0: Error! Submitting the launching of entire job on $Sim(HostType) host ($Sim(Host)) failed.\n\n$Message"
         return 0
      }

      Debug::TraceProc "MLDP0: Launching entire job has been submitted successfully on $Sim(HostType) host ($Sim(Host))."

   } else {

      set length 10000

      #----- Launch entire job without using soumet.
      Debug::TraceProc "MLDP0: Launching entire job on $Sim(HostType) host ($Sim(Host))."
      Exp::Launch "$Sim(JobScript)" "[Info::Code ::MLDP0::Sim $Sim(Info) :]" $length $Sim(JobOutFile)

   }

   . config -cursor left_ptr

}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::ModeLeave>
# Creation   : Mai 2000 - J.P. Gauthier - CMC/CMOE
#
# But        : Terminer le mode MLDP0.
#
# Parametres :
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP0::ModeLeave { } {
   variable Data

   if { $Page::Data(ToolMode)=="MLDP0" } {
      SPI::ToolMode SPI Zoom
   }

   Viewport::UnAssign $Data(Frame) $Data(VP) GRID
}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::Move>
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

proc MLDP0::Move { Frame VP } {
   variable Sim
   variable Data

   set Sim(GridLat) $Viewport::Map(LatCursor)
   set Sim(GridLon) $Viewport::Map(LonCursor)

   MLDP0::GridDef
}

proc MLDP0::MoveDone { Canvas VP } { }
proc MLDP0::MoveInit { Canvas VP } { }
proc MLDP0::DrawDone { Canvas VP } { }
proc MLDP0::Draw     { Canvas VP } { }
proc MLDP0::DrawInit { Canvas VP } { }

#----------------------------------------------------------------------------
# Nom        : <MLDP0::ReloadLaunchParams>
# Creation   : 4 October 2007 - A. Malo - CMC/CMOE
#
# But        : Reload launching parameters.
#
# Parametres :
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP0::ReloadLaunchParams { } {
   global   GDefs
   variable Sim
   variable Tmp

   if { $Tmp(Meteo) != $Sim(Meteo) } {

      #----- Set temporary meteo model variable.
      set Tmp(Meteo) $Sim(Meteo)

      #----- Set architecture variables.
      MLDP0::SetArchVariables

      #----- Set list of available hosts and default host.
      MLDP0::SetHosts True

      #----- Set list of available queues and default queue.
      MLDP0::SetQueues True

      #----- Set list of available number of CPUs for meteorological preprocessing.
      MLDP0::SetNbCPUsMeteo True

      #----- Set meteorological database directories according to meteorological model.
      MLDP0::SetMetDataDir

   }

}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::ReloadMetData>
# Creation   : 28 August 2007 - A. Malo - CMC/CMOE
#
# But        : Reload met data.
#
# Parametres :
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP0::ReloadMetData { } {
   global   GDefs
   variable Sim
   variable Tmp

   if { $Tmp(Host) != $Sim(Host) } {

      #----- Set temporary host name variable.
      set Tmp(Host) $Sim(Host)

      #----- Set architecture variables.
      MLDP0::SetArchVariables

      #----- Set list of available queues and default queue.
      MLDP0::SetQueues True

      #----- Set list of available number of CPUs for meteorological preprocessing.
      MLDP0::SetNbCPUsMeteo True

      #----- Set meteorological database directories according to meteorological model.
      MLDP0::SetMetDataDir

      #----- Get meteorological data.
      if { ![MLDP0::GetMetData] } {
         return False
      }

   }

   return True

}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::Result>
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

proc MLDP0::Result { Type } {
   variable Sim
   variable Tmp

   #----- Recuperer les noms de fichiers resultats avec retour sur les precedentes

   set files [File $Exp::Data(SelectSim) [Exp::Path] $Type True]

   Info::Decode ::MLDP0::Tmp $Sim(Info) $Exp::Data(SelectSim)
   SPI::FileOpen NEW FieldBox "(MLDP0) $Tmp(NoExp) $Tmp(Name) ($Type)" "" $files
}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::SetAccidentDate>
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

proc MLDP0::SetAccidentDate { } {
   variable Sim

   set Sim(AccYear)  [clock format $Sim(AccSeconds) -format "%Y" -gmt true] ; #----- Year of accident date.
   set Sim(AccMonth) [clock format $Sim(AccSeconds) -format "%m" -gmt true] ; #----- Month of accident date.
   set Sim(AccDay)   [clock format $Sim(AccSeconds) -format "%d" -gmt true] ; #----- Day of accident date.

}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::SetArchVariables>
# Creation   : 2 October 2007 - A. Malo - CMC/CMOE
#
# But        : Set architecture variables:
#                - Host architecture (Linux, IRIX64 or AIX) ;
#                - Host type (local or remote) ;
#                - Time command (time or hpmcount) ;
#                - Flag indicating if using 'soumet' command (1) or not (0)
#                  to submit job.
#
# Parametres :
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP0::SetArchVariables { } {
   variable Sim
   global   GDefs

   set host $Sim(Host)

   #----- Set host architecture.
   if { $host == "pollux" || $host == "castor" } {
      set Sim(Arch) "IRIX64"
   } elseif { $host == "maia" || $host == "saiph" } {
      set Sim(Arch) "AIX"
   } else {
      set Sim(Arch) "Linux"
   }

   #----- Set host type: local or remote.
   if { $Sim(Arch) == $GDefs(Arch) } {
      set Sim(HostType) "local"
   } else {
      set Sim(HostType) "remote"
   }

   #----- Define time command by default.
   set Sim(Timing) "time"

   #----- Set flag indicating if using 'soumet' command or not.
   switch $Sim(Arch) {
      "Linux"  {
         set Sim(IsUsingSoumet) 0
      }
      "IRIX64" {
         set Sim(IsUsingSoumet) 1
      }
      "AIX"    {
         set Sim(Timing)        "hpmcount"
         set Sim(IsUsingSoumet) 1
      }
   }

}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::SetGridScaleRes>
# Creation   : 22 August 2007 - A. Malo - CMC/CMOE
#
# But        : Set grid scale name and grid scale resolution.
#
# Parametres :
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP0::SetGridScaleRes { } {
   variable Sim

   if { [llength $Sim(Scale)] > 1 } {

      set string $Sim(Scale)
      set Sim(GridRes)  [string trimleft  [lindex $Sim(Scale) 1] "("] ; #----- Grid scale resolution [km].
      set Sim(GridSize) [string trimright [lindex $Sim(Scale) 3] ")"] ; #----- Grid size NIxNJ.
      set Sim(Scale)    [lindex $Sim(Scale) 0]                        ; #----- Grid scale name.

   } else {

      set idx [lsearch -regexp $Sim(ListScale) "$Sim(Scale)*"]
      if { $idx != -1 } {
         set string [lindex $Sim(ListScale) $idx]
         set Sim(GridRes)  [string trimleft  [lindex $string 1] "("] ; #----- Grid scale resolution [km].
         set Sim(GridSize) [string trimright [lindex $string 3] ")"] ; #----- Grid size NIxNJ.
      }

   }

   set idx [string first "x" $Sim(GridSize)]
   if { $idx != -1 } {
      set Sim(NI) [string range $Sim(GridSize) 0 [expr $idx - 1]]
      set Sim(NJ) [string range $Sim(GridSize) [expr $idx + 1] end]
   }

   set Sim(GridResolution) $Sim(GridRes)        ; #----- Grid resolution [km].
   set Sim(GridRes) [expr $Sim(GridRes) * 1000] ; #----- Convert grid resolution from [km] to [m].
}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::SetHosts>
# Creation   : 2 October 2007 - A. Malo - CMC/CMOE
#
# But        : Set default host name and list of available hosts.
#
# Parametres :
#    <Flag>  : Flag indicating if updating list of hosts in interface.
#
# Retour     :
#
# Remarques  : - Use back-end cluster as default machine for
#                GEM Regional and GEM Global models.
#              - Allow user to run MLDP0 model on :
#                  - Linux workstation
#                  - front-end machine
#                  - back-end clusters
#                for GEM Regional and GEM Global models.
#
#----------------------------------------------------------------------------

proc MLDP0::SetHosts { Flag } {
   variable Sim
   variable Tmp
   global   GDefs

   #----- Define host name for running the model.
   if { $Sim(Meteo) == "reg" || $Sim(Meteo) == "glb" } {
      #----- Use back-end cluster as default machine for GEM Regional and GEM Global models.
      set Sim(Host) [lindex $GDefs(BackEnd) 0]
   } else {
      set Sim(Host) $GDefs(Host)
   }

   #----- Set temporary variable for host name.
   set Tmp(Host) $Sim(Host)

   #----- Reset architecture variables.
   MLDP0::SetArchVariables

   #----- Define list of available hosts.
   set Sim(Hosts) $GDefs(Host)

   #----- Add front-end machine to list of available hosts.
   if { [lsearch -exact $Sim(Hosts) $GDefs(FrontEnd)] == -1 } {
      lappend Sim(Hosts) $GDefs(FrontEnd)
   }

   #----- Add back-end machines to list of hosts.
   foreach host $GDefs(BackEnd) {
      lappend Sim(Hosts) $host
   }

   if { $Flag } {
      MLDP0::UpdateListHosts
   }

}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::SetIsotopesInfo>
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

proc MLDP0::SetIsotopesInfo { } {
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
# Nom        : <MLDP0::SetMetDataDir>
# Creation   : 28 August 2007 - A. Malo - CMC/CMOE
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

proc MLDP0::SetMetDataDir { } {
   global GDefs
   variable Sim

   #----- Set met database by default.
   MetData::Path eta $Sim(Meteo) MLDP0::Sim(DBaseDiag) MLDP0::Sim(DBaseProg)

   if { $Sim(Meteo) == "reg" } {

      if { [lsearch -exact $GDefs(BackEnd) $Sim(Host)] != -1 } {
         #----- Set met database on back-end.
         #set Sim(DBaseDiag) "$Sim(Host):/fs/ops/cmo/eer/afse/mldp/dbase/prog/regeta"
         #set Sim(DBaseProg) "$Sim(Host):/fs/ops/cmo/eer/afse/mldp/dbase/prog/regeta"
         set Sim(DBaseDiag) "$Sim(Host):/fs/ops/cmo/gridpt/dbase/trial/regeta2"
         set Sim(DBaseProg) "$Sim(Host):/fs/ops/cmo/gridpt/dbase/prog/regeta"
      } else {
         #----- Set met database on host.
         #set Sim(DBaseDiag) "/data/cmod8/afseeer/mldp/dbase/prog/regeta"
         #set Sim(DBaseProg) "/data/cmod8/afseeer/mldp/dbase/prog/regeta"
         set Sim(DBaseDiag) "/data/gridpt/dbase/trial/regeta2"
         set Sim(DBaseProg) "/data/gridpt/dbase/prog/regeta"
      }

   } elseif { $Sim(Meteo) == "glb" } {

      if { [lsearch -exact $GDefs(BackEnd) $Sim(Host)] != -1 } {
         #----- Set met database on back-end.
         set Sim(DBaseDiag) "$Sim(Host):/fs/ops/cmo/gridpt/dbase/trial/glbeta2"
         set Sim(DBaseProg) "$Sim(Host):/fs/ops/cmo/gridpt/dbase/prog/glbeta"
      } else {
         #----- Set met database on host.
         set Sim(DBaseDiag) "/data/gridpt/dbase/trial/glbeta2"
         set Sim(DBaseProg) "/data/gridpt/dbase/prog/glbeta"
      }

   }

}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::SetNbCPUsMeteo>
# Creation   : 4 October 2007 - A. Malo - CMC/CMOE
#
# But        : Set default number of processes (CPUs) for meteorological
#              preprocessing and list of available number of CPUs.
#
# Parametres :
#    <Flag>  : Flag indicating if updating list of number of CPUs for
#              meteorological preporcesing in interface.
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP0::SetNbCPUsMeteo { Flag } {
   variable Sim

   #----- Set number of CPUs for meteorological preprocessing according to architecture.
   switch $Sim(Arch) {
      "Linux"  {
         set ErrorCode [catch { set Sim(NbCPUsMeteo) [lindex [exec grep "processor" /proc/cpuinfo | wc -l] 0] } Message]
         if { $ErrorCode != 0 } {
            Debug::TraceProc "MLDP0: Warning! Unable to find number of avaible CPUs on $Sim(HostType) host $Sim(Host).\n\n$Message"
            set Sim(NbCPUsMeteo) 1
         }
         set Sim(ListNbCPUsMeteo) 1
         for { set i 2 } { $i <= $Sim(NbCPUsMeteo) } { incr i } {
            lappend Sim(ListNbCPUsMeteo) $i
         }
      }
      "IRIX64" {
         set Sim(NbCPUsMeteo)     1
         set Sim(ListNbCPUsMeteo) { 1 }
      }
      "AIX"    {
         set Sim(NbCPUsMeteo)     16
         set Sim(ListNbCPUsMeteo) { 1 2 4 8 16 }
      }
   }

   if { $Flag } {
      MLDP0::UpdateListNbCPUsMeteo
   }

}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::SetQueues>
# Creation   : 2 October 2007 - A. Malo - CMC/CMOE
#
# But        : Set default queue and list of available queues.
#
# Parametres :
#    <Flag>  : Flag indicating if updating list of queues in interface.
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP0::SetQueues { Flag } {
   variable Sim
   global   GDefs

   set Sim(Queue) "none" ; #----- Default queue.

   #----- Set list of available queues according to architecture, username and host.
   switch $Sim(Arch) {
      "Linux"  {
         set Sim(Queues) $Sim(Queue)
      }
      "IRIX64" {
         set Sim(Queues) $Sim(Queue)
      }
      "AIX" {
         set Sim(Queue)  "development"
         set Sim(Queues) $Sim(Queue)
         if { $Sim(Username) == "afseeer" && $Sim(Host) == "maia" } {
            lappend Sim(Queues) "production"
         }
      }
   }

   if { $Flag } {
      MLDP0::UpdateListQueues
   }

}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::SetSrc>
# Creation   : Fevrier 2003 - A. Malo - CMC/CMOE
#
# But        : Initialiser les coordonnees de la source.
#
# Parametres :
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP0::SetSrc { } {
   variable Sim

   #----- Initialize coordinates of center of grid according to selected source.
   set idx [lsearch -exact $MLDP0::Sim(Names) $MLDP0::Sim(Src)]
   set Sim(GridLat) [format "%.6f" [lindex [lindex $MLDP0::Sim(Pos) $idx] 1]]
   set Sim(GridLon) [format "%.6f" [lindex [lindex $MLDP0::Sim(Pos) $idx] 2]]

   #----- Define polar stereographic grid.
   MLDP0::GridDef
}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::SimInitNew>
# Creation   : Octobre 1999 - J.P. Gauthier - CMC/CMOE
#
# But        : Initialise un tableau de defintions de simulation pour une
#              nouvelle simulation.
#
# Parametres :
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP0::SimInitNew { } {
   global   GDefs
   global   env
   variable Sim
   variable Tmp

   set Sim(Pos)      $Exp::Data(Pos)          ; #----- List of sources containing name and geographical coordinates (lat, lon).
   set Sim(NoExp)    $Exp::Data(No)           ; #----- Experiment number.
   set Sim(Name)     $Exp::Data(Name)         ; #----- Name of experiment.
   set Sim(NbSrc)    [llength $Sim(Pos)]      ; #----- Number of sources.

   set Sim(Names)    {}
   set Sim(CoordSrc) {}
   foreach src $Sim(Pos) {
      lappend Sim(Names) [lindex $src 0]      ; #----- List of source names.
      set lat [format "%.6f" [lindex $src 1]] ; #----- Latitude.
      set lon [format "%.6f" [lindex $src 2]] ; #----- Longitude.
      lappend Sim(CoordSrc) "$lat $lon"       ; #----- List of geographical coordinates (lat, lon).
   }
   set Sim(Src) [lindex [lindex $Sim(Pos) 0] 0]                 ; #----- Name of first source.
   set Sim(Lat) [format "%.6f" [lindex [lindex $Sim(Pos) 0] 1]] ; #----- Latitude of first source.
   set Sim(Lon) [format "%.6f" [lindex [lindex $Sim(Pos) 0] 2]] ; #----- Longitude of first source.

   set Sim(State)     1    ; #----- State of simulation.
   set Sim(NoSim)    -1    ; #----- Simulation number.
   set Sim(NoPrev)   -1    ; #----- Previous simulation number.
   set Sim(Mode)      prog ; #----- Type of meteorological data.

   set Sim(TabPrevNo)       -1 ; #----- Previous tab no.
   set Sim(IsScenarioValid)  0 ; #----- Flag indicating if emission scenario has been validated successfully (1) or not (0).

   #----- Set source type according to experiment data type.
   if { $Exp::Data(Type) == 0 || $Exp::Data(Type) == 3 } {
      #----- Volcano (0) or fire (3) source.
      set Sim(SrcType) "volcano"
   } elseif { $Exp::Data(Type) == 4 } {
      #----- Virus (4) source.
      set Sim(SrcType) "virus"
   } else {
      #----- Nuclear accident (1), CTBT (2), pollutant spill (5), or other (6) sources.
      set Sim(SrcType) "accident"
   }

   set Sim(AccSeconds)   [clock seconds]                                        ; #----- Actual time.
   set Sim(AccYear)      [clock format $Sim(AccSeconds) -format "%Y" -gmt true] ; #----- Year of accident date.
   set Sim(AccMonth)     [clock format $Sim(AccSeconds) -format "%m" -gmt true] ; #----- Month of accident date.
   set Sim(AccDay)       [clock format $Sim(AccSeconds) -format "%d" -gmt true] ; #----- Day of accident date.
   set Sim(AccHour)      [clock format $Sim(AccSeconds) -format "%H" -gmt true] ; #----- Hour of accident date.
   set Sim(AccMin)       [clock format $Sim(AccSeconds) -format "%M" -gmt true] ; #----- Minutes of accident date.

   set Sim(IsResFileSizeChecked) 0                                   ; #----- Flag indicating if results file size has been checked (1) or not (0).
   set Sim(IsMetFileSizeChecked) 0                                   ; #----- Flag indicating if met data file size has been checked (1) or not (0).
   set Sim(Duration)             72                                  ; #----- Simulation duration [hr].
   set Tmp(Duration)             $Sim(Duration)                      ; #----- Temporary variable for simulation duration.
   set Sim(OutputTimeStepMin)    180                                 ; #----- Output time step [min].
   if { $Sim(Duration) <= 48 || $Sim(SrcType) == "volcano" } {
      set Sim(OutputTimeStepMin) 60                                  ; #----- Output time step [min].
   }
   set Tmp(OutputTimeStepMin)    $Sim(OutputTimeStepMin)             ; #----- Temporary variable for output time step.
   set Sim(OutputTimeStepSec)    [expr $Sim(OutputTimeStepMin)*60]   ; #----- Output time step [s].
   set Sim(ModelTimeStepMin)     10                                  ; #----- Internal model time step [min].
   set Sim(ModelTimeStepSec)     [expr $Sim(ModelTimeStepMin)*60]    ; #----- Internal model time step [s].
   set Sim(Event)                [lindex $Sim(ListEvent) 0]          ; #----- Type of event.
   set Sim(Scale)                "MESO"                              ; #----- Grid resolution string.
   set Sim(NI)                   229                                 ; #----- Number of X-grid points.
   set Sim(NJ)                   229                                 ; #----- Number of Y-grid points.
   set Sim(NK)                   25                                  ; #----- Number of vertical levels in the model.
   set Sim(Meteo)                glb                                 ; #----- Meteorological model.
   set Tmp(Meteo)                $Sim(Meteo)                         ; #----- Temporary variable for meteorological model.
   set Sim(Delta)                3                                   ; #----- Time interval for meteorological data files [hr].
   set Tmp(Delta)                $Sim(Delta)                         ; #----- Temporary variable for time interval between met data files.
   set Sim(ListVerticalLevels)   $Sim(OrigListVerticalLevels)        ; #----- List of vertical levels [m].
   set Sim(VerticalLevels)       [lindex $Sim(ListVerticalLevels) 0] ; #----- Vertical levels [m].
   set Sim(VarMesoscale)         1.00                                ; #----- Horizontal wind velocity variance for mesoscale fluctuations [m2/s2].
   set Sim(Timescale)            10800                               ; #----- Lagrangian time scale [s].
   set Sim(ReflectionLevel)      0.9999                              ; #----- Reflection level [hyb|eta|sig].
   set Sim(PrevReflectionLevel)  $Sim(ReflectionLevel)               ; #----- Previous reflection level [hyb|eta|sig].

   set Sim(EmScenario)           "default"                           ; #----- Scenario name.
   set Sim(EmMass)               0.0                                 ; #----- Total mass released.
   set Sim(EmMassMode)           0                                   ; #----- Total mass released mode.
   set Sim(EmList)               {}                                  ; #----- List of emission scenarios.
   set Sim(EmNbIntervals)        0                                   ; #----- Number of emission intervals.
   set Sim(EmEffectiveDuration)  0.0                                 ; #----- Effective emission duration, only release periods [s].
   set Sim(EmTotalDuration)      0.0                                 ; #----- Total emission duration, including release and lull periods [s].
   set Sim(EmNumberParticles)    50000                               ; #----- Number of particles.
   set Sim(EmNbIso)              0                                   ; #----- Number of isotopes.
   set Sim(EmIsoSymbol)          ""                                  ; #----- List of isotopes.
   set Sim(EmIsoQuantity)        ""                                  ; #----- Total release quantity for each isotope.
   set Tmp(EmDensity)            2.500e+12                           ; #----- Density of a particle [microgram/m3].

   #----- Initialize maximum plume height [m] and column radius [m].
   if { $Sim(SrcType) == "volcano" } {        #----- Volcano source type.
      set Sim(EmHeight)    10000.0
      set Sim(EmRadius)    1000.0
      set Sim(EmIsoSymbol) TRACER
      set Sim(EmNbIso)     1
   } elseif { $Sim(SrcType) == "accident" } { #----- Accident source type.
      set Sim(EmHeight) 500.0
      set Sim(EmRadius) 100.0
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
   if { $Sim(SrcType)=="accident" || $Sim(SrcType)=="virus" } {
      set Sim(EmDensity)  $NA
      set Sim(EmMass)     $NA
      set Sim(EmSizeDist) $NA
   }

   if { $Sim(SrcType) == "volcano" } {
      set Sim(EmDensity)       $Tmp(EmDensity) ; #----- Particle density [microgram/m3].
      set Sim(EmSizeDist)      [lindex [lindex $Sim(ListEmSizeDist) $GDefs(Lang)] 3] ; #----- Particle size distribution.
      set Sim(EmSizeDistValue) 3         ; #----- Particle size distribution flag.
      set Sim(EmMassMode)      0         ; #----- Total released mass mode
                                           #----- 0: Empirical Formula of Sparks et al. (1997). For this mode, mass cannot
                                           #-----    be modified manually. ;
                                           #----- 1: Edition. For this mode, mass can be modified manually for specific purposes.
      set Sim(EmMassModeOld)   $Sim(EmMassMode)
      set Sim(EmIsoQuantity)   $NA
   }

   set Sim(EmVerticalDist)      [lindex [lindex $Sim(ListEmVerticalDist) $GDefs(Lang)] 0] ; #----- Plume vertical distribution.
   set Sim(EmVerticalDistValue) 0                                                         ; #----- Plume vertical distribution flag.

   set Sim(Username)         $env(USER)                ; #----- Define username.
   set Sim(EmailAddress)     "$Sim(Username)@ec.gc.ca" ; #----- Username email address.
   set Tmp(EmailAddress)     $Sim(EmailAddress)        ; #----- Default username email address.
   set Sim(ListEmailAddress) $Sim(EmailAddress)        ; #----- List of email addresses.
   set Sim(IsEmailAddress)   0                         ; #----- Flag indicating if sending email to user for monitoring entire job (1) or not (0).
   set Sim(FlagEmailAddress) [lindex [lindex $Sim(ListOptOnOff) $GDefs(Lang)] $Sim(IsEmailAddress)] ; #----- Flag indicating if sending email to user for monitoring entire job (on) or not (off).

   #----- Set grid scale resolution.
   MLDP0::SetGridScaleRes

   #----- Update emission starting time.
   MLDP0::UpdateEmissionStartingTime

   #----- Set list of available hosts and default host.
   MLDP0::SetHosts False

   #----- Set list of available queues and default queue.
   MLDP0::SetQueues False

   #----- Set list of available number of CPUs for meteorological preprocessing.
   MLDP0::SetNbCPUsMeteo False

   #----- Set meteorological data directories according to meteorological model.
   MLDP0::SetMetDataDir

   #----- Read available scenario files.
   if { [MLDP0::EmissionRead] } {
      MLDP0::EmissionSelect
   }

   #----- Initialize coordinates of center of grid according to selected source and
   #----- define polar stereographic grid.
   MLDP0::SetSrc

}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::SimLaunchCheck>
# Creation   : Octobre 1999 - J.P.Gauthier - CMC/CMOE
#
# But        : Effectuer tout les checks et pretraitements et lancer
#              la simulation.
#
# Parametres :
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP0::SimLaunchCheck { } {
   global   GDefs
   variable Sim

   #----- Check if meteorological data files were retrieve successfully.
   if { !$Sim(IsMetDataOK) } {
      if { ![MLDP0::GetMetData] } {
         return 0
      }
   }

   #----- Validate launching queue type.
   if { ![MLDP0::ValidateQueue] } {
      return 0
   }

   #----- Validate email address.
   if { ![MLDP0::ValidateEmail] } {
      return 0
   }

   #----- Define directories and files.
   MLDP0::DefineDirFiles

   #----- Check available disk space.
   if { ![MLDP0::CheckAvailableDiskSpace] } {
      return 0
   }

   #----- Verify input parameters set by the user before launching the model.
   if { [Exp::Params . MLDP0 $Sim(Info)] } {

      destroy .mldp0new ; #----- Destroy interface.

      #----- Close MLDP0 mode.
      MLDP0::ModeLeave

      #----- Create directories on host.
      if { [MLDP0::CreateDirectories] } {

         #----- Create ASCII input files for :
         #-----   - meteorological preprocessing script ;
         #-----   - model script ;
         #-----   - launch script.
         if { [MLDP0::CreateMeteoInputFiles] && [MLDP0::CreateModelInputFile] && [MLDP0::CreateLaunchInputFile] } {

            #----- Create scripts for launching :
            #-----   - meteorological preprocessing ;
            #-----   - model ;
            #-----   - pool info encoding.
            if { [MLDP0::CreateScriptLaunchMeteo] && [MLDP0::CreateScriptLaunchModel] && [MLDP0::CreateScriptLaunchEncode] } {

               #----- Create scripts for launching entire job.
               if { [MLDP0::CreateScriptsLaunchJob] } {

                  #----- Launch job.
                  MLDP0::LaunchJob

               }

            }

         }

      }

      #----- Relire les experiences
      Model::Check 0

   }

}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::SimLaunchInit>
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

proc MLDP0::SimLaunchInit { Tab No } {
   global   GDefs
   variable Sim

   .mldp0new config -cursor watch

   if { $No != 0 && $Sim(TabPrevNo) == 0 } {
      #----- Validate output and model time steps.
      if { ![MLDP0::ValidateNbSrc] || ![MLDP0::ValidateTimeSteps] || ![MLDP0::ValidateSimulationDuration] || ![MLDP0::ValidateOtherParams] } {
         TabFrame::Select $Tab 0
         return 0
      }
   }

   if { $No != 1 && $Sim(TabPrevNo) == 1 } {
      #----- Validate emission column parameters.
      if { ![MLDP0::ValidateEmissionColumn] } {
         TabFrame::Select $Tab 1
         return 0
      }
   }

   if { $No == 2 && $Sim(TabPrevNo) != 2 } {

#      $Tab config -cursor watch
      .mldp0new config -cursor watch

      #----- Validate emission scenario if not validated yet.
      if { !$Sim(IsScenarioValid) } {
         if { ![MLDP0::ValidateDurationsVsModelTimeStep] } {
            TabFrame::Select $Tab 1
            return 0
         }
      }

      #----- Set isotopes information for pool of simulation.
      MLDP0::SetIsotopesInfo

      #----- Get meteorological data according to met database, time interval between files, release accident date-time.
      if { ![MLDP0::GetMetData] } {
         .mldp0new config -cursor left_ptr
         return 0
      }

   }

#   $Tab config -cursor left_ptr
   .mldp0new config -cursor left_ptr

   #----- Set previous tab no.
   set Sim(TabPrevNo) $No
}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::SimSuppress>
# Creation   : Octobre 1999 - J.P. Gauthier - CMC/CMOE
#
# But        : Supprimer une simulation ainsi que toutes ses continuations.
#
# Parametres :
#   <Confirm> : Confirmation de la suppression
#   <Info>    : Identificateur de la simulation
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP0::SimSuppress { Confirm Info } {
   global GDefs
   variable Msg
   variable Lbl
   variable Sim

   . config -cursor watch
   update idletasks

   set path "[Exp::Path]/[Info::Path $Sim(Info) $Info]"

   if { $Confirm } {

      #----- Verifier la validite des parametres.

      set answer [Dialog::CreateDefault . 400 "Message" "[lindex $Msg(SuppressSim) $GDefs(Lang)]\n\n$path" \
        warning 0 [lindex $Lbl(Yes) $GDefs(Lang)] [lindex $Lbl(No) $GDefs(Lang)]]

      if { $answer == 1 } {
         return
      }
   }

   #----- Supprimer la simulation et ses descendants

   MLDP0::SimSuppressResults [Exp::Path] $Info

   #----- Relire les experiences

   Model::Check 0
   . config -cursor left_ptr
}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::SimSuppressResults>
# Creation   : Octobre 1999 - J.P. Gauthier - CMC/CMOE
#
# But        : Supprime les resultats d'une simulation.
#
# Parametres :
#   <Path>   : Path du MLDP0.pool
#   <Info>   : Descriptif de la simultation a supprimer
#
# Retour     :
#   <NoSim>  : Numero de l'experience percedente
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP0::SimSuppressResults { Path Info } {
   global   GDefs
   variable Sim
   variable Msg

   SPI::Progress 0

   #----- Extraire les informations sur l'experience.

   Info::Decode ::MLDP0::Sim $Sim(Info) $Info

   #----- Determiner la localisation du fichier

   set path "$Path/[Info::Path $Sim(Info) $Info]"

   #----- Supprimer les donnees sur le serveur.

   Debug::TraceProc "MLDP0: Suppressing simulation $path"
   SPI::Progress 50 "[lindex $Msg(Suppressing) $GDefs(Lang)] (Server)" Model::Data(Job)

   Exp::Kill    $Info
   Info::Delete $Path/MLDP0.pool $Info
   SPI::Progress 100 [lindex $Msg(SuppressDone) $GDefs(Lang)] Model::Data(Job)
   file delete -force $path

   Debug::TraceProc "MLDP0: Suppressed data on Server."

   #----- Retour du numero de simulation que l'on vient de supprimer

   SPI::Progress 0 "" Model::Data(Job)
   return $Sim(NoSim)
}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::SpeciesDelete>
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

proc MLDP0::SpeciesDelete { Idx } {
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

      MLDP0::UpdateEmissionDurationsTotalQuantityAccident
   }
}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::SpeciesFormat>
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

proc MLDP0::SpeciesFormat { Line } {
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

            MLDP0::UpdateEmissionDurationsTotalQuantityAccident
         } else {
            #----- Display warning message if radioactive half-life is less than 15 minutes.
            Dialog::CreateDefault .mldp0new 500 "[lindex $Lbl(Warning) $GDefs(Lang)]" "[lindex $Warning(HalfLife) $GDefs(Lang)] $symbol." warning 0 "OK"

            puts stderr ""
            puts stderr "WARNING: Isotope $symbol has a radioactive half-life too short (less than 15 minutes) to generate relevant simulation results."
            puts stderr "         This isotope will be ignored."
            puts stderr "         Half-life: $halflife s."
         }
      }
   }
}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::UpdateListHosts>
# Creation   : 2 October 2007 - A. Malo - CMC/CMOE
#
# But        : Update list of available hosts in interface.
#
# Parametres :
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP0::UpdateListHosts { } {
   variable Sim

   #----- Update list of available hosts.
   Option::Set $Sim(HostFrm) $Sim(Hosts)

}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::UpdateListNbCPUsMeteo>
# Creation   : 29 August 2007 - A. Malo - CMC/CMOE
#
# But        : Update list of number of CPUs for meteorological
#              preprocessing.
#
# Parametres :
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP0::UpdateListNbCPUsMeteo { } {
   variable Sim

   #----- Update list of available number of CPUs for meteorological preprocessing.
   Option::Set $Sim(NbCPUsMeteoFrm) $Sim(ListNbCPUsMeteo)

}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::UpdateListQueues>
# Creation   : 2 October 2007 - A. Malo - CMC/CMOE
#
# But        : Update list of available queues in interface.
#
# Parametres :
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP0::UpdateListQueues { } {
   variable Sim

   #----- Enable widget.
   $Sim(QueueFrm).b configure -state normal

   if { $Sim(Queue) == "none" } {

      #----- Disable widget.
      $Sim(QueueFrm).b configure -state disabled

   }

   #----- Update list of available queues.
   Option::Set $Sim(QueueFrm) $Sim(Queues)

}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::UpdateListVerticalLevels>
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

proc MLDP0::UpdateListVerticalLevels { } {
   variable Sim

   #----- Validate reflection level.
   if { ![MLDP0::ValidateReflectionLevel] } {
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

   unset NewListVerticalLevels
}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::ValidateDensity>
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

proc MLDP0::ValidateDensity { } {
   global GDefs
   variable Error
   variable Sim

   #----- Verify if density is positive.

   set number [string is double -strict -failindex idx $Sim(EmDensity)]

   if { $number==0 && $idx==-1 } {
      Dialog::CreateError .mldp0new "[lindex $Error(EmDensityOutRange) $GDefs(Lang)] $Sim(EmDensity) [lindex $Error(UnitDensity) $GDefs(Lang)]." $GDefs(Lang) 600
      return 0
   } elseif { $number== 0 || $Sim(EmDensity)<=0 } {
      Dialog::CreateError .mldp0new "[lindex $Error(EmDensity) $GDefs(Lang)] $Sim(EmDensity) [lindex $Error(UnitDensity) $GDefs(Lang)]" $GDefs(Lang) 600
      return 0
   }

   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::ValidateEmail>
# Creation   : 2 November 2007 - A. Malo - CMC/CMOE
#
# But        : Validate email address.
#
# Parametres :
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP0::ValidateEmail { } {
   global GDefs
   variable Error
   variable Sim
   variable Tmp
   variable Lbl
   variable Warning

   if { $Sim(IsEmailAddress) } {

      set err 0

      if { $Sim(EmailAddress) == "" } {
         set err 1
      } elseif { ![string match "*@ec.gc.ca" $Sim(EmailAddress)] } {
         set err 1
      }

      set idx [string last "@" $Sim(EmailAddress)]

      if { $idx == -1 } {
         set err 1
      } else {
         set name [string range $Sim(EmailAddress) 0 [expr $idx - 1]]
         set listchars { \  , ; : ~ ` ! @ \# $ % ^ & * ? \( \) + / = < > \" \\ [ ] \{ \} | é è ê ë à â ä ì î ï ç É È Ê Ë À Â Ä Ì Î Ï Ç }
         foreach char $listchars {
            if { [string last "${char}" $name] != -1 } {
               set err 1
               break
            }
         }
         if { $name == "" } {
            set err 1
         }
      }

      if { $err } {
         Dialog::CreateError .mldp0new "[lindex $Error(EmailAddress) $GDefs(Lang)] $Sim(EmailAddress)" $GDefs(Lang) 600
         focus $Sim(EmailEnt)
         return 0
      }

      #----- Display warning if email is different than default one.
      if { $Sim(EmailAddress) != $Tmp(EmailAddress) } {
         set answer [Dialog::CreateDefault .mldp0new 400 "[lindex $Lbl(Warning) $GDefs(Lang)]" "[lindex $Warning(EmailAddress) $GDefs(Lang)]\n\n[lindex $Warning(EmailAddress2) $GDefs(Lang)] $Sim(EmailAddress)\n[lindex $Warning(EmailAddress3) $GDefs(Lang)] $Tmp(EmailAddress)" \
                     warning 1 [lindex $Lbl(Yes) $GDefs(Lang)] [lindex $Lbl(No) $GDefs(Lang)]]

         if { $answer } {
            focus $Sim(EmailEnt)
            return 0
         }
      }

   }

   return 1

}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::ValidateEmissionColumn>
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

proc MLDP0::ValidateEmissionColumn { } {
   global GDefs
   variable Sim
   variable Error

   #----- Validate number of particles.
   if { ![MLDP0::ValidateNumberParticles] } {
      focus $Sim(EmissionColumnFrame).nbpart.ent
      return 0
   }

   #----- If source is a volcano type.
   if { $Sim(SrcType) == "volcano" } {

      #----- Validate density.
      if { ![MLDP0::ValidateDensity] } {
         focus $Sim(EmissionColumnFrame).density.ent
         return 0
      }

      #----- Validate total mass released.
      if { ![MLDP0::ValidateMass] } {
         focus $Sim(EmissionColumnFrame).mass.e
         return 0
      }

   }

   #----- Validate maximum plume height.
   if { ![MLDP0::ValidatePlumeHeight] } {
      focus $Sim(EmissionColumnFrame).height.ent
      return 0
   }

   #----- Validate column radius.
   if { ![MLDP0::ValidateRadius] } {
      focus $Sim(EmissionColumnFrame).radius.ent
      return 0
   }

   #----- Compute total mass released.
   if { $Sim(SrcType) == "volcano" } {
      MLDP0::ComputeMass
   }

   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::ValidateMass>
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

proc MLDP0::ValidateMass { } {
   global   GDefs
   variable Error
   variable Sim

   #----- Verify if total mass released is positive.

   set number [string is double -strict -failindex idx $Sim(EmMass)]

   if { $number == 0 && $idx == -1 } {
      Dialog::CreateError .mldp0new "[lindex $Error(MassRange) $GDefs(Lang)] $Sim(EmMass) [lindex $Error(UnitMass) $GDefs(Lang)]" $GDefs(Lang) 600
      return 0
   } elseif { $number == 0 || $Sim(EmMass) <= 0 } {
      Dialog::CreateError .mldp0new "[lindex $Error(Mass) $GDefs(Lang)] $Sim(EmMass) [lindex $Error(UnitMass) $GDefs(Lang)]" $GDefs(Lang) 600
      return 0
   }

   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::ValidateMassInputParams>
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

proc MLDP0::ValidateMassInputParams { } {
   global GDefs
   variable Sim
   variable Error

   #----- Validate particle density.
   if { ![MLDP0::ValidateDensity] } {
      focus $Sim(EmissionColumnFrame).density.ent
      return 0
   }

   #----- Validate maximum plume height.
   if { ![MLDP0::ValidatePlumeHeight] } {
      focus $Sim(EmissionColumnFrame).height.ent
      return 0
   }

   #----- Validate emission durations according to model time step
   #----- if scenario has not been validated yet.
   if { !$Sim(IsScenarioValid) } {
      if { ![MLDP0::ValidateDurationsVsModelTimeStep] } {
         return 0
      }
   }

   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::ValidateNbSrc>
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

proc MLDP0::ValidateNbSrc { } {
   global GDefs
   variable Warning
   variable Lbl
   variable Sim

   #----- Verify if number of sources is less than (or equal to) maximum number of sources.

   if { $Sim(NbSrc) > $Sim(MaxNbSrc) } {
      Dialog::CreateDefault .mldp0new 400 "[lindex $Lbl(Warning) $GDefs(Lang)]" "[lindex $Warning(NbSrc1) $GDefs(Lang)] $Sim(NbSrc).\n[lindex $Warning(NbSrc2) $GDefs(Lang)] $Sim(MaxNbSrc).\n[lindex $Warning(NbSrc3) $GDefs(Lang)] $Sim(MaxNbSrc) [lindex $Warning(NbSrc4) $GDefs(Lang)]" warning 0 "OK"
      set Sim(NbSrc) $Sim(MaxNbSrc) ; #----- Reset number of sources.
   }

   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::ValidateNumberParticles>
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

proc MLDP0::ValidateNumberParticles { } {
   global GDefs
   variable Error
   variable Sim

   #----- Verify if number of particles is positive and greater or equal to 1000.

   set number [string is integer -strict -failindex idx $Sim(EmNumberParticles)]

   if { $number == 0 && $idx == -1 } {
      Dialog::CreateError .mldp0new "[lindex $Error(EmNumberParticlesOutRange) $GDefs(Lang)] $Sim(EmNumberParticles)." $GDefs(Lang) 600
      return 0
   } elseif { $number == 0 || $Sim(EmNumberParticles) < 1000 } {
      Dialog::CreateError .mldp0new "[lindex $Error(EmNumberParticles) $GDefs(Lang)] $Sim(EmNumberParticles)." $GDefs(Lang) 600
      return 0
   }

   #----- Verify if number of particles is an integer multiple number of 1000.

   if { [expr fmod($Sim(EmNumberParticles),1000)] > $Sim(EmEpsilon) } {
      Dialog::CreateError .mldp0new "[lindex $Error(EmNumberParticles2) $GDefs(Lang)] $Sim(EmNumberParticles)." $GDefs(Lang) 600
      return 0
   }

   #----- Verify if number of particles is less than (or equal to) maximum number of particles.

   if { $Sim(EmNumberParticles) > $Sim(EmMaxNumberParticles) } {
      Dialog::CreateError .mldp0new "[lindex $Error(EmNumberParticles3) $GDefs(Lang)] $Sim(EmNumberParticles).\n[lindex $Error(EmNumberParticles4) $GDefs(Lang)] $Sim(EmMaxNumberParticles)." $GDefs(Lang) 600
      return 0
   }

   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::ValidateOtherParams>
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

proc MLDP0::ValidateOtherParams { } {
   global GDefs
   variable Error
   variable Sim

   #----- Validate vertical levels for concentration calculations.
   if { ![MLDP0::ValidateVerticalLevels] } {
      focus $Sim(VerticalLevelsEnt)
      return 0
   }

   #----- Validate horizontal wind velocity variance for mesoscale fluctuations.
   if { ![MLDP0::ValidateVarianceMesoscale] } {
      focus $Sim(VarMesoscaleEnt)
      return 0
   }

   #----- Validate Lagrangian time scale for mesoscale fluctuations.
   if { ![MLDP0::ValidateTimescale] } {
      focus $Sim(TimescaleEnt)
      return 0
   }

   #----- Validate bottom reflection level of particles in the atmosphere.
   if { ![MLDP0::ValidateReflectionLevel] } {
      focus $Sim(ReflectionLevelEnt)
      return 0
   }

   #----- Update list of vertical levels according to reflection level.
   MLDP0::UpdateListVerticalLevels

   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::ValidatePlumeHeight>
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

proc MLDP0::ValidatePlumeHeight { } {
   global GDefs
   variable Error
   variable Sim

   #----- Verify if maximum plume height is positive.

   set number [string is double -strict -failindex idx $Sim(EmHeight)]

   if { $number == 0 && $idx == -1 } {
      Dialog::CreateError .mldp0new "[lindex $Error(HeightRange) $GDefs(Lang)] $Sim(EmHeight) $Error(UnitMeters)" $GDefs(Lang) 600
      return 0
   } elseif { $number == 0 || $Sim(EmHeight) <= 0 } {
      Dialog::CreateError .mldp0new "[lindex $Error(Height) $GDefs(Lang)] $Sim(EmHeight) $Error(UnitMeters)" $GDefs(Lang) 600
      return 0
   }

   #----- Verify if maximum plume height is lower or equal to 30000 meters.

   if { $Sim(EmHeight) > 30000.0 } {
      Dialog::CreateError .mldp0new "[lindex $Error(Height2) $GDefs(Lang)] $Sim(EmHeight) $Error(UnitMeters)" $GDefs(Lang) 600
      return 0
   }

   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::ValidateQueue>
# Creation   : 7 February 2008 - A. Malo - CMC/CMOE
#
# But        : Validate type of submitting queue/class.
#
# Parametres :
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP0::ValidateQueue { } {
   global GDefs
   variable Sim
   variable Lbl
   variable Warning

   if { $Sim(Queue) == "production" } {

      set answer [Dialog::CreateDefault .mldp0new 400 "[lindex $Lbl(Warning) $GDefs(Lang)]" "[lindex $Warning(Queue) $GDefs(Lang)]" \
                      warning 1 [lindex $Lbl(Yes) $GDefs(Lang)] [lindex $Lbl(No) $GDefs(Lang)]]

      if { $answer } {
         set Sim(Queue) "development"
         return 0
      }

   }

   return 1

}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::ValidateRadius>
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

proc MLDP0::ValidateRadius { } {
   global GDefs
   variable Error
   variable Sim

   #----- Verify if column radius is positive.
   set number [string is double -strict -failindex idx $Sim(EmRadius)]

   if { $number == 0 && $idx == -1 } {
      Dialog::CreateError .mldp0new "[lindex $Error(RadiusRange) $GDefs(Lang)] $Sim(EmRadius) $Error(UnitMeters)" $GDefs(Lang) 600
      return 0
   } elseif { $number == 0 || $Sim(EmRadius) < 0 } {
      Dialog::CreateError .mldp0new "[lindex $Error(Radius) $GDefs(Lang)] $Sim(EmRadius) $Error(UnitMeters)" $GDefs(Lang) 600
      return 0
   }

   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::ValidateReflectionLevel>
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

proc MLDP0::ValidateReflectionLevel { } {
   global GDefs
   variable Error
   variable Sim

   #----- Verify if reflection level is positive.

   set number [string is double -strict -failindex idx $Sim(ReflectionLevel)]

   if { $number == 0 && $idx == -1 } {
      Dialog::CreateError .mldp0new "[lindex $Error(ReflectionLevelRange) $GDefs(Lang)] $Sim(ReflectionLevel) $Error(UnitHybEtaSig)" $GDefs(Lang) 600
      return 0
   } elseif { $number == 0 || $Sim(ReflectionLevel) <= 0.0 } {
      Dialog::CreateError .mldp0new "[lindex $Error(ReflectionLevel) $GDefs(Lang)] $Sim(ReflectionLevel) $Error(UnitHybEtaSig)" $GDefs(Lang) 600
      return 0
   }

   #----- Verify if reflection level falls within the range [0.9900, 1.0000].

   if { $Sim(ReflectionLevel) > 1.0 || $Sim(ReflectionLevel) < 0.9900 } {
      Dialog::CreateError .mldp0new "[lindex $Error(ReflectionLevel2) $GDefs(Lang)] $Sim(ReflectionLevel) $Error(UnitHybEtaSig)" $GDefs(Lang) 600
      return 0
   }

   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::ValidateTimescale>
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

proc MLDP0::ValidateTimescale { } {
   global GDefs
   variable Error
   variable Sim

   #----- Verify if time scale is positive.

   set number [string is double -strict -failindex idx $Sim(Timescale)]

   if { $number == 0 && $idx == -1 } {
      Dialog::CreateError .mldp0new "[lindex $Error(TimescaleRange) $GDefs(Lang)] $Sim(Timescale) $Error(UnitSeconds)" $GDefs(Lang) 600
      return 0
   } elseif { $number == 0 || $Sim(Timescale) <= 0.0 } {
      Dialog::CreateError .mldp0new "[lindex $Error(Timescale) $GDefs(Lang)] $Sim(Timescale) $Error(UnitSeconds)" $GDefs(Lang) 600
      return 0
   }

   #----- Verify if timescale is lower or equal to 21600 s.

   if { $Sim(Timescale) > 21600.0 } {
      Dialog::CreateError .mldp0new "[lindex $Error(Timescale2) $GDefs(Lang)] $Sim(Timescale) $Error(UnitSeconds)" $GDefs(Lang) 600
      return 0
   }

   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::ValidateVarianceMesoscale>
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

proc MLDP0::ValidateVarianceMesoscale { } {
   global GDefs
   variable Error
   variable Sim

   #----- Verify if variance is positive.

   set number [string is double -strict -failindex idx $Sim(VarMesoscale)]

   if { $number == 0 && $idx == -1 } {
      Dialog::CreateError .mldp0new "[lindex $Error(VarMesoscaleRange) $GDefs(Lang)] $Sim(VarMesoscale) $Error(UnitM2PS2)" $GDefs(Lang) 600
      return 0
   } elseif { $number == 0 || $Sim(VarMesoscale) < 0.0 } {
      Dialog::CreateError .mldp0new "[lindex $Error(VarMesoscale) $GDefs(Lang)] $Sim(VarMesoscale) $Error(UnitM2PS2)" $GDefs(Lang) 600
      return 0
   }

   #----- Verify if variance is lower or equal to 10.0 m2/s2.

   if { $Sim(VarMesoscale) > 10.0 } {
      Dialog::CreateError .mldp0new "[lindex $Error(VarMesoscale2) $GDefs(Lang)] $Sim(VarMesoscale) $Error(UnitM2PS2)" $GDefs(Lang) 600
      return 0
   }

   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDP0::ValidateVerticalLevels>
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

proc MLDP0::ValidateVerticalLevels { } {
   global GDefs
   variable Error
   variable Sim

   #----- Number of vertical levels for concentration calculations.
   set Sim(NbVerticalLevels) [llength $Sim(VerticalLevels)]

   #----- Verify if number of concentration vertical levels is greater than 1 and
   #----- less than (or equal to) maximum number of vertical levels.
   if { $Sim(NbVerticalLevels) < 2 || $Sim(NbVerticalLevels) > $Sim(MaxNbVerticalLevels) } {
      Dialog::CreateError .mldp0new "[lindex $Error(VerticalLevels1) $GDefs(Lang)][lindex $Error(VerticalLevels2) $GDefs(Lang)] $Sim(NbVerticalLevels).\n[lindex $Error(VerticalLevels3) $GDefs(Lang)] $Sim(MaxNbVerticalLevels).\n[lindex $Error(VerticalLevels4) $GDefs(Lang)] $Sim(VerticalLevels) $Error(UnitMeters)" $GDefs(Lang) 700
      return 0
   }

   #----- Verify if all concentration vertical levels are positive and sorted in increasing order.
   for { set i 0 } { $i < $Sim(NbVerticalLevels) } { incr i } {
      set level [lindex $Sim(VerticalLevels) $i]

      set idx ""
      set number [string is double -strict -failindex idx $level]
      if { $number == 0 && $idx == -1 } {
         Dialog::CreateError .mldp0new "[lindex $Error(VerticalLevelsRange) $GDefs(Lang)] $level $Error(UnitMeters)" $GDefs(Lang) 600
         return 0
      } elseif { $number == 0 || ($number == 1 && $level < 0) } {
         Dialog::CreateError .mldp0new "[lindex $Error(VerticalLevels5) $GDefs(Lang)] $level $Error(UnitMeters)" $GDefs(Lang) 600
         return 0
      }

      if { $i > 0 } {
         set prevlevel [lindex $Sim(VerticalLevels) [expr $i - 1]]
         if { $level <= $prevlevel } {
            Dialog::CreateError .mldp0new "[lindex $Error(VerticalLevels6) $GDefs(Lang)] $Sim(VerticalLevels) $Error(UnitMeters)" $GDefs(Lang) 600
            return 0
         }
      }

   }

   return 1
}
