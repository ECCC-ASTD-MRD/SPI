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
# Nom      : <TRAJECT::Close>
# Creation : Avril 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Fermer l'interface et nettoyer les repertoires.
#
# Parametres :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc TRAJECT::Close { } {
   global GDefs
   variable Sim

   destroy .trajectnew
}

#-------------------------------------------------------------------------------
# Nom      : <TRAJECT::InitNew>
# Creation : Octobre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialisation des parametres de la trajectoire.
#
# Parametres :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc TRAJECT::InitNew { } {
   variable Sim

   TabFrame::Select .trajectnew.opt 0

   set Sim(NoExp)  $Exp::Data(No)
   set Sim(Name)   $Exp::Data(Name)
   set Sim(Lat)    $Exp::Data(Lat)
   set Sim(Lon)    $Exp::Data(Lon)
   set Sim(Pos)    $Exp::Data(Pos)
   set Sim(No)     -1
   set Sim(Last)   -1

   set Sim(Second)  [clock seconds]
   set Sim(Date)    [clock format $Sim(Second) -format "%a %b %d %Y" -gmt true]
   set Sim(AccHour) [clock format $Sim(Second) -format "%H" -gmt true]
   set Sim(AccMin)  00

   set Sim(Method)   "Trajectoire"
   set Sim(TimeStep) "3600.0"
   set Sim(DateEnd)  ""

   set Sim(BatchStart) 0
   set Sim(Duration)   72

   if { $Exp::Data(Type) == 0 } {

      #----- Ajuster les fonctions de niveau selon le type volcan.
      set Sim(LevelUnit)   "MILLIBARS"
      set Sim(Level1)        700.0
      set Sim(Level2)        500.0
      set Sim(Level3)        250.0
   } else {

      #----- Ajuster les fonctions de niveau selon tout autre type.
      set Sim(LevelUnit)   "METRES"
      set Sim(Level1)       500.0
      set Sim(Level2)       1500.0
      set Sim(Level3)       3000.0
   }

   for { set i 4 } { $i <= 25 } { incr i } {
       set Sim(Level$i)     ""
   }

   set Model::Param(Host) [lindex $Sim(Hosts) 0]
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
   SPI::FileOpen NEW TrajBox "$Exp::Data(No) $Exp::Data(Name)" "" [Exp::Path]/[Info::Path $Sim(Info) $Exp::Data(SelectSim)]/results/traject.points
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

   Dialog::CreateWait . [lindex $Msg(MetGet) $GDefs(Lang)] 600

   set Sim(AccStamp) [fstdstamp fromdate $Sim(AccYear)$Sim(AccMonth)$Sim(AccDay) $Sim(AccHour)000000]

   if { $Sim(Method) == "Trajectoire" } {
      set Sim(Data) [MetData::File $Sim(AccStamp) $Model::Param(DBaseDiag) $Model::Param(DBaseProg) F 1 $Sim(Delta)]
   } else {
      set Sim(Data) [MetData::File $Sim(AccStamp) $Model::Param(DBaseDiag) $Model::Param(DBaseProg) B 1 $Sim(Delta)]
   }
   set Sim(Mode) [MetData::GetMode $Sim(Data) False]
   Dialog::DestroyWait

   #----- Get the levels
   set Sim(Level) ""
   for { set i 1 } { $i<=25 } { incr i } {
      if { $Sim(Level$i)!="" } {
         lappend Sim(Level) "$Sim(Level$i)"
      } else {
         break
      }
   }

   #----- Extract relevant met files according to available meteorological data files and simulation duration.
   return [Model::ParamsMetData TRAJECT]
}

#-------------------------------------------------------------------------------
# Nom      : <TRAJECT::LaunchParams>
# Creation : Avril 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialiser le lancement en recuperant les dates limites et en verifiant
#            les selections de l'usager.
#
# Parametres :
#   <Path>   : Identificateur de la fenetre d'onglet
#   <No>     : Numero de l'onglet selectionne
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc TRAJECT::LaunchParams { Path No } {
   global   GDefs
   variable Lbl
   variable Msg
   variable Sim

   if { $No==0 } {
      return True
   }

   set Sim(AccYear)  [clock format $Sim(Second) -format "%Y" -gmt true]
   set Sim(AccMonth) [clock format $Sim(Second) -format "%m" -gmt true]
   set Sim(AccDay)   [clock format $Sim(Second) -format "%d" -gmt true]
   set Sim(Second)   [clock scan "$Sim(AccYear)$Sim(AccMonth)$Sim(AccDay) $Sim(AccHour):00" -gmt True]

   if { $Sim(Method) == "Trajectoire" } {
      set Sim(Retro) False
   } else {
      set Sim(Retro) True
   }

   #----- Get meteorological data according to met database, time interval between files, release accident date-time.
   if { ![GetMetData] } {
      return False
   }
   return True
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

   if { $Sim(Retro) } {
      set mode BACKWARD
   } else {
      set mode FORWARD
   }

   #----- Creer le fichier de donnees meteo
   set f [open $Sim(Path)/tmp/data_std_eta.in w 0644]
   puts $f $Sim(MeteoDataFiles)
   close $f

   #----- Get the particles list
   set parts {}
   foreach pos $Sim(Pos) {
      foreach level $Sim(Level) {
         lappend parts [list [lindex $pos 2] [lindex $pos 1] $level [lindex $pos 0]]
      }
   }

   if { $Sim(LevelUnit)=="METRES" } {
      set unit MAGL
   } else {
      set unit PRESSURE
   }

   #----- Creation du fichier de directives
   set f [open $Sim(Path)/tmp/TRAJECT.in w 0644]
      puts $f "'[string toupper $Sim(Name)] '"

      if { $Sim(Retro) } {
         puts $f ".TRUE.   Mode retro-trajectoire ?"
      } else {
         puts $f ".FALSE.  Mode retro-trajectoire ?"
      }

      if { $Sim(LevelUnit) == "METRES" } {
         puts $f "'H'      Niveaux en metres"
      } else {
         puts $f "'P'      Niveaux en millibars"
      }

      puts $f "[expr int($Sim(TimeStep))].0   Pas interne secondes"
      puts $f "[expr [llength $Sim(Level)]*[llength $Sim(Pos)]]        Nombre de position de parcelles"
      foreach part $parts {
         puts $f "$part"
      }
      puts $f "$Sim(AccYear)     Annee de l'accident"
      puts $f "$Sim(AccMonth)       Mois de l'accident"
      puts $f "$Sim(AccDay)       Jour de l'accident"
      puts $f "$Sim(AccHour)       Heure de l'accident"
   close $f

   if { $Model::Param(IsUsingSoumet) } {

      #----- Run will be remote, setup what's needed on remote host.
      if { $Model::Param(Remote) } {

         #----- Create simulation directories .
         set ErrorCode [catch { exec ssh -l $GDefs(FrontEndUser) -n -x $Model::Param(Host) mkdir -p $Sim(PathRun) $Sim(PathRun)/meteo $Sim(PathRun)/results $Sim(PathRun)/tmp } Message]
         if { $ErrorCode != 0 } {
            Debug::TraceProc "(ERROR) Unable to create simulation directories on $Model::Param(Host).\n\n$Message"
            return False
         }

         #----- Copy needed files.
         set ErrorCode [catch { eval exec scp -p $Sim(Path)/tmp/sim.pool [glob $Sim(Path)/tmp/*.in] $GDefs(FrontEndUser)@$Model::Param(Host):$Sim(PathRun)/tmp } Message]
         if { $ErrorCode != 0 } {
            Debug::TraceProc "(ERROR) Copying meteorological preprocessing input file and script on ($Model::Param(Host)) has failed.\n\n$Message"
            return False
         }
         Debug::TraceProc "(INFO) Meteorological preprocessing input files and script have been copied on ($Model::Param(Host)) successfully."
      }

      set file [open $Sim(Path)/tmp/Model_TRAJECT.in w 0644]

      puts $file "#----- Logger specific parameters"
      puts $file "LOG_MAIL=$Model::Param(EmailAddress)"
      puts $file "LOG_MAILTITLE=\"$Sim(Model) (SPI)\""
      puts $file "LOG_FILE=$Sim(PathRun)/tmp/Model_TRAJ.out"
      puts $file "LOG_LEVEL=INFO"
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
      puts $file "MODEL_POST=0"
      puts $file "MODEL_CLEAN=1"
      puts $file "MODEL_TRACE=$GDefs(DirData)/trace"
      puts $file ""
      puts $file "#----- Model specific parameters"
      puts $file "TRAJECT_METEO=\"$Sim(MeteoDataFiles)\""
      puts $file "TRAJECT_INC=$Sim(BatchStart)"
      puts $file "TRAJECT_LEN=$Sim(Duration)"
      puts $file "TRAJECT_INPUT=$Sim(PathRun)/tmp/$Sim(Model).in"
      puts $file "TRAJECT_RESULT=$Sim(PathRun)/results/traject.points"
      puts $file "TRAJECT_LOGLEVEL=INFO"

      close $file

      exec echo "soumet+++  $env(EER_DIRSCRIPT)/Model.sh -args $Sim(PathRun)/tmp/Model_TRAJECT.in -mach $Model::Param(Host) \
         -t 3600 -cm !G -listing $env(HOME)/listings/eer_Experiment -cl $Model::Param(Queue)" >$Sim(Path)/tmp/soumet.out
      set ErrorCode [catch { exec soumet+++  $env(EER_DIRSCRIPT)/Model.sh -args $Sim(PathRun)/tmp/Model_TRAJECT.in -mach $Model::Param(Host) \
         -t 3600 -cm 1G -listing $env(HOME)/listings/eer_Experiment -cl $Model::Param(Queue) >>$Sim(Path)/tmp/soumet.out } Message]
   } else {
      set id [Exp::Id $info]
      simulation create $id -type trajectory
      simulation param $id -title $Sim(Name) -timestep $Sim(TimeStep) -sigt 0.15 -sigb 0.997 -ptop 10.0  \
         -mode $mode -unit $unit -date $Sim(Second) -particles $parts -data $Sim(MeteoDataFiles) -output $Sim(Path)/results/traject.points \
         -tinc $Sim(BatchStart) -tlen $Sim(Duration)
      simulation define $id -tag $info -loglevel 3 -logfile $Sim(Path)/tmp/traject.log

      #----- Launch simulation within a new thread
      eval set tid1 \[thread::create \{ load $GDefs(Dir)/Shared/$GDefs(Arch)/libTclSim$GDefs(Ext) TclSim\; simulation run $id\}\]

      Exp::ThreadUpdate $id $GDefs(DirData)/$Sim(NoExp)_$Sim(Name)/TRAJECT.pool [simulation param $id -result]
   }
   return True
}

#-------------------------------------------------------------------------------
# Nom      : <TRAJECT::SimSuppress>
# Creation : Octobre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Supprimer une simulation.
#
# Parametres  :
#   <Confirm> : Confirmation de la suppression
#   <Info>    : Identificateur de la simulation
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc TRAJECT::SimSuppress { Confirm Info } {
   global GDefs
   variable Msg
   variable Lbl
   variable Sim

   . config -cursor watch
   update idletasks

   set path "[Exp::Path]/[Info::Path $Sim(Info)  $Info]"

   if { $Confirm } {

      #----- Verifier la validitee des parametres

      set answer [Dialog::CreateDefault . 400 "Message" "[lindex $Msg(SuppressSim) $GDefs(Lang)]\n\n$path" \
         warning 0 [lindex $Lbl(Yes) $GDefs(Lang)] [lindex $Lbl(No) $GDefs(Lang)]]

      if { $answer == 1 } {
         return
      }
   }

   #----- Supprimer la simulation et ses descendants

   Debug::TraceProc "TRAJECT: Suppressing trajectory: $path"

   Exp::ThreadKill [Exp::Id $Info]
   Info::Delete [Exp::Path]/TRAJECT.pool $Info
   file delete -force $path

   #----- Relire les experiences

   . config -cursor left_ptr
   Model::Check 0
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

      set Sim(LevelUnit) "METRES"

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
