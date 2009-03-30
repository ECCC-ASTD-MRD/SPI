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
# Nom      : <TRAJECT::ConfigDate>
# Creation : Aout 1998 - J.P. Gauthier - CMC/CMOE
#
# But      : Modifie le label de date d'arrivee/depart selon le cas.
#
# Parametres :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc TRAJECT::ConfigDate { } {
   global GDefs
   variable Sim
   variable Lbl

   if { $Sim(Method) == "Trajectoire" } {
      .trajectnew.opt.frame0.d.date.l config -text "[lindex [lindex $Lbl(Date) 0] $GDefs(Lang)]"
      .trajectnew.opt.frame1.d.b config -text "[lindex [lindex $Lbl(Date) 1] $GDefs(Lang)]"
   } else {
      .trajectnew.opt.frame0.d.date.l config -text "[lindex [lindex $Lbl(Date) 1] $GDefs(Lang)]"
      .trajectnew.opt.frame1.d.b config -text "[lindex [lindex $Lbl(Date) 0] $GDefs(Lang)]"
   }
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

   set Sim(BasePath)          [Exp::Path]
   set Sim(Meteo)             glb

   TRAJECT::SetMetDataDir $Sim(Meteo)

   set Sim(NoExp)  $Exp::Data(No)
   set Sim(Name)   $Exp::Data(Name)
   set Sim(Lat)    $Exp::Data(Lat)
   set Sim(Lon)    $Exp::Data(Lon)
   set Sim(Pos)    $Exp::Data(Pos)

   set Sim(Second)  [clock seconds]
   set Sim(Date)    [clock format $Sim(Second) -format "%a %b %d %Y" -gmt true]
   set Sim(AccHour) [clock format $Sim(Second) -format "%H" -gmt true]
   set Sim(AccMin)  00

   set Sim(Method)   "Trajectoire"
   set Sim(TimeStep) "3600.0"
   set Sim(DateEnd)  ""

   set Sim(DeltaS)   0
   set Sim(DeltaL)   0

   if { $Exp::Data(Type) == 0 } {

      # ----- Ajuster les fonctions de niveau selon le type volcan.

      set Sim(LevelUnit)   "MILLIBARS"

      set Sim(Level1)        700.0
      set Sim(Level2)        500.0
      set Sim(Level3)        250.0
   } else {

      # ----- Ajuster les fonctions de niveau selon tout autre type.

      set Sim(LevelUnit)   "METRES"

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

   SPI::FileOpen NEW TrajBox "$Exp::Data(No) $Exp::Data(Name)" "" [Exp::Path]/[Info::Path $Sim(Info) $Exp::Data(SelectSim)]/traject.points
}

#----------------------------------------------------------------------------
# Nom        : <TRAJECT::SetMetDataDir>
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

proc TRAJECT::SetMetDataDir { MetModel } {
   variable Sim

   MetData::Path eta $MetModel TRAJECT::Sim(DBaseDiag) TRAJECT::Sim(DBaseProg)
}

#-------------------------------------------------------------------------------
# Nom      : <TRAJECT::SimInitLaunch>
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

proc TRAJECT::SimInitLaunch { Path No } {
   global   GDefs
   variable Lbl
   variable Msg
   variable Sim

   if { $No==0 } {
      return
   }

   .trajectnew config -cursor watch
   update idletasks

   .trajectnew.opt.frame1.d.l.list delete 0 end

   set Sim(AccYear)  [clock format $Sim(Second) -format "%Y" -gmt true]
   set Sim(AccMonth) [clock format $Sim(Second) -format "%m" -gmt true]
   set Sim(AccDay)   [clock format $Sim(Second) -format "%d" -gmt true]
   set Sim(Second)   [clock scan "$Sim(AccYear)$Sim(AccMonth)$Sim(AccDay) $Sim(AccHour):00" -gmt True]

   #----- Determine les fichiers necessaires a la simulation.

   set dacc [fstdstamp fromdate $Sim(AccYear)$Sim(AccMonth)$Sim(AccDay) $Sim(AccHour)000000]
   Debug::TraceProc "TRAJECT: Starting date: $dacc"

   if { $Sim(Method) == "Trajectoire" } {
      set Sim(Data) [MetData::File $dacc $Sim(DBaseDiag) $Sim(DBaseProg) F 1 $Sim(Delta)]
   } else {
      set Sim(Data) [MetData::File $dacc $Sim(DBaseDiag) $Sim(DBaseProg) B 1 $Sim(Delta)]
   }

   if { [llength $Sim(Data)] <=1 } {
      Dialog::CreateError . "[lindex $Msg(Files) $GDefs(Lang)]" $GDefs(Lang)
      .trajectnew config -cursor left_ptr
      TabFrame::Select .trajectnew.opt 0
      return
   }

   set dmin [lindex [lindex $Sim(Data) 0] 1]
   set dmax [lindex [lindex $Sim(Data) end] 1]

   Debug::TraceProc "TRAJECT: MetData from $dmin to $dmax"

   #----- On s'assure que la date de l'accident est bien bornee
   #      avec les bornes ci-haut.

   if { $dacc < [lindex [lindex $Sim(Data) 0] 0] || $dacc > [lindex [lindex $Sim(Data) end] 0] } {
      Dialog::CreateError . "[lindex $Msg(Dates) $GDefs(Lang)]\
         [lindex [lindex $Sim(Data) 0] 1] - [lindex [lindex $Sim(Data) end] 1]" $GDefs(Lang)
      .trajectnew config -cursor left_ptr
      TabFrame::Select .trajectnew.opt 0
      return
   }

   #----- Selection de la date de fin.

   if { $Sim(Method) == "Trajectoire" } {
      set datasel  [lrange $Sim(Data) 1 end]
      foreach item $datasel {
          .trajectnew.opt.frame1.d.l.list insert end [lindex $item 1]
      }
      .trajectnew.opt.frame1.d.l.list see end
   } else {
      set datasel [lrange $Sim(Data) 0 [expr [llength $Sim(Data)]-2]]
      foreach item $datasel {
          .trajectnew.opt.frame1.d.l.list insert end [lindex $item 1]
      }
      .trajectnew.opt.frame1.d.l.list see 0
   }
   .trajectnew.opt.frame1.d.l.list selection clear 0 end
   set TRAJECT::Sim(Duration) "??? Hrs"

   .trajectnew config -cursor left_ptr
}

#-------------------------------------------------------------------------------
# Nom      : <TRAJECT::SimLaunch>
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

proc TRAJECT::SimLaunch { } {
   global GDefs
   variable Lbl
   variable Msg
   variable Error
   variable Sim

   if { [lindex $Sim(Duration) 0]=="???" } {
      Dialog::CreateError . [lindex $Error(EndDate) $GDefs(Lang)] $GDefs(Lang)
      return
   }
   set Sim(Duration) [lindex $Sim(Duration) 0]

   #----- Creation du repertoire.
   set Sim(NoSim) [Info::Request $Sim(BasePath)/TRAJECT.pool]
   set Sim(Path)  "$Sim(BasePath)/TRAJECT.$Sim(NoSim).$Sim(AccYear)$Sim(AccMonth)$Sim(AccDay).$Sim(AccHour)00"
   file mkdir $Sim(Path)

   #----- Recuperer le range de fichier selon la selection de l'usager
   set end [.trajectnew.opt.frame1.d.l.list curselection]

   if { $Sim(Method) == "Trajectoire" } {

      #----- On s'assure d'incrementer l'indice associe a la
      #      selection afin de reajuster l'element choisi avec la
      #      'vrai' liste ( $Sim(Data) ) et non ( $datasel ).

      set Sim(Data) [lrange $Sim(Data) 0 [incr end]]
      set Sim(Retro) True
   } else {
      set Sim(Data) [lrange $Sim(Data) $end end]
      set Sim(Retro) False
   }
   set Sim(Mode) [MetData::GetMode $Sim(Data)]

   #----- Get the metdata files
   set files {}
   foreach file $Sim(Data) {
      lappend files [lindex $file 2]
   }
   exec echo [join $files "\n"] > $Sim(Path)/data_std_sim

   #----- Get the levels
   set Sim(Level) ""
   set nblvl 0
   for { set i 1 } { $i<=25 } { incr i } {
      if { $Sim(Level$i)!="" } {
         lappend Sim(Level) "$Sim(Level$i)"
         incr nblvl
      } else {
         break
      }
   }

   #----- Get the particles list
   set parts {}
   foreach pos $Sim(Pos) {
      foreach level $Sim(Level) {
         lappend parts [list [lindex $pos 2] [lindex $pos 1] $level [lindex $pos 0]]
      }
   }

   if { $Sim(Retro) } {
      set mode FORWARD
   } else {
      set mode BACKWARD
   }

   if { $Sim(LevelUnit)=="METRES" } {
      set unit MAGL
   } else {
      set unit PRESSURE
   }

   set Sim(State) 2
   set info [Info::Code ::TRAJECT::Sim $Sim(Info) :]
   Info::Set $Sim(BasePath)/TRAJECT.pool $info

   Debug::TraceProc "TRAJECT: Launching model on : $Sim(Host)"

   #----- Creation du fichier de directives
   set f [open $Sim(Path)/entre w 0644]

   puts $f "'[string toupper $Sim(Name)] '"

   if { $Sim(Retro) } {
      puts $f ".FALSE.  Mode retro-trajectoire ?"
   } else {
      puts $f ".TRUE.   Mode retro-trajectoire ?"
   }

   if { $Sim(LevelUnit) == "METRES" } {
      puts $f "'H'      Niveaux en metres"
   } else {
      puts $f "'P'      Niveaux en millibars"
   }

   puts $f "[expr int($Sim(TimeStep))].0   Pas interne secondes"
   puts $f "[expr $nblvl*[llength $Sim(Pos)]]        Nombre de position de parcelles"
   foreach part $parts {
      puts $f "$part"
   }
   puts $f "$Sim(AccYear)     Annee de l'accident"
   puts $f "$Sim(AccMonth)       Mois de l'accident"
   puts $f "$Sim(AccDay)       Jour de l'accident"
   puts $f "$Sim(AccHour)       Heure de l'accident"

   close $f

   if { $Sim(Host)!=$GDefs(Host) } {

      Info::Set $Sim(Path)/sim.pool $info 1

      #----- Creation du script de lancement
      set f [open $Sim(Path)/traject.sh w 0755]

      puts $f "#!/bin/ksh"
      puts $f ". ~/.profile"
      puts $f "set -x"
      puts $f "arch=`uname -s`"
      puts $f "ulimit -s 500000"
      puts $f "ulimit -m unlimited"
      puts $f "ulimit -d unlimited"
      puts $f "cd $Sim(Path)"
      puts $f "$GDefs(Dir)/Bin/\${arch}/Traj -i entre -fich10 `cat data_std_sim` -tinc $Sim(DeltaS) -tlen $Sim(DeltaL) -o traject.points"
      puts $f "exec $GDefs(Dir)/Script/SimDone.sh sim.pool ../TRAJECT.pool"

      close $f

      if { $Sim(Queue)!="none" } {
         exec soumet++ $Sim(Path)/traject.sh -cm 300M -t 3600 -mach $Sim(Host) -cl $Sim(Queue) -listing $Sim(Path)
      } else {
         exec ssh -l $GDefs(FrontEndUser) -n -x $Sim(Host) "$Sim(Path)/traject.sh > $Sim(Path)/traject.out 2>&1" &
      }
   } else {
      set id [Exp::Id $info]
      simulation create $id -type trajectory
      simulation param $id -title $Sim(Name) -timestep $Sim(TimeStep) -sigt 0.15 -sigb 0.997 -ptop 10.0  \
         -mode $mode -unit $unit -date $Sim(Second) -particles $parts -data $files -output $Sim(Path)/traject.points \
         -tinc $Sim(DeltaS) -tlen $Sim(DeltaL)
      simulation define $id -tag $info -loglevel 3 -logfile $Sim(Path)/traject.log

      #----- Launch simulation within a new thread
      eval set tid1 \[thread::create \{ load $GDefs(Dir)/Shared/$GDefs(Arch)/libTclSim$GDefs(Ext) TclSim\; simulation run $id\}\]

      Exp::ThreadUpdate $id $Sim(BasePath)/TRAJECT.pool [simulation param $id -result]
   }

   #----- Relire les experiences
      destroy .trajectnew
   Model::Check 0
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
