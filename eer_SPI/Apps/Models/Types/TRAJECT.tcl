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

#----------------------------------------------------------------------------
# Nom      : <TRAJECT::PoolInfo>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Extrait les parametres d'une ligne  pool dans une structure.
#
# Parametres :
#   <Pool>   : Ligne non modifiee du fichier pool
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc TRAJECT::PoolInfo { Pool } {

   set Exp::Data(NoSim)  [Info::Strip $Pool NoSim]
   set Exp::Data(NoPrev) [Info::Strip $Pool NoPrev]
   set Exp::Data(State)  [Info::Strip $Pool State]
   set Exp::Data(Desc)   "[Info::Strip $Pool Duration] Hrs [Info::Strip $Pool Meteo][Info::Strip $Pool Mode] ($Exp::Data(NoSim))"
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
# Nom      : <TRAJECT::SimLaunchNew>
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

proc TRAJECT::SimLaunchNew { } {
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

   set date "$Sim(AccYear)$Sim(AccMonth)$Sim(AccDay)"

   if { $date<=19981009 } {
      set Sim(Bin) traj-SIGMA
   } else {
      set Sim(Bin) traj-RK
   }

   #----- Recuperer le range de fichier selon la selection de l'usager

   set end [.trajectnew.opt.frame1.d.l.list curselection]

   if { $Sim(Method) == "Trajectoire" } {

      #----- On s'assure d'incrementer l'indice associe a la
      #      selection afin de reajuster l'element choisi avec la
      #      'vrai' liste ( $Sim(Data) ) et non ( $datasel ).

      incr end

      set Sim(Data) [lrange $Sim(Data) 0 $end]
   } else {
      set Sim(Data) [lrange $Sim(Data) $end end]
      set end
   }

   #----- Creer le fichier de donnees meteo

   set f [open $Sim(Path)/data_std_sim w 0644]
   foreach file $Sim(Data) {
      puts $f [lindex $file 2]
   }
   close $f

   #----- Determiner le mode de l'experience selon les donnees
   #      0=prognosis
   #      1=analysys
   #      2=mixte

   set anal [lsearchsub $Sim(Data) *trial* 2]
   set prog [lsearchsub $Sim(Data) *prog* 2]

   if { $anal!=-1 && $prog!=-1 } {
      set Sim(Mode) mixte
      set mode 2
   } elseif { $anal!=-1 } {
      set Sim(Mode) diag
      set mode 1
   } else {
      set Sim(Mode) prog
      set mode 0
   }

   #----- Creation du fichier de directives

   set f [open $Sim(Path)/entre w 0644]

   puts $f "'[string toupper $Sim(Name)] '"
   puts $f $mode

   if { $Sim(Method) != "Trajectoire" } {
      puts $f ".TRUE.   Mode retro-trajectoire ?"
      set Sim(Retro) true
   } else {
      puts $f ".FALSE.  Mode retro-trajectoire ?"
      set Sim(Retro) false
   }

   if { $Sim(LevelUnit) == "METRES" } {
      puts $f "'H'      Niveaux en metres"
   } else {
      puts $f "'P'      Niveaux en millibars"
   }

   puts $f "[expr int($Sim(TimeStep))].0   Pas interne secondes"

   #----- Recuperer les niveaux valides

   set Sim(Level) ""
   set nblvl 0

   for { set i 1 } { $i <=25 } { incr i } {
      if { $Sim(Level$i) != "" } {
         lappend Sim(Level) "$Sim(Level$i)"
         incr nblvl
      } else {
         break
      }
   }

   puts $f "[expr $nblvl*[llength $Sim(Pos)]]        Nombre de position de parcelles"
   foreach pos $Sim(Pos) {
      foreach level $Sim(Level) {
         puts $f "[lindex $pos 2] [lindex $pos 1] $level [lindex $pos 0]"
      }
   }
   puts $f "$Sim(AccYear)     Annee de l'accident"
   puts $f "$Sim(AccMonth)       Mois de l'accident"
   puts $f "$Sim(AccDay)       Jour de l'accident"
   puts $f "$Sim(AccHour)       Heure de l'accident"

   close $f

   #----- Creer de commandes

   set f [open $Sim(Path)/traject.sh w 0755]

   puts $f "#!/bin/ksh"
   puts $f ". ~/.profile"
   puts $f "set -x"
   puts $f "arch=`uname -s`"
   puts $f "cd $Sim(Path)"

   if { $Sim(DeltaS) > 0 } {
      puts $f "$GDefs(Dir)/Script/TrajectBatch.tcl $Sim(Path) $Sim(Bin) $Sim(DeltaS) $Sim(DeltaL) $Sim(AccYear)$Sim(AccMonth)$Sim(AccDay)$Sim(AccHour) $Sim(Retro)"
   } else {
      puts $f "$GDefs(Dir)/Script/TrajectLaunch.sh $GDefs(Dir)/Bin/\${arch}/$Sim(Bin) $GDefs(Dir)/Script $Sim(Path)"
   }

   puts $f "exec $GDefs(Dir)/Script/SimDone.sh $Sim(Path)/sim.pool $Sim(Path)/../TRAJECT.pool"
   close $f

   set Sim(State) 1
   set pool [Info::Code ::TRAJECT::Sim $Sim(Info) :]
   exec echo "$pool" >> $Sim(Path)/sim.pool

   set Sim(State) 2
   set pool [Info::Code ::TRAJECT::Sim $Sim(Info) :]
   exec echo "$pool" >> $Sim(BasePath)/TRAJECT.pool

   if { $Sim(Queue)!="none" } {
       Debug::TraceProc "TRAJECT: Submitting model on : $Sim(Host)"
       exec soumet++ $Sim(Path)/traject.sh -cm 300M -t 3600 -mach $Sim(Host) -cl $Sim(Queue) -listing $Sim(Path)
   } else {
      Debug::TraceProc "TRAJECT: Launching model on : $Sim(Host)"

      if { $Sim(Host)!=$GDefs(Host) } {
         exec rsh -l $GDefs(FrontEndUser) -n $Sim(Host) "$Sim(Path)/traject.sh > $Sim(Path)/traject.out 2>&1" &
      } else {
         Exp::Launch "$Sim(Path)/traject.sh" "$pool" [expr 36+[llength $Sim(Data)]*240*[llength $Sim(Level)]*[llength $Sim(Pos)]] $Sim(Path)/traject.out
      }
   }

   destroy .trajectnew

   #----- Relire les experiences

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
#   <Pool>    : Identificateur de la simulation
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc TRAJECT::SimSuppress { Confirm Pool } {
   global GDefs
   variable Msg
   variable Lbl
   variable Sim

   . config -cursor watch
   update idletasks

   set path "[Exp::Path]/[Info::Path $Sim(Info)  $Pool]"

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

   Exp::Kill    $Pool
   Info::Delete [Exp::Path]/TRAJECT.pool $Pool
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
