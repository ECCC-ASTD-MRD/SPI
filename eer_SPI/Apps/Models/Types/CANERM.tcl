#============================================================================
# Environnement Canada
# Centre Meteorologique Canadien
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet     : Interface pour la gestion des experiences.
# Fichier    : <CANERM.tcl>
# Creation   : Octobre 1999 - J.P. Gauthier - CMC/CMOE
#
# Description: Description des procedures relatives au module CANERM.
#
# Remarques  :
#
#============================================================================

#----- Fichiers complementaires
source $GDefs(Dir)/Apps/Models/Types/CANERM.txt
source $GDefs(Dir)/Apps/Models/Types/CANERM.ctes
source $GDefs(Dir)/Apps/Models/Types/CANERM.int

#-------------------------------------------------------------------------------
# Nom      : <CANERM::CheckIntensity>
# Creation : Octobre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Cette procedure permet de verifier l'intensite d'une especes.
#
# Parametres :
#   <Combo>  : Identificateur du ComboBox
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc CANERM::CheckIntensity { Combo } {
   global   GDefs
   variable Error
   variable Sim

   if { [catch { set h [expr $Sim(Intensity) + 0.1] }] || $Sim(Intensity) == "" } {
      Dialog::CreateError . [lindex $Error(Intensity) $GDefs(Lang)] $GDefs(Lang)
   } else {
      set index [ComboBox::Index $Combo exact $Sim(Iso)]
      set Sim(IsoRelease) [lreplace $Sim(IsoRelease) $index $index $Sim(Intensity)]
   }
}

#----------------------------------------------------------------------------
# Nom      : <CANERM::File>
# Creation : Octobre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer le nom des fichier resultats.
#
# Parametres :
#    <Info>  : Ligne d'info
#    <Path>  : Path de l'experience
#    <Type>  : Type de fichier resultats
#    <Back>  : Recuperation des fichiers des simulations precedente
# Retour:
#   <Files>  : Liste des path complets des fichiers resultats dans l'ordre croissant
#
# Remarques :
#   - La procedure boucle sur toutes les simulations precedentes en remontant l'arbre
#
#----------------------------------------------------------------------------

proc CANERM::File { Info Path Type Back } {
   variable Sim
   variable Tmp

   set expstd   ""
   set expmetf  ""
   set expmeteo ""
   set exppost  ""
   set carryon  True

   while { $carryon } {

      Info::Decode ::CANERM::Tmp $Sim(Info) $Info

      set nbper [string trimleft $Tmp(NbPer) 0]
      if { $nbper=="" } {
         set nbper 0
      }

      set simpath $Path/[Info::Path $Sim(Info) $Info]
      set simprec "$Tmp(SimYear)$Tmp(SimMonth)$Tmp(SimDay)$Tmp(SimHour)"
      set simlist ""

      switch $Type {
         "metf"    { set expmetf  "$simpath/results/${simprec}_000m $expmetf" }
         "meteo"   { set expmeteo "[glob $simpath/meteo/*] $expmeteo" }
         "post"    { if { [file exists $simpath/results/${simprec}_000p] } { set exppost "$simpath/results/${simprec}_000p $exppost" } }
         "result"  { set hour $Tmp(FreqOut)
                     while { $hour <= [Convert::ModuloVal [expr $nbper*$Tmp(Dt)] $Tmp(FreqOut) +] } {
                        set file ${simprec}_[format "%03i" $hour]c
                        lappend simlist "$simpath/results/$file"
                        incr hour $Tmp(FreqOut)
                     }
                   }
         "all"     {  set expmetf  "$simpath/results/${simprec}_000m $expmetf"
                      if { [file exists $simpath/results/${simprec}_000p] } { set exppost "$simpath/results/${simprec}_000p $exppost" }
                      set hour $Tmp(FreqOut)
                      while { $hour <= [Convert::ModuloVal [expr $nbper*$Tmp(Dt)] $Tmp(FreqOut) +] } {
                         set file ${simprec}_[format "%03i" $hour]c
                         lappend simlist "$simpath/results/$file"
                         incr hour $Tmp(FreqOut)
                      }
                   }
      }
      set expstd "$simlist $expstd"

      if { $Back && $Tmp(NoPrev)!=-1 } {
         set Info [lindex [Info::Find $simpath/../CANERM.pool $Sim(Info) NoSim $Tmp(NoPrev)] 0]
      } else {
         set carryon  False
      }

      #----- On ajoute a premiere heure 00 de toute la serie de simulation
      if { $Type=="result" || $Type=="all" } {
         set expstd "$simpath/results/${simprec}_000c $expstd"
      }
   }
   return "$expmetf $expmeteo $exppost $expstd"
}

#----------------------------------------------------------------------------
# Nom      : <CANERM::GridDef>
# Creation : Mai 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Debuter le mode CANERM.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc CANERM::GridDef { } {
   variable Sim
   variable Data

   set Data(Frame)  $Page::Data(Frame)
   set Data(VP)     $Viewport::Data(VP)
   set Sim(Grid)    [MetData::GridDefinePS $Sim(Scale) $Sim(NI) $Sim(NJ) $Sim(GridLat) $Sim(GridLon) GRID]

   fstdfield configure GRID -rendergrid 1 -colormap FLDMAPDefault -color black -font XFont10

   Viewport::Assign $Data(Frame) $Data(VP) GRID
   Viewport::UpdateData $Data(Frame)
}

#-------------------------------------------------------------------------------
# Nom      : <CANERM::Move>
# Creation : Octobre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectuer la fonction de deplacement de la selection sur la projection.
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <VP>     : Identificateur du Viewport
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc CANERM::Move { Frame VP } {
   variable Sim
   variable Data

   set Sim(GridLat) $Viewport::Map(LatCursor)
   set Sim(GridLon) $Viewport::Map(LonCursor)

   CANERM::GridDef
}

proc CANERM::MoveDone { Frame VP } { }
proc CANERM::MoveInit { Frame VP } { }
proc CANERM::DrawDone { Frame VP } { }
proc CANERM::Draw     { Frame VP } { }
proc CANERM::DrawInit { Frame VP } { }

#----------------------------------------------------------------------------
# Nom      : <CANERM::ModeLeave>
# Creation : Mai 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Terminer le mode CANERM.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc CANERM::ModeLeave { } {
   variable Data

   if { $Page::Data(ToolMode) == "CANERM" } {
      SPI::ToolMode SPI Zoom
   }

   Viewport::UnAssign $Data(Frame) $Data(VP) GRID
}

#-------------------------------------------------------------------------------
# Nom        : <CANERM::SimCreateErsinp>
# Creation   : Octobre 1999 - J.P. Gauthier - CMC/CMOE
#
# But        : Creer le fichier "CANERM.in" contenant les parametres pour CANERM.
#
# Parametres :
#    <Path>  : Path du repertoire "tmp" de la simulation ou creer le "ersinp".
#
# Retour     :
#
# Remarques  :
#
#-------------------------------------------------------------------------------

proc CANERM::SimCreateErsinp { Path } {
   variable Sim
   variable Tmp

   #----- Determiner la date de la permiere simulation

   set Tmp(NoPrev) $Sim(NoPrev)
   set Sim(Date0) $Sim(SimYear)$Sim(SimMonth)$Sim(SimDay)
   set Sim(Time0) $Sim(SimHour)

   while { $Tmp(NoPrev)!=-1 } {
      Info::Decode ::CANERM::Tmp $Sim(Info) [lindex [Info::Find ${Path}/../CANERM.pool $Sim(Info) NoSim $Tmp(NoPrev)] 0]
      set Sim(Date0) $Tmp(SimYear)$Tmp(SimMonth)$Tmp(SimDay)
      set Sim(Time0) $Tmp(SimHour)
   }

   # ----- creer le fichier <ersinp> en s'assurant qu'il n'y ait pas
   #       de blanc apres le signe "=".

   Debug::TraceProc "CANERM: Creating ersinp file: (DT:$Sim(Dt) NbPer:$Sim(NbPer))"
   set f [open $Path/tmp/CANERM.in w]

   puts $f "ITYPE1 =$Sim(IType1)"
   puts $f "ITYPE2 =$Sim(IType2)"
   puts $f "ISCALE =$Sim(Scale)"
   puts $f "LAT    =$Sim(Lat)"
   puts $f "LON    =$Sim(Lon)"
   puts $f "IDATES0=$Sim(Date0)"
   puts $f "ITIMES0=$Sim(Time0)000000"
   puts $f "IDATES =$Sim(SimYear)$Sim(SimMonth)$Sim(SimDay)"
   puts $f "ITIMES =$Sim(SimHour)000000"
   puts $f "IDATEA =$Sim(AccYear)$Sim(AccMonth)$Sim(AccDay)"
   puts $f "ITIMEA =$Sim(AccHour)000000"
   puts $f "DTIS   =$Sim(DTIS)"
   puts $f "DTIN   =$Sim(DTIN)"
   puts $f "ICSDTIN=$Sim(ISauve)"
   puts $f "NTST   =$Sim(NbPer)"
   puts $f "DT     =$Sim(Dt).0"
   puts $f "ZBVAL  =[expr double($Sim(EmHeight))]"
   puts $f "ZBTYP  =$Sim(EmHeightUnit)"
   puts $f "AVAL   =$Sim(FnVert)"
   puts $f "FNTYP  =$Sim(FnTime)"
   puts $f "DELAI  =[expr $Sim(Delai) * 3600].0"
   puts $f "XPERIOD=[expr $Sim(EmDuration) * 3600].0"
   puts $f "NBESP  =[llength $Sim(IsoName)]"

   foreach name $Sim(IsoName) int $Sim(IsoRelease) half $Sim(IsoHalf) dry $Sim(IsoDry) wet $Sim(IsoWet) {
      puts $f "ISO    =[string toupper $name]"
      puts $f "INTENSE=$int"
      puts $f "THALF  =$half"
      puts $f "AFLDS  =$dry"
      puts $f "AFLDW  =$wet"
   }
   close $f
}

#-------------------------------------------------------------------------------
# Nom        : <CANERM::GetMetData>
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

proc CANERM::GetMetData { } {
   global   GDefs
   variable Error
   variable Sim
   variable Msg

   Dialog::CreateWait . [lindex $Msg(MetGet) $GDefs(Lang)] 600

   #----- Get available meteorological files.
   set Sim(AccStamp) [fstdstamp fromdate $Sim(AccYear)$Sim(AccMonth)$Sim(AccDay) $Sim(AccHour)000000]
   set Sim(Data)     [MetData::File $Sim(AccStamp) $Model::Param(DBaseDiag) $Model::Param(DBaseProg) F 0 $Sim(Delta)]
   set Sim(Mode)     [MetData::GetMode $Sim(Data) False]

   if { $Sim(Mode)=="diag" } {
      set Sim(Delta) 6
      set Sim(Data) [MetData::File $Sim(AccStamp) $Model::Param(DBaseDiag) $Model::Param(DBaseProg) F 0 $Sim(Delta)]
   }
   Dialog::DestroyWait

   #----- Extract relevant met files according to available meteorological data files and simulation duration.
   if { ![Model::ParamsMetData CANERM] } {
      return False
   }

   #----- Determiner le Dt
   set t0      [lindex [lindex $Sim(Data) 0] 0]
   set t1      [lindex [lindex $Sim(Data) 1] 0]
   set Sim(Dt) [expr int([fstdstamp diff $t1 $t0])]

   #----- Determiner le nombre de periode
   set t1         [lindex [lindex $Sim(Data) end] 0]
   set Sim(NbPer) [expr int([fstdstamp diff $t1 $t0])/$Sim(Dt)]

   return True
}

#----------------------------------------------------------------------------
# Nom      : <CANERM::SimInitCont>
# Creation : Octobre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialise un tableau de defintions de simulation pour une
#            continuation de simulation.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc CANERM::SimInitCont { } {
   variable Sim

   Info::Decode ::CANERM::Sim $Sim(Info) $Exp::Data(SelectSim)

   set Sim(PGrid)  $Sim(Grid)
   set Sim(NoPrev) $Sim(NoSim)
   set Sim(State)  4
   set Sim(NI)     229
   set Sim(NJ)     229

   set Sim(First)  False
   set Sim(EmHeightUnit) METRES

   fstdfield free GRID
   fstdfield create GRID $Sim(NI) $Sim(NJ) 1
   fstdfield define GRID -NOMVAR GRID
   fstdfield configure GRID -rendergrid 1 -colormap FLDMAPDefault -color black -font XFont10
   fstdfield define GRID -GRTYP [lindex $Sim(Grid) 0] [lindex $Sim(Grid) 3] [lindex $Sim(Grid) 4] [lindex $Sim(Grid) 5] [lindex $Sim(Grid) 6]
   set grid [fstdfield stats GRID -gridpoint [expr $Sim(NI)/2+1] [expr $Sim(NJ)/2+1]]
   set Sim(GridLat) [lindex $grid 0]
   set Sim(GridLon) [lindex $grid 1]
}

#----------------------------------------------------------------------------
# Nom      : <CANERM::SimInitNew>
# Creation : Octobre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialise un tableau de defintions de simulation pour une
#            nouvelle simulation.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc CANERM::SimInitNew { } {
   global   GDefs
   variable Sim

   #-----Cas nucleaire ou volcanique
   if { $Exp::Data(Type) == 0 } {
      set iso [IsoBox::Get VOLCAN]
      set Sim(FnVertDesc) CONSTANT
      set Sim(FnVert)     0.0
      set Sim(ISauve)     1
      set Sim(EmHeight)   10000.0
      set Sim(EmDuration) 1
   } else {
      set iso [IsoBox::Get  Cs-137]
      set Sim(FnVertDesc) CONSTANT
      set Sim(FnVert)     0.0
      set Sim(ISauve)     3
      set Sim(EmHeight)   0.0
      set Sim(EmDuration) 6
   }

   set Sim(AccSeconds)   [clock seconds]
   set Sim(AccYear)      [clock format $Sim(AccSeconds) -format %Y -gmt true]
   set Sim(AccMonth)     [clock format $Sim(AccSeconds) -format %m -gmt true]
   set Sim(AccDay)       [clock format $Sim(AccSeconds) -format %d -gmt true]
   set Sim(AccHour)      [clock format $Sim(AccSeconds) -format %H -gmt true]
   set Sim(AccMin)       00

   set Sim(Version)      "3.1"
   set Sim(State)        "4"
   set Sim(NoExp)        "$Exp::Data(No)"
   set Sim(NoSim)        -1
   set Sim(NoPrev)       -1
   set Sim(FreqOut)      $Sim(FreqOut)
   set Sim(EmHeightUnit) METRES
   set Sim(Event)        "[lindex $Sim(ListEvent) 0]"
   set Sim(SimYear)      "0"
   set Sim(SimMonth)     "0"
   set Sim(SimDay)       "0"
   set Sim(SimHour)      "0"
   set Sim(NbPer)        "0"
   set Sim(Dt)           "3"
   set Sim(DTIN)         "3600.0"
   set Sim(DTIS)         "0000.0"
   set Sim(Mode)         "prog"
   set Sim(Meteo)        "glb"
   set Sim(Delta)         6
   set Sim(Scale)        "FINE"
   set Sim(FnTime)       "CONSTANT"
   set Sim(Delai)        "0"
   set Sim(IType1)       "STATIC"
   set Sim(IType2)       "NPLA"
   set Sim(NI)           229
   set Sim(NJ)           229
   set Sim(IsoNb)        1
   set Sim(IsoName)      [lindex $iso 0]
   set Sim(IsoRelease)   [lindex $iso 1]
   set Sim(IsoHalf)      [lindex $iso 2]
   set Sim(IsoDry)       [lindex $iso 3]
   set Sim(IsoWet)       [lindex $iso 4]
   set Sim(IsoUnit)      [lindex $iso 6]

   set Sim(Src)          [lindex $Exp::Data(Pos) 0]
   set Sim(Pos)          $Exp::Data(Pos)

   set Sim(Name)         $Exp::Data(Name)
   set Sim(Lat)          [lindex $Sim(Src) 1]
   set Sim(Lon)          [lindex $Sim(Src) 2]
   set Sim(GridLat)      $Sim(Lat)
   set Sim(GridLon)      $Sim(Lon)

   set Sim(Iso)          $Sim(IsoName)
   set Sim(Intensity)    $Sim(IsoRelease)
   set Sim(First)        True

   fstdfield free GRID
}

#----------------------------------------------------------------------------
# Nom      : <CANEMR::Launch>
# Creation : Octobre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Lancer un script d'execution sur le backend.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc CANERM::Launch { } {
   global GDefs
   variable Sim

   Model::ParamsMeteoInput CANERM

   set Sim(GridChanged) 0
   set Sim(State)       0

   CANERM::SimResultName
   CANERM::SimCreateErsinp $Sim(Path)

   #----- Continuation
   if { $Sim(NoPrev)!=-1 } {
      CANERM::SimPrevious $Sim(Path)

      #----- On verifie si la grille a changee
      if { $Sim(Grid)!=$Sim(PGrid) } {
         set Sim(GridChanged) 1
      }
   }

   if { $Model::Param(IsUsingSoumet) } {

      if { $Model::Param(Remote) } {

         #----- Create simulation directories on remote host.
         set ErrorCode [catch { exec ssh -l $GDefs(FrontEndUser) -n -x $Model::Param(Host) mkdir -p $Sim(PathRun) $Sim(PathRun)/meteo $Sim(PathRun)/results $Sim(PathRun)/tmp } Message]
         if { $ErrorCode != 0 } {
            Debug::TraceProc "(ERROR) Unable to create simulation directories on $Model::Param(Host).\n\n$Message"
            return False
         }

         #----- Copy needed files on remote host.
         set ErrorCode [catch { eval exec scp -p  $Sim(Path)/tmp/sim.pool [glob $Sim(Path)/tmp/*.in] $GDefs(FrontEndUser)@$Model::Param(Host):$Sim(PathRun)/tmp } Message]
         if { $ErrorCode != 0 } {
            Debug::TraceProc "(ERROR) Copying meteorological preprocessing input file and script on ($Model::Param(Host)) has failed.\n\n$Message"
            return False
         }
         Debug::TraceProc "(INFO) Meteorological preprocessing input files and script have been copied on ($Model::Param(Host)) successfully."
      }

      #----- Launching with soumet.
      exec echo "ssh -l $GDefs(FrontEndUser) -n -x $Model::Param(Host) . ~/.profile\; soumet+++  /home/afsr/005/eer_SPI/eer_SPI/Script/Model.sh -args $Sim(PathRun)/tmp/Model_CANERM.in -mach $Model::Param(Host) \
         -cm 800M -t 3600 -listing $env(HOME)/listings/eer_Experiment -cl $$Model::Param(Queue)" >$Sim(Path)/tmp/soumet.out
      set ErrorCode [catch { exec ssh -l $GDefs(FrontEndUser) -n -x $Model::Param(Host) . ~/.profile\; soumet+++  /home/afsr/005/eer_SPI/eer_SPI/Script/Model.sh -args $Sim(PathRun)/tmp/Model_CANERM.in -mach $Model::Param(Host) \
         -cm 800M -t 3600 -listing $env(HOME)/listings/eer_Experiment -cl $$Model::Param(Queue)" >>$Sim(Path)/tmp/soumet.out" }]

      if { $ErrorCode } {
         Debug::TraceProc "(ERROR) Submitting the job on $Model::Param(Host) failed.\n\n$Message"
         return False
      }
      Debug::TraceProc "(INFO) Job has been submitted successfully on $Model::Param(Host)."
   } else {
      Debug::TraceProc "CANERM: Launching model on : $Model::Param(Host)"
      Exp::Launch "/home/afsr/005/eer_SPI/eer_SPI/Script/Model.sh $Sim(Path)/tmp/Model_CANERM.in" "[Info::Code ::CANERM::Sim $Sim(Info) :]" \
         [expr 147+($Sim(FreqOut)/$Sim(ISauve))*95+($Sim(FreqOut)/$Sim(Dt))*156] \
         $Sim(Path)/tmp/Model_CANERM.out
   }

   Debug::TraceProc "CANERM: Launching simulation request: $$Model::Param(Queue) $Model::Param(Host) $Path"

   exec $GDefs(Dir)/Script/GenerateMetfields.tcl $Path/tmp $Sim(Date0)$Sim(Time0) $Sim(SimYear)$Sim(SimMonth)$Sim(SimDay)$Sim(SimHour) $Path/tmp/data_std_pres.in &

   return True
}

#-------------------------------------------------------------------------------
# Nom        : <CANERM::LaunchParams>
# Creation   : Juin 2001 - J.P.Gauthier - CMC/CMOE
#
# But        : Effectuer toutes les verifications de parametres et recuperer les donnees
#              meteorologiques disponibles pour la simulation.
#
# Parametres :
#   <New>    : Nouvelle simulation ???
#   <Tab>    : Frame parent de l'onglet
#   <No>     : Numero de l'onglet
#
# Retour     :
#
# Remarques  :
#
#-------------------------------------------------------------------------------

proc CANERM::LaunchParams { New Tab No } {
   global   GDefs
   variable Sim
   variable Error

   if { $New } {
      if { $No!=2 } {
         return True
      }

      if { $Sim(IsoNb)==0 } {
         Debug::TraceProc "CANERM: Not enough pollutants"
         Dialog::CreateError .canermnew [lindex $Error(NbIso) $GDefs(Lang)] $GDefs(Lang)
         TabFrame::Select $Tab 0
         return False
      }

      if { $Sim(EmHeight)>30000 } {
         Debug::TraceProc "CANERM: Height over 30000 meters"
         Dialog::CreateError .canermnew [lindex $Error(Height) $GDefs(Lang)] $GDefs(Lang)
         TabFrame::Select $Tab 0
         return False
      }

      #----- On Determine la date de simulation propice
      set Sim(AccYear)  [clock format $Sim(AccSeconds) -format %Y -gmt true]
      set Sim(AccMonth) [clock format $Sim(AccSeconds) -format %m -gmt true]
      set Sim(AccDay)   [clock format $Sim(AccSeconds) -format %d -gmt true]
   } else {
      if { $No!=1 } {
         return True
      }

      #----- On Determine la date de simulation propice
      set nbhour [expr [string trimleft $Sim(NbPer) 0] * $Sim(Dt)]
      set sec    [clock scan "$nbhour hours" -base [clock scan "$Sim(SimYear)$Sim(SimMonth)$Sim(SimDay) $Sim(SimHour):00" -gmt True] -gmt True]

      set Sim(SimYear)  [clock format $sec -format "%Y" -gmt True]
      set Sim(SimMonth) [clock format $sec -format "%m" -gmt True]
      set Sim(SimDay)   [clock format $sec -format "%d" -gmt True]
      set Sim(SimHour)  [clock format $sec -format "%H" -gmt True]
   }

   #----- Get meteorological data according to met database, time interval between files, release accident date-time.
   if { ![GetMetData] } {
      return False
   }
   return True
}

#---------------------------------------------------------------------------
# Nom      : <CANERM::SimRelaunch>
# Creation : Octobre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Lance une job suspendue.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc CANERM::SimRelaunch { } {
   global GDefs
   variable Sim
   variable Msg

   . config -cursor watch
   SPI::Progress 0

   #----- Extraire les informations sur l'experience.
   SPI::Progress 10 [lindex $Msg(JobInfo) $GDefs(Lang)] Model::Data(Job)
   Info::Decode ::CANERM::Sim $Sim(Info) $Exp::Data(SelectSim)

   #----- Determiner la localisation du fichier
   set simpath [Exp::Path]/[Info::Path $Sim(Info) $Exp::Data(SelectSim)]
   set script  [FileBox::Create . $simpath/tmp Load [list {CANERM Launch Script {eer*}}]]

   if { $script=="" } {
      SPI::Progress 0 [lindex $Msg(JobCancel) $GDefs(Lang)] Model::Data(Job)
      return
   }
   set script [lindex [split $script /] end]

   if { [CANERM::SimSusp .] } {

      #----- Lancer le script

      Debug::TraceProc "CANERM: Launching $script"
      SPI::Progress 70 "[lindex $Msg(Launch) $GDefs(Lang)] $script" Model::Data(Job)
      set Sim(State) 2
      Info::Set $simpath/../CANERM.pool [Info::Code ::CANERM::Sim $Sim(Info) :]
      SPI::Progress 100 "" Model::Data(Job)
      CANERM::Launch $simpath/tmp $script $$Model::Param(Queue) $Model::Param(Host)

      #----- Relire les experiences
      Model::Check 0
      SPI::Progress 0 "[lindex $Msg(LaunchDone) $GDefs(Lang)] ($script)" Model::Data(Job)
   } else {
      SPI::Progress 0 "[lindex $Msg(JobCancel) $GDefs(Lang)] ($script)" Model::Data(Job)
   }
   . config -cursor left_ptr
}

#-------------------------------------------------------------------------------
# Nom        : <CANERM::SimPrevious>
# Creation   : Octobre 1999 - J.P.Gauthier - CMC/CMOE
#
# But        : Recuperation des noms de fichiers resultats des simulations
#              precedentes.
#
# Parametres :
#   <Path>   : Path de la simulation
#
# Retour     :
#
# Remarques  :
#
#-------------------------------------------------------------------------------

proc CANERM::SimPrevious { Path } {
   global GDefs
   variable Sim
   variable Tmp
   variable Error

   set resultlist ""
   set result ""

   set info [lindex [Info::Find $Path/../CANERM.pool $Sim(Info) NoSim $Sim(NoPrev) Name $Sim(Name)] 0]
   set res  [glob $Path/../[Info::Path $Sim(Info) $info]]

   set Sim(PrevRestart) "$res/results/$Sim(SimYear)$Sim(SimMonth)$Sim(SimDay)$Sim(SimHour)_000r"

   #----- Parcourir les simulations precedentes pour en obtenir les fichiers resultats
   set Tmp(NoPrev) $Sim(NoPrev)

   while { $info!="" && $Tmp(NoPrev)!=-1 } {

      Info::Decode ::CANERM::Tmp $Sim(Info) $info
      set path [Info::Path $Sim(Info) $info]

      catch { set result [glob $Path/../$path/results/$Tmp(SimYear)$Tmp(SimMonth)$Tmp(SimDay)$Tmp(SimHour)_*c] }

      if { [llength $result] == 0 } {
         Dialog::CreateDefault . 700 "Message" "$path\n\n[lindex $Error(Previous) $GDefs(Lang)]" warning 0 Ok
      }
      set resultlist "[lsort $result] $resultlist "
      set info [lindex [Info::Find $Path/../CANERM.pool $Sim(Info) NoSim $Tmp(NoPrev)] 0]
   }

   #----- Inscrire la liste des resultats precedent dans le repertoire temporaire.
   exec echo "$resultlist" > $Path/tmp/previous
   Debug::TraceProc "CANERM: Previous files: $resultlist"
}

#-------------------------------------------------------------------------------
# Nom        : <CANERM::SimResultName>
# Creation   : Octobre 1999 - J.P. Gauthier - CMC/CMOE
#
# But        : Permet de creer le nom des fichiers resultats et du fichier 'restart'.
#
# Parametres :
#
# Retour     :
#
# Remarques  :
#
#-------------------------------------------------------------------------------

proc CANERM::SimResultName { } {
   variable Sim

   #----- Determine le nom des fichiers resultats.

   set Sim(Tape40)   $Sim(SimYear)$Sim(SimMonth)$Sim(SimDay)$Sim(SimHour)

   #----- Determine le nom du fichier 'restart'.
   set nbhour [expr $Sim(NbPer) * $Sim(Dt)]
   set sec    [clock scan "$nbhour hours" -base [clock scan "$Sim(SimYear)$Sim(SimMonth)$Sim(SimDay) $Sim(SimHour):00" -gmt True] -gmt True]
   set Sim(Restart) [clock format $sec -format "%Y%m%d%H" -gmt True]
}

#---------------------------------------------------------------------------
# Nom      : <CANERM::Result>
# Creation : Juillet 1998 - J.P. Gauthier - CMC/CMOE
#
# But      : Recuperation des resultats et affichage par SPI.
#
# Parametres :
#    <Type>  : Type de fichier (standard ou post)
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc CANERM::Result { Type } {
   variable Sim
   variable Tmp

   #----- Recuperer les noms de fichiers resultats avec retour sur les precedentes
   set files [File $Exp::Data(SelectSim) [Exp::Path] $Type True]

   Info::Decode ::CANERM::Tmp $Sim(Info) $Exp::Data(SelectSim)
   SPI::FileOpen NEW FieldBox "(CANERM) $Tmp(NoExp) $Tmp(Name) ($Type)" "" $files
}

#-------------------------------------------------------------------------------
# Nom      : <CANERM::SimSuppress>
# Creation : Octobre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Supprimer une simulation ainsi que toutes ses continuations.
#
# Parametres  :
#   <Confirm> : Confirmation de la suppression
#   <Info>    : Identificateur de la simulation
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc CANERM::SimSuppress { Confirm Info } {
   global GDefs
   variable Msg
   variable Lbl
   variable Sim

   . config -cursor watch
   update idletasks

   if { $Confirm } {

      #----- Verifier la validitee des parametres
      set answer [Dialog::CreateDefault . 400 "Message" "[lindex $Msg(SuppressSim) $GDefs(Lang)]\n\n" \
        warning 0 [lindex $Lbl(Yes) $GDefs(Lang)] [lindex $Lbl(No) $GDefs(Lang)]]

      if { $answer == 1 } {
         return
      }
   }

   #----- Supprimer la simulation et ses descendants
   set path [Exp::Path]

   while { $Info!="" } {

      set nosim [CANERM::SimSuppressResults $path $Info]
      set Info  [lindex [Info::Find $path/CANERM.pool $Sim(Info) NoPrev $nosim] 0]
   }

   #----- Relire les experiences
   Model::Check 0
   . config -cursor left_ptr
}

#---------------------------------------------------------------------------
# Nom      : <CANERM::SimSuppressResults>
# Creation : Octobre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Supprime les resultats d'une simulation.
#
# Parametres :
#   <Path>   : Path du CANERM.pool
#   <Info>   : Descriptif de la simultation a supprimer
#
# Retour:
#   <NoSim>  : Numero de l'experience percedente
#
# Remarques :
#
#----------------------------------------------------------------------------

proc CANERM::SimSuppressResults { Path Info } {
   global   GDefs
   variable Msg
   variable Sim

   SPI::Progress 0

   Exp::Kill $Info

   #----- Extraire les informations sur l'experience.
   Info::Decode ::CANERM::Sim $Sim(Info) $Info

   #----- Determiner la localisation du fichier
   set path [Info::Path $Sim(Info) $Info]

   #----- Supprimer les donnees sur le serveur.
   Debug::TraceProc "CANERM: Suppressing simulation $path"
   SPI::Progress 50 "[lindex $Msg(Suppressing) $GDefs(Lang)] (FrontEnd)" Model::Data(Job)

   SPI::Progress 100 [lindex $Msg(SuppressDone) $GDefs(Lang)] Model::Data(Job)
   file delete -force $Path/$path
   Debug::TraceProc "CANERM: Suppressed data on Server."

   Info::Delete $Path/CANERM.pool $Info

   #----- Retour du numero de simulation que l'on vient de supprimer
   SPI::Progress 0 "" Model::Data(Job)
   return $Sim(NoSim)
}

#-------------------------------------------------------------------------------
# Nom      : <CANERM::SpeciesDelete>
# Creation : Novembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Retire une espece de la liste selectionne.
#
# Parametres :
#   <Frame>  : Identificateur du frame
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc CANERM::SpeciesDelete { Frame } {
   global   GDefs
   variable Sim

   set index [ComboBox::Del $Frame.type.entry.polluant $Sim(Iso)]

   #----- Verifier si l'item a bien ete supprimer
   if { $index != -1 } {
      set Sim(IsoName)    [lreplace $Sim(IsoName) $index $index]
      set Sim(IsoRelease) [lreplace $Sim(IsoRelease) $index $index]
      set Sim(IsoUnit)    [lreplace $Sim(IsoUnit) $index $index]
      set Sim(IsoHalf)    [lreplace $Sim(IsoHalf) $index $index]
      set Sim(IsoDry)     [lreplace $Sim(IsoDry) $index $index]
      set Sim(IsoWet)     [lreplace $Sim(IsoWet) $index $index]

      #----- Si on est pas au maximum de polluant
      if { [incr Sim(IsoNb) -1] < $CANERM::Sim(MaxIso) } {
         $Frame.type.entry.nb config -bg $GDefs(ColorLight)
      }

      #----- Afficher le premier element de la liste.
      if { !$Sim(IsoNb) } {
         $Frame.type.entry.intensity config -state disabled
      }
      set Sim(Intensity) [lindex $Sim(IsoRelease) 0]
      set Sim(Iso)       [lindex $Sim(IsoName) 0]
   }
}


#-------------------------------------------------------------------------------
# Nom      : <CANERM::SpeciesFormat>
# Creation : Aout 1997 - J.P. Gauthier - CMC/CMOE
#
# But      : Met en format la ligne retourne par le module de selection d'especes
#
# Parametres :
#   <Frame>  : Identificateur du frame
#   <Line>   : Ligne de definiton d'un isotope
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc CANERM::SpeciesFormat { Frame Line } {
   variable Sim

   #----- Verification de nombre de parametres inclus dans la ligne
   if { [llength $Line] == 11 } {

      if { [ComboBox::Add $Frame.type.entry.polluant [lindex $Line 0]] != -1 } {
         set Sim(Iso)       [lindex $Line 0]
         set Sim(Intensity) 1.00e+00
         if { $Sim(Iso) == "VOLCAN" || [regexp "TRACER" $Sim(Iso)] } {
            set Sim(Intensity) 1.00e+18
         }

         set IsoUnit "BQ"
         if { $Sim(Iso) == "VOLCAN" } {
            set IsoUnit "MICRO_G"
         } elseif { [regexp "TRACER" $Sim(Iso)] } {
            set IsoUnit "UNITS"
         }
         lappend Sim(IsoUnit)    $IsoUnit
         lappend Sim(IsoName)    $Sim(Iso)
         lappend Sim(IsoRelease) $Sim(Intensity)
         lappend Sim(IsoHalf)    [lindex $Line 5]
         lappend Sim(IsoDry)     [lindex $Line 6]
         lappend Sim(IsoWet)     [lindex $Line 7]

         $Frame.type.entry.intensity config -state normal

         if { [incr Sim(IsoNb)] == $Sim(MaxIso) } {
            $Frame.type.entry.nb config -bg red
         }
      }
   }
}
