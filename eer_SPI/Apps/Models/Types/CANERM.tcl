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
      Dialog::Error . $Error(Intensity)
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

      Info::Decode ::CANERM::Tmp $Info

      set nbper [string trimleft $Tmp(NbPer) 0]
      if { $nbper=="" } {
         set nbper 0
      }

      set simpath $Path/[Info::Path $Info]
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
         set Info [lindex [Info::Find $simpath/../CANERM.pool CANERM NoSim $Tmp(NoPrev)] 0]
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

   Model::ParamsGridDefine CANERM
}

proc CANERM::MoveDone { Frame VP } { }
proc CANERM::MoveInit { Frame VP } { }
proc CANERM::DrawDone { Frame VP } { }
proc CANERM::Draw     { Frame VP } { }
proc CANERM::DrawInit { Frame VP } { }

#-------------------------------------------------------------------------------
# Nom        : <CANERM::CreateModelInput>
# Creation   : Octobre 1999 - J.P. Gauthier - CMC/CMOE
#
# But        : Creer le fichier "ersinp" contenant les parametres pour CANERM.
#
# Parametres :
#
# Retour     :
#
# Remarques  :
#
#-------------------------------------------------------------------------------

proc CANERM::CreateModelInput { } {
   variable Sim
   variable Tmp

   #----- Determiner le nom du restart
   set nbhour [expr $Sim(NbPer) * $Sim(Dt)]
   set sec    [clock scan "$nbhour hours" -base [clock scan "$Sim(SimYear)$Sim(SimMonth)$Sim(SimDay) $Sim(SimHour):00" -gmt True] -gmt True]
   set Sim(Restart) [clock format $sec -format "%Y%m%d%H" -gmt True]

   #----- Determiner la date de la permiere simulation
   set Tmp(NoPrev) $Sim(NoPrev)
   set Sim(Date0) $Sim(SimYear)$Sim(SimMonth)$Sim(SimDay)
   set Sim(Time0) $Sim(SimHour)

   while { $Tmp(NoPrev)!=-1 } {
      Info::Decode ::CANERM::Tmp [lindex [Info::Find $Sim(Path)/../CANERM.pool CANERM NoSim $Tmp(NoPrev)] 0]
      set Sim(Date0) $Tmp(SimYear)$Tmp(SimMonth)$Tmp(SimDay)
      set Sim(Time0) $Tmp(SimHour)
   }

   # ----- creer le fichier <ersinp> en s'assurant qu'il n'y ait pas
   #       de blanc apres le signe "=".

   set f [open  $Sim(Path)/tmp/ersinp.in w]

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
   puts $f "ZBTYP  =METRES"
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
# Nom        : <CANERM::CreateScriptInput>
# Creation   : Juillet 2009 - J.P. Gauthier - CMC/CMOE
#
# But        : Creer le fichier "Model_CANERM.in" contenant les parametres necessaires
#              pour une simulation automatique de CANERM.
#
# Parametres :
#
# Retour     :
#
# Remarques  :
#
#-------------------------------------------------------------------------------

proc CANERM::CreateScriptInput { } {
   variable Sim
   global GDefs

   #----- Create ASCII file containing directives for launching entire job.
   set file [open $Sim(Path)/tmp/Model_CANERM.in w 0644]

      puts $file "#----- Logger specific parameters"
      puts $file "LOG_MAILTO=\"$Model::Param(EMail)\""
      puts $file "LOG_MAILTITLE=\"$Sim(Model) ($Model::Param(App))\""
      puts $file "LOG_FILE=$Sim(PathRun)/tmp/Model_CANERM.out"
      puts $file "LOG_LEVEL=$Model::Param(LogLevel)"
      puts $file "LOG_TIME=$Model::Param(LogTime)"
      puts $file "LOG_JOBID=$Sim(Model)"

      if { !$Model::Param(Auto) } {
         puts $file "LOG_JOBCLASS=INTERACTIVE"
      }

      puts $file ""
      puts $file "#----- Job general parameters"
      puts $file "MODEL_SOFTWARE=$Model::Param(App)"
      puts $file "MODEL_NAME=CANERM"
      puts $file "MODEL_TYPE=\"\""
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
      puts $file "#----- Model specific parameters"
      puts $file "CANERM_FREQIN=$Sim(FreqIn)"
      puts $file "CANERM_FREQOUT=$Sim(FreqOut)"
      puts $file "CANERM_PREVDIR=$Sim(PathPrev)"
      puts $file "CANERM_PREVRESTART=$Sim(PrevRestart)"
      puts $file "CANERM_RESTART=$Sim(Restart)_000r"
      puts $file "CANERM_NOPREV=$Sim(NoPrev)"
      puts $file "CANERM_ISOLST=$Sim(IsoName)"
      puts $file "CANERM_ACCDATE=$Sim(AccYear)$Sim(AccMonth)$Sim(AccDay)$Sim(AccHour)"
      puts $file "CANERM_SIMDATE=$Sim(SimYear)$Sim(SimMonth)$Sim(SimDay)$Sim(SimHour)"

      if { $Sim(ReNewMeteo)!="" } {
         puts $file "CANERM_METEO=$Sim(ReNewMeteo)"
      } elseif { $Model::Param(DBaseLocal) } {
         puts $file "CANERM_METEO=$Sim(Path)"
      } else {
         puts $file "CANERM_METEO=$Sim(Meteo)"
      }
      puts $file "CANERM_GRIDCHANGED=$Sim(GridChanged)"

   close $file
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

   #----- Skip if this is a relaunch since we use the same meteo
   if { $Sim(ReNewMeteo)!="" } {
      return True
   }

   Dialog::Wait . $Msg(MetGet)

   #----- Get available meteorological files.
   set Sim(Data)     [MetData::File $Sim(RunStamp) $Model::Param(DBaseDiag) $Model::Param(DBaseProg) F 0 $Sim(Delta)]
   set Sim(Mode)     [MetData::GetMode $Sim(Data) False]

   if { $Sim(Mode)=="diag" } {
      set Sim(Delta) 6
      set Sim(Data) [MetData::File $Sim(RunStamp) $Model::Param(DBaseDiag) $Model::Param(DBaseProg) F 0 $Sim(Delta)]
   }
   Dialog::WaitDestroy

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
# Nom      : <CANERM::InitCont>
# Creation : Octobre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialise un tableau de defintions de simulation pour une
#            continuation de simulation.
#
# Parametres :
#   <Type>   : Type de source
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc CANERM::InitCont { Type } {
   variable Sim

   set Sim(PGrid)  $Sim(Grid)
   set Sim(NoPrev) $Sim(NoSim)

   #----- On Determine la date de simulation propice
   set nbhour [expr [string trimleft $Sim(NbPer) 0]*$Sim(Dt)]
   set sec    [clock scan "$nbhour hours" -base [clock scan "$Sim(SimYear)$Sim(SimMonth)$Sim(SimDay) $Sim(SimHour):00" -gmt True] -gmt True]

   set Sim(SimYear)  [clock format $sec -format "%Y" -gmt True]
   set Sim(SimMonth) [clock format $sec -format "%m" -gmt True]
   set Sim(SimDay)   [clock format $sec -format "%d" -gmt True]
   set Sim(SimHour)  [clock format $sec -format "%H" -gmt True]
}

#----------------------------------------------------------------------------
# Nom      : <CANERM::InitNew>
# Creation : Octobre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialise un tableau de defintions de simulation pour une
#            nouvelle simulation.
#
# Parametres :
#   <Type>   : Type de source
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc CANERM::InitNew { Type } {
   global   GDefs
   variable Sim

   set Sim(FreqOut)      $Sim(FreqOut)
   set Sim(Event)        "[lindex $Sim(ListEvent) 0]"
   set Sim(NbPer)        "0"
   set Sim(Dt)           "3"
   set Sim(DTIN)         "3600.0"
   set Sim(DTIS)         "0000.0"
   set Sim(Mode)         "prog"
   set Sim(Meteo)        "glb"
   set Sim(Delta)         6
   set Sim(Scale)        "FINE"
   set Sim(NI)           229
   set Sim(NJ)           229
   set Sim(AccMin)       00

   #-----Cas nucleaire ou volcanique
   if { $Type == 0 } {
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

   set Sim(FnTime)       "CONSTANT"
   set Sim(Delai)        "0"
   set Sim(IType1)       "STATIC"
   set Sim(IType2)       "NPLA"
   set Sim(IsoNb)        1
   set Sim(IsoName)      [lindex $iso 0]
   set Sim(IsoRelease)   [lindex $iso 1]
   set Sim(IsoHalf)      [lindex $iso 2]
   set Sim(IsoDry)       [lindex $iso 3]
   set Sim(IsoWet)       [lindex $iso 4]
   set Sim(IsoUnit)      [lindex $iso 6]
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
   global env
   variable Sim

   Model::ParamsMeteoInput CANERM
   CANERM::CreateModelInput

   #----- Continuation
   if { $Sim(NoPrev)!=-1 } {
      CANERM::SimPrevious $Sim(Path)

      #----- On verifie si la grille a changee
      if { $Sim(Grid)!=$Sim(PGrid) } {
         set Sim(GridChanged) 1
      }
   }

   #----- On cree le fichier necessaire au modele (Model_CANERM.in)
   CANERM::CreateScriptInput

   if { $Model::Param(IsUsingSoumet) } {

      #----- Meteo is local, launch it's processing and wait for it.
      if { $Model::Param(DBaseLocal) } {
         if { ![Dialog::Default .modelnew 400 WARNING $Warning(MetLocal) "" 0 $Lbl(No) $Lbl(Yes)] } {
            return
         }

         Dialog::Wait . $Msg(MetGet)
         exec $env(EER_DIRSCRIPT)/Model_MeteoCANERM.sh $Sim(Path)/tmp $Sim(Meteo) $Sim(ISauve) 1
         Dialog::WaitDestroy
      }

      #----- Copy needed file to run host:directory.
      Model::ParamsCopy CANERM

      #----- Launching with soumet.
      exec echo "#!/bin/sh\n\nord_soumet $env(EER_DIRSCRIPT)/Model.sh -args $Sim(PathRun)/tmp/Model_CANERM.in -mach $Model::Param(Host) \
         -cm 800M -t 3600 -listing $Model::Param(Listings) $Model::Param(Op) -queue $Model::Param(Queue)" >$Sim(Path)/tmp/Model_Launch.sh
      exec chmod 755 $Sim(Path)/tmp/Model_Launch.sh
      eval set err \[catch \{ exec ord_soumet $env(EER_DIRSCRIPT)/Model.sh -args $Sim(PathRun)/tmp/Model_CANERM.in -mach $Model::Param(Host) \
         -cm 800M -t 3600 -listing $Model::Param(Listings) $Model::Param(Op) -queue $Model::Param(Queue) 2>@1 \} msg\]
      catch { exec echo "$msg" > $Sim(Path)/tmp/Model_Launch.out }

      if { $err } {
         Log::Print ERROR "Submitting the job on $Model::Param(Host) failed:\n\n\t$msg"
         return False
      }
      Log::Print INFO "Job has been submitted successfully on $Model::Param(Host)."
   } else {
      Log::Print INFO "Launching model on $Model::Param(Host)"
      exec echo "#!/bin/sh\n\n$env(EER_DIRSCRIPT)/Model.sh $Sim(Path)/tmp/Model_CANERM.in" >$Sim(Path)/tmp/Model_Launch.sh
      exec $env(EER_DIRSCRIPT)/Model.sh $Sim(Path)/tmp/Model_CANERM.in &
   }

   if { $Sim(ReNewMeteo)!="" } {
      eval file copy -force [glob $Sim(ReNewMeteo)/../results/*m] $Sim(Path)/results
   } else {
      exec $env(EER_DIRSCRIPT)/GenerateMetfields.tcl $Sim(Path)/tmp $Sim(Date0)$Sim(Time0) $Sim(SimYear)$Sim(SimMonth)$Sim(SimDay)$Sim(SimHour) $Sim(Path)/tmp/data_std_pres.in &
   }

   return True
}

#-------------------------------------------------------------------------------
# Nom        : <CANERM::ParamsCheck>
# Creation   : Juin 2001 - J.P.Gauthier - CMC/CMOE
#
# But        : Effectuer toutes les verifications de parametres et recuperer les donnees
#              meteorologiques disponibles pour la simulation.
#
# Parametres :
#   <Tab>    : Frame parent de l'onglet
#   <No>     : Numero de l'onglet
#
# Retour     :
#
# Remarques  :
#
#-------------------------------------------------------------------------------

proc CANERM::ParamsCheck { Tab No } {
   global   GDefs
   variable Sim
   variable Error

   #----- Skip if this is a relaunch
   if { $Sim(ReNewMeteo)!="" } {
      return True
   }

   #----- Check for last tab
   set nb [expr [TabFrame::NbFrame $Tab]-1]
   if { $No!=$nb } {
      return True
   }

   if { $Sim(NoPrev)==-1 } {
      if { $Sim(IsoNb)==0 } {
         Dialog::Error .modelnew $Error(NbIso)
         TabFrame::SelectPrevious $Tab
         return False
      }

      if { $Sim(EmHeight)>30000 } {
         Dialog::Error .modelnew $Error(Height)
         TabFrame::SelectPrevious $Tab
         return False
      }

      #----- On Determine la date de simulation propice
      set Sim(AccYear)  [clock format $Sim(AccSeconds) -format %Y -gmt true]
      set Sim(AccMonth) [clock format $Sim(AccSeconds) -format %m -gmt true]
      set Sim(AccDay)   [clock format $Sim(AccSeconds) -format %d -gmt true]
      set Sim(RunStamp) [fstdstamp fromdate $Sim(AccYear)$Sim(AccMonth)$Sim(AccDay) $Sim(AccHour)000000]

      set Sim(Name)         [lindex $Sim(GridSrc) 0]
      set Sim(Lat)          [lindex $Sim(GridSrc) 1]
      set Sim(Lon)          [lindex $Sim(GridSrc) 2]
   } else {
      #----- On Determine la date de simulation propice
      set Sim(RunStamp) [fstdstamp fromdate $Sim(SimYear)$Sim(SimMonth)$Sim(SimDay) $Sim(SimHour)000000]
   }

   #----- Get meteorological data according to met database, time interval between files, release accident date-time.
   if { ![GetMetData] } {
      return False
   }
   return True
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

   set info [lindex [Info::Find $Path/../CANERM.pool CANERM NoSim $Sim(NoPrev) NameExp $Sim(NameExp)] 0]
   set res  [glob $Path/../[Info::Path $info]]

   set Sim(PrevRestart) "$res/results/$Sim(SimYear)$Sim(SimMonth)$Sim(SimDay)$Sim(SimHour)_000r"

   #----- Parcourir les simulations precedentes pour en obtenir les fichiers resultats
   set Tmp(NoPrev) $Sim(NoPrev)

   while { $info!="" && $Tmp(NoPrev)!=-1 } {

      Info::Decode ::CANERM::Tmp $info
      set path [Info::Path $info]

      catch { set result [glob $Path/../$path/results/$Tmp(SimYear)$Tmp(SimMonth)$Tmp(SimDay)$Tmp(SimHour)_*c] }

      if { [llength $result] == 0 } {
         Dialog::Error . $Error(Previous) "$path\n\n"
      }
      set resultlist "[lsort $result] $resultlist "
      set info [lindex [Info::Find $Path/../CANERM.pool CANERM NoSim $Tmp(NoPrev)] 0]
   }

   #----- Inscrire la liste des resultats precedent dans le repertoire temporaire.
   exec echo "$resultlist" > $Path/tmp/previous.in
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

   Info::Decode ::CANERM::Tmp $Exp::Data(SelectSim)
   SPI::FileOpen NEW FieldBox "(CANERM) $Tmp(NoExp) $Tmp(Name) ($Type)" "" $files
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
