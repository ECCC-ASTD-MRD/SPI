#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Package d'interface pour SPI
# Fichier  : SatData.tcl
# Creation : Octobre 2000
#
# Description:
#    Permet l'obtention de donnees satellitaires.
#
# Remarques :
#   - la resolution est variable et exprime en degre.
#
#   - la localisation selon la longitude est :
#
#        GOES 8 / GOES 12           :   75 W  +/- 83 degres
#        GOES 9 / GOES 10 / GOES 11 :  135 W  +/- 83 degres
#        METEOSAT                   :    0
#        GMS                        :  140 E
#
#===============================================================================

#----- Lire les sources d'execution

source $GDefs(Dir)/Apps/Tools/SatData/SatData.ctes
source $GDefs(Dir)/Apps/Tools/SatData/SatData.txt
source $GDefs(Dir)/Apps/Tools/SatData/SatData.int

#-------------------------------------------------------------------------------
# Nom      : <SatData::Close>
# Creation : Octobre 2000 - J.P. Gauthier - CMC/CMOE -
#
# But      : Ferme l'interface.
#
# Parametres :
#
# Remarques :
#    -On doit redefinir les fonctions de range des projections
#
#-------------------------------------------------------------------------------

proc SatData::Close { } {
   global GDefs
   variable Data

   if { $Page::Data(ToolMode) == "SatData" } {
      SPI::ToolMode SPI Zoom
   }

   #----- Reinitialiser les parametres

   foreach sat { GOES11 GOES12 } no { 0 1 } {
      set SatData::Data(View$sat) 0
   }

   set Data(Coo)     ""
   set Data(Select)  0
   set Data(Active)  0

   if { $Data(Canvas)!="" } {
      $Data(Canvas) delete RANGESATDATA SAT
      SPI::IcoDel SATCENTER
   }

   #----- Fermer la boite de champs de donnees satellitaires

   if { [FieldBox::Exist $Data(Box)]  } {
      FieldBox::Close $Data(Box)
      set Data(Box) ""
   }

   trace vdelete SPI::Src(Info) w { SatData::Source }
   foreach file [glob -nocomplain $GDefs(DirEER)/eer_Tmp/SAT*] {
      file delete $file
   }
   destroy .satdata

   if { !$SPI::Param(Window) } { SPI::Quit }
}

#-------------------------------------------------------------------------------
# Nom      : <SatData::DataExtract>
# Creation : Juillet 1998 - J.P. Gauthier - CMC/CMOE
#
# But     : Effectue le travail d'extraction des donnees satellites.
#
# Parametres    :
#  <InFile>     : Nom du fichier de donnees satellites
#
# Retour:
#
# Remarques :
#   Aucune.
#
#-------------------------------------------------------------------------------

proc SatData::DataExtract { InFile } {
   global GDefs
   variable Data
   variable Msg

   #----- get the date from filename.

   set InFileDate [string range [exec basename $InFile] 0 12]
   set IdSat [string range [exec basename $InFile] 13 15]

   #----- setup output file.

   set outfile "$GDefs(DirEER)/eer_Tmp/SAT_${InFileDate}${IdSat}"

   InfoFrame::Msg .satdata.info.msg "[lindex $Msg(Extract) $GDefs(Lang)] ($InFile)"
   update idletasks

   #----- execute the converter program.

   if { $GDefs(FrontEnd)!=$GDefs(Host) } {
      catch { exec ssh pollux -l $GDefs(FrontEndUser) ". ~/.profile; /users/dor/afsh/sat/bin/sat.hdfrawcnvrt.pl -d $InFileDate -v -o $outfile -s $IdSat -g \"$Data(Grille)\" > $GDefs(DirEER)/eer_Tmp/SAT_hdf2fstd.out" }
   } else {
      Log::Print ERROR "Option 2 not available right now, sorry! ( $GDefs(FrontEnd) $GDefs(Host) )"
      if { $trace } {
         Dialog::Error .satdata $message
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <SatData::DataGet>
# Creation : Fevrier 1997 - S. Trudel - CMC/CMOE
#
# But     : Recuperations des donnees satellitaires selectionees.
#
# Parametres :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc SatData::DataGet { }  {
   global GDefs
   variable Data
   variable Lbl
   variable Msg

   if  { !$Data(Select) } {
       Dialog::Error .satdata $Msg(Range)
       return
   }

   #----- Calculer le traitement a effectuer

  set todo 0
  foreach sat { GOES11 GOES12 } {
      foreach c { nV } {
         incr todo [llength [.satdata.sel.s$sat.ch.c$c.list curselection]]
      }
   }
   incr todo [llength [.satdata.seluser.ch.list curselection]]
   InfoFrame::Set100 .satdata.info.msg $todo

   #----- Y a t il un traitement a effectuer
   if { !$todo } {
       Dialog::Error .satdata $Msg(Data)
       return
   }

   .satdata config -cursor watch
   update idletasks

   #----- Fermer le fichier courant de donnees satellitaires

   if { [FieldBox::Exist $Data(Box)] } {
      set FieldBox::Data$Data(Box)::Data(FileCurrent) -1
      FieldBox::FileClose $Data(Box)
   }

   #----- Creer les directives de recuperations

   SatData::Directive

   #----- Si un fichier autre est choisit

   foreach item [.satdata.seluser.ch.list curselection] {
      #ST SatData::DataExtract [.satdata.seluser.ch.list get $item] $Data(ResultFile) $Data(UserPath) U
      SatData::DataExtract $SatData::Data(UserPath)/[.satdata.seluser.ch.list get $item]
      catch { exec editfst2000 -s $GDefs(DirEER)/eer_Tmp/SAT_$file -d $Data(ResultFile) -i 0 }
      catch { exec pgsm2000 -iment $GDefs(DirEER)/eer_Tmp/SAT_$file -ozsrt $Data(ResultFile) << "ENDPGSM
 SORTIE(STD,1000,A)
 LIREE('Z9','O',-1,0,0,4,'        ')
C ---- canal 5 pour les cas d'archives.
 MOINSE('Z9','O',-1,0,0,5,'        ')
 ECRITS('Z9',-16,-1,0,0,45,'O','',-1,IMPRIM)
ENDPGSM" }

      InfoFrame::Incr .satdata.info.msg 1
   }
   .satdata.seluser.ch.list selection clear 0 end

   #----- Recuperer les donnees selectionnees pour GOES11 et GOES12.

   foreach sat { GOES11 GOES12 } ch5ou6 { 5 6 } {
      foreach c { nV } {
         foreach item [.satdata.sel.s$sat.ch.c$c.list curselection] {
            set file [.satdata.sel.s$sat.ch.c$c.list get $item]
            #ST SatData::DataExtract $file $Data(ResultFile) $Data(CPath$sat)$Data(C$c) $c
            SatData::DataExtract $file

            #----- Calcul du 4-5.

            InfoFrame::Msg .satdata.info.msg "[lindex $Msg(45) $GDefs(Lang)] $item."
            catch { exec editfst2000 -s $GDefs(DirEER)/eer_Tmp/SAT_$file -d $Data(ResultFile) -i 0 }
            catch { exec pgsm2000 -iment $GDefs(DirEER)/eer_Tmp/SAT_$file -ozsrt $Data(ResultFile) << "ENDPGSM
 SORTIE(STD,1000,A)
 LIREE('Z9','O',-1,0,0,4,'        ')
C ---- canal 5 pour GOES 11 ou canal 6 pour GOES 12.
 MOINSE('Z9','O',-1,0,0,${ch5ou6},'        ')
 ECRITS('Z9',-16,-1,0,0,45,'O','',-1,IMPRIM)
ENDPGSM" }

            InfoFrame::Incr .satdata.info.msg 1
         }
        .satdata.sel.s$sat.ch.c$c.list selection clear 0 end
      }

   }

   #----- Suppression des fichiers temporaires

   foreach file [glob -nocomplain $GDefs(DirEER)/eer_Tmp/SAT*] {
       file delete $file
   }

   #----- Reouvrir le fichier courant de donnees satellitaires

   if { ![FieldBox::Exist $Data(Box)] } {
      set Data(Box) [FieldBox::Create . SatData]
   }
   FieldBox::FileOpen $Data(Box) $Data(ResultFile)

   set Data(Msg) ""
   InfoFrame::Incr .satdata.info.msg 0
   .satdata config -cursor left_ptr
}

#-------------------------------------------------------------------------------
# Nom      : <SatData::Directive>
# Creation : Fevrier 1997 - S. Trudel - CMC/CMOE
#
# But     : Ecrire les directives utilisees par le programme 'hdf2raw'.
#
# Parametres :
#
# Remarques :
#   - Le nouveau programme 'sat.hdfrawcnvrt.pl' utilise la directive
#     GRILLE de pgsm pour une projection LAT/LON pour l'instant.
#     Voici la reference :
#
#-------------------------------------------------------------------------------

proc SatData::Directive { }  {
   global GDefs
   variable Data

   #----- Effectue la translation (origine du referentiel du globe est au centre a droite )
   Log::Print DEBUG "(lat, lon) inf left = ($Data(Lat0), $Data(Lon0))"

   #----- Determine le nombre de points de grille en i et j.
   set latdiff [expr abs($Data(Lat1) - $Data(Lat0))]
   set londiff [expr abs($Data(Lon1) - $Data(Lon0))]

   if { $londiff > 180 } {
      set londiff [expr abs($londiff - 360)]
   }

   set ni [expr round([expr ($londiff / $Data(Res))])]
   set nj [expr round([expr ($latdiff / $Data(Res))])]

   Log::Print DEBUG "diff lat=$latdiff lon=$londiff"

   #----- Ecrire le fichier de directives.
   set Data(Grille) "GRILLE(LATLON,${ni},${nj},$Data(Lat0),$Data(Lon0),$Data(Res),$Data(Res))"

   Log::Print DEBUG "$Data(Grille)"
}

#----------------------------------------------------------------------------
# Nom      : <SatData::DomainDelete>
# Creation : Juillet 1998 - J.P. Gauthier - CMC/CMOE
#
# But      : Suppression des domaines.
#
# Parametres :
#   <Combo>  : Path du combobox
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc SatData::DomainDelete { Combo } {
   global GDefs
   variable Data
   variable Lbl
   variable Msg

   if { ([ComboBox::Index $Combo exact $Data(Name)] != -1) && ($Data(Name) != "") } {

      if { ![Dialog::Default . 300 WARNING $Msg(Del)] "\n\n$Data(Name)" 1 $Lbl(Yes) $Lbl(No)] } {

         file copy -force $Data(File) $Data(File).old

         #----- Catch pour recuperer la sortie du grep en cas d'erreur (aucun domaine dans le grep)
         catch { exec egrep -v "$Data(Name).*" $Data(File).old > $Data(File) }

         ComboBox::Del $Combo "$Data(Name)"
         set Data(Name) ""
         if { $Data(Canvas) != "" } {
            $Data(Canvas) delete RANGESATDATA
         }
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <SatData::DomainRead>
# Creation : Mai 1998 - J.P. Gauthier - CMC/CMOE
#
# But      : Lire la liste des domaines sauvegarder.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc SatData::DomainRead { } {
   variable Data

   set Data(List) {}
   set Data(LName) {}

   if { ![catch { set file [open $Data(File) r] }]  } {

      while { ![eof $file] } {

         gets $file line
         if { [string index $line 0] != "#" && [string length $line] > 0 } {
            set line [split $line ","]
            lappend Data(List) $line
            lappend Data(LName) [lindex $line 0]
         }
      }
      close $file
  }
}

#----------------------------------------------------------------------------
# Nom      : <SatData::DomainSave>
# Creation : Mai 1998 - J.P. Gauthier - CMC/CMOE
#
# But      : Sauvegarder des domaines.
#
# Parametres :
#   <Combo>  : Path du combobox
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc SatData::DomainSave { Combo } {
   global GDefs
   variable Data
   variable Msg


   regsub -all " |,"  $Data(Name) _ Data(Name)

   #----- Si le nom est unique on sauvegarde

   if { [lsearch -exact $Data(LName) $Data(Name)] == -1 && $Data(Name) != "" } {

      set file [open $Data(File) a]
      puts $file "$Data(Name),$Data(Res),$Data(Lat0),$Data(Lon0),$Data(Lat1),$Data(Lon1)"
      close $file

      #----- Ajout a la liste des domaines

      lappend Data(LName) $Data(Name)
      lappend Data(List) "$Data(Name) $Data(Res) $Data(Lat0) $Data(Lon0) $Data(Lat1) $Data(Lon1)"

      #----- Ajout dans le combobox

      ComboBox::Add $Combo $Data(Name)
   } else {
      Dialog::Error .satdata $Msg(Domain)
   }
}

#----------------------------------------------------------------------------
# Nom      : <SatData::DomainShow>
# Creation : Mai 1998 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche le domaine selectionne.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc SatData::DomainShow { } {
   variable Data

   #----- Extraire l'index du nom a travers la liste de nom

   set index [lsearch -exact $Data(LName) $Data(Name)]

   #----- Utiliser l'index pour acceder au reste des parametres

   if { $index != -1 } {

      set Data(Select) 1
      set Data(Res)  [lindex [lindex $Data(List) $index] 1]
      set Data(ORes) $Data(Res)
      SatData::ResApply

      if { $Data(Canvas)!="" } {
         $Data(Canvas) delete RANGESATDATA
      }

      set data        [lindex $Data(List) $index]
      set Data(Lat0)  [lindex $data 2]
      set Data(Lon0)  [lindex $data 3]
      set Data(Lat1)  [lindex $data 4]
      set Data(Lon1)  [lindex $data 5]

      set Data(Canvas) $Page::Data(Canvas)
      set Data(Frame)  $Page::Data(Frame)
      set Data(VP)     $Viewport::Data(VP)

      Viewport::DrawRange $Data(Frame) $Viewport::Data(VP) $Data(Lat0) $Data(Lon0) $Data(Lat1) $Data(Lon1) RANGESATDATA darkgreen
      SatData::DrawCoords $Data(Canvas)
   }
}

#----------------------------------------------------------------------------
# Nom      : <SatData::DrawCoords>
# Creation : Juin 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche les coordonnees du domaine.
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc SatData::DrawCoords { Canvas } {
   global GDefs
   variable Data

   set Data(Coo) "[Convert::FormatCoord $Data(Lat1) $Data(Lon0) MIN] / [Convert::FormatCoord $Data(Lat0) $Data(Lon1) MIN]"

   $Canvas create text [expr [winfo width $Canvas]/2] [winfo height $Canvas] \
      -text $Data(Coo) -tags RANGESATDATA -anchor s -font XFont12
}

#-------------------------------------------------------------------------------
# Nom      : <SatData::GetFiles>
# Creation : Septembre 1998 - J.P. Gauthier - CMC/CMOE
#
# But     : Inserer les fichiers de donnees satellites brutes dans les
#           listbox.
#
# Parametres :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc SatData::GetFiles { } {
   variable Data

   foreach sat { GOES11 GOES12 } {
      foreach c { nV } {

        Log::Print DEBUG "$Data(CPath$sat) , $Data(C$c)"

         foreach file [exec ls -1r $Data(CPath$sat)/$Data(C$c)/ ] {
            if { [string match ??????????_??g?? $file] } {
               .satdata.sel.s$sat.ch.c$c.list insert end $file
            }
         }
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <SatData::GetUserFiles>
# Creation : Octobre 2000 - J.P. Gauthier - CMC/CMOE
#
# But     : Inserer les fichiers de donnees satellites brutes de l'usager dans le
#           listbox.
#
# Parametres :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc SatData::GetUserFiles { Path } {
   variable Data

   if { $Path=="" } {
      return
   }

   set Data(UserPath) $Path
   .satdata.seluser.ch.list delete 0 end

   foreach file [glob $Path/*] {
      set file [file tail $file]
      .satdata.seluser.ch.list insert end $file
   }
}

#-------------------------------------------------------------------------------
# Nom      : <SatData::Source>
# Creation : Mai 2000 - J.P. Gauthier - CMC/CMOE -
#
# But      : Recupere la selection d'une source.
#
# Parametres      :
#  <Array>   : Variable array
#  <Index>   : Index dans la variable Array
#  <Op>      : Operation effectuer sur la variable
#
# Remarques :
#   -Cette procedure repond a un "trace" sur une variable a l'interieur de SPI
#    afin de recuperer l'information necessaire automatiquement (SPI::Src(Info))
#
#-------------------------------------------------------------------------------

proc SatData::Source { Array Index Op } {
   variable Data

   regsub -all " "  $SPI::Src(Name) _ Data(Name)

   #----- Affichage du domaine autour de la source

   set Data(Res) 0.01
   set Data(Select) 1
   set Data(Canvas) $Page::Data(Canvas)
   set Data(Frame)  $Page::Data(Frame)
   set Data(VP)     $Viewport::Data(VP)

   SatData::ResApply

   if { $Data(Canvas)!="" } {
      $Data(Canvas) delete SatCenter RANGESATDATA
   }

   set Data(Lat0) [expr $SPI::Src(Lat) - 5]
   set Data(Lat1) [expr $SPI::Src(Lat) + 5]
   set Data(Lon0) [Viewport::CheckCoord [expr $SPI::Src(Lon) - 5]]
   set Data(Lon1) [Viewport::CheckCoord [expr $SPI::Src(Lon) + 5]]

   #----- Effectuer une rotation pour se centrer sur la source

   set Viewport::Map(Lat) $SPI::Src(Lat)
   set Viewport::Map(Lon) $SPI::Src(Lon)
   Viewport::Rotate $Data(Frame)

   #----- Afficher le domaine

   Viewport::DrawRange $Data(Frame) $Viewport::Data(VP) $Data(Lat0) $Data(Lon0) $Data(Lat1) $Data(Lon1) RANGESATDATA darkgreen
   SatData::DrawCoords $Data(Canvas)

   # ----- Creer une nouvelle icone.

   SPI::IcoAdd $Data(Frame) SATCENTER "" [list [list $Data(Name) $SPI::Src(Lat) $SPI::Src(Lon) $SPI::Src(Elev) [lindex $Locator::Data(Icons) $SPI::Src(Type)]]]
}

#-------------------------------------------------------------------------------
# Nom      : <SatData::ResApply>
# Creation : Fevrier 1997 - S. Trudel - CMC/CMOE
#
# But      : calculer la distance specifique a la nouvelle resolution et
#            redimensionner le domaine actif.
#
# Parametres :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc SatData::ResApply { } {
   variable Data

   #----- Si une selection de domaine est faite, redimmensionner le domaine

   if { $Data(Coo)!="" } {

      #----- Extraire les ni nj

      set latdiff [expr abs($Data(Lat1) - $Data(Lat0))]
      set londiff [expr abs($Data(Lon1) - $Data(Lon0))]

      if { $londiff > 180 } {
         set londiff [expr abs($londiff - 360)]
      }
      set ni [expr ($londiff / $Data(ORes))]
      set nj [expr ($latdiff / $Data(ORes))]

      #----- Redimmensionner le domaine

      set Lat0 [expr $Data(Lat0) + ($nj * $Data(ORes)/2) - ($nj * $Data(Res)/2)]
      set Lat1 [expr $Lat0 + ($nj * $Data(Res))]

      if { $Lat0 > -90.0 && $Lat0 < 90.0 && $Lat1 > -90.0 && $Lat1 < 90.0 } {

         #----- Refaire le calcul des lat car l'assignation perd de la precision

         set Data(Lat0) $Lat0
         set Data(Lat1) $Lat1
         set Data(Lon0) [Viewport::CheckCoord [expr $Data(Lon0) + ($ni * $Data(ORes)/2) - ($ni * $Data(Res)/2)]]
         set Data(Lon1) [Viewport::CheckCoord [expr $Data(Lon0) + ($ni * $Data(Res))]]

         $Data(Canvas) delete RANGESATDATA

         Viewport::DrawRange $Data(Frame) $Viewport::Data(VP) $Data(Lat0) $Data(Lon0) $Data(Lat1) $Data(Lon1) RANGESATDATA darkgreen
         SatData::DrawCoords $Data(Canvas)
      } else {
         set Data(Res) $Data(ORes)
         bell
         return
      }
   }
   set Data(LonMax) [expr ($Data(NIMax) * $Data(Res))]
   set Data(LatMax) [expr ($Data(NJMax) * $Data(Res))]
   set Data(ORes) $Data(Res)
}

#-------------------------------------------------------------------------------
# Nom      : <SatData::Move>
# Creation : Octobre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectuer la fonction de deplacement de la selection sur la projection.
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <VP>     : Identificateur du Viewport
#
# Remarques :
#    - Ces fonctions sont appele par le package Page au besoin.
#
#-------------------------------------------------------------------------------

proc SatData::Move { Frame VP } {
   variable Data

   #----- Effectuer la translation

   set Lat0 [expr $Data(Lat0) + $Viewport::Map(LatCursor) - $Data(LatDelta)]
   set Lat1 [expr $Data(Lat1) + $Viewport::Map(LatCursor) - $Data(LatDelta)]

   if { $Lat0 > -90.0 && $Lat0 < 90.0 && $Lat1 > -90.0 && $Lat1 < 90.0 } {

      set Data(Lat0) $Lat0
      set Data(Lat1) $Lat1
      set Data(Lon0) [Viewport::CheckCoord [expr $Data(Lon0) + $Viewport::Map(LonCursor) - $Data(LonDelta)]]
      set Data(Lon1) [Viewport::CheckCoord [expr $Data(Lon1) + $Viewport::Map(LonCursor) - $Data(LonDelta)]]
   }

   if { $Data(Canvas)!="" } {
      $Data(Canvas) delete RANGESATDATA
   }

   #----- Reaffecter le point de reference de translation

   set Data(LonDelta) $Viewport::Map(LonCursor)
   set Data(LatDelta) $Viewport::Map(LatCursor)
   set Data(Canvas)   $Page::Data(Canvas)
   set Data(Frame)    $Frame
   set Data(VP)       $VP

   Viewport::DrawRange $Frame $VP $Data(Lat0) $Data(Lon0) $Data(Lat1) $Data(Lon1) RANGESATDATA darkgreen
   SatData::DrawCoords $Data(Canvas)
}

#-------------------------------------------------------------------------------
# Nom      : <SatData::MoveDone>
# Creation : Octobre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Finaliser la fonction de deplacement de la selection sur la projection.
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <VP>     : Identificateur du Viewport
#
# Remarques :
#    - Ces fonctions sont appele par le package Page au besoin.
#
#-------------------------------------------------------------------------------

proc SatData::MoveDone { Frame VP } {
}

#-------------------------------------------------------------------------------
# Nom      : <SatData::MoveInit>
# Creation : Octobre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialiaser la fonction de deplacement de la selection sur la projection.
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <VP>     : Identificateur du Viewport
#
# Remarques :
#    - Ces fonctions sont appele par le package Page au besoin.
#
#-------------------------------------------------------------------------------

proc SatData::MoveInit { Frame VP } {
   variable Data

   set Data(LonDelta) $Viewport::Map(LonCursor)
   set Data(LatDelta) $Viewport::Map(LatCursor)
   set SatData::Data(Name) ""
}

#-------------------------------------------------------------------------------
# Nom      : <SatData::Draw>
# Creation : Octobre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer une selection sur la projection.
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <VP>     : Identificateur du viewport
#
# Remarques :
#    - Ces fonctions sont appele par le package Page au besoin.
#
#-------------------------------------------------------------------------------

proc SatData::Draw { Frame VP } {
   variable Data

   #----- Recuperer les coordonnees du dernier point

   set Lon $Viewport::Map(LonCursor)
   set Lat $Viewport::Map(LatCursor)

   set latdiff [expr abs($Lat - $Data(Lat0))]
   set londiff [expr abs($Lon - $Data(Lon0))]

   if { $londiff > 180 } {
      set londiff [expr abs($londiff - 360)]
   }
   if { $latdiff<=$Data(LatMax) } {
      set Data(Lat1) $Lat
   }
   if {  $londiff<=$Data(LonMax) } {
      set Data(Lon1) $Lon
   }

   if { $Data(Canvas)!="" } {
      $Data(Canvas) delete RANGESATDATA
   }

   set Data(Canvas) $Page::Data(Canvas)
   set Data(Frame)  $Frame
   set Data(VP)     $VP
   set Data(Select) 1

   Viewport::DrawRange $Frame $VP $Data(Lat0) $Data(Lon0) $Data(Lat1) $Data(Lon1) RANGESATDATA darkgreen
   SatData::DrawCoords $Data(Canvas)
}

#-------------------------------------------------------------------------------
# Nom      : <SatData::DrawDone...>
# Creation : Octobre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Terminer la selection sur la projection.
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <VP>     : Identificateur du Viewport
#
# Remarques :
#    - Ces fonctions sont appele par le package Page au besoin.
#
#-------------------------------------------------------------------------------

proc SatData::DrawDone { Frame VP } {
   variable Data

   if { $Data(Lat0) > $Data(Lat1) } {
      set tmp  $Data(Lat0)
      set Data(Lat0) $Data(Lat1)
      set Data(Lat1) $tmp
   }
   if { $Data(Lon0) > $Data(Lon1) } {
      set tmp  $Data(Lon0)
      set Data(Lon0) $Data(Lon1)
      set Data(Lon1) $tmp
   }

   #----- Verifier le cas de changement de -180 a 180

   if {  [expr $Data(Lon0) * $Data(Lon1)] < 0.0 && [expr abs($Data(Lon0)) + abs($Data(Lon1))] > 180 } {

      set tmp  $Data(Lon0)
      set Data(Lon0) $Data(Lon1)
      set Data(Lon1) $tmp
   }
}

#-------------------------------------------------------------------------------
# Nom      : <SatData::DrawInit>
# Creation : Octobre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialiser la fonctions de selection sur la projection.
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <VP>     : Identificateur du Viewport
#
# Remarques :
#    - Ces fonctions sont appele par le package Page au besoin.
#
#-------------------------------------------------------------------------------

proc SatData::DrawInit  { Frame VP } {
   variable Data

   set Data(Lon0)   $Viewport::Map(LonCursor)
   set Data(Lat0)   $Viewport::Map(LatCursor)
   set Data(Coo)    "[Convert::FormatCoord $Data(Lat0) $Data(Lon0) MIN]\
                    / [Convert::FormatCoord $Data(Lat0) $Data(Lon0) MIN]"
   set SatData::Data(Name) ""
}

#----------------------------------------------------------------------------
# Nom      : <SatData::UpdateItems>
# Creation : Juin 2002 - J.P. Gauthier - CMC/CMOE
#
# But      :
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc SatData::UpdateItems { Frame } {
   global GDefs
   variable Data

   if { $Frame==$Data(Frame) } {

      $Data(Canvas) delete SAT RANGESATDATA

      if { ![llength [info commands $Data(VP)]] } {
         set Data(VP) ""
         return
      }

      if { $Data(Coo)!="" } {
         Viewport::DrawRange $Data(Frame) $Data(VP) $Data(Lat0) $Data(Lon0) $Data(Lat1) $Data(Lon1) RANGESATDATA darkgreen
         SatData::DrawCoords $Data(Canvas)
      }

      if { $Data(ViewGOES11) } {
         Viewport::DrawArea $Data(Frame) $Data(VP) $Data(CoordGOES11) "$Page::Data(Tag)$Data(VP) SAT" SATGOES11 \
            blue blue @$GDefs(Dir)/Resources/Bitmap/raydiagright08.xbm False 1
      }
      if { $Data(ViewGOES12) } {
         Viewport::DrawArea $Data(Frame) $Data(VP) $Data(CoordGOES12) "$Page::Data(Tag)$Data(VP) SAT" SATGOES12 \
            red red @$GDefs(Dir)/Resources/Bitmap/raydiagleft08.xbm False 1
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <SatData::AsProject>
# Creation : Aout 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Sauvegarder l'etat de l'outils dans un projet SPI.
#
# Parametres :
#   <File>   : Descripteur de fichier ou ecrire les commandes
#
# Remarques :
#    - Le fichier est deja ouvert, il suffit d'y ecrire les commandes a executer
#      afin de re-instaurer l'outils dans son etat actuel.
#
#-------------------------------------------------------------------------------

proc SatData::AsProject { File } {
   variable Data
   variable Param

   if { [winfo exists .satdata] } {
      puts $File "#----- Tool: SatData\n"
      puts $File "set SatData::Param(Dock)   $Param(Dock)"
      puts $File "set SatData::Param(Geom)   [winfo geometry .satdata]"
      puts $File "SatData::Window"
      puts $File "\n"
   }
}
