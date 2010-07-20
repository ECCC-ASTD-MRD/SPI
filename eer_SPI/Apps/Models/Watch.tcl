#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie de "Widget" Tk.
# Fichier  : Watch.tcl
# Creation : Septembre 2001 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Fonctions de manipualtions des Watchs.
#
# Remarques :
#
#===============================================================================

package require IsoBox

namespace eval Watch {
   global   GDefs
   variable Data
   variable Lbl
   variable Msg
   variable Error
   variable Bubble

   #----- Variables relatives aux Watchs

   set Data(Models)    "CANERM TRAJECT SATDATA"           ;#Liste des modeles disponibles
   set Data(Branch)    ""                                 ;#Liste des branches d'experiences ouvertes
   set Data(BranchSim) ""                                 ;#Liste des branches de simulation d'experiences ouvertes
   set Data(Frame)     ""                                 ;#Container
   set Data(File)      $GDefs(DirWatch)/Ini/EERAuto.ini   ;#Fichier d'experiences
   set Data(Select)    ""                                 ;#Experience selectionnee

   #----- Constantes relatives au modele CANERM

   set Data(CANERMSpecieMax)  5

   #----- Constantes relatives au modele Trajectoire

   set Data(PathTRAJECT) "$GDefs(DirWatch)/Data/Trajectory/data_EER"
   set Data(PathCANERM)  "$GDefs(DirWatch)/Data/Canerm/data"
   set Data(PathSATDATA) "$GDefs(DirWatch)/Data/Satellite/data"

   #----- Labels

   set Lbl(New)        { "Nouveau" "New" }
   set Lbl(Yes)        { "Oui" "Yes" }
   set Lbl(No)         { "Non" "No" }
   set Lbl(Params)     { "Parametres ..." "Parameters ..." }
   set Lbl(Suppress)   { "Supprimer" "Delete" }
   set Lbl(Apply)      { "Appliquer" "Apply" }
   set Lbl(Active)     { "Actives" "Actives" }
   set Lbl(Channel)    { "Canal" "Channel" }
   set Lbl(Close)      { "Fermer" "Close" }
   set Lbl(Enable)     { "Activer" "Enable" }
   set Lbl(Elev)       { "Hauteur" "Height" }
   set Lbl(Intensity)  { "Intensite" "Intensity" }
   set Lbl(Level)      { "Niveau" "Level" }
   set Lbl(Species)    { "Especes" "Species" }
   set Lbl(Title)      { "Parametres" "Parameters" }
   set Lbl(Type)       { "Type" "Type" }
   set Lbl(TypeSource) { VAAC RSMC CTBT SPCL }
   set Lbl(Result)     { "Resultats" "Results" }

   #--- Definitions des messages

   set Msg(Suppress)   { "Voulez-vous reellement desactiver cette veille ?"
                         "Do you really want to desactivate this watch ?" }

   #----- Definitions des messages d'erreurs

   set Error(Exist)     { "Une source portant ce nom est deja activee."
                          "There is already an automated source by that name." }
   set Error(Info)      { "Certaine informations son manquantes."
                          "Missing information." }
   set Error(CANERM)    { "Aucune espece n'a ete selectionne pour le modele CANERM."
                          "No species were selected for the CANERM model." }
   set Error(TRAJ)      { "La specification des niveaux pour le modele Trajectoire est incorrecte."
                          "Trajectory model level specification is wrong." }

   #----- Definitions des bulles d'aides

   set Bubble(CANERM)   { "Activation des simulations automatiques\ndu modele CANERM"
                          "Activate automated CANERM simulation" }
   set Bubble(TRAJ)     { "Activation des simulations automatiques\ndu modele de trajectoire"
                          "Activate automated trajectory simulation" }
   set Bubble(SAT)      { "Activation de la recuperation de donnees\nsatellitaires automatique" }

}

#-------------------------------------------------------------------------------
# Nom      : <Watch::AllClose>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Fermer toutes les branches.
#
# Parametres :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Watch::AllClose { } {
   variable Data

   set Data(Branch)    ""
   set Data(BranchSim) ""
   Watch::CreateTree
}

#-------------------------------------------------------------------------------
# Nom      : <Watch::AllOpen>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Ouvrir toutes les branches.
#
# Parametres :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Watch::AllOpen { } {
   variable Data

   foreach exp $Data(List) {
      set no   [lindex $exp 0]
      foreach model $Data(Models) {
         lappend Data(BranchSim) $model$no
      }
      lappend Data(Branch) $no
   }

   Watch::CreateTree
}

#----------------------------------------------------------------------------
# Nom      : <Watch::Create>
# Creation : Septembre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer le widget des watches.
#
# Parametres :
#   <Frame>  : Frame dans lequel creer le widget
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Watch::Create { Frame } {
   global GDefs
   variable Lbl
   variable Bubble
   variable Data

   set Data(Frame) $Frame

   frame $Frame.info
      canvas $Frame.info.canvas -bg white -relief sunken -bd 1 -yscrollcommand "$Frame.info.scroll set" \
         -scrollregion "1 1 280 5000" -width 200 -height 1
      scrollbar $Frame.info.scroll -orient vertical -bd 1 -width 10 \
         -command "$Frame.info.canvas yview"
      pack $Frame.info.canvas -side left -fill both -expand true
      pack $Frame.info.scroll -side left -fill y

      bind $Frame.info.canvas <Button-4> "$Frame.info.canvas yview scroll -1 units"
      bind $Frame.info.canvas <Button-5> "$Frame.info.canvas yview scroll 1 units"

   frame $Frame.opt
      button $Frame.opt.open -image PLUS -relief flat -bd 0 -overrelief raised -command "Watch::AllOpen"
      button $Frame.opt.close -image MINUS -relief flat -bd 0 -overrelief raised -command "Watch::AllClose"
      checkbutton $Frame.opt.bubble -image INFO -relief sunken -bd 1 -overrelief raised -offrelief flat -indicatoron False  -selectcolor $GDefs(ColorLight) \
         -onvalue 1 -offvalue 0 -variable CanvasBubble::Data(State$Frame.info.canvas) -command "CanvasBubble::Activate $Frame.info.canvas"
      button $Frame.opt.new -compound left -image BOMB -text [lindex $Lbl(New) $GDefs(Lang)] -relief flat -bd 0 -overrelief raised \
         -command "Model::New $Frame Watch::New \"[lindex $Lbl(New) $GDefs(Lang)]\" 0"
      pack $Frame.opt.open $Frame.opt.close $Frame.opt.bubble $Frame.opt.new -side left -padx 2

   pack $Frame.opt -side top -fill x -padx 2
   pack $Frame.info -side top -fill both -expand true  -padx 2 -pady 2

   Bubble::Create $Frame.opt.open   [lindex $Model::Bubble(Plus) $GDefs(Lang)]
   Bubble::Create $Frame.opt.close  [lindex $Model::Bubble(Minus) $GDefs(Lang)]
   Bubble::Create $Frame.opt.bubble [lindex $Model::Bubble(Bubble) $GDefs(Lang)]
}

#-------------------------------------------------------------------------------
# Nom      : <Watch::CreateTree>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer l'arborescence des watchs.
#
# Parametres :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Watch::CreateTree { } {
   global GDefs
   variable Data
   variable TData

   set canvas $Data(Frame).info.canvas
   set y 15

   #----- Cleanup du canvas

   $canvas delete TREE SIGN WATCH SIM SIMSELECT
   $canvas create rectangle -10 -10 -10 -10 -outline black -fill $GDefs(ColorHighLight) -tags "SIMSELECT"

   foreach watch $Data(List) {

      set name [lindex $watch 0]
      set lat  [lindex $watch 1]
      set lon  [lindex $watch 2]
      set type [lindex $watch 3]

      $canvas create bitmap 10 $y -bitmap $Model::Resources(Plus) -tags "SIGN PWATCH$name"
      $canvas create image 30 $y -image [lindex $Model::Resources(Icos) $type] -tags "WATCH"
      $canvas create text 43 $y -text "$name" -anchor w -tags "WATCH WATCH$name" -font $GDefs(Font) -fill black

      CanvasBubble::Create $canvas WATCH$name "Coord: ($lat , $lon)"
      $canvas bind WATCH$name  <ButtonPress-3> "Watch::Extract Watch::Data \"$watch\" ; Watch::PopUp %X %Y"
      $canvas bind WATCH$name  <ButtonPress-1> "Watch::Extract Watch::Data \"$watch\""
      $canvas bind PWATCH$name <ButtonPress-1> "Watch::SelectBranch $name"

      set y0 [set y1 [expr $y+10]]

      #----- On creer les branches des modeles si necesaire

      if { [lsearch -exact $Data(Branch) $name] != -1 } {

         $canvas itemconfigure PWATCH$name -bitmap $Model::Resources(Minus)

         Watch::Extract Watch::TData $watch
         set name [string tolower $TData(Name)]
         set nameU [string toupper $TData(Name)]
         regsub -all "_WATCH" $nameU "" nameU

         if { $TData(CANERM) } {

            set y1 [incr y 21]

            $canvas create line 10 $y 20 $y -tags TREE
            $canvas create text 40 $y -text CANERM -font $GDefs(Font) -anchor w -tags TREE
            $canvas create bitmap 30 $y -bitmap $Model::Resources(Plus) -tags "SIGN PCANERM$name"
            $canvas bind PCANERM$name <ButtonPress-1> "Watch::SelectBranch CANERM$name"

            if { [lsearch -exact $Data(Branch) CANERM$name] != -1 } {
               $canvas itemconfigure PCANERM$name -bitmap $Model::Resources(Minus)
               foreach hour { 00 03 06 09 12 18 } {
                  set y1 [incr y 20]
                  $canvas create line 30 $y 45 $y -tags TREE
                  $canvas create text 50 $y -text $hour -font $GDefs(Font) -anchor w -tags "CANERM$name$hour TREE"
                  $canvas bind CANERM$name$hour <ButtonPress-1> "Watch::Extract Watch::Data \"$watch\" ; Watch::SelectSim CANERM$name$hour"
                  $canvas bind CANERM$name$hour <ButtonPress-3> "Watch::Extract Watch::Data \"$watch\" ; Watch::SelectSim CANERM$name$hour; \
                  Watch::PopUpSim %X %Y CANERM \[lsort \[glob -nocomplain $Data(PathCANERM)/${nameU}_$hour\]\]"
               }
               $canvas create line 30 [expr $y0+20] 30 $y1 -tags TREE
            }
         }

         if { $TData(TRAJECT) } {

            set y1 [incr y 21]

            $canvas create line 10 $y 35 $y -tags TREE
            $canvas create text 40 $y -text TRAJECT -font $GDefs(Font) -anchor w -tags "TRAJECT$name TREE"
            $canvas bind TRAJECT$name <ButtonPress-1> "Watch::Extract Watch::Data \"$watch\" ; Watch::SelectSim TRAJECT$name"
            $canvas bind TRAJECT$name <ButtonPress-3> "Watch::Extract Watch::Data \"$watch\" ; Watch::SelectSim TRAJECT$name; \
            Watch::PopUpSim %X %Y TRAJECT \[lsort \[glob -nocomplain $Data(PathTRAJECT)/${nameU}*.points\]\]"
         }

         if { $TData(SATDATA) } {

            set y1 [incr y 21]

            $canvas create line 10 $y 35 $y -tags TREE
            $canvas create text 40 $y -text SATDATA -font $GDefs(Font) -anchor w -tags "SATDATA$name TREE"
            $canvas bind SATDATA$name <ButtonPress-1> "Watch::Extract Watch::Data \"$watch\" ; Watch::SelectSim SATDATA$name"
            $canvas bind SATDATA$name <ButtonPress-3> "Watch::Extract Watch::Data \"$watch\" ; Watch::SelectSim SATDATA$name; \
            Watch::PopUpSim %X %Y SATDATA \[lsort \[glob -nocomplain $Data(PathSATDATA)/${nameU}_*_??\]\]"
         }
         $canvas create line 10 $y0 10 $y1 -tags TREE
      }
      incr y 21
   }

   $canvas bind SIGN <Enter> "$canvas config -cursor hand1"
   $canvas bind SIGN <Leave> "$canvas config -cursor left_ptr"
}

#-------------------------------------------------------------------------------
# Nom      : <Watch::Extract>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Extraire les informations de la watch.
#
# Parametres :
#   <IVar>   : Variable
#   <Pool>   : Informations a decoder
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Watch::Extract { IVar Pool } {
   upvar $IVar Var

   set Var(Name)   [lindex $Pool 0]
   set Var(Lat)    [lindex $Pool 1]
   set Var(Lon)    [lindex $Pool 2]
   set Var(Type)   [lindex $Pool 3]

   set Var(SATDATA) [lindex $Pool 4]
   set Var(TRAJECT) [lindex $Pool 5]
   set Var(CANERM)  [lindex $Pool 6]

   set Var(SATC4)        [lindex $Pool 7]
   set Var(SATC45)       [lindex $Pool 8]
   set Var(SATCV)        [lindex $Pool 9]

   set Var(TRAJLevel1)   [lindex $Pool 10]
   set Var(TRAJLevel2)   [lindex $Pool 11]
   set Var(TRAJLevel3)   [lindex $Pool 12]
   set Var(TRAJUnit)     [lindex $Pool 13]

   set Var(CANERMElev)   [lindex $Pool 14]
   set Var(CANERMNb)     [lindex $Pool 15]

   #----- Lire les especes

   set Var(CANERMSpecieList) ""
   set Var(CANERMSpecies)    ""
   set Var(CANERMSpecie)     ""
   set Var(CANERMIntensity)  ""

   for { set i 0 } { $i < $Var(CANERMNb) } { incr i 1 } {
      set lst [lrange $Pool [expr 16+(5*$i)] [expr 20+(5*$i)]]
      lappend Var(CANERMSpecieList) $lst
      lappend Var(CANERMSpecies)    [lindex $lst 0]

      set Var(CANERMSpecie)    [lindex $lst 0]
      set Var(CANERMIntensity) [lindex $lst 1]
   }

   catch { Watch::ParamsInit }
}

#-------------------------------------------------------------------------------
# Nom      : <Watch::IsoDispatch>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE -
#
# But      : Recoit la ligne de retour du selecteur, en extrait la partie commande et la
#            partie data et appelle la fonction necessaire au traitement.
#
# Parametres      :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Watch::IsoDispatch { Ligne } {
   variable Data

  if { [ComboBox::Index $Data(Tab2).species.list.box exact [lindex $Ligne 0]] == -1 } {

      set Data(CANERMSpecie)         [lindex $Ligne 0]
      set Data(CANERMIntensity)      [lindex $Ligne 1]
      lappend Data(CANERMSpecieList) [lrange $Ligne 0 4]

      ComboBox::Add $Data(Tab2).species.list.box $Data(CANERMSpecie)
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Watch::IsoDelete>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Supprime un des choix de selection dans la liste des isotopes.
#
# Parametres  :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Watch::IsoDelete { } {
   variable Data

   #----- Rechercher dans la liste l'espece correspondant

   foreach item $Data(CANERMSpecieList) {

      if { $Data(CANERMSpecie) == [lindex $item 0] } {

         set idx [lsearch $Data(CANERMSpecieList) $item]
         set Data(CANERMSpecieList) [lreplace $Data(CANERMSpecieList) $idx $idx]
         set Data(CANERMSpecie)     ""
         set Data(CANERMIntensity)  ""
         ComboBox::Del $Data(Tab2).species.list.box [lindex $item 0]
         break
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Watch::IsoModif>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Modifie la valeur de l'intensite de l'isotope selectionnee.
#
# Parametres  :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Watch::IsoModif { } {
   variable Data

   #----- Rechercher dans la liste l'espece correspondant

   foreach item $Data(CANERMSpecieList) {

      if { $Data(CANERMSpecie) == [lindex $item 0] } {

         set Data(CANERMIntensity) [format "%1.2E" $Data(CANERMIntensity)]
         set idx [lsearch $Data(CANERMSpecieList) $item]
         set item [lreplace $item 1 1 $Data(CANERMIntensity)]
         set Data(CANERMSpecieList) [lreplace $Data(CANERMSpecieList) $idx $idx $item]
         break
      }
   }
   focus -force .
}

#-------------------------------------------------------------------------------
# Nom      : <Watch::IsoSelect>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche l'intensite de l'isotope selectionne.
#
# Parametres  :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Watch::IsoSelect { } {
   variable Data

   #----- Rechercher dans la liste l'isotope correspondant

   foreach item $Data(CANERMSpecieList) {

      if { $Data(CANERMSpecie) == [lindex $item 0] } {
         set Data(CANERMIntensity) [lindex $item 1]
         break
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Watch::New>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer une nouvelle Watch dans la liste existante.
#
# Parametres:
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Watch::New { } {
   global GDefs
   variable Data
   variable Error

   #----- Verifier la validitee des parametres

   if { $Model::Data(Name)=="" || $Model::Data(Lat1)=="" || $Model::Data(Lon1)=="" } {
       Dialog::CreateError .expnew [lindex $Error(Info) $GDefs(Lang)] $GDefs(Lang)
       return 0
   }

   if { "$Model::Data(Unit)" == "DDD MM" } {
      Model::SwitchCoord
   }

   set Data(Lat)  [format "%2.6f" $Model::Data(Lat1)]
   set Data(Lon)  [format "%2.6f" $Model::Data(Lon1)]
   set Data(Type) $Model::Data(Type)

   regsub -all "\[^a-zA-Z0-9\]" $Model::Data(Name) "-" Data(Name)
   regsub -all "\[-\]\[-\]*" $Data(Name) "-" Data(Name)
   set Data(Name) [string toupper $Data(Name)]

   #----- On verifie que le nom n'existe pas deja

   foreach item $Data(List) {
      if { "[lindex $item 0]"=="$Data(Name)_WATCH" } {
         Dialog::CreateError . [lindex $Error(Exist) $GDefs(Lang)] $GDefs(Lang)
         return 0
      }
   }

   set line "$Data(Name)_WATCH $Data(Lat) $Data(Lon) $Data(Type) 0 0 0 0 0 0 250 500 700 MILLIBARS"
   Debug::TraceProc "Adding automated source : $line"
   exec echo "$line" >> $Data(File)

   Model::Check 0
   Model::TypeSelect none 2
   return 1
}

#-------------------------------------------------------------------------------
# Nom      : <Watch::Params>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Interface des parametres d'une Watch
#
# Parametres :
#   <Frame>  : Identificateur du frame
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Watch::Params { Frame } {
   global   GDefs
   variable Lbl
   variable Bubble
   variable Data

   if { ![winfo exists .watchparams] } {

      toplevel     .watchparams
      wm transient .watchparams $Frame
      wm geometry  .watchparams =320x200+[winfo rootx $Frame]+[winfo rooty $Frame]
      wm resizable .watchparams 0 0

      TabFrame::Create .watchparams.sims 1 ""
      pack .watchparams.sims -side top -fill both -expand true -padx 5 -pady 5

      #----- Modele Trajectoire

      set Data(Tab1) [TabFrame::Add .watchparams.sims 1 TRAJECT False]

         checkbutton $Data(Tab1).enable -text [lindex $Lbl(Enable) $GDefs(Lang)] -variable Watch::Data(TRAJECT) -command "Watch::ParamsInit"
         pack $Data(Tab1).enable -side top -padx 5 -pady 10 -anchor w

         frame $Data(Tab1).level
            frame $Data(Tab1).level.lbl
               label $Data(Tab1).level.lbl.lv1 -text "[lindex $Lbl(Level) $GDefs(Lang)] 1"
               label $Data(Tab1).level.lbl.lv2 -text "[lindex $Lbl(Level) $GDefs(Lang)] 2"
               label $Data(Tab1).level.lbl.lv3 -text "[lindex $Lbl(Level) $GDefs(Lang)] 3"
               pack $Data(Tab1).level.lbl.lv1 $Data(Tab1).level.lbl.lv2 $Data(Tab1).level.lbl.lv3 -side top -padx 5 -ipady 1 -fill y
            frame $Data(Tab1).level.entry
               entry $Data(Tab1).level.entry.lv1 -width 7 -relief sunken -bd 1 -textvar Watch::Data(TRAJLevel1) -bg $GDefs(ColorLight)
               entry $Data(Tab1).level.entry.lv2 -width 7 -relief sunken -bd 1 -textvar Watch::Data(TRAJLevel2) -bg $GDefs(ColorLight)
               entry $Data(Tab1).level.entry.lv3 -width 7 -relief sunken -bd 1 -textvar Watch::Data(TRAJLevel3) -bg $GDefs(ColorLight)
               pack $Data(Tab1).level.entry.lv1 $Data(Tab1).level.entry.lv2 \
                  $Data(Tab1).level.entry.lv3 -side top -fill y -ipady 1
            button $Data(Tab1).level.unit -textvariable Watch::Data(TRAJUnit) -bd 1 -command "Watch::SwitchElev"
            pack $Data(Tab1).level.lbl $Data(Tab1).level.entry $Data(Tab1).level.unit -side left -fill y

      #----- Modele CANERM

      set Data(Tab2) [TabFrame::Add .watchparams.sims 1 CANERM False]

         checkbutton $Data(Tab2).enable -text [lindex $Lbl(Enable) $GDefs(Lang)] -variable Watch::Data(CANERM) \
            -command "Watch::ParamsInit" -anchor w
         pack $Data(Tab2).enable -side top -padx 5 -pady 10 -anchor w

         frame $Data(Tab2).species
            frame $Data(Tab2).species.list
               label $Data(Tab2).species.list.lbl -text [lindex $Lbl(Species) $GDefs(Lang)] -width 10 -anchor w

               button $Data(Tab2).species.list.plus -bd 1 -bitmap @$GDefs(Dir)/Resources/Bitmap/plus.ico \
                  -command "IsoBox::Create $Data(Tab2).species Watch::IsoDispatch" -width 12
               button $Data(Tab2).species.list.moins -bd 1 -bitmap @$GDefs(Dir)/Resources/Bitmap/minus.ico  \
                  -command "Watch::IsoDelete" -width 12
               ComboBox::Create $Data(Tab2).species.list.box Watch::Data(CANERMSpecie) noedit unsorted nodouble \
                  $Data(CANERMSpecieMax) "" 7 5 "Watch::IsoSelect"
               pack $Data(Tab2).species.list.lbl $Data(Tab2).species.list.box \
                    $Data(Tab2).species.list.plus $Data(Tab2).species.list.moins -side left -fill y

            frame $Data(Tab2).species.intensity
               label $Data(Tab2).species.intensity.lbl -text [lindex $Lbl(Intensity) $GDefs(Lang)] -width 10 -anchor w
               entry $Data(Tab2).species.intensity.entry -width 12 -relief sunken -bd 1 \
                  -textvar Watch::Data(CANERMIntensity) -bg $GDefs(ColorLight)
               pack $Data(Tab2).species.intensity.lbl -side left
               pack $Data(Tab2).species.intensity.entry -side left -fill x

            frame $Data(Tab2).species.elev
               label $Data(Tab2).species.elev.lbl -text [lindex $Lbl(Elev) $GDefs(Lang)] -width 10 -anchor w
               entry $Data(Tab2).species.elev.entry -width 12 -relief sunken -bd 1 \
                  -textvar Watch::Data(CANERMElev) -bg $GDefs(ColorLight)
               pack $Data(Tab2).species.elev.lbl -side left
            pack $Data(Tab2).species.elev.entry -side left -fill x
            pack $Data(Tab2).species.list $Data(Tab2).species.intensity \
               $Data(Tab2).species.elev -fill x -anchor w

      #----- Donnees Satellitaires

      set Data(Tab3) [TabFrame::Add .watchparams.sims 1 SATDATA False]

         checkbutton $Data(Tab3).enable -text [lindex $Lbl(Enable) $GDefs(Lang)] -variable Watch::Data(SATDATA) \
            -command "Watch::ParamsInit" -anchor w
         pack $Data(Tab3).enable -side top -padx 5 -pady 10 -anchor w

         frame $Data(Tab3).channels -relief sunken -bd 1
            checkbutton $Data(Tab3).channels.c4 -text " [lindex $Lbl(Channel) $GDefs(Lang)] 4   " \
               -variable Watch::Data(SATC4) -bd 1 -indicatoron false
            checkbutton $Data(Tab3).channels.c45 -text " [lindex $Lbl(Channel) $GDefs(Lang)] 4-5 " \
               -variable Watch::Data(SATC45) -bd 1 -indicatoron false
            checkbutton $Data(Tab3).channels.cv -text " [lindex $Lbl(Channel) $GDefs(Lang)] Vis " \
               -variable Watch::Data(SATCV) -bd 1 -indicatoron false
            pack $Data(Tab3).channels.c4 $Data(Tab3).channels.c45 \
               $Data(Tab3).channels.cv -side top -ipady 2

      #----- Commandes

      frame .watchparams.command -relief ridge -bd 2
         button .watchparams.command.apply -text [lindex $Lbl(Apply) $GDefs(Lang)] \
            -command "Watch::Write" -bd 1
         button .watchparams.command.close -text [lindex $Lbl(Close) $GDefs(Lang)]  -bd 1 \
            -command "destroy .watchparams"
         pack .watchparams.command.apply .watchparams.command.close -side left -fill x -expand true
      pack .watchparams.command -side top -fill x -padx 5 -pady 5

      bind $Data(Tab2).species.intensity.entry <Leave> "Watch::IsoModif"
      TabFrame::Select .watchparams.sims 0
   }

   ParamsInit

   #----- Installer les parametres

   if { $Data(CANERM) } {

      ComboBox::DelAll  $Data(Tab2).species.list.box

      foreach item $Data(CANERMSpecieList) {
         ComboBox::Add $Data(Tab2).species.list.box [set Data(CANERMSpecie) [lindex $item 0]]
      }
      IsoSelect
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Watch::ParamsInit>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialisation des parametres de simulation.
#
# Parametres :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Watch::ParamsInit { } {
   global   GDefs
   variable Data
   variable Lbl

   wm title .watchparams "[lindex $Lbl(Title) $GDefs(Lang)]:$Data(Name)"

   #----- Verifier le modele Trajectoire

   if { $Data(TRAJECT) } {
      pack after $Data(Tab1).enable  $Data(Tab1).level top
   } else {
      pack forget  $Data(Tab1).level
   }

   #----- Verifier le modele CANERM

   if { $Data(CANERM) } {
      pack after $Data(Tab2).enable $Data(Tab2).species top
   } else {
      pack forget $Data(Tab2).species
   }

   #----- Verifier les donnees satellitaires

   if { $Data(SATDATA) } {
      pack after $Data(Tab3).enable $Data(Tab3).channels top
   } else {
      pack forget $Data(Tab3).channels
   }

   #----- Ajuster les parametres selon le type de source

   switch "$Data(Type)" {
      0 {
         $Data(Tab3).channels.c4  configure -state normal
         $Data(Tab3).channels.c45 configure -state normal
         $Data(Tab3).channels.cv  configure -state normal

         if { $Data(TRAJECT)==0 } {
            set Data(TRAJUnit)  "MILLIBARS"
            set Data(TRAJLevel1) 700.0
            set Data(TRAJLevel2) 500.0
            set Data(TRAJLevel3) 250.0
         }

         if { $Data(CANERM)==0 } {
            set Data(CANERMElev)       10000.0
            set Data(CANERMSpecieList) [list [lrange [IsoBox::Get VOLCAN] 0 4] ]
            set Data(CANERMIntensity)  1.0e+18
            set Data(CANERMSpecie)     VOLCAN
         }
      }
      default {
         $Data(Tab3).channels.c4  configure -state disabled
         $Data(Tab3).channels.c45 configure -state disabled
         $Data(Tab3).channels.cv  configure -state normal

         if { $Data(TRAJECT)==0 } {
            set Data(TRAJUnit)  "METRES"
            set Data(TRAJLevel1) 500.0
            set Data(TRAJLevel2) 1500.0
            set Data(TRAJLevel3) 3000.0
         }
         if { $Data(CANERM)==0 } {
            set Data(CANERMElev)       0.0
            set Data(CANERMSpecieList) [list [lrange [IsoBox::Get 137-Cs] 0 4] ]
            set Data(CANERMIntensity)  1.0e+00
            set Data(CANERMSpecie)     137-Cs
           }
        }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Watch::PopUp>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Afficher le popup contextuel des Watchs.
#
# Parametres    :
#    <X>        : ...
#    <Y>        : ...
#
# Retour :
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Watch::PopUp { X Y } {
   global   GDefs
   variable Data
   variable Lbl

   if { ![winfo exists .watchpop] } {

      menu .watchpop -tearoff 0 -bd 1 -type normal -activeborderwidth 1
         .watchpop add command -label ""  -command "Model::TypeSelect none 2 \$Watch::Data(Name); SPI::Locate \$Watch::Data(Lat) \$Watch::Data(Lon)" \
             -background $GDefs(ColorHighLight) -activebackground $GDefs(ColorHighLight)
         .watchpop add command -label [lindex $Lbl(Params) $GDefs(Lang)] -command " Watch::Params ."
         .watchpop add separator
         .watchpop add command -label [lindex $Lbl(Suppress) $GDefs(Lang)] -command "Watch::Suppress"
   }

   .watchpop entryconfigure 0 -label "$Data(Name)"
   tk_popup .watchpop $X $Y 0
}

#----------------------------------------------------------------------------
# Nom      : <Watch::PopUpSim>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Afficher le popup contextuel des simulations Watchs.
#
# Parametres    :
#    <X>        : ...
#    <Y>        : ...
#
# Retour :
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Watch::PopUpSim { X Y Mode args } {
   global   GDefs
   variable Data
   variable Lbl

   if { ![winfo exists .watchpopsim] } {

      menu .watchpopsim -tearoff 0 -bd 1 -type normal -activeborderwidth 1
         .watchpopsim add command -label ""  -command "SPI::Locate \$Watch::Data(Lat) \$Watch::Data(Lon)" \
            -background $GDefs(ColorHighLight) -activebackground $GDefs(ColorHighLight)
         .watchpopsim add separator
         .watchpopsim add command -label [lindex $Lbl(Result) $GDefs(Lang)] -command ""
   }

   #----- Initialiser le menu

   .watchpopsim entryconfigure 0 -label "$Data(Name)"

   if { $args != "" } {
      .watchpopsim entryconfigure 2 -state normal
      if { "$Mode"=="TRAJECT" } {
         .watchpopsim entryconfigure 2 -command "SPI::FileOpen NEW TrajBox \"\" [list $FileBox::Type(TRAJ)] $args"
      } else {
         .watchpopsim entryconfigure 2 -command "SPI::FileOpen NEW FieldBox \"\" [list $FileBox::Type(FSTD)] $args"
      }
   } else {
         .watchpopsim entryconfigure 2 -state disabled
   }

   tk_popup .watchpopsim $X $Y 0
}

#-------------------------------------------------------------------------------
# Nom      : <Watch::SelectBranch>
# Creation : Octobre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectuer la selection de la branche.
#
# Parametres :
#   <No>     : Numero de la branche.
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Watch::SelectBranch { No } {
   variable Data

   set idx [lsearch -exact $Data(Branch) $No]

   if { $idx == -1 } {
      lappend Data(Branch) $No
   } else {
      set Data(Branch) [lreplace $Data(Branch) $idx $idx]
   }
   Watch::CreateTree
}

#---------------------------------------------------------------------------
# Nom      : <Watch::SelectSim>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Selection d'une simulation.
#
# Parametres :
#   <Tag>    : Tag associe
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Watch::SelectSim { Tag } {
   variable Data

   if { $Tag != "" } {
      eval $Data(Frame).info.canvas coords SIMSELECT [$Data(Frame).info.canvas bbox $Tag]
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Watch::Suppress>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Supprimer une Watch.
#
# Parametres  :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Watch::Suppress { } {
   global   GDefs
   variable Data
   variable Msg
   variable Lbl

   set nodel [Dialog::CreateDefault . 400 "Message" "[lindex $Msg(Suppress) $GDefs(Lang)]\n\n$Data(Name)" \
     warning 0 [lindex $Lbl(Yes) $GDefs(Lang)] [lindex $Lbl(No) $GDefs(Lang)]]

   if { $nodel } {
      return
   }

   #----- Supprimer la simulation et ses descendants

   Debug::TraceProc "Suppressing watch:  $Data(Name)"

   file rename -force $Data(File) $Data(File).old
   catch { exec grep -i -v $Data(Name).* $Data(File).old > $Data(File) }
   exec chmod 664           $Data(File)
   exec chgrp $GDefs(Group) $Data(File)

   #----- Supprimer tous les resultats pour la source

   set name [string tolower $Data(Name)]
   catch { eval file delete -force [glob $Sim(TRAJPathData)/*${name}_*] }
   catch { eval file delete -force [glob $Sim(CANPathData)/${name}_*] }
   catch { eval file delete -force [glob $Sim(SATPathData)/${name}_*] }

   #----- Relire les experiences

   Model::Check 0
   Model::TypeSelect none 2
}

#---------------------------------------------------------------------------
# Nom      : <Watch::Read>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Lire la liste des watchs.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Watch::Read { } {
   variable Data

   set Data(List) ""

   if { [file readable $Data(File)] } {
      set file [open $Data(File)]

      while { ![eof $file] } {
         gets $file line
         if { $line != "" && [string range $line 0 0] != "#" } {
            lappend Data(List) $line
         }
      }
      close $file
      set Data(List) [lsort -dictionary $Data(List)]
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Watch::SwitchElev>
# Creation : Octobre 1999 - J.P. Gauthier - CMC/CMOE
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

proc Watch::SwitchElev { } {
   variable Data

   if { [string compare $Data(TRAJUnit) "MILLIBARS"] == 0 } {

      set Data(TRAJUnit) "METRES"
      set Data(TRAJLevel1) [format "%0.1f" [Convert::Millibar2Meter $Data(TRAJLevel1)]]
      set Data(TRAJLevel2) [format "%0.1f" [Convert::Millibar2Meter $Data(TRAJLevel2)]]
      set Data(TRAJLevel3) [format "%0.1f" [Convert::Millibar2Meter $Data(TRAJLevel3)]]

   } else {

      set Data(TRAJUnit) "MILLIBARS"
      set Data(TRAJLevel1) [format "%0.1f" [Convert::Meter2Millibar $Data(TRAJLevel1)]]
      set Data(TRAJLevel2) [format "%0.1f" [Convert::Meter2Millibar $Data(TRAJLevel2)]]
      set Data(TRAJLevel3) [format "%0.1f" [Convert::Meter2Millibar $Data(TRAJLevel3)]]
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Watch::Write>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Inscrit les informations de simulation dans le pool
#
# Parametres :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Watch::Write { } {
   global GDefs
   variable Data
   variable Error
   variable Sim

   #----- Verification des valeurs pour CANERM

   if { $Data(CANERM) } {
      if { [llength $Data(CANERMSpecieList)] == 0 } {
         Dialog::CreateError . [lindex $Error(CANERM) $GDefs(Lang)] $GDefs(Lang)
         return
      }
   }

   #----- Verification des valeurs pour TRAJ

   if { $Data(TRAJECT) } {
      if { $Data(TRAJLevel1) == "" || $Data(TRAJLevel2) == "" || $Data(TRAJLevel3) == "" } {
         Dialog::CreateError . [lindex $Error(TRAJ) $GDefs(Lang)] $GDefs(Lang)
         return
      }
   }

   #----- Creation des options de CANERM

   if { $Data(CANERM) } {
      set species [llength $Data(CANERMSpecieList)]
      foreach item $Data(CANERMSpecieList) {
         set species "$species $item"
      }
   } else {
      set species 0
   }

   #----- Formater la ligne de description

   set line "$Data(Name) $Data(Lat) $Data(Lon)\
   $Data(Type) $Data(SATDATA) $Data(TRAJECT) $Data(CANERM)\
   $Data(SATC4) $Data(SATC45) $Data(SATCV)\
   $Data(TRAJLevel1) $Data(TRAJLevel2) $Data(TRAJLevel3) $Data(TRAJUnit) $Data(CANERMElev)\
   $species"

   #----- Retirer la source du fichier de source actives

   file rename -force $Data(File) $Data(File).old
   catch { exec grep -i -v $Data(Name).* $Data(File).old > $Data(File) }

   #----- Ajouter la ligne de description

   Debug::TraceProc "Writing automated source info : $line"
   exec echo $line >> $Data(File)

   exec chmod 664           $Data(File)
   exec chgrp $GDefs(Group) $Data(File)

   #----- Relire les experiences

   Model::Check 0

   destroy .watchparams
}
