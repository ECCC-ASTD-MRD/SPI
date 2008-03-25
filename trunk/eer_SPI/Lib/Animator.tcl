#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet    : Librairie de definitions pour l'animations des projections
# Fichier   : Animator.tcl
# Creation  : Mai 1999 - J.P. Gauthier - CMC/CMOE
#
# Description: Definitions des procedures et fonctions pour animer les projections
#              et ce qui y est affiche.
#
# Fonctions:
#
#   Animator::Window           { Parent }
#   Animator::EmptyPlayList    { }
#   Animator::GetPlayListField { }
#   Animator::GetPlayListObs   { }
#   Animator::GetPlayList      { }
#   Animator::Limits           { }
#   Animator::Play             { }
#   Animator::PlayFile         { }
#   Animator::Step             { Index }
#   Animator::StepTo           { Idx }
#   Animator::FlyPath          { Cam Type }
#   Animator::FlyPointAdd      { List }
#   Animator::FlyPointDel      { List }
#   Animator::FlyPointSelect   { List }
#   Animator::FlyPointSet      { List }
#   Animator::FlyTo            { Cam Frame Speed }
#
# Remarques :
#   Aucune
#
#===============================================================================

package provide Animator 4.0

proc IdAnimator { Show } {

   if { $Show } {
      puts "(INFO) Loading Standard CMC/CMOE Widget Package Animator Version 4.0"
   }
}

#----- Definitions des constantes

namespace eval Animator {
   variable Data
   variable Play
   variable Fly
   variable Lbl
   variable Param

   set Param(Title)    { "Animateur" "Animator" }
   set Param(Geom)     { 275x275+[winfo rootx $Parent]+[winfo rooty $Parent] }
   set Param(Version)  4.0

   #----- Variable relative au playback

   set Play(Cache)       0               ;#Cache des pixmaps de frame
   set Play(IP3)         1               ;#Validation du IP3 en recherche de champs
   set Play(Cycle)       0               ;#Bouclage de l'animation
   set Play(Stop)        1               ;#Variable de surveillance de l'evenement d'arret
   set Play(Frame)       0               ;#Liste des frames temporel
   set Play(Frames)      ""              ;#Liste des frames temporel
   set Play(Length)      0               ;#Longueur du data
   set Play(Idx)         0               ;#Numero de frame en cours
   set Play(Idx0)        0               ;#Index de depart
   set Play(Idx1)        0               ;#Index de fin
   set Play(VPs)         ""              ;#Liste des viewports
   set Play(Dir)         0               ;#Direction du play (-1 back, 0 stop, 1 forward)
   set Play(Delai)       0               ;#Delai entre les frames en milliemes de secondes
   set Play(Label)       ""              ;#Identification du traitement courant
   set Play(Canvas)      ""              ;#Canvas dans lequel on anime
   set Play(Page)        ""              ;#Frame dans lequel on anime
   set Play(Filename)    ""              ;#Fichier des frames
   set Play(File)        0               ;#Enregistrement des frames
   set Play(Type)        DATE            ;#Type d'animation
   set Play(Types)       "DATE IP1 IP2 IP3 ETIKET"
   set Play(Data)        {}              ;#Data a animer

   set Fly(Speed)        5               ;#Vitesse du vol
   set Fly(List)         {}              ;#Liste des pointrs de controles
   set Fly(Frame)        0
   set Fly(WayNo)        0
   set Fly(Length)       0                           ;#Nombre maximum de frames
   set Fly(Controls)     {}                          ;#
   set Fly(From)         ""                          ;#Liste des positions
   set Fly(Up)           ""                          ;#Liste des aspects
   set Fly(Show)         0                           ;#Affichage des fly points
   set Fly(Lat)          ""                          ;#Liste des latitudes
   set Fly(Lon)          ""                          ;#Liste des longitudes
   set Fly(Path)         DEFAULT                     ;#Path courant
   set Fly(Paths)       { AROUND CIRCLE TO THROUGH }

   #----- Definitions des labels

   set Lbl(Convert)        { "Conversion vers" "Converting to" }
   set Lbl(Done)           { "Fin image" "Done frame" }
   set Lbl(Empty)          { "Vide" "Empty" }
   set Lbl(Stop)           { "Pause" "Stop" }
   set Lbl(Print)          { "Impression" "Printing" }
   set Lbl(Read)           { "Lecture" "Reading" }
   set Lbl(IP3)            { "Valider IP3" "Validate IP3" }
   set Lbl(Data)           { "Données" "Data" }
   set Lbl(Fly)            { "Survol" "Flyby" }
   set Lbl(On)             { "Animer selon" "Animate on" }

   #----- Definitions des bulles d'aide

   set Bubble(Off)         { "Fermer la boite d'animation" "Close the animator window" }
   set Bubble(Cache)       { "Cache les images en memoires" "Cache animation frames in memory" }
   set Bubble(Cycle)       { "Bouclage de l'animation" "Cycle the animation" }
   set Bubble(Rewind)      { "Retour au debut" "Rewind to the beginning" }
   set Bubble(Forwind)     { "Retour a la fin" "Go to end" }
   set Bubble(PlayBack)    { "Jouer vers l'arriere" "Play backward" }
   set Bubble(PlayFile)    { "Enregistrer les images" "Save the frames to file" }
   set Bubble(StepBack)    { "Une image vers l'arriere" "Step on frame backward" }
   set Bubble(Stop)        { "Stop" "Stop" }
   set Bubble(StepForward) { "Une image vers l'avant" "Step one frame forward" }
   set Bubble(PlayForward) { "Jouer vers l'avant" "Play forward" }
   set Bubble(Delai)       { "Delai entre les images (millisecondes)" "Delai between frames (milliseconds)" }
   set Bubble(Idx0)        { "Index de depart de l'animation" "Animation starting index" }
   set Bubble(Idx1)        { "Index de fin de l'animation" "Animation's ending index" }
   set Bubble(State)       { "Action en cours" "Current action" }
   set Bubble(Scroll)      { "Deroulement de l'animation" "Animation scroller" }
   set Bubble(Type)        { "Selection du type d'animation" "Animation type selection" }

   set Bubble(FlyPreset) { "Sélection de vol prédéfini" "Selection of predefined flybys" }
   set Bubble(FlyDel)    { "Supprimer le point de contrôle courant" "Delete the current control point" }
   set Bubble(FlyIns)    { "Insérer un nouveau point de contrôle" "Insert a new control point" }
   set Bubble(FlyRep)    { "Remplacer le point de contrôle courant" "Replace the current control point" }
   set Bubble(FlyOff)    { "Fermer la boite de survol" "Close the flyby window" }
   set Bubble(FlyShow)   { "Afficher le chemin de vol" "Display the flyby path" }
   set Bubble(FlyFly)    { "Animer le survol" "Animate the flyby" }
   set Bubble(FlySpeed)  { "Vitesse du survol" "Flyby speed" }

   catch { package require Bubble ; IdBubble False }
}

proc Animator::Close { } {
   variable Fly

   set Fly(Length) 0
   set Fly(WayNo)  0

   Animator::EmptyPlayList
   destroy .anim
}

#----------------------------------------------------------------------------
# Nom      : <Animator::Window>
# Creation : Mai 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer l'interface d'animation.
#
# Parametres :
#   <Parent> : Identificateur de la fenetre parent
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Animator::Window { { Parent .} } {
   global GDefs
   variable Data
   variable Play
   variable Fly
   variable Lbl
   variable Bubble
   variable Param

   if { [winfo exists .anim] } {
      return
   }

   toplevel     .anim
   wm transient .anim $Parent
   wm title     .anim "[lindex $SPI::Title(SPI) $GDefs(Lang)] $GDefs(Version) ([lindex $Param(Title) $GDefs(Lang)] $Param(Version))"
   eval wm geom .anim $Param(Geom)
   wm protocol  .anim WM_DELETE_WINDOW Animator::Close

   TabFrame::Create .anim.tab 1 {}
   pack .anim.tab -side top -fill both -expand true -padx 2 -pady 2

   set Data(Tab1) [TabFrame::Add .anim.tab 1 [lindex $Lbl(Data) $GDefs(Lang)] False ""]

      frame $Data(Tab1).lbl
         label $Data(Tab1).lbl.lbl0 -text "[lindex $Lbl(On) $GDefs(Lang)] ("
         checkbutton $Data(Tab1).lbl.ip3 -indicatoron true -variable Animator::Play(IP3) -text [lindex $Lbl(IP3) $GDefs(Lang)] -onvalue 1 \
            -offvalue 0 -command "Animator::EmptyPlayList" -indicatoron false -relief flat -bd 1 -overrelief raised
         label $Data(Tab1).lbl.lbl1 -text ")"
         pack $Data(Tab1).lbl.lbl0 $Data(Tab1).lbl.ip3 $Data(Tab1).lbl.lbl1 -side left

      labelframe $Data(Tab1).type -labelwidget $Data(Tab1).lbl
      frame  $Data(Tab1).type.f -relief sunken -bd 1
      foreach type $Play(Types) {
         radiobutton $Data(Tab1).type.f.t$type -indicatoron false -variable Animator::Play(Type) -text $type -value $type \
            -command "Animator::EmptyPlayList" -bd 1 -relief flat -overrelief raised
         pack $Data(Tab1).type.f.t$type -side top -fill x -expand True
      }
      pack $Data(Tab1).type.f -side top -fill both -padx 2 -pady 2
      pack $Data(Tab1).type -side top -fill both -padx 5 -pady 2

   set Data(Tab2) [TabFrame::Add .anim.tab 1 [lindex $Lbl(Fly) $GDefs(Lang)] False ""]
      frame $Data(Tab2).head
         button $Data(Tab2).head.add  -image PLUS -bd 0 -relief flat -overrelief raised -command "Animator::FlyPointAdd $Data(Tab2).way.list.box"
         button $Data(Tab2).head.del  -image MINUS -bd 0 -relief flat -overrelief raised -command "Animator::FlyPointDel $Data(Tab2).way.list.box"
         button $Data(Tab2).head.rep  -image FINGER -bd 0 -relief flat -overrelief raised -command "Animator::FlyPointSet $Data(Tab2).way.list.box"
         checkbutton $Data(Tab2).head.show -image CAMPATH -indicatoron 0 -relief sunken -bd 1 -overrelief raised -offrelief flat -variable Animator::Fly(Show) -onvalue 1 -offvalue 0 \
            -command { projcam configure $Page::Data(Frame) -show $Animator::Fly(Show) ; $Page::Data(Canvas) itemconf $Viewport::Data(VP) -camera $Page::Data(Frame) }
         radiobutton $Data(Tab2).head.fly  -image MODEFLY -indicatoron 0 -relief sunken -bd 1 -overrelief raised -offrelief flat -variable Page::Data(Mode) -value Fly\
            -command { Page::ModeSelect Fly ; set Page::Data(ToolMode) SPI ; set Animator::Fly(Dir) 1; ProjCam::Fly2 $Page::Data(Frame) $Page::Data(Frame) }
         pack $Data(Tab2).head.del $Data(Tab2).head.add $Data(Tab2).head.show -side right -padx 2 -pady 2
      pack $Data(Tab2).head -side top -fill x

      frame $Data(Tab2).way
         frame $Data(Tab2).way.preset -relief sunken -bd 1
         foreach path $Fly(Paths) {
            checkbutton $Data(Tab2).way.preset.d$path -bd 1 -indicatoron False -onvalue $path -offvalue 0 -selectcolor "" \
               -variable Animator::Fly(Path) -image FLY$path -command "$Data(Tab2).way.list.box delete 0 end ; Animator::FlyPath \$Page::Data(Frame) \$Animator:::Fly(Path)"
            pack $Data(Tab2).way.preset.d$path -side top
         }
         pack $Data(Tab2).way.preset -side left  -anchor n

         frame $Data(Tab2).way.list
            listbox $Data(Tab2).way.list.box -relief sunken -bd 1 -selectmode single -width 1 -height 1 -background white  -exportselection False\
               -listvariable Animator::Fly(List) -yscrollcommand "$Data(Tab2).way.list.scroll set"
            scrollbar $Data(Tab2).way.list.scroll -bd 1 -width 10 -command "$Data(Tab2).way.list.box yview"
            scale $Data(Tab2).way.list.speed -from 600 -to 0.1 -resolution 0.1 -variable Animator::Fly(Speed) -relief raised -bd 1 \
               -relief flat -orient vertical -width 15 -sliderlength 10 -command { Animator::FlyPath $Page::Data(Frame) $Animator:::Fly(Path); catch }  -showvalue false
            pack $Data(Tab2).way.list.box -side left -fill both -expand true
            pack $Data(Tab2).way.list.scroll $Data(Tab2).way.list.speed -side left -fill y
         pack $Data(Tab2).way.list -side left -fill both -expand true
         frame $Data(Tab2).way.opt

         pack  $Data(Tab2).way.opt -side left
      pack $Data(Tab2).way -side top -fill both -expand true

      bind $Data(Tab2).way.list.box <Double-ButtonRelease-1> "Animator::FlyPointSelect $Data(Tab2).way.list.box"

      Bubble::Create $Data(Tab2).way.preset     [lindex $Bubble(FlyPreset) $GDefs(Lang)]
      Bubble::Create $Data(Tab2).head.del       [lindex $Bubble(FlyDel) $GDefs(Lang)]
      Bubble::Create $Data(Tab2).head.add       [lindex $Bubble(FlyIns) $GDefs(Lang)]
      Bubble::Create $Data(Tab2).head.rep       [lindex $Bubble(FlyRep) $GDefs(Lang)]
      Bubble::Create $Data(Tab2).head.show      [lindex $Bubble(FlyShow) $GDefs(Lang)]
      Bubble::Create $Data(Tab2).head.fly       [lindex $Bubble(FlyFly) $GDefs(Lang)]
      Bubble::Create $Data(Tab2).way.list.speed [lindex $Bubble(FlySpeed) $GDefs(Lang)]

   frame .anim.params -relief raised -bd 1
      scale .anim.params.frame -from 0 -to 0 -resolution 1 -variable Animator::Play(Idx) -relief raised -bd 1 \
         -relief flat -orient horizontal -width 15 -sliderlength 10 -command "Animator::StepTo" -label "Frame" -showvalue true
      scale .anim.params.lapse -from 0 -to 500 -resolution 10 -variable Animator::Play(Delai) -relief raised -bd 1 \
         -relief flat -orient vertical -width 15 -sliderlength 10 -command ""  -length 35 -showvalue false
      scale .anim.params.idx0 -from 0 -to 0 -resolution 1 -variable Animator::Play(Idx0) -relief raised -bd 1 \
         -relief flat -orient horizontal -width 5 -sliderlength 10 -command "Animator::Range 0" -showvalue false
      scale .anim.params.idx1 -from 0 -to 0 -resolution 1 -variable Animator::Play(Idx1) -relief raised -bd 1 \
         -relief flat -orient horizontal -width 5 -sliderlength 10 -command "Animator::Range 1" -showvalue false
      pack .anim.params.lapse -side right -anchor s
      pack .anim.params.frame .anim.params.idx0 .anim.params.idx1 -side top -fill x -expand true
   pack .anim.params -side top -fill x  -padx 2

   frame .anim.info
      label .anim.info.lbl -relief raised -bd 1 -bg $GDefs(ColorLight) -textvariable Animator::Play(Label) -anchor w -width 23
      pack .anim.info.lbl -side left -fill both -expand true -ipady 2
   pack .anim.info -side top -fill x -padx 2

   frame .anim.comm
      button .anim.comm.off -image ERROR -bd 1 -command { Animator::EmptyPlayList ; destroy .anim }
      checkbutton .anim.comm.cache -image VCRUNLOCK -selectimage VCRLOCK -bd 1 -variable Animator::Play(Cache) -indicatoron False -selectcolor "" -onvalue 1 -offvalue 0
      checkbutton .anim.comm.cycle -image VCRCYCLE -bd 1 -variable Animator::Play(Cycle) -indicatoron False -selectcolor ""
      button .anim.comm.rewind -image VCRREWIND -bd 1 \
         -command { set Animator::Play(Idx) $Animator::Play(Idx0) ; set Animator::Play(Stop) 1 ; Animator::Play }
      radiobutton .anim.comm.playback -image VCRPLAYB -bd 1 -variable Animator::Play(Dir) -indicatoron False -value -1 -selectcolor ""\
         -command { set Animator::Play(Stop) 0 ; Animator::Play }
      button .anim.comm.stepback -image VCRFRAMEB -bd 1 \
         -command { Animator::Step -1 }
      button .anim.comm.stop -image VCRSTOP -bd 1 \
         -command { set Animator::Play(Stop) 1 }
      button .anim.comm.stepforward -image VCRFRAMEF -bd 1 \
         -command { Animator::Step 1 }
      radiobutton .anim.comm.playforward -image VCRPLAYF -bd 1 -variable Animator::Play(Dir) -indicatoron False -value 1  -selectcolor ""\
         -command { set Animator::Play(Stop) 0 ; Animator::Play }
      button .anim.comm.forwind -image VCRFORWIND -bd 1 \
         -command { set Animator::Play(Idx) $Animator::Play(Idx1) ; set Animator::Play(Stop) 1 ; Animator::Play }
      radiobutton .anim.comm.playfile  -image VCRSAVE -bd 1 -variable Animator::Play(File) -indicatoron False -value 1  -selectcolor "" \
         -command { Animator::PlayFile }
      pack .anim.comm.off .anim.comm.cache .anim.comm.cycle .anim.comm.playback .anim.comm.stepback .anim.comm.rewind \
           .anim.comm.stop .anim.comm.forwind .anim.comm.stepforward .anim.comm.playforward .anim.comm.playfile\
           -side left -fill both -expand true
   pack .anim.comm -side top -fill x -padx 2 -pady 2

   #----- Creation des bulles d'aides

   Bubble::Create .anim.comm.cache       [lindex $Bubble(Cache) $GDefs(Lang)]
   Bubble::Create .anim.comm.cycle       [lindex $Bubble(Cycle) $GDefs(Lang)]
   Bubble::Create .anim.comm.rewind      [lindex $Bubble(Rewind) $GDefs(Lang)]
   Bubble::Create .anim.comm.forwind     [lindex $Bubble(Forwind) $GDefs(Lang)]
   Bubble::Create .anim.comm.playback    [lindex $Bubble(PlayBack) $GDefs(Lang)]
   Bubble::Create .anim.comm.stepback    [lindex $Bubble(StepBack) $GDefs(Lang)]
   Bubble::Create .anim.comm.stop        [lindex $Bubble(Stop) $GDefs(Lang)]
   Bubble::Create .anim.comm.stepforward [lindex $Bubble(StepForward) $GDefs(Lang)]
   Bubble::Create .anim.comm.playforward [lindex $Bubble(PlayForward) $GDefs(Lang)]
   Bubble::Create .anim.comm.playfile    [lindex $Bubble(PlayFile) $GDefs(Lang)]
   Bubble::Create .anim.comm.off         [lindex $Bubble(Off) $GDefs(Lang)]
   Bubble::Create .anim.params.lapse     [lindex $Bubble(Delai) $GDefs(Lang)]
   Bubble::Create .anim.params.frame     [lindex $Bubble(Scroll) $GDefs(Lang)]
   Bubble::Create .anim.params.idx0      [lindex $Bubble(Idx0) $GDefs(Lang)]
   Bubble::Create .anim.params.idx1      [lindex $Bubble(Idx1) $GDefs(Lang)]
   Bubble::Create .anim.info.lbl         [lindex $Bubble(State) $GDefs(Lang)]
   Bubble::Create .$Data(Tab1).type      [lindex $Bubble(Type) $GDefs(Lang)]

   TabFrame::Select .anim.tab 0
}

#----------------------------------------------------------------------------
# Nom      : <Animator::EmptyPlayList>
# Creation : Decembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Vide la liste des frames d'animation.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Animator::EmptyPlayList { } {
   global GDefs
   variable Play


  if { [llength $Play(Frames)] == 0 } {
      return
   }

   projection configure $Play(Page) -date $Viewport::Data(Seconds)
   projection configure $Play(Page) -date 0
   set Play(Stop)    1
   $Play(Canvas) configure -cursor watch

   #----- Liberer les champs

   set newlist {}

   foreach field $FSTD::Data(ListTool) {
      if { [string match ANI* $field] } {
         fstdfield free $field
      } else {
         lappend newlist $field
      }
   }
   set FSTD::Data(ListTool) $newlist
   set Obs::Data(ListTool) {}

   #----- Liberer les listes et Reinitialiser les frames de viewports

   foreach vp $Play(VPs) {
      catch { unset Play(Data$vp) }
      foreach frame $Play(Frames) {
         catch { unset Play($vp$frame) }
      }
      $Play(Canvas) itemconf $vp -frame 0 -data ""
   }

   #----- Modifier l'indicateur

   set Play(Frames)  ""
   set Play(VPs)     ""
   set Play(Label)   ""
   CVClock::Time $Play(Page) -1 0

   Viewport::UpdateData $Play(Page)
   $Play(Canvas) configure -cursor left_ptr
}

#----------------------------------------------------------------------------
# Nom      : <Animator::GetPlayList>
# Creation : Decembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Creation de la liste des frames d'animation.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Animator::GetPlayList { } {
   variable Play

   if { $Play(Stop) } {
      set Play(Dir) 0
      return
   }

   #----- initialiser le processus

   $Play(Canvas) configure -cursor watch
   update idletasks

   set Play(Frames) {}

   #----- Recuperer les diverses donnees temporelles

   Animator::GetPlayListField

   if { $Play(Type)=="DATE" } {
      Animator::GetPlayListTraj
      Animator::GetPlayListObs
   }

   #----- Trier les champs en ordre temporel

   set Play(Frames) [lsort -dictionary -increasing -unique $Play(Frames)]
   set Play(VPs)    [lsort -dictionary -increasing -unique $Play(VPs)]
   set Play(Length) [llength $Play(Frames)]
   set Play(Length) [expr $Play(Length)>0?$Play(Length)-1:0]

   Animator::Limits

   $Play(Canvas) configure -cursor left_ptr
}

#----------------------------------------------------------------------------
# Nom      : <Animator::Limits>
# Creation : Mai 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Definir les limites d'animations.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Animator::Limits { } {
   variable Play
   variable Fly

   set idx1 [expr $Play(Length)>$Fly(Length)?$Play(Length):$Fly(Length)]

   .anim.params.frame configure -to $idx1
   .anim.params.idx0  configure -to $idx1
   .anim.params.idx1  configure -to $idx1

   set Play(Idx0)  0
   set Play(Idx1)  $idx1

   if { $Play(Idx)>$Play(Idx1) } {
      set Play(Idx) $Play(Idx1)
   }
}

#----------------------------------------------------------------------------
# Nom      : <Animator::GetPlayListField>
# Creation : Decembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Recuperer les champs.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Animator::GetPlayListField { } {
   global GDefs env
   variable Play
   variable Lbl

   set no 0
   foreach vp $Play(VPs) {
      foreach fld $Viewport::Data(Data$vp) {

         if { ![fstdfield is $fld] } {
            continue
         }

         set tags [fstdfield stats $fld -tag]
         set box  [lindex $tags 2]

         #----- On recupere les parametres du champs selectionne

         set var     [fstdfield define $fld -NOMVAR]
         set ip1     [fstdfield define $fld -IP1]
         set ip2     [fstdfield define $fld -IP2]
         set ip3     [fstdfield define $fld -IP3]
         set etiket  [fstdfield define $fld -ETIKET]
         set date    [fstdfield define $fld -DATEV]

         foreach field [FieldBox::GetContent $box] {

            set fid     [lindex $field 0]
            set idx     [lindex $field 1]
            set tvar    [lindex $field 2]
            set tip1    [lindex $field 4]
            set tip2    [lindex $field 5]
            set tip3    [lindex $field 6]
            set tetiket [string trim [lindex $field 7]]
            set tdate   [lindex $field 9]

            #----- Si ce champs fait partie de la serie temporelle correspondante

            set in 0
            switch $Play(Type) {

               "IP1"     { if { $var==$tvar && "$etiket"=="$tetiket" && "$date"=="$tdate" && [expr $Play(IP3)?$ip3==$tip3:1] } { set in  1 } }
               "IP2"     { if { $var==$tvar && "$etiket"=="$tetiket" &&  $ip1==$tip1 && [expr $Play(IP3)?$ip3==$tip3:1] } { set in  1 } }
               "IP3"     { if { $var==$tvar && "$etiket"=="$tetiket" && "$date"=="$tdate" &&  $ip1==$tip1 } { set in  1 } }
               "ETICKET" { if { $var==$tvar && "$date"=="$tdate" && $ip1==$tip1 && [expr $Play(IP3)?$ip3==$tip3:1] } { set in  1 } }
               "DATE"    { if { $var==$tvar && $ip1==$tip1 && "$etiket"=="$tetiket" && [expr $Play(IP3)?$ip3==$tip3:1] } { set in  1 } }
            }

            if { $in } {
               set Play(Label) "[lindex $Lbl(Read) $GDefs(Lang)] $var $fid $idx"
               update idletask

               fstdfield read ANI$no $fid $idx
               fstdfield stats ANI$no -tag $tags

               lappend FSTD::Data(ListTool) ANI$no
               FSTD::ParamUpdate ANI$no

               switch $Play(Type) {
                  "IP1"     { set info [fstdgrid convip $tip1] }
                  "IP2"     { set info $tip2 }
                  "IP3"     { set info $tip3 }
                  "ETICKET" { set info $tetiket }
                  "DATE"    { set dat  [fstdstamp todate [fstdfield define ANI$no -DATEV]]
                              set info [fstdstamp toseconds [fstdfield define ANI$no -DATEV]]
                            }
               }
               #----- Ajouter a la liste du frame temporel correspondant

               if { ![info exists Play($vp$info)] } {
                  set Play($vp$info) ""
                  lappend Play(Frames) $info
               }

               lappend Play($vp$info) ANI$no
               incr no
            }

            #----- On verifie les demandes d'arret

            update
            if { $Play(Stop) } {
               set Play(File)  0
               set Play(Dir) 0
               Animator::EmptyPlayList
               return
            }
         }
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Animator::GetPlayListObs>
# Creation : Decembre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Recupere les temps des observations.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Animator::GetPlayListObs { } {
   variable Play

   foreach vp $Play(VPs) {
      foreach obs [Viewport::Assigned $Play(Page) $vp observation] {

         set tags [observation stats $obs -tag]
         set box  [lindex $tags 2]

         foreach o [ObsBox::GetContent $box] {

            if { [observation configure $obs -desc]==[observation configure $o -desc] } {
               set sec [observation define $o -DATE]

               if { ![info exists Play($vp$sec)] } {
                  set Play($vp$sec) ""
                  lappend Play(Frames) $sec
               }
               observation stats $o -tag $tags
               lappend Play($vp$sec) $o
               lappend Obs::Data(ListTool) $o
            }
            #----- On verifie les demandes d'arret

            update
            if { $Play(Stop) } {
               set Play(File)  0
               set Play(Dir) 0
               Animator::EmptyPlayList
               return
            }
         }
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Animator::GetPlayListTraj>
# Creation : Decembre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Recupere les temps des trajectoires.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Animator::GetPlayListTraj { } {
   variable Play

   foreach vp $Play(VPs) {
      foreach traj [Viewport::Assigned $Play(Page) $vp trajectory] {
         foreach parcel [trajectory define $traj -PARCELS] {
            set sec [lindex $parcel 0]

            if { ![info exists Play($vp$sec)] } {
               set Play($vp$sec) ""
               lappend Play(Frames) $sec
               lappend Play(VPs)    $vp
            }
         }

         #----- Ajouter les trajectoires a tout les temps selon le VP
         #      de cette maniere, elles restent affichees passer la date
         #      de fin

         foreach sec $Play(Frames) {
            lappend Play($vp$sec) $traj

            #----- On verifie les demandes d'arret

            update
            if { $Play(Stop) } {
               set Play(File)  0
               set Play(Dir) 0
               Animator::EmptyPlayList
               return
            }
         }
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Animator::Step>
# Creation : Mai 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectue un saut frame par frame.
#
# Parametres :
#   <Incr>   : Increment du frame
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Animator::Step { Incr } {
   variable Play

   #----- Verifier les limites

   incr Play(Idx) $Incr

   if { $Play(Idx)>$Play(Idx1) } {
      set Play(Idx) $Play(Idx1)
   }
   if { $Play(Idx)<$Play(Idx0) } {
      set Play(Idx) $Play(Idx0)
   }

   #----- Affectuer la saut de frame

   set Play(Stop) 1
   Animator::Play
}

#----------------------------------------------------------------------------
# Nom      : <Animator::StepTo>
# Creation : Mai 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectue un saut a un frame precis.
#
# Parametres :
#   <Idx>    : Index du frame
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Animator::StepTo { Idx } {
   variable Play

   #----- Affectuer la saut de frame

   if { $Play(Idx)>$Play(Idx1) } {
      set Play(Idx) $Play(Idx1)
   }
   if { $Play(Idx)<$Play(Idx0) } {
      set Play(Idx) $Play(Idx0)
   }

   set Play(Stop) 1
   Animator::Play
}

#----------------------------------------------------------------------------
# Nom      : <Animator::Play>
# Creation : Mai 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectue l'animations des frames.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Animator::Play { } {
   global GDefs
   variable Play
   variable Fly
   variable Lbl

   set Play(Canvas)  $Page::Data(Canvas)
   set Play(Page)    $Page::Data(Frame)
   set Play(VPs)     [Page::Registered $Page::Data(Frame) Viewport]

   if { ![llength $Play(Frames)] } {
      Animator::GetPlayList
   }

   if { ![llength $Play(Frames)] && !$Fly(Length) } {
      set Play(File) 0
      set Play(Dir)  0
      set Play(Stop) 1
      return
   }

   set no  0
   set end 0

   #--- Forever play "lala lala lala la shebop shebop lala lala ..."

   while (1) {

      #----- Calculer les frame de data et flyby

      if { $Play(Length) } { set Play(Frame) [expr $Play(Idx)%($Play(Length)+1)] }
      if { $Fly(Length)  } { set Fly(Frame)  [expr $Play(Idx)%($Fly(Length)+1)] }

      #----- Fly around

      if { $Fly(Length) } {
         set frame [expr $Play(Idx)*($Fly(Speed)/100.0)]
         projcam define $Play(Page) -fly $frame

         if { [llength $Fly(Lat)] } {
            projection configure $Play(Page) -location $Fly(Lat) [expr $Fly(Lon)+$frame]
         }
      }

      #----- Determiner le temps courant

      set info [lindex $Play(Frames) $Play(Frame)]

      #----- Recuperer l'information

      foreach vp $Play(VPs) {
         if { [info exists Play($vp$info)] } {

            #----- Applique la macro de calcul

            set Play(Data$vp) $Play($vp$info)
            set Play(Data)    [FieldCalc::Macro $vp ANI$vp $Play($vp$info)]

            if { $Play(Cache) && !$Play(File) } {
               $Play(Canvas) itemconf $vp -frame [expr $Play(Idx)+1] -data $Play(Data) -backbuffer True
            } else {
               $Play(Canvas) itemconf $vp -frame 0 -data $Play(Data) -backbuffer $OpenGL::Param(BBuf)
            }

            #----- Animer les viewport liees
            Viewport::LinkDo $vp
         }
      }

      #----- Appeler la fonction de mises a jour des informations

      Miniport::UpdateData $Play(Page)
      Page::Update $Play(Page) [expr [llength $Play(Frames)]?0:1]
      Page::UpdateCommand $Play(Page)

      #----- Modifier les indicateurs

      if { $Play(Type)=="DATE" && $info!="" } {
         projection configure $Play(Page) -date $info
         set label [clock format $info -format "%R, %a %b %d %Y" -gmt true]
         CVClock::Time $Play(Page) $info [expr ($Play(Idx)-$Play(Idx0))*100.0/($Play(Idx1)-$Play(Idx0))]
      } else {
         projection configure $Play(Page) -date $Viewport::Data(Seconds)
         projection configure $Play(Page) -date 0
         set label $info
      }

      set Play(Label) "$label"

      #----- Imprimer dans un fichier

      if { $Play(File) } {

         if { $Play(Type)=="DATE" && $info!="" && !$Fly(Length) } {
            set id [clock format $info -format "%Y%m%d_%H.%M.%S" -gmt true]
         } else {
            set id [format "%04i" $no]
         }

         set PrintBox::Print(FullName) $Play(Filename)_$id
         set PrintBox::Print(Type)     File

         set Play(Label)  "[lindex $Lbl(Print) $GDefs(Lang)] $PrintBox::Print(FullName)"
         PrintBox::Print $Play(Page) 0 0 [Page::CanvasWidth $Play(Page)] [Page::CanvasHeight $Play(Page)]
         set Play(Label)  "[lindex $Lbl(Done) $GDefs(Lang)] $id"
         incr no

         #----- En impression on clean apres chaque frame

         foreach field $Play(Data) {
            if { [fstdfield is $field] } {
               fstdfield clean $field
            }
         }
      }

      #----- On verifie les demandes d'arret

      update
      if { $Play(Stop) } {
         break
      }

      #----- Incrementer le frame

      if { $Play(Dir)==1 } {
         if { $Play(Idx)>=$Play(Idx1) } {
            if { $Play(Cycle) } {
               set Play(Idx) $Play(Idx0)
               if { $Play(File) } {
                  break;
               }
            } else {
               break;
            }
         } else {
            incr Play(Idx) $Play(Dir)
         }
      } else {
         if { $Play(Idx)<=$Play(Idx0) } {
            if { $Play(Cycle) } {
               set Play(Idx) $Play(Idx1)
               if { $Play(File) } {
                  break;
               }
            } else {
                break;
            }
         } else {
            incr Play(Idx) $Play(Dir)
         }
      }

      #----- Attendre le delai specifie

      after $Play(Delai)
   }

   set Play(File) 0
   set Play(Dir)  0
   set Play(Stop) 1
}

#----------------------------------------------------------------------------
# Nom      : <Animator::PlayFile>
# Creation : Octobre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Enregistrer les frames sur disque.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Animator::PlayFile { } {
   global GDefs
   variable Play

   set Play(Filename) [FileBox::Create . "" Save $PrintBox::Type(RASTER)]

   if { $Play(Filename)!="" } {
      set PrintBox::Print(Device) [string trimleft [file extension $Play(Filename)] "."]
      set Play(Filename)  [file rootname $Play(Filename)]
      set Play(Idx)       $Play(Idx0)
      set Play(Dir)       1
      set Play(Cycle)     0
      set Play(Stop)      0
      Animator::Play
    } else {
     set Play(File)       0
     set Play(Dir)        0
   }
}

#----------------------------------------------------------------------------
# Nom      : <Animator::Range>
# Creation : Aout 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Limiter la selection du range temporel.
#
# Parametres :
#   <Side>   : Sens de la selection
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Animator::Range { Side args } {
   variable Play

   if { $Side==0 } {
      if { $Play(Idx0)>=$Play(Idx1) } {
         set Play(Idx0) [expr $Play(Idx1)-1]
      }
      set info [lindex $Play(Frames) $Play(Idx0)]
      if { $Play(Idx)<$Play(Idx0) } {
         set Play(Idx) $Play(Idx0)
      }
   } else {
      if { $Play(Idx1)<=$Play(Idx0) } {
         set Play(Idx1) [expr $Play(Idx0)+1]
      }
      set info [lindex $Play(Frames) $Play(Idx1)]
      if { $Play(Idx)>$Play(Idx1) } {
         set Play(Idx) $Play(Idx1)
      }
   }

   #----- Ajuster l'information

   if { $Play(Type)=="DATE" && $info!="" } {
      set Play(Label) [clock format $info -format "%R, %a %b %d %Y" -gmt true]
   } else {
      set Play(Label) $info
   }
}

#----------------------------------------------------------------------------
# Nom      : <Animator::FlyPath>
# Creation : Avril 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Calculer l'un des paths par default selon la position actuelle
#
# Parametres :
#   <Cam>    : Identificateur de Camera
#   <Type>   : Type de path par default
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Animator::FlyPath { Cam Type } {
   variable Data
   variable Fly

   upvar #0 ProjCam::Data${Cam}::Cam  cam

   set plen $Fly(Length)

   set Fly(Controls) {}
   set Fly(Lat)      ""
   set Fly(Lon)      ""
   set Fly(From)     ""
   set Fly(Up)       ""
   set Fly(Length)   0

   switch $Type {
      "CIRCLE" {
         projcam configure $Cam -from $cam(From) -up $cam(Up)
         eval lappend Fly(Controls) $cam(From) $cam(To) $cam(Up)

         projcam define $Cam -circlefrom [expr $cam(CFX)+45] $cam(CFY) $cam(CFZ)
         eval lappend Fly(Controls) [projcam configure $Cam -from] [projcam configure $Cam -to] [projcam configure $Cam -up]

         projcam define $Cam -circlefrom [expr $cam(CFX)+90] $cam(CFY) $cam(CFZ)
         eval lappend Fly(Controls) [projcam configure $Cam -from] [projcam configure $Cam -to] [projcam configure $Cam -up]

         projcam define $Cam -circlefrom [expr $cam(CFX)+135] $cam(CFY) $cam(CFZ)
         eval lappend Fly(Controls) [projcam configure $Cam -from] [projcam configure $Cam -to] [projcam configure $Cam -up]

         projcam define $Cam -circlefrom [expr $cam(CFX)+180] $cam(CFY) $cam(CFZ)
         eval lappend Fly(Controls) [projcam configure $Cam -from] [projcam configure $Cam -to] [projcam configure $Cam -up]

         projcam define $Cam -circlefrom [expr $cam(CFX)+225] $cam(CFY) $cam(CFZ)
         eval lappend Fly(Controls) [projcam configure $Cam -from] [projcam configure $Cam -to] [projcam configure $Cam -up]

         projcam define $Cam -circlefrom [expr $cam(CFX)+270] $cam(CFY) $cam(CFZ)
         eval lappend Fly(Controls) [projcam configure $Cam -from] [projcam configure $Cam -to] [projcam configure $Cam -up]

         projcam define $Cam -circlefrom [expr $cam(CFX)+315] $cam(CFY) $cam(CFZ)
         eval lappend Fly(Controls) [projcam configure $Cam -from] [projcam configure $Cam -to] [projcam configure $Cam -up]

         eval lappend Fly(Controls) $cam(From) $cam(To) $cam(Up)
      }
      "AROUND" {
         projcam define $Cam -path ""
         set Fly(Lon) $Viewport::Map(Lon)
         set Fly(Lat) $Viewport::Map(Lat)
         set Fly(Length) 360
      }
      "TO" {
         projcam configure $Cam -from $cam(From) -up $cam(Up)
         eval lappend Fly(Controls) $cam(From) $cam(To) $cam(Up)

         projcam define $Cam -circlefrom $cam(CFX) [expr (-89.0+$cam(CFY))/2.0] [expr $cam(CFZ)/3.0]
         eval lappend Fly(Controls) [projcam configure $Cam -from] [projcam configure $Cam -to] [projcam configure $Cam -up]

         projcam define $Cam -circlefrom $cam(CFX) -89.0 1e-20
         eval lappend Fly(Controls) [projcam configure $Cam -from] [projcam configure $Cam -to] [projcam configure $Cam -up]
      }
      "THROUGH" {
         projcam configure $Cam -from $cam(From) -up $cam(Up)
         eval lappend Fly(Controls) $cam(From) $cam(To) $cam(Up)

         projcam define $Cam -circlefrom [expr $cam(CFX)+90.0] -89.0 0.0
         eval lappend Fly(Controls) [projcam configure $Cam -from] [projcam configure $Cam -to] $cam(Up)

         projcam define $Cam -circlefrom [expr $cam(CFX)+180.0] $cam(CFY)  $cam(CFZ)
         eval lappend Fly(Controls) [projcam configure $Cam -from] [projcam configure $Cam -to] [projcam configure $Cam -up]
      }
      "DEFAULT" {
         foreach idx $Fly(List) {
            eval lappend Fly(Controls) [lindex $ProjCam::Data(Params$idx) 1] [lindex $ProjCam::Data(Params$idx) 0] [lindex $ProjCam::Data(Params$idx) 2]
         }
      }
   }

   if { $Fly(Controls)!="" } {
      eval projcam define $Cam -path $Fly(Controls)
      $Page::Data(Canvas) itemconf $Viewport::Data(VP) -camera $Page::Data(Frame)

      set Fly(Length) [expr [llength $Fly(Controls)]/9-1]
   }

   set Fly(Length) [expr int($Fly(Length)*(100.0/$Fly(Speed)))]
   if { $plen!=$Fly(Length) } {
      Animator::Limits
   }
}

#----------------------------------------------------------------------------
# Nom      : <Animator::FlyPointAdd>
# Creation : Fevrier 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Ajout d'un point de controle
#
# Parametres :
#    <List>  : Widget listbox
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Animator::FlyPointAdd { List } {
   variable Fly

   set Fly(Length)  0
   set Fly(Path) DEFAULT

   if { [set idx [$List curselection]]!="" } {
      incr idx
   } else {
      set idx end
   }

   set wp "Waypoint [incr Fly(WayNo)]"
   ProjCam::Mem $Page::Data(Frame) $wp
   $List insert $idx $wp
   Animator::FlyPath $Page::Data(Frame) DEFAULT
}

#----------------------------------------------------------------------------
# Nom      : <Animator::FlyPointDel>
# Creation : Fevrier 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Suppression d'un point de controle
#
# Parametres :
#    <List>  : Widget listbox
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Animator::FlyPointDel { List } {
   variable Fly

   set Fly(Length) 0

   if { [set idx [$List curselection]]!="" } {
      set wp [$List get $idx]
      $List delete $idx
      $List selection set 0

      set ProjCam::Data(Params$wp) ""
   }
   Animator::FlyPath $Page::Data(Frame) DEFAULT
}

#----------------------------------------------------------------------------
# Nom      : <Animator::FlyPointSelect>
# Creation : Fevrier 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Selection d'un point de controle
#
# Parametres :
#    <List>  : Widget listbox
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Animator::FlyPointSelect { List } {

   if { [set idx [$List curselection]]!="" } {
      ProjCam::Select $Page::Data(Frame) $Page::Data(Frame) [$List get $idx]
   }
}

#----------------------------------------------------------------------------
# Nom      : <Animator::FlyPointSet>
# Creation : Fevrier 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Ajout d'un point de controle
#
# Parametres :
#    <List>  : Widget listbox
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Animator::FlyPointSet { List } {
   variable Fly

   set Fly(Length) 0

   if { [set idx [$List curselection]]!="" } {
      set wp [$List get $idx]
      ProjCam::Mem $Page::Data(Frame) $wp
   }
   Animator::FlyPath $Page::Data(Frame) DEFAULT
}
