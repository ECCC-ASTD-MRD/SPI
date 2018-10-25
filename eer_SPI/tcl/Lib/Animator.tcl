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
#   Animator::MiniWindow       { Parent Frame }
#   Animator::MiniSelect       { Parent Frame }
#   Animator::EmptyPlayList    { }
#   Animator::GetPlayListField { }
#   Animator::GetPlayListObs   { }
#   Animator::GetPlayList      { }
#   Animator::Limits           { }
#   Animator::Play             { }
#   Animator::PlayFile         { { Filename "" } }
#   Animator::PlayWeb          { }
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

package provide Animator 4.2

catch { SPI::Splash "Loading Widget Package Animator 4.2" }

#----- Definitions des constantes

namespace eval Animator {
   variable Data
   variable Play
   variable Fly
   variable Lbl
   variable Param
   variable Error

   set Param(Title)    { "Animateur" "Animator" }
   set Param(Geom)     { 305x275+[winfo rootx $Parent]+[winfo rooty $Parent] }
   set Param(Version)  4.0

   set Param(WebHost)      0               ;#Hote pour leas animations web
   set Param(WebDest)      0               ;#Path pour les animations web
   set Param(WebURL)       ""              ;#Web adress
   set Param(WebExt)       png             ;#Extension des images pour l'animation web
   set Param(WebKeyChars)  "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
   set Param(WebKeyLen)    20              ;#Length of the web key

   #----- Variable relative au playback

   set Play(Now)          ""              ;#Current frame info 
   set Play(Mode)         ""              ;#Current mode of animation, empty if not animating
   set Play(Cache)        0               ;#Cache des pixmaps de frame
   set Play(IP3)          1               ;#Validation du IP3 en recherche de champs
   set Play(Typvar)       1               ;#Validation du typvar en recherche de champs
   set Play(Cycle)        0               ;#Bouclage de l'animation
   set Play(Stop)         1               ;#Variable de surveillance de l'evenement d'arret
   set Play(Frame)        0               ;#Liste des frames temporel
   set Play(Frames)       ""              ;#Liste des frames temporel
   set Play(Length)       0               ;#Longueur du data
   set Play(Idx)          0               ;#Numero de frame en cours
   set Play(Idx0)         0               ;#Index de depart
   set Play(Idx1)         0               ;#Index de fin
   set Play(VPs)          ""              ;#Liste des viewports
   set Play(Dir)          0               ;#Direction du play (-1 back, 0 stop, 1 forward)
   set Play(Delai)        0               ;#Delai entre les frames en milliemes de secondes
   set Play(Label)        ""              ;#Identification du traitement courant
   set Play(Canvas)       ""              ;#Canvas dans lequel on anime
   set Play(Page)         ""              ;#Frame dans lequel on anime
   set Play(Filename)     ""              ;#Fichier des frames
   set Play(File)         0               ;#Enregistrement des frames
   set Play(Type)         DATE            ;#Type d'animation
   set Play(Types)        { DATE IP1 IP2 IP3 ETIKET }
   set Play(Data)         {}              ;#Data a animer
   set Play(Mini)         {}              ;#Active miniplayer

   set Play(Web)          0               ;#Enregistrement des frames + transformation en animation web
   set Play(Mail)         ""              ;#Mail to adress for link info

   set Fly(Speed)         0.01            ;#Vitesse du vol
   set Fly(List)          {}              ;#Liste des pointrs de controles
   set Fly(Frame)         0
   set Fly(WayNo)         0
   set Fly(Length)        0                           ;#Nombre maximum de frames
   set Fly(Controls)      {}                          ;#
   set Fly(From)          ""                          ;#Liste des positions
   set Fly(Up)            ""                          ;#Liste des aspects
   set Fly(Show)          0                           ;#Affichage des fly points
   set Fly(Path)          DEFAULT                     ;#Path courant
   set Fly(Paths)        { AROUND CIRCLE TO THROUGH }

   #----- Definitions des labels

   set Lbl(Convert)        { "Conversion vers" "Converting to" }
   set Lbl(Done)           { "Fin" "Done" }
   set Lbl(Empty)          { "Vide" "Empty" }
   set Lbl(Stop)           { "Pause" "Stop" }
   set Lbl(Print)          { "Impression" "Printing" }
   set Lbl(Read)           { "Lecture" "Reading" }
   set Lbl(Validate)       { "Valider" "Validate" }
   set Lbl(Data)           { "Données" "Data" }
   set Lbl(Fly)            { "Survol" "Flyby" }
   set Lbl(On)             { "Animer selon" "Animate on" }
   set Lbl(Copy)           { "Copie des images" "Copying images" }
   set Lbl(Web)            { "Creation de la page" "Creating page" }

   set Lbl(FileNameEnter)  { "Entrer le nom de base des fichiers images qui seront générés :" \
                             "Enter image files base name :" }
   set Lbl(FileNameTitle)  { "Nom du fichier" "File name" }
   set Lbl(WebURL)         { "L'url de l'animation web est le suivant :" "The web animation url is :" }

   #----- Definitions des bulles d'aide

   set Bubble(Off)         { "Fermer la boite d'animation" "Close the animator window" }
   set Bubble(Cache)       { "Cache les images en memoires" "Cache animation frames in memory" }
   set Bubble(Cycle)       { "Bouclage de l'animation" "Cycle the animation" }
   set Bubble(Rewind)      { "Retour au debut" "Rewind to the beginning" }
   set Bubble(Forwind)     { "Retour a la fin" "Go to end" }
   set Bubble(PlayBack)    { "Jouer vers l'arriere" "Play backward" }
   set Bubble(PlayFile)    { "Enregistrer les images" "Save the frames to file" }
   set Bubble(PlayWeb)     { "Enregistrer les images sur une page web" "Save the frames to file on a web page" }
   set Bubble(StepBack)    { "Une image vers l'arriere" "Step on frame backward" }
   set Bubble(Stop)        { "Stop" "Stop" }
   set Bubble(StepForward) { "Une image vers l'avant" "Step one frame forward" }
   set Bubble(PlayForward) { "Jouer vers l'avant" "Play forward" }
   set Bubble(Delai)       { "Delai entre les images (millisecondes)" "Delay between frames (milliseconds)" }
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

   #----- Messages d'erreur

   set Error(WebAnimMake)  { "Une erreur est survenue lors de la génération de l'animation web." "An error occured while generating web animation." }
   set Error(WebAnimZip)   { "Une erreur est survenue lors de la création du fichier zip, le zip ne sera pas transféré." "An error occured while generating the zipped animation. The zipped animation will not be transfered." }
   set Error(WebAnimXfer)  { "Une erreur est survenue lors du transfert de l'animation web." "An error occured during web animation transmission." }
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
   global GDefs env
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

#   bind .anim.tab  <<NotebookTabChanged>> {puts stderr [.anim.tab index current] }
   TabFrame::Create .anim.tab 1 {}
   pack .anim.tab -side top -fill both -expand true -padx 2 -pady 2

#   frame .anim.tab.f1
#   set Data(Tab1) [.anim.tab add .anim.tab.f1 -text  [lindex $Lbl(Data) $GDefs(Lang)]]
   set Data(Tab1) [TabFrame::Add .anim.tab 1 [lindex $Lbl(Data) $GDefs(Lang)] False ""]

      frame $Data(Tab1).lbl
         label $Data(Tab1).lbl.lbl0 -text "[lindex $Lbl(On) $GDefs(Lang)] ([lindex $Lbl(Validate) $GDefs(Lang)] "
         checkbutton $Data(Tab1).lbl.ip3 -variable Animator::Play(IP3) -text IP3 -onvalue 1 \
            -offvalue 0 -command "Animator::EmptyPlayList" -indicatoron false -relief flat -bd 1 -overrelief raised
         label $Data(Tab1).lbl.lbl1 -text "|"
         checkbutton $Data(Tab1).lbl.tvar -variable Animator::Play(Typvar) -text Typvar -onvalue 1 \
            -offvalue 0 -command "Animator::EmptyPlayList" -indicatoron false -relief flat -bd 1 -overrelief raised
         label $Data(Tab1).lbl.lbl2 -text ")"
         pack $Data(Tab1).lbl.lbl0 $Data(Tab1).lbl.ip3 $Data(Tab1).lbl.lbl1 $Data(Tab1).lbl.tvar $Data(Tab1).lbl.lbl2 -side left

      labelframe $Data(Tab1).type -labelwidget $Data(Tab1).lbl
      frame  $Data(Tab1).type.f -relief sunken -bd 1
      foreach type $Play(Types) {
         radiobutton $Data(Tab1).type.f.t$type -indicatoron false -variable Animator::Play(Type) -text $type -value $type \
            -command "Animator::EmptyPlayList" -bd 1 -relief flat -overrelief raised
         pack $Data(Tab1).type.f.t$type -side top -fill x -expand True
      }
      pack $Data(Tab1).type.f -side top -fill both -padx 2 -pady 2
      pack $Data(Tab1).type -side top -fill both -padx 5 -pady 2

#   frame .anim.tab.f2
#   set Data(Tab2) [.anim.tab add .anim.tab.f2 -text [lindex $Lbl(Fly) $GDefs(Lang)]]
   set Data(Tab2) [TabFrame::Add .anim.tab 1 [lindex $Lbl(Fly) $GDefs(Lang)] False ""]
      frame $Data(Tab2).head
         button $Data(Tab2).head.add  -image PLUS -bd 0 -relief flat -overrelief raised -command "Animator::FlyPointAdd $Data(Tab2).way.list.box"
         button $Data(Tab2).head.del  -image MINUS -bd 0 -relief flat -overrelief raised -command "Animator::FlyPointDel $Data(Tab2).way.list.box"
         button $Data(Tab2).head.rep  -image FINGER -bd 0 -relief flat -overrelief raised -command "Animator::FlyPointSet $Data(Tab2).way.list.box"
         checkbutton $Data(Tab2).head.show -image CAMPATH -indicatoron 0 -relief sunken -bd 1 -overrelief raised -offrelief flat -variable Animator::Fly(Show) -onvalue 1 -offvalue 0 \
            -command { projcam configure $Page::Data(Frame) -show $Animator::Fly(Show) ; $Page::Data(Canvas) itemconf $Page::Data(VP) -camera $Page::Data(Frame) }
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
            scale $Data(Tab2).way.list.speed -from 0.1 -to 0.001 -resolution 0.001 -variable Animator::Fly(Speed) -relief raised -bd 1 \
               -relief flat -orient vertical -width 15 -sliderlength 10 -command { Animator::FlyPath $Page::Data(Frame) $Animator:::Fly(Path); catch }  -showvalue false
            pack $Data(Tab2).way.list.box -side left -fill both -expand true
            pack $Data(Tab2).way.list.scroll $Data(Tab2).way.list.speed -side left -fill y
         pack $Data(Tab2).way.list -side left -fill both -expand true
         frame $Data(Tab2).way.opt

         pack  $Data(Tab2).way.opt -side left
      pack $Data(Tab2).way -side top -fill both -expand true

      bind $Data(Tab2).way.list.box <Double-ButtonRelease-1> "Animator::FlyPointSelect $Data(Tab2).way.list.box"

      Bubble::Create $Data(Tab2).way.preset     $Bubble(FlyPreset)
      Bubble::Create $Data(Tab2).head.del       $Bubble(FlyDel)
      Bubble::Create $Data(Tab2).head.add       $Bubble(FlyIns)
      Bubble::Create $Data(Tab2).head.rep       $Bubble(FlyRep)
      Bubble::Create $Data(Tab2).head.show      $Bubble(FlyShow)
      Bubble::Create $Data(Tab2).head.fly       $Bubble(FlyFly)
      Bubble::Create $Data(Tab2).way.list.speed $Bubble(FlySpeed)

   frame .anim.params -relief raised -bd 1
      scale .anim.params.frame -from $Play(Idx0) -to $Play(Idx1) -resolution 1 -variable Animator::Play(Idx) -relief raised -bd 1 \
         -relief flat -orient horizontal -width 15 -sliderlength 10 -command "Animator::StepTo" -label "Frame" -showvalue true
      scale .anim.params.lapse -from 0 -to 500 -resolution 10 -variable Animator::Play(Delai) -relief raised -bd 1 \
         -relief flat -orient vertical -width 15 -sliderlength 10 -command ""  -length 35 -showvalue false
      scale .anim.params.idx0 -from $Play(Idx0) -to $Play(Idx1) -resolution 1 -variable Animator::Play(Idx0) -relief raised -bd 1 \
         -relief flat -orient horizontal -width 5 -sliderlength 10 -command "Animator::Range 0" -showvalue false
      scale .anim.params.idx1 -from $Play(Idx0) -to $Play(Idx1) -resolution 1 -variable Animator::Play(Idx1) -relief raised -bd 1 \
         -relief flat -orient horizontal -width 5 -sliderlength 10 -command "Animator::Range 1" -showvalue false
      pack .anim.params.lapse -side right -anchor s
      pack .anim.params.frame .anim.params.idx0 .anim.params.idx1 -side top -fill x -expand true
   pack .anim.params -side top -fill x  -padx 2

   bind .anim <ButtonPress-4> "Animator::Step 1"
   bind .anim <ButtonPress-5> "Animator::Step -1"
   
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
           .anim.comm.stop .anim.comm.forwind .anim.comm.stepforward .anim.comm.playforward .anim.comm.playfile \
          -side left -fill both -expand true

      #----- If the web animator script is available
      if { [info exists env(EER_DIRSCRIPT)] && [file exists $env(EER_DIRSCRIPT)/e.image_animator] && [file executable $env(EER_DIRSCRIPT)/e.image_animator] } {
         radiobutton .anim.comm.playweb -image VCRWEB -bd 1 -variable Animator::Play(Web) -indicatoron False -value 1  -selectcolor "" \
            -command { Animator::PlayWeb }
         pack .anim.comm.playweb -side left -fill both -expand true
      }

   pack .anim.comm -side top -fill x -padx 2 -pady 2

   #----- Creation des bulles d'aides

   Bubble::Create .anim.comm.cache       $Bubble(Cache)
   Bubble::Create .anim.comm.cycle       $Bubble(Cycle)
   Bubble::Create .anim.comm.rewind      $Bubble(Rewind)
   Bubble::Create .anim.comm.forwind     $Bubble(Forwind)
   Bubble::Create .anim.comm.playback    $Bubble(PlayBack)
   Bubble::Create .anim.comm.stepback    $Bubble(StepBack)
   Bubble::Create .anim.comm.stop        $Bubble(Stop)
   Bubble::Create .anim.comm.stepforward $Bubble(StepForward)
   Bubble::Create .anim.comm.playforward $Bubble(PlayForward)
   Bubble::Create .anim.comm.playfile    $Bubble(PlayFile)
   Bubble::Create .anim.comm.playweb     $Bubble(PlayWeb)
   Bubble::Create .anim.comm.off         $Bubble(Off)
   Bubble::Create .anim.params.lapse     $Bubble(Delai)
   Bubble::Create .anim.params.frame     $Bubble(Scroll)
   Bubble::Create .anim.params.idx0      $Bubble(Idx0)
   Bubble::Create .anim.params.idx1      $Bubble(Idx1)
   Bubble::Create .anim.info.lbl         $Bubble(State)
   Bubble::Create .$Data(Tab1).type      $Bubble(Type)

   TabFrame::Select .anim.tab 0
}

#----------------------------------------------------------------------------
# Nom      : <Animator::MiniWindow>
# Creation : Fevrier 2015 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer une mini interface d'animation pour les toolbar
#
# Parametres :
#   <Parent> : Identificateur de la fenetre parent
#   <Frame>  : Frame qui sera anime
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Animator::MiniWindow { Parent Frame } {
   global GDefs env
   variable Data
   variable Play
   variable Lbl
   variable Bubble
   variable Param

   if { [winfo exists $Parent.anim] } {
      return
   }

   frame $Parent.anim -relief sunken -bd 0
      scale $Parent.anim.frame -from 0 -to 0 -resolution 1 -variable Animator::Play(Idx) -relief flat -bd 1 \
         -relief flat -orient horizontal -width 19 -sliderlength 10 -command "Animator::StepTo" -showvalue False

      button $Parent.anim.rewind -image VCRREWIND -bd 1 -relief flat -overrelief raised \
         -command { set Animator::Play(Idx) $Animator::Play(Idx0) ; set Animator::Play(Stop) 1 ; Animator::Play }
      button $Parent.anim.stop -image VCRSTOP -relief flat -bd 1 -overrelief raised \
         -command { set Animator::Play(Stop) 1 }
      button $Parent.anim.stepback -image VCRFRAMEB -relief flat -bd 1 -overrelief raised \
         -command "Animator::MiniSelect $Parent $Frame; Animator::Step -1"
      button $Parent.anim.stepforward -image VCRFRAMEF -relief flat -bd 1 -overrelief raised \
         -command "Animator::MiniSelect $Parent $Frame; Animator::Step 1"
      radiobutton $Parent.anim.playforward -image VCRPLAYF -relief sunken -bd 1 -overrelief raised -offrelief flat -variable Animator::Play(Dir) -indicatoron False -value 1  -selectcolor ""\
         -command "Animator::MiniSelect $Parent $Frame; set Animator::Play(Stop) 0; Animator::Play"
      button $Parent.anim.forwind -image VCRFORWIND -bd 1 -relief flat -overrelief raised \
         -command { set Animator::Play(Idx) $Animator::Play(Idx1) ; set Animator::Play(Stop) 1 ; Animator::Play }
      pack $Parent.anim.rewind $Parent.anim.stepback $Parent.anim.stop $Parent.anim.stepforward $Parent.anim.playforward $Parent.anim.forwind -side left

       #----- If the web animator script is available
       if { [info exists env(EER_DIRSCRIPT)] && [file exists $env(EER_DIRSCRIPT)/e.image_animator] && [file executable $env(EER_DIRSCRIPT)/e.image_animator] } {
         radiobutton $Parent.anim.playweb -image VCRWEB -relief sunken -bd 1 -overrelief raised -offrelief flat -variable Animator::Play(Web) -indicatoron False -value 1 -selectcolor "" \
            -command "Animator::MiniSelect $Parent $Frame; Animator::PlayWeb"
         pack $Parent.anim.playweb -side left -fill x
      }
      pack $Parent.anim.frame -side left -fill y -expand true

   set Play(Cycle) 1
   
   bind $Parent.anim.frame <ButtonPress-4> "Animator::Step 1"
   bind $Parent.anim.frame <ButtonPress-5> "Animator::Step -1"

   #----- Creation des bulles d'aides

   Bubble::Create $Parent.anim.rewind      $Bubble(Rewind)
   Bubble::Create $Parent.anim.stepback    $Bubble(StepBack)
   Bubble::Create $Parent.anim.stop        $Bubble(Stop)
   Bubble::Create $Parent.anim.stepforward $Bubble(StepForward)
   Bubble::Create $Parent.anim.playforward $Bubble(PlayForward)
   Bubble::Create $Parent.anim.playweb     $Bubble(PlayWeb)
   Bubble::Create $Parent.anim.frame       $Bubble(Scroll)
   Bubble::Create $Parent.anim.forwind     $Bubble(Forwind)
   Bubble::Create $Parent.anim.rewind      $Bubble(Rewind)

   Animator::MiniSelect $Parent $Frame
   
   return $Parent.anim
}

#----------------------------------------------------------------------------
# Nom      : <Animator::MiniSelect>
# Creation : Fevrier 2015 - J.P. Gauthier - CMC/CMOE
#
# But      : S'assure que le mini player selectionne est celui en cours d'animation
#
# Parametres :
#   <Parent> : Identificateur de la fenetre parent
#   <Frame>  : Frame qui sera anime
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Animator::MiniSelect { Parent Frame } {
   variable Play

   if { $Play(Mini)!="$Parent" } {
      Animator::EmptyPlayList

      Page::Activate $Frame
      set Play(Page)    $Page::Data(Frame)
      set Play(Canvas)  $Page::Data(Canvas)
   }
   set Play(Mini) $Parent
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
   variable Play


  if { [llength $Play(Frames)] == 0 } {
      return
   }

   projection configure $Play(Page) -date $Viewport::Data(Seconds)
   projection configure $Play(Page) -date 0
   set Play(Stop) 1
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

   #----- Ajouter les données aux pas de temps intermédiaire le plus petit
   foreach vp $Play(VPs) {
      set pframe [lindex $Play(Frames) 0]
      foreach frame [lrange $Play(Frames) 1 end] {
         set i 0
         if { [info exists Animator::Play($vp$pframe)] } {
            foreach d $Play($vp$pframe) {
               if { [fstdfield is $d] } {
                  set idx [lindex [split $d .] 1]

                  #----- We might be missing one of the end frame
                  catch {
                     if { [lsearch -glob $Play($vp$frame) "ANI.$idx.*" ]==-1 } {
                        set Play($vp$frame) [linsert $Play($vp$frame) [incr idx $i] $d]
                     }
                  }
               } else {
                  incr i
               }
            }
         }
         set pframe $frame
      }
   }

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

   #----- If interface is up
   if { [winfo exists .anim.params] } {
      .anim.params.frame configure -to $idx1
      .anim.params.idx0  configure -to $idx1
      .anim.params.idx1  configure -to $idx1
   }
   if { [winfo exists $Play(Mini)] } {
      $Play(Mini).anim.frame configure -to $idx1
   }
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

   set f 0
   foreach vp $Play(VPs) {
      foreach fld $Viewport::Data(Data$vp) {

         if { [fstdfield is $fld] } {

            set tags [fstdfield stats $fld -tag]
            set box  [lindex $tags 2]

            #----- On recupere les parametres du champs selectionne

            set var     [fstdfield define $fld -NOMVAR]
            set tvar    [fstdfield define $fld -TYPVAR]
            set ip1     [fstdfield define $fld -IP1]
            set ip2     [fstdfield define $fld -IP2]
            set ip3     [fstdfield define $fld -IP3]
            set etiket  [fstdfield define $fld -ETIKET]
            
            #----- Sepcial case for secconds at 0
            if { [set sec [fstdstamp toseconds [fstdfield define $fld -DATEV]]] } {
               set date    [clock format $sec -format "%Y%m%d%H%M" -timezone :UTC]
            } else {
               set date 000000000000
            }
         } elseif { [gribfield is $fld] } {

            set tags [gribfield stats $fld -tag]
            set box  [lindex $tags 2]

            #----- On recupere les parametres du champs selectionne

            set var     [fstdfield define $fld -NOMVAR]
            set tvar    .+
            set ip1     [fstdfield define $fld -IP1]
            set ip2     \\d+
            set ip3     \\d+
            set etiket  .+
            set date    [clock format [fstdfield define $fld -DATEV] -format "%Y%m%d%H%M" -timezone :UTC]
         } else {
            continue
         }

         if { !$Play(IP3) } {
            set ip3 \\d+
         }
         if { !$Play(Typvar) } {
             set tvar .+
         }

         switch $Play(Type) {
            "IP1"     { set str "^$var\\s+$tvar\\s.+ .+\\s+.+ .+\\s.+ .+\\s+$etiket\\s+$date \\d+ \\d+ \\d+ $ip2 $ip3 .+field$" }
            "IP2"     { set str "^$var\\s+$tvar\\s.+ .+\\s+.+ .+\\s.+ .+\\s+$etiket\\s+\\d+ \\d+ \\d+ $ip1 \\d+ $ip3 .+field$" }
            "IP3"     { set str "^$var\\s+$tvar\\s.+ .+\\s+.+ .+\\s.+ .+\\s+$etiket\\s+$date \\d+ \\d+ \\d+ $ip2 \\d+ .+field$" }
            "ETIKET"  { set str "^$var\\s+$tvar\\s.+ .+\\s+.+ .+\\s.+ .+\\s+.+\\s+$date \\d+ \\d+ $ip1 $ip2 $ip3 .+field$" }
            "DATE"    { set str "^$var\\s+$tvar\\s.+ .+\\s+.+ .+\\s.+ .+\\s+$etiket\\s+\\d+ \\d+ \\d+ $ip1 \\d+ $ip3 .+field$" }
         }
         set no 0
         foreach field [lsearch -all -inline -regexp [FieldBox::GetContent $box] $str] {

            #----- Do not use masks
            if  { [string index [lindex $field 1] 0]=="@" } {
               continue
            }
            set fid     [lindex $field end-5]
            set idx     [lindex $field end-4]
            set type    [lindex $field end]

            set Play(Label) "[lindex $Lbl(Read) $GDefs(Lang)] $var $fid $idx"
            update idletask

            eval $type read ANI.$f.$no $fid $idx
            eval $type stats ANI.$f.$no -tag \$tags

            #----- copy configuration object
            fstdfield configure ANI.$f.$no -dataspec [fstdfield configure $fld -dataspec]

            switch $Play(Type) {
               "IP1"     { set info [lrange $field 2 3] }
               "IP2"     { set info [lrange $field 4 5] }
               "IP3"     { set info [lrange $field 6 7] }
               "ETIKET"  { set info [lindex $field 8] }
               "DATE"    { set info [fstdstamp toseconds [fstdfield define ANI.$f.$no -DATEV]] }
            }
            #----- Ajouter a la liste du frame temporel correspondant

            if { ![info exists Play($vp$info)] } {
               set Play($vp$info) ""
               lappend Play(Frames) $info
            }
            lappend Play($vp$info) ANI.$f.$no
            incr no

            #----- On verifie les demandes d'arret
            update
            if { $Play(Stop) } {
               set Play(File)  0
               set Play(Web) 0
               set Play(Dir) 0
               Animator::EmptyPlayList
               return
            }
         }
         incr f
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
               foreach sec [lsort -unique [observation define $o -DATE]] {
                  set sec [expr $sec<=0?0:$sec]

                  if { ![info exists Play($vp$sec)] } {
                     lappend Play(Frames) $sec
                  }
                  lappend Play($vp$sec) $o
               }
               observation stats $o -tag $tags
               lappend Obs::Data(ListTool) $o
            }
            #----- On verifie les demandes d'arret

            update
            if { $Play(Stop) } {
               set Play(File)  0
               set Play(Web) 0
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
               set Play(Web) 0
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

   set Play(Stop) 0
   if { ![llength $Play(Frames)] } {
      Animator::GetPlayList
   }

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

   set Play(Page)    $Page::Data(Frame)
   set Play(Canvas)  $Page::Data(Canvas)

   #----- If no viewport is active in the active page, try for a graph
   if { ![llength [set Play(VPs) [Page::Registered $Play(Page) Viewport]]] } {
      if { ![catch { upvar #0 Graph::${Graph::Data(Type)}::${Graph::Data(Type)}${Graph::Data(Graph)}::Data  data}] && [info exists data(FrameData)] } {

         set Play(Page)    $data(FrameData)
         set Play(Canvas)  $data(FrameData).page.canvas
         set Play(VPs)     [Page::Registered $Play(Page) Viewport]
      }
   }

   if { ![llength $Play(Frames)] } {
      Animator::GetPlayList
   }

   if { [llength $Play(Frames)]<=1 && !$Fly(Length) } {
      set Play(File) 0
      set Play(Web) 0
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
         set frame [expr $Play(Idx)*$Fly(Speed)]
         set coords [projcam define $Play(Page) -fly $frame]
         set lat [lindex $coords 0]
         set lon [lindex $coords 1]

         if { $lat!=-999 } {
            projection configure $Play(Page) -location $lat $lon
         }
      }

      #----- Determiner le temps courant
      set Play(Now) [set info [lindex $Play(Frames) $Play(Frame)]]

      if { [llength $info]>1 } {
         #----- If the playlist is a command
         eval set info \[$info\]
      } else {
         #----- Playlist is a single item, Get related info
         foreach vp $Play(VPs) {
            if { [info exists Play($vp$info)] } {

               #----- Applique la macro de calcul
               set Play(Data$vp) $Play($vp$info)
               set Play(Data)    [concat $Play($vp$info) [FieldCalc::Operand $vp $Play($vp$info)]]

               #----- Check for persistent data (time=0)
               if { $info!=0 && [info exists Play(${vp}0)] } {
                  lappend Play(Data) $Play(${vp}0)
               }
               if { $Play(Cache) && !$Play(File) } {
                  $Play(Canvas) itemconf $vp -frame [expr $Play(Idx)+1] -data $Play(Data)
               } else {
                  $Play(Canvas) itemconf $vp -frame 0 -data $Play(Data)
               }

               #----- Animer les viewport liees
               Viewport::LinkDo $vp
            }
         }
      }
      set Play(Mode) $Play(Type)
      
      #----- Appeler la fonction de mises a jour des informations
      foreach mini $Miniport::Data(Mini$Play(Page)) {
         Miniport::UpdateData $Play(Page) $mini
      }
      Page::Update $Play(Page) [expr [llength $Play(Frames)]?0:1]
      Page::UpdateCommand $Play(Page)

      #----- Modifier les indicateurs
      if { $Play(Type)=="DATE" && $info!="" } {
         projection configure $Play(Page) -date $info
         set label [clock format $info -format "%T, %a %b %d %Y" -timezone :UTC]
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
            set id [clock format $info -format "%Y%m%d_%H%M%S" -timezone :UTC]_UTC
         } else {
            set id [format "%04i" $no]
         }

         set PrintBox::Param(FullName) $Play(Filename)_${id}
         set PrintBox::Print(Type)     File

         set Play(Label)  "[lindex $Lbl(Print) $GDefs(Lang)] $PrintBox::Param(FullName)"
         PrintBox::Print $Play(Page) 0 0 [Page::CanvasWidth $Play(Page)] [Page::CanvasHeight $Play(Page)]
         set Play(Label)  "[lindex $Lbl(Done) $GDefs(Lang)] $id"
         incr no

         #----- En impression on clean apres chaque frame
         foreach field $Play(Data) {
            if { [fstdfield is $field True] } {
               fstdfield clean $field
            }
         }
      }

      #----- On verifie les demandes d'arret
      update
      if { $Play(Stop) || (!$Play(Length) && !$Fly(Length)) } {
         set Play(Web) 0
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

   set Play(Now)   ""
   set Play(Mode)  ""
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
# Parametres   :
#   <Filename> : Nom du fichier
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Animator::PlayFile { { Filename "" } } {
   variable Play

   if { $Filename=="" } {
      set Play(Filename) [FileBox::Create . "" Save [linsert $PrintBox::Param(Formats) 0 $PrintBox::Param(Format)]]
   } else {
      set Play(Filename) $Filename
   }

   if { $Play(Filename)!="" } {
      set PrintBox::Print(Device) [string trimleft [file extension $Play(Filename)] "."]
      set Play(Filename)  [file rootname $Play(Filename)]
      set Play(Idx)       $Play(Idx0)
      set Play(Dir)       1
      set Play(Cycle)     0
      set Play(Stop)      0
      Animator::Play
    } else {
      set Play(File)      0
      set Play(Dir)       0
   }
}

#----------------------------------------------------------------------------
# Nom      : <Animator::PlayWeb>
# Creation : Aout 2010 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Enregistrer les frames et les transferer sur un serveur.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Animator::PlayWeb { } {
   global env
   global GDefs
   variable Play
   variable Param
   variable Lbl
   variable Error

   if { !$Play(Web) || $Param(WebHost)=="" || $Param(WebDest)=="" } {
      set Play(Web) 0
      return
   }

   #----- Get a name from user but default to current experiment if there is any
   if { $Model::Param(Show) && $Exp::Data(No)!="" && $Exp::Data(Name)!="" } {
      set Play(Filename) $Exp::Data(No)_$Exp::Data(Name)
   } else {
      set Play(Filename) ""
   }
   set filename [Dialog::Get . $Lbl(FileNameTitle) $Lbl(FileNameEnter) Animator::Play(Filename)]
   if { $filename=="" } {
      set Play(Web) 0
      return
   }

   #----- Find a random directory name hard to guess and whom name doesn't exist
   set nbchars [string length $Param(WebKeyChars)]

   while { True } {
      #----- Generate a key
      set randstr ""
      for { set i 0 } { $i<$Param(WebKeyLen) } { incr i } {
         append randstr [string index $Param(WebKeyChars) [expr int(rand()*$nbchars)]]
      }

      #----- Make sure it does not already exists
      set base "$env(HOME)/.spi/Tmp/$randstr"
      if { ![file exists $base] && [catch { exec ssh $Param(WebHost) "mkdir '$Param(WebDest)/$randstr'" }]==0 } {
         break
      }
   }
   file mkdir [set path $base/$filename]

   #----- Generate image files
   set Play(File)   1
   set Play(WebURLPath) $Param(WebURL)/$randstr/$filename/anim.html
   Animator::PlayFile $path/$filename.$Param(WebExt)

   #----- If user cancelled or an error occured
   if { !$Play(Web) } {
      catch { file delete -force $base }
      catch { exec ssh $Param(WebHost) "rm -r '$Param(WebDest)/$randstr'" }
      return
   }
   set Play(Web) 0

   #----- Generate animation files
   set Play(Label) "[lindex $Lbl(Web) $GDefs(Lang)]"
   update idletasks
   set err [catch { exec $env(EER_DIRSCRIPT)/e.image_animator -p $path -b $filename -e $Param(WebExt) } msg]
   if { $err } {
      Dialog::Error . $Error(WebAnimMake) "\n$msg"
      catch { file delete -force $base }
      catch { exec ssh $Param(WebHost) "rm -r '$Param(WebDest)/$randstr'" }
      return
   }

   #----- Generate animation zipfile
   set p [pwd]
   cd  $base
   set err [catch { exec zip -r ${filename}/Animation.zip ${filename} 2>@1 } msg]
   cd $p
   if { $err } {
      Dialog::Error . $Error(WebAnimZip) "\n$msg"
   }

   #----- Set permissions
   catch { eval exec chmod 644 [glob $path/*] }
   catch { exec chmod 755 $path }
   catch { exec chmod 751 $base }

   #----- Transfer files
   set Play(Label) "[lindex $Lbl(Copy) $GDefs(Lang)]"
   update idletasks
   set err [catch { exec scp -rp $base $Param(WebHost):$Param(WebDest) } msg]
   if { $err } {
      Dialog::Error . $Error(WebAnimXfer) "\n$msg"
      catch { file delete -force $base }
      catch { exec ssh $Param(WebHost) "rm -r '$Param(WebDest)/$randstr'" }
      return
   }

   #----- Give the user the url path
   Dialog::Give . { URL URL } $Lbl(WebURL) $Animator::Play(WebURLPath) $Animator::Play(Mail)

   catch { file delete -force $base }
   set Play(Label) "[lindex $Lbl(Done) $GDefs(Lang)]"
   update idletasks
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
      set Play(Label) [clock format $info -format "%R, %a %b %d %Y" -timezone :UTC]
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
   set Fly(Coords)   {}
   set Fly(From)     ""
   set Fly(Up)       ""
   set Fly(Length)   0
   
   switch $Type {
      "CIRCLE" {
         projcam configure $Cam -from $cam(From) -up $cam(Up)
         lappend Fly(Controls) [list $cam(From) $cam(To) $cam(Up) $cam(Lens)]

         projcam define $Cam -circlefrom [expr $cam(CFX)+45] $cam(CFY) $cam(CFZ)
         lappend Fly(Controls) [list [projcam configure $Cam -from] [projcam configure $Cam -to] [projcam configure $Cam -up] $cam(Lens)]

         projcam define $Cam -circlefrom [expr $cam(CFX)+90] $cam(CFY) $cam(CFZ)
         lappend Fly(Controls) [list [projcam configure $Cam -from] [projcam configure $Cam -to] [projcam configure $Cam -up] $cam(Lens)]

         projcam define $Cam -circlefrom [expr $cam(CFX)+135] $cam(CFY) $cam(CFZ)
         lappend Fly(Controls) [list [projcam configure $Cam -from] [projcam configure $Cam -to] [projcam configure $Cam -up] $cam(Lens)]

         projcam define $Cam -circlefrom [expr $cam(CFX)+180] $cam(CFY) $cam(CFZ)
         lappend Fly(Controls) [list [projcam configure $Cam -from] [projcam configure $Cam -to] [projcam configure $Cam -up] $cam(Lens)]

         projcam define $Cam -circlefrom [expr $cam(CFX)+225] $cam(CFY) $cam(CFZ)
         lappend Fly(Controls) [list [projcam configure $Cam -from] [projcam configure $Cam -to] [projcam configure $Cam -up] $cam(Lens)]

         projcam define $Cam -circlefrom [expr $cam(CFX)+270] $cam(CFY) $cam(CFZ)
         lappend Fly(Controls) [list [projcam configure $Cam -from] [projcam configure $Cam -to] [projcam configure $Cam -up] $cam(Lens)]

         projcam define $Cam -circlefrom [expr $cam(CFX)+315] $cam(CFY) $cam(CFZ)
         lappend Fly(Controls) [list [projcam configure $Cam -from] [projcam configure $Cam -to] [projcam configure $Cam -up] $cam(Lens)]

         lappend Fly(Controls) [list $cam(From) $cam(To) $cam(Up) $cam(Lens)]
      }
      "AROUND" {
         set ll [projection configure $Cam -location]
         foreach lon { 0 10 20 30 40 50 60 70 80 90 100 110 120 130 140 150 160 170 180 -170 -160 -150 -140 -130 -120 -110 -100 -90 -80 -70 -60 -50 -40 -30 -20 -10 } {
            lappend Fly(Controls) [list $cam(From) $cam(To) $cam(Up) $cam(Lens) [lindex $ll 0] [expr [lindex $ll 1]+$lon]]
          }
      }
      "TO" {
         projcam configure $Cam -from $cam(From) -up $cam(Up)
         lappend Fly(Controls) [list $cam(From) $cam(To) $cam(Up) $cam(Lens)]

         projcam define $Cam -circlefrom $cam(CFX) [expr (-89.0+$cam(CFY))/2.0] [expr $cam(CFZ)/3.0]
         lappend Fly(Controls) [list [projcam configure $Cam -from] [projcam configure $Cam -to] [projcam configure $Cam -up] $cam(Lens)]

         projcam define $Cam -circlefrom $cam(CFX) -89.0 1e-20
         lappend Fly(Controls) [list [projcam configure $Cam -from] [projcam configure $Cam -to] [projcam configure $Cam -up] $cam(Lens)]
      }
      "THROUGH" {
         projcam configure $Cam -from $cam(From) -up $cam(Up)
         lappend Fly(Controls) [list $cam(From) $cam(To) $cam(Up) $cam(Lens)]

         projcam define $Cam -circlefrom [expr $cam(CFX)+90.0] -89.0 0.0
         lappend Fly(Controls) [list [projcam configure $Cam -from] [projcam configure $Cam -to] $cam(Up) $cam(Lens)]

         projcam define $Cam -circlefrom [expr $cam(CFX)+180.0] $cam(CFY)  $cam(CFZ)
         lappend Fly(Controls) [list [projcam configure $Cam -from] [projcam configure $Cam -to] [projcam configure $Cam -up] $cam(Lens)]
      }
      "DEFAULT" {
         foreach idx $Fly(List) {
            lappend Fly(Controls) [list [lindex $ProjCam::Data(Params$idx) 1] [lindex $ProjCam::Data(Params$idx) 0] [lindex $ProjCam::Data(Params$idx) 2] [lindex $ProjCam::Data(Params$idx) 3] [lindex $ProjCam::Data(Params$idx) end-1] [lindex $ProjCam::Data(Params$idx) end]]
         }
      }
   }

   if { $Fly(Controls)!="" } {
      projcam define $Cam -path $Fly(Controls)
      $Page::Data(Canvas) itemconf $Page::Data(VP) -camera $Page::Data(Frame)

      set Fly(Length) [expr [llength $Fly(Controls)]-1]
   }

   set Fly(Length) [expr int($Fly(Length)/$Fly(Speed))]
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
