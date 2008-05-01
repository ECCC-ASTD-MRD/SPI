#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie de "Widget" Tk.
# Fichier  : Model-*.*.tcl
# Version  : 3.0
# Creation : Octobre 1999 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Permet d'afficher une arborescence d'experiences.
#
# Remarques :
#
#===============================================================================

package require Dialog        ; IdDialog        false
package require CanvasBubble  ; IdCanvasBubble  false
package require Calendar      ; IdCalendar      true
package require Clock         ; IdClock         true
package require MetData       ; IdMetData       true
package require Tree          ; IdTree          true

#----- Inclure les type d'experiences

source $GDefs(Dir)/Apps/Models/Meteo.tcl
source $GDefs(Dir)/Apps/Models/Exp.tcl
source $GDefs(Dir)/Apps/Models/Watch.tcl

#----- Inclure les types de modeles

source $GDefs(Dir)/Apps/Models/Types/CANERM.tcl
source $GDefs(Dir)/Apps/Models/Types/TRAJECT.tcl
source $GDefs(Dir)/Apps/Models/Types/SATDATA.tcl
source $GDefs(Dir)/Apps/Models/Types/MLCD.tcl
source $GDefs(Dir)/Apps/Models/Types/MLDP0.tcl
source $GDefs(Dir)/Apps/Models/Types/MLDP1.tcl

namespace eval Model {
   global   GDefs
   variable Lbl
   variable Bubble
   variable Title
   variable Data
   variable Param
   variable Resources

   set Param(Dock) True
   set Param(Geom) { 350x630+[winfo rootx .]+[winfo rooty .] }

   set Data(Delay)   60000                       ;#Delai de refresh des experiences (millisecondes)
   set Data(Handle)  ""                          ;#Numero de tache du refresh (Pour la cancellation)
   set Data(Unit)    "DDD.CC"                    ;#Unite des coordonnees de l'experience selectionnee
   set Data(Show)    False
   set Data(Job)     ""

   set Title           { "Modélisation" "Modeling" }

   #----- Labels

   set Lbl(Name)       { "Nom" "Name" }
   set Lbl(Diag)       { "Diagnostiques" "Diagnostics" }
   set Lbl(Prog)       { "Prognostiques" "Prognostics" }
   set Lbl(Emerg)      { "Urgences" "Emergencies" }
   set Lbl(Source)     { "Sélecteur de source" "Source selector" }
   set Lbl(Lat)        { "Latitude  (Nord+ Sud-) :" "Latitude  (North+ South-):" }
   set Lbl(Lon)        { "Longitude (Est+ Ouest-):" "Longitude (East+ West-)  :" }
   set Lbl(Type)       { "Type d'expérience      :" "Experiment type          :" }
   set Lbl(Create)     { "Créer" "Create" }
   set Lbl(Cancel)     { "Annuler" "Cancel" }
   set Lbl(Checked)    { "Pool vérifié à" "Checked pool at" }
   set Lbl(Select)     { "Sélectionner" "Select" }
   set Lbl(Warning)    { "Attention" "Warning" }
   set Lbl(Watch)      { "Veilles" "Watch" }
   set Lbl(MetPath)    { "Répertoire des données météorologiques" "Meteorological data path" }

   #----- Bulles d'aide

   set Bubble(PathSel) { "Liste des dépots d'experiences disponibles" "List of experiments available" }
   set Bubble(PathAdd) { "Ajouter un repertoire a la liste\ndes dépots d'experiences disponibles" "Add a path to the list of experiments available" }
   set Bubble(Dock)    { "Attacher/Détacher l'interface de la fenêtre principale" "Dock/Undock window from main window" }
   set Bubble(Close)   { "Fermer l'outils" "Close the tool" }
   set Bubble(Bubble)  { "Activer les bulles d'informations" "Activate information bubbles" }
   set Bubble(Plus)    { "Ouvrir toutes les branches" "Open up all branches" }
   set Bubble(Minus)   { "Fermer toutes les branches" "Close all branches" }
   set Bubble(SRC)     { "Ouvre le sélecteur de sources" "Open the source selector" }
   set Bubble(Name)    { "Nom de l'expérience\nCe nom sera utilisé sur tous les produits"
                         "Experiment name\nThis name will be used on every product" }
   set Bubble(Info)    { "Informations de positionnement (id,lat,lon)" "Position information (id,lat,lon)" }
   set Bubble(Coord)   { "Permet de changer ou de convertir\nle type de coordonnées en degrées-minutes\nou degrées centième"
                         "Use to change or convert the coordinate format in degree-minute or degree-hundreth" }
   set Bubble(Create)  { "Créer l'expérience" "Create the experiment" }
   set Bubble(Cancel)  { "Annuler, retour à la liste des expériences" "Cancel, return to the experiment list" }
   set Bubble(Type0)   { "Type de source volcanique" "Volcanic source type" }
   set Bubble(Type1)   { "Type de source nucléaire"  "Nuclear source type" }
   set Bubble(Type2)   { "Type de source CTBT" "CTBT source type" }
   set Bubble(Type3)   { "Type de source feu" "Fire source type" }
   set Bubble(Type4)   { "Type de source épidemie" "Epidemic source type" }
   set Bubble(Type5)   { "Type de source déversement" "Spill source type" }
   set Bubble(Type6)   { "Autres Types de sources" "Other source type" }

   image create photo ICO_VOLC    -file $GDefs(Dir)/Resources/Image/Symbol/Icon/Type_VOLCANO.gif
   image create photo ICO_NUCL    -file $GDefs(Dir)/Resources/Image/Symbol/Icon/Type_NUCLEAR.gif
   image create photo ICO_CTBT    -file $GDefs(Dir)/Resources/Image/Symbol/Icon/Type_CTBT.gif
   image create photo ICO_FIRE    -file $GDefs(Dir)/Resources/Image/Symbol/Icon/Type_FIRE.gif
   image create photo ICO_OTHE    -file $GDefs(Dir)/Resources/Image/Symbol/Icon/Type_OTHER.gif
   image create photo ICO_BIO     -file $GDefs(Dir)/Resources/Image/Symbol/Icon/Type_BIO.gif
   image create photo ICO_SPILL   -file $GDefs(Dir)/Resources/Image/Symbol/Icon/Type_SPILL.gif

   set Resources(Icos)  "ICO_VOLC ICO_NUCL ICO_CTBT ICO_FIRE ICO_BIO ICO_SPILL ICO_OTHE"
   set Resources(Plus)  "@$GDefs(Dir)/Resources/Bitmap/plus.ico"
   set Resources(Minus) "@$GDefs(Dir)/Resources/Bitmap/minus.ico"
}

#-------------------------------------------------------------------------------
# Nom      : <Model::DrawCurrent>
# Creation : Mai 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Afficher l'icone de l'experience courante sur la projection.
#
# Parametres :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Model::DrawCurrent { } {
   variable Data
   variable Resources

   set oka [catch { set h [expr int($SPI::Src(Lat))] }]
   set oko [catch { set h [expr int($SPI::Src(Lon))] }]

   if { !$oka && !$oko } {
      SPI::IcoAdd $Page::Data(Frame) SOURCE "" [list [list "$SPI::Src(Name)" $SPI::Src(Lat) $SPI::Src(Lon) 0 [lindex $Resources(Icos) $SPI::Src(Type)]]]
   }
}

#----------------------------------------------------------------------------
# Nom      : <Model::Window>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer une boite d'experiences.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Model::Window { { Show "" } } {
   global   GDefs
   variable Lbl
   variable Bubble
   variable Data
   variable Title
   variable Param

   if { $Show!="" } {
      set Data(Show) $Show
   }

   if { ![winfo exists .model] } {

      if { $Param(Dock) } {
         frame .model
         SPI::Dock .model
      } else {
         toplevel          .model
         wm title          .model "[lindex $SPI::Title(SPI) $GDefs(Lang)] $GDefs(Version) ([lindex $Title $GDefs(Lang)])"
         wm transient      .model .
         eval wm geometry  .model $Param(Geom)
         wm protocol       .model WM_DELETE_WINDOW { set Model::Data(Show) False; Model::Destroy }
      }

      frame .model.dock -relief raised -bd 1
         button .model.dock.sel -image DOCK -anchor w -relief flat -bd 0 -overrelief raised -command Model::Dock
         button .model.dock.del -image DOCKDELETE -anchor w -relief flat -bd 0 -overrelief raised -command { set Model::Data(Show) False; Model::Destroy }
         label .model.dock.info -textvariable Model::Data(Job) -relief sunken -bd 1 -anchor w -width 21 -bg $GDefs(ColorLight)
         pack .model.dock.sel .model.dock.del -side left
         pack .model.dock.info -side left -fill x -expand true
      pack .model.dock -side bottom -fill x

      Bubble::Create .model.dock.sel [lindex $Bubble(Dock) $GDefs(Lang)]
      Bubble::Create .model.dock.del [lindex $Bubble(Close) $GDefs(Lang)]

      TabFrame::Create .model.tab 1 Model::TypeSelect 1 350
      pack .model.tab -side top -fill both -expand true

      Meteo::Create [TabFrame::Add .model.tab 1 "Meteo" False]
      Exp::Create   [TabFrame::Add .model.tab 1 [lindex $Lbl(Emerg) $GDefs(Lang)] False]
      Watch::Create [TabFrame::Add .model.tab 1 [lindex $Lbl(Watch) $GDefs(Lang)] False]
   }

   if { !$Data(Show) } {
      Model::Destroy
   }

   #----- Preparer la liste.

   Model::Check $Data(Delay)

   TabFrame::Select .model.tab 1

   return .model
}

#----------------------------------------------------------------------------
# Nom      : <Model::Dock>
# Creation : Mars 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Inserer la boite dans l'interface.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Model::Dock { } {
   variable Param

   if { $Param(Dock) } {
      set Param(Dock) False
   } else {
      set Param(Dock) True
   }

   Model::Destroy
   Model::Window
}

#-------------------------------------------------------------------------------
# Nom      : <Model::Check>
# Creation : Fevrier 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Verifie les repertoires toute les "ms" millisecondes.
#
# Parametres :
#   <MS>     : Nombre de millisecondes d'interval.
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Model::Check { MS } {
   global GDefs
   variable Data
   variable Lbl

   if { [winfo exists .model] } {

      .model config -cursor watch
      update idletask

      Exp::Read
      Exp::CreateTree

      Watch::Read
      Watch::CreateTree

      set Data(Job) "[lindex $Lbl(Checked) $GDefs(Lang)] [clock format [clock seconds] -format %T -gmt true]"

      .model config -cursor left_ptr
   }

   #----- Instauration de l'evenement de verification de repertoires.

   if { $MS != 0 } {
      set Data(Handle) [after $MS [list Model::Check $MS]]
   }
}

#----------------------------------------------------------------------------
# Nom      : <Model::Destroy>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Detruire la boite d'experiences.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Model::Destroy { } {
   variable Data

   #----- Ajuster la geometrie

   TabFrame::Destroy .model.tab
   destroy .model

   #----- Supprimer le refresh.

   after cancel $Data(Handle)

   #----- Supprimer l'affichage des icones

   SPI::IcoDel WATCH
   SPI::IcoDel EXPERIMENT
}

#----------------------------------------------------------------------------
# Nom      : <Model::GetMetPath>
# Creation : Juin 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Permet de selectionner les path des fichiers de donnees meteo
#            d'analyse et de prognostique.
#
# Parametres :
#   <Parent> : Identificateur de la fenetre parent
#   <DiagVar>: Variable contenant le repertoires des fichiers diagnostiques
#   <ProgVar>: Variable contenant le repertoire des donnees prognostiques
#
# Retour     :
#   <Valid>  : True ou False.
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Model::GetMetPath { Parent DiagVar ProgVar } {
   global GDefs
   variable Lbl

   set Data(ShowPath) 0

   toplevel     .metpath

   wm title     .metpath [lindex $Lbl(MetPath) $GDefs(Lang)]
   wm resizable .metpath 0 0
   wm transient .metpath $Parent
   wm geom      .metpath +[expr [winfo rootx $Parent]+50]+[expr [winfo rooty $Parent]+50]

   frame .metpath.diag -relief raised -bd 1
     button .metpath.diag.select -text [lindex $Lbl(Diag) $GDefs(Lang)] -relief groove -bd 2 \
        -command "set path \[FileBox::Create . \$$DiagVar Path \"\" \]; if { \$path != \"\" } { set $DiagVar \$path }"
     entry .metpath.diag.path -bg $GDefs(ColorLight) -textvariable $DiagVar -relief sunken -bd 1 -width 40
     pack .metpath.diag.select .metpath.diag.path -side left -fill y

   frame .metpath.prog -relief raised -bd 1
     button .metpath.prog.select -text [lindex $Lbl(Prog) $GDefs(Lang)] -relief groove -bd 2 \
        -command "set path \[FileBox::Create . \$$ProgVar Path \"\" \]; if { \$path != \"\" } { set $ProgVar \$path }"
     entry .metpath.prog.path -bg $GDefs(ColorLight) -textvariable $ProgVar -relief sunken -bd 1 -width 40
     pack .metpath.prog.select .metpath.prog.path -side left -fill y

   frame .metpath.command
     button .metpath.command.ok -text "Ok" -relief raised -bd 1 -command "set Model::Data(ShowPath) 1"
     pack .metpath.command.ok -side top -fill both

   pack .metpath.diag .metpath.prog .metpath.command -side top -fill x

   #----- Attendre la selection

   grab .metpath
   tkwait variable Model::Data(ShowPath)
   destroy .metpath
}

#-------------------------------------------------------------------------------
# Nom      : <Model::New>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE -
#
# But      : Creation d'une nouvelle experience.
#
# Parametres :
#   <Parent> : Identificateur de la fenetre parent.
#
# Retour :
#
# Remarques :
#    -On utilise les variables interne de SPI SPI::Src(*) pour recuperer directement
#     les informations provenant du selecteur de source et du dictionnaire.
#
#-------------------------------------------------------------------------------

proc Model::New { Parent Command Label Single } {
   global GDefs
   variable Lbl
   variable Bubble
   variable Data
   variable Resources

   if { [winfo exist .expnew] } {
      return
   }

   toplevel     .expnew
   wm transient .expnew $Parent
   wm geom      .expnew +[expr [winfo rootx $Parent]+50]+[expr [winfo rooty $Parent]+50]
   wm protocol  .expnew WM_DELETE_WINDOW { }
   wm resizable .expnew 0 0
   wm title     .expnew "$Label"

   #----- Initialiser les variables de localisations

   set Data(Type)  0
   set Data(Name)  "New experiment"
   set Data(Pos)   1
   set Data(Id1)   "-"
   set Data(Name1) "New location"
   set Data(Lat1)  0.0
   set Data(Lon1)  0.0

   for { set i 2 } { $i <=50 } { incr i } {
      set Data(Id$i)   "-"
      set Data(Name$i) ""
      set Data(Lat$i)  ""
      set Data(Lon$i)  ""
   }

   frame .expnew.hd
      checkbutton .expnew.hd.mode -variable Page::Data(ToolMode) -onvalue Model -offvalue SPI -image ARROW -indicatoron 0 \
         -bd 1 -selectcolor $GDefs(ColorFrame) -command { SPI::ToolMode $Page::Data(ToolMode) Data True }
      button .expnew.hd.src -text "Identification      Lat          Lon" -anchor w -bd 1 -command "Locator::Window 0"
      pack .expnew.hd.mode -side left -fill y
      pack .expnew.hd.src -side left -fill both -expand true
   pack .expnew.hd -side top -fill x -expand true

   frame .expnew.info -bd 1 -relief raised
      frame .expnew.info.l

      if { $Single } {
         frame .expnew.info.l.loc1
            radiobutton .expnew.info.l.loc1.sel -text "01" -indicatoron false -bd 1 \
               -variable Model::Data(Pos) -value 1 -selectcolor $GDefs(ColorHighLight)
            EntryVar::Create .expnew.info.l.loc1.name Model::Data(Name) string 25 "Model::DrawCurrent" \
               -bd 1 -bg $GDefs(ColorLight)
            EntryVar::Create .expnew.info.l.loc1.lat Model::Data(Lat1) coordinate 0 "Model::DrawCurrent" \
               -bd 1 -width 7 -bg $GDefs(ColorLight) -justify right
            EntryVar::Create .expnew.info.l.loc1.lon Model::Data(Lon1) coordinate 0 "Model::DrawCurrent" \
               -bd 1 -width 7 -bg $GDefs(ColorLight) -justify right
            pack .expnew.info.l.loc1.sel -ipadx 2  -side left
            pack .expnew.info.l.loc1.name .expnew.info.l.loc1.lat .expnew.info.l.loc1.lon -side left -fill y -expand true
         pack .expnew.info.l.loc1 -side top -fill y -expand true
      } else {

         foreach i "1 2 3" {
            frame .expnew.info.l.loc$i
               radiobutton .expnew.info.l.loc$i.sel -text "0$i" -indicatoron false -bd 0 \
                  -variable Model::Data(Pos) -value $i -selectcolor $GDefs(ColorHighLight)
               EntryVar::Create .expnew.info.l.loc$i.name Model::Data(Name$i) string 25 "Model::DrawCurrent" \
                  -bd 1 -bg $GDefs(ColorLight)
               EntryVar::Create .expnew.info.l.loc$i.lat Model::Data(Lat$i) coordinate 0 "Model::DrawCurrent" \
                  -bd 1 -width 12 -bg $GDefs(ColorLight) -justify right
               EntryVar::Create .expnew.info.l.loc$i.lon Model::Data(Lon$i) coordinate 0 "Model::DrawCurrent" \
                  -bd 1 -width 12 -bg $GDefs(ColorLight) -justify right
               pack .expnew.info.l.loc$i.sel -ipadx 2 -side left
               pack .expnew.info.l.loc$i.name .expnew.info.l.loc$i.lat .expnew.info.l.loc$i.lon -side left
            pack .expnew.info.l.loc$i -side top
         }
         scale .expnew.info.sc -orient vertical -command "Model::Scroll" -relief flat -sliderlength 10 -width 10 -bd 1\
            -showvalue false -length 50 -from 1 -to 48 -resolution 1
      }
      button .expnew.info.coord -relief groove -bd 2 -textvariable Model::Data(Unit)\
         -command "Model::SwitchCoord"

      if { $Single } {
         pack .expnew.info.l .expnew.info.coord -side left -fill y -expand true
         pack .expnew.info -side top
      } else {
         pack .expnew.info.l .expnew.info.sc .expnew.info.coord -side left -fill y
         pack .expnew.info -side top
      }

   frame .expnew.group -bd 1 -relief raised
      label .expnew.group.lbl -text "Id"
      entry .expnew.group.ent -textvariable Model::Data(Name) -width 20 -bd 1 -bg $GDefs(ColorLight)

      frame .expnew.group.type -bd 1 -relief sunken
         foreach type { 0 1 2 3 4 5 6 } ico { VAAC RSMC CTBT FIRE BIO SPILL SPCL } {
            radiobutton .expnew.group.type.t$ico -image [lindex $Resources(Icos) $type] -variable Model::Data(Type) \
               -value $type -indicatoron False -selectcolor $GDefs(ColorFrame) -command "Model::DrawCurrent" -bd 1
            pack .expnew.group.type.t$ico -side left -fill x -expand true

            Bubble::Create .expnew.group.type.t$ico [lindex $Bubble(Type$type) $GDefs(Lang)]
         }
      pack .expnew.group.lbl -side left -anchor w
      pack .expnew.group.ent -side left -fill y
      pack .expnew.group.type -side left -fill both -expand true

   pack .expnew.group -side top -fill x

   frame .expnew.commands
      button .expnew.commands.create -text [lindex $Lbl(Create) $GDefs(Lang)] \
         -command "if { \[$Command\] } { Model::NewClose }" \
         -relief raised -bd 1
      button .expnew.commands.cancel -text [lindex $Lbl(Cancel) $GDefs(Lang)] \
         -command "Model::NewClose" -relief raised -bd 1
      pack .expnew.commands.create .expnew.commands.cancel -side left -fill x -expand true
   pack .expnew.commands -side top -fill x

   Bubble::Create .expnew.group.ent       [lindex $Bubble(Name) $GDefs(Lang)]
   Bubble::Create .expnew.info.coord      [lindex $Bubble(Coord) $GDefs(Lang)]
   Bubble::Create .expnew.info            [lindex $Bubble(Info) $GDefs(Lang)]
   Bubble::Create .expnew.commands.create [lindex $Bubble(Create) $GDefs(Lang)]
   Bubble::Create .expnew.commands.cancel [lindex $Bubble(Cancel) $GDefs(Lang)]

   trace variable SPI::Src(Info) w "Model::Source"
}

#-------------------------------------------------------------------------------
# Nom      : <Model::NewClose>
# Creation : Novembre 2002 - J.P. Gauthier - CMC/CMOE -
#
# But      : Terminer la selection de source pour une experience
#
# Parametres :
#
# Retour :
#
# Remarques :
#    -On remet en mode zoom
#    -On supprime les icones
#    -On supprime la trace des sources
#    -On detruit la fenetre
#
#-------------------------------------------------------------------------------

proc Model::NewClose { } {

   if { $Page::Data(ToolMode)=="Model" } {
      SPI::ToolMode SPI Zoom
   }
   SPI::IcoDel SOURCE

   trace vdelete SPI::Src(Info) w { Model::Source }
   destroy .expnew
}

#----------------------------------------------------------------------------
# Nom      : <Model::Source>
# Creation : Mars 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Recuperer la selection des outils de localisations.
#
# Parametres :
#    <>      : Parametres de renvoie des trace
#
# Retour :
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Model::Source { Array Index Op } {
   variable Data

   if { $SPI::Src(Info)!="" } {
      set Data(Type)           $SPI::Src(Type)

      set Data(Id$Data(Pos))   $SPI::Src(No)
      set Data(Name$Data(Pos)) "$SPI::Src(Name)"
      set Data(Lat$Data(Pos))  $SPI::Src(Lat)
      set Data(Lon$Data(Pos))  $SPI::Src(Lon)

      set Data(Name) $Data(Name1)
   }
}

#----------------------------------------------------------------------------
# Nom      : <Model::Scroll>
# Creation : Mars 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectuer un "scroll" de la liste de localisations.
#
# Parametres    :
#    <Frame>    : Identificateur du frame
#    <Val>      : Position actuelle
#
# Retour :
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Model::Scroll { Val } {
   global GDefs
   variable Lbl

   .expnew.info.l.loc1.sel  configure -text "[format "%02i" $Val]" -value $Val
   .expnew.info.l.loc1.name configure -textvariable Model::Data(Name$Val)
   .expnew.info.l.loc1.lat  configure -textvariable Model::Data(Lat$Val)
   .expnew.info.l.loc1.lon  configure -textvariable Model::Data(Lon$Val)

   incr Val
   .expnew.info.l.loc2.sel  configure -text "[format "%02i" $Val]" -value $Val
   .expnew.info.l.loc2.name configure -textvariable Model::Data(Name$Val)
   .expnew.info.l.loc2.lat  configure -textvariable Model::Data(Lat$Val)
   .expnew.info.l.loc2.lon  configure -textvariable Model::Data(Lon$Val)

   incr Val
   .expnew.info.l.loc3.sel  configure -text "[format "%02i" $Val]" -value $Val
   .expnew.info.l.loc3.name configure -textvariable Model::Data(Name$Val)
   .expnew.info.l.loc3.lat  configure -textvariable Model::Data(Lat$Val)
   .expnew.info.l.loc3.lon  configure -textvariable Model::Data(Lon$Val)
}

#-------------------------------------------------------------------------------
# Nom      : <Model::SwitchCoord>
# Creation : Octobre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Faire la conversion en degre ou en minute.
#
# Parametres :
#
# Retour     :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Model::SwitchCoord { } {
   variable Data

   if { "$Data(Unit)" == "DDD MM" } {
      set Data(Unit) "DDD.CC"
      for { set i 1 } { $i <= 50 } { incr i } {
         if { $Data(Lat$i)!="" } {
            set Data(Lat$i) [Convert::Minute2Decimal $Data(Lat$i)]
         }
         if { $Data(Lon$i)!="" } {
            set Data(Lon$i) [Convert::Minute2Decimal $Data(Lon$i)]
         }
      }
   } else {
      set Data(Unit) "DDD MM"
      for { set i 1 } { $i <= 50 } { incr i } {
      if { $Data(Lat$i)!="" } {
            set Data(Lat$i) [Convert::Decimal2Minute $Data(Lat$i) 5]
         }
         if { $Data(Lon$i)!="" } {
            set Data(Lon$i) [Convert::Decimal2Minute $Data(Lon$i) 5]
         }
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Model::TypeSelect>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Mettre en place les parametres selon l'onglet (type)
#            selectionne.
#
# Parametres :
#   <Frame>  : Frame de l'onglet
#   <No>     : Numero de l'onglet
#   <Icon>   : Index d'une icone specifique
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Model::TypeSelect { Frame No { Icon "" } } {
   variable Resources

   set icos ""

   switch $No {
      0 {
         SPI::IcoDel WATCH
         SPI::IcoDel EXPERIMENT
      }
      1 {
         foreach exp $Exp::Data(List) {
            if { $Icon=="" || [lindex $exp 1]==$Icon} {
               foreach loc [lindex $exp 3] {
                  lappend icos "\"[lindex $loc 0] [lindex $exp 0]:[lindex $exp 1]\"\
                     [lindex $loc 1] [lindex $loc 2] 0 [lindex $Resources(Icos) [lindex $exp 2]]"
               }
            }
         }
         SPI::IcoDel WATCH
         SPI::IcoAdd $Page::Data(Frame) EXPERIMENT "" $icos
        }
      2 {
         foreach watch $Watch::Data(List) {
            if { $Icon=="" || [lindex $watch 0]==$Icon} {
               lappend icos "[lindex $watch 0] [lindex $watch 1] [lindex $watch 2] 0 [lindex $Resources(Icos) [lindex $watch 3]]"
            }
         }
         SPI::IcoDel EXPERIMENT
         SPI::IcoAdd $Page::Data(Frame) WATCH "" $icos
        }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Model::Draw...>
# Creation : Octobre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Fonctions de manipulation sur la projection
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <VP>      : Identificateur du Viewport
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Model::DrawInit { Frame VP } {
   variable Data

   set Data(Id$Data(Pos))   "-"
   set Data(Name$Data(Pos)) "MousePoint"
   set Data(Lat$Data(Pos))   $Viewport::Map(LatCursor)
   set Data(Lon$Data(Pos))   $Viewport::Map(LonCursor)
}

proc Model::Draw { Frame VP } {
   variable Data

   set Data(Id$Data(Pos))   "-"
   set Data(Name$Data(Pos)) "MousePoint"
   set Data(Lat$Data(Pos))   $Viewport::Map(LatCursor)
   set Data(Lon$Data(Pos))   $Viewport::Map(LonCursor)
}

proc Model::DrawDone { Frame VP } {
}

proc Model::MoveInit { Frame VP } {
}

proc Model::Move { Frame VP } {
}
proc Model::MoveDone { Frame VP } {
}
