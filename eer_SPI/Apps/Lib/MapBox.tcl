#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie de fonctions pour fichier standards.
# Fichier  : MapBox.tk
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# Description: Ce package s'occupe des taches relatives a la configuration des
#              palettes de couleurs.
#
# Fonctions:
#   MapBox::Config           { args }
#   MapBox::ControlAdd       { Canvas X }
#   MapBox::ControlBind      { Canvas X Idx Color }
#   MapBox::ControlColor     { Canvas Id }
#   MapBox::ControlDel       { Canvas Id }
#   MapBox::ControlMove      { Canvas Id X }
#   MapBox::ControlPopup     { Canvas X Y XI Id }
#   MapBox::ControlSet       { Id X }
#   MapBox::ControlValue     { Canvas X }
#   MapBox::Create           { Parent Apply Map args }
#   MapBox::Delete           { Widget }
#   MapBox::List             { Widget }
#   MapBox::ListImg          { Widget }
#   MapBox::Save             { Widget }
#   MapBox::Select           { Map }
#   MapBox::Update           { }
#
#===============================================================================

package provide MapBox 3.0

catch { SPI::Splash "Loading Widget Package MapBox 3.0" }

namespace eval MapBox {
   variable Data
   variable Lbl
   variable Msg
   variable Control
   global env

   #----- Parametres de la palette de couleur associee au champs

   set Data(Curves)   { EXPONENTIAL CUBIC SQUARE LINEAR SQUAREROOT CUBICROOT LOGARITHMIC }
   set Data(CurveIdx) 3
   set Data(Name)     ""
   set Data(Init)     False  ;#Flag d'initialisation de l'interface

   set Data(Red)      100      ;#Pourcentage de rouge dans la palette
   set Data(Green)    100      ;#Pourcentage de vert dans la palette
   set Data(Blue)     100      ;#Pourcentage de bleue dans la palette
   set Data(Alpha)    100      ;#Pourcentage d'opacite
   set Data(Min)      0        ;#Minimum dans la palette
   set Data(Max)      100      ;#Maximum dans la palette
   set Data(Curve)    LINEAR   ;#Courbe d'etendue des couleurs
   set Data(Interp)   ""       ;#Interpolation des couleurs
   set Data(Invert)   0        ;#Inverse de la courbe de distribution
   set Data(List)     ""       ;#Liste des noms de palettes

   set Data(Map)      ""                            ;#Colormap a editer
   set Data(Command)  ""                            ;#Command d'application
   set Data(RealTime) [expr $OpenGL::Param(Res)<=1] ;#Reaffichage interactif

   set Control(X0) 5
   set Control(Y0) 30

   catch { set Data(Dir) $env(HOME)/.spi/Colormap }

   #----- Definitions des labels

   set Lbl(Add)    { "Ajouter un point de controle" "Add control point" }
   set Lbl(Del)    { "Supprimer le point de controle" "Delete control point" }
   set Lbl(Color)  { "Selectionner la couleur" "Select color" }

   set Lbl(Params)  { "Parametres" "Parameters" }
   set Lbl(Map)     { "Palette" "Colormap" }
   set Lbl(Edit)    { "Edition" "Edit" }
   set Lbl(Curve)   { "Courbe" "Curve" }
   set Lbl(Max)     { "Max   " "Max  " }
   set Lbl(Min)     { "Min   " "Min  " }
   set Lbl(Red)     { "Rouge " "Red  " }
   set Lbl(Green)   { "Vert  " "Green" }
   set Lbl(Blue)    { "Bleue " "Blue " }
   set Lbl(Alpha)   { "Alpha " "Alpha" }
   set Lbl(Close)   { "Fermer" "Close" }
   set Lbl(Apply)   { "Appliquer" "Apply" }
   set Lbl(Yes)     { "Oui" "Yes" }
   set Lbl(No)      { "Non" "No" }
   set Lbl(Color)   { "Couleur" "Color" }
   set Lbl(Smooth)  { "Lisse" "Smooth" }
   set Lbl(Fix)     { "Fixe" "Fixed" }
   set Lbl(Invert)  { "Inverse" "Invert" }

   set Msg(Exist)   { "Cette palette existe deja, voulez vous la remplacer ?" "This colormap exists. Do yo wish to replace it ?" }
   set Msg(Save)    { "Veuillez entrer le nom de la palette a sauvegarder" "Please entre the name of the colormap to be saved" }
   set Msg(Saved)   { "La palette suivante a ete sauvegardee" "The following colormap has been saved" }
   set Msg(Delete)  { "Voulez-vous vraiment supprimer cette palette ?" "Do you really want do delete this colormap ?" }

   #----- Definitions des bulles

   set Bubble(List)  { "Liste des paletes de couleurs disponibles\n(REC_... Palettes de XREC et MAX2)"
                       "Available colormap list\n(REC_... XREC and MAX2 Colormaps)" }

   set Bubble(Curve) { "Courbe de distribution des couleurs"
                       "Color distribution curve" }

   set Bubble(Min)   { "Ajustement du minimum de la palette"
                       "Adjust colormap mimimum" }

   set Bubble(Max)   { "Ajustement du maximum de la palette"
                       "Adjust colormap maximum" }

   set Bubble(Red)   { "Ajustement du ratio de rouge"
                       "Adjust red ratio" }

   set Bubble(Green) { "Ajustement du ratio de vert"
                       "Adjust green ratio" }

   set Bubble(Blue)  { "Ajustement du ratio de bleue"
                       "Adjust blue ratio" }

   set Bubble(Alpha) { "Ajustement du ratio d'opacité"
                       "Adjust opacity ratio" }

   set Bubble(Save)  { "Sauvegarder la définition de palette courante"
                       "Save the current colormap definition" }

   set Bubble(Del)   { "Supprimer définitivement la palette sélectionnée de la liste des palettes"
                       "Definitively removes the selected colormap from the colormap list" }

   set Bubble(Name)  { "Nom de la définition de palette courante"
                       "Name of the current colormap definition" }

   set Bubble(Map)   { "Echantillonage de la palette courante"
                       "Current colormap sampling" }

   set Bubble(Real)  { "Applique les parametres interactivement"
                       "Apply parameters interactively" }

   set Bubble(Apply) { "Appliquer les parametres"
                       "Apply the parameters" }

   set Bubble(Close) { "Fermer sans appliquer les parametres"
                       "Close without applying the parameters" }
   catch {
      image create photo MAPBOXIMG -width 256 -height 20
   }
}

#-------------------------------------------------------------------------------
# Nom      : <MapBox::Config>
# Creation : Aout 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectue la configuration des palettes.
#
# Parametres :
#   <args>   : Valeur passe par les "scale"
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc MapBox::Config { args } {
   variable Data

   colormap configure $Data(Map) -MMratio $Data(Min) $Data(Max) -curve rgba $Data(Curve) \
      -RGBAratio $Data(Red) $Data(Green) $Data(Blue) $Data(Alpha) -interp $Data(Interp) -invertx rgba $Data(Invert)
   MapBox::Update
}

#-------------------------------------------------------------------------------
# Nom      : <MapBox::ControlAdd>
# Creation : Decembre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Ajout d'un point de controle.
#
# Parametres :
#   <Canvas> : Identificateur du canvas
#   <X>      : Coordonnee X de la souris
#
# Retour     :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc MapBox::ControlAdd { Canvas X } {
   variable Control
   variable Data

   set idx [expr $X-$Control(X0)]
   set color [colormap configure $Data(Map) -index $idx]
   eval colormap control $Data(Map) -add $idx $color
   eval set rgb \[format \"#%02x%02x%02x\" $color\]

   MapBox::ControlBind $Canvas $X $idx $rgb
   MapBox::Update
}

#-------------------------------------------------------------------------------
# Nom      : <MapBox::ControlBind>
# Creation : Decembre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Activation d'un point de controle.
#
# Parametres :
#   <Canvas> : Identificateur du canvas
#   <X>      : Coordonnee X dans la palette
#   <Idx>    : Index dans la palette
#   <Color>  : Code de couleur RGB
#
# Retour     :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc MapBox::ControlBind { Canvas X Idx Color } {
   variable Control
   variable Data

   set Control($Control(Nb)) $Idx

   $Canvas create polygon $X $Control(Y0) [expr $X-4] [expr $Control(Y0)+8] [expr $X+4] [expr $Control(Y0)+8] \
      -outline $Color -width 0 -fill $Color -tags "CTRL CTRL$Control(Nb)"

   $Canvas bind CTRL$Control(Nb) <Enter>           "$Canvas configure -cursor hand1"
   $Canvas bind CTRL$Control(Nb) <Leave>           "$Canvas configure -cursor left_ptr"
   $Canvas bind CTRL$Control(Nb) <B1-Motion>       "MapBox::ControlMove $Canvas $Control(Nb) %x"

   $Canvas bind CTRL$Control(Nb) <ButtonPress-1>   "MapBox::ControlSet $Control(Nb) %x"
   $Canvas bind CTRL$Control(Nb) <ButtonPress-3>   "MapBox::ControlPopup $Canvas %X %Y %y $Control(Nb)"

   incr Control(Nb)
}

#-------------------------------------------------------------------------------
# Nom      : <MapBox::ControlColor>
# Creation : Decembre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Selectionner la couleur d'un point de controle.
#
# Parametres :
#   <Canvas> : Identificateur du canvas
#   <Id>     : Identificateur du point de controle
#
# Retour     :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc MapBox::ControlColor { Canvas Id } {
   variable Control
   variable Data

   set idx  $Control($Id)
   set col  [colormap configure $Data(Map) -index $idx]
   set cin  [format "%02x%02x%02x" [lindex $col 0] [lindex $col 1] [lindex $col 2]]
   set alp  [format "%02x" [lindex $col 3]]
   set cout [ColorBox::Create .mapbox #$cin$alp]

   if { $cin!=$cout } {
      $Canvas itemconfigure CTRL$Id -fill $cout -outline $cout
      colormap control $Data(Map) -add $Control($Id) $ColorBox::Data(R) $ColorBox::Data(G) $ColorBox::Data(B) $ColorBox::Data(A)
      MapBox::Update
   }
}

#-------------------------------------------------------------------------------
# Nom      : <MapBox::ControlDel>
# Creation : Decembre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Supprimer un point de controle.
#
# Parametres :
#   <Canvas> : Identificateur du canvas
#   <Id>     : Identificateur du point de controle
#
# Retour     :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc MapBox::ControlDel { Canvas Id } {
   variable Control
   variable Data

   colormap control $Data(Map) -del $Control($Id)
   $Canvas delete CTRL$Id
   MapBox::Update
}

#-------------------------------------------------------------------------------
# Nom      : <MapBox::ControlInit>
# Creation : Decembre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialiser les points de controles.
#
# Parametres :
#   <Canvas> : Identificateur du canvas
#
# Retour     :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc MapBox::ControlInit { Canvas } {
   variable Control
   variable Data

   set Control(Nb) 0

   $Canvas delete CTRL

   foreach point [colormap control $Data(Map) -list] {

      set x [lindex $point 0]
      set r [lindex $point 1]
      set g [lindex $point 2]
      set b [lindex $point 3]
      set a [lindex $point 4]

      set X   [expr $x+$Control(X0)]
      set rgb [format "#%02x%02x%02x" $r $g $b]

      MapBox::ControlBind $Canvas $X $x $rgb
   }
}

#-------------------------------------------------------------------------------
# Nom      : <MapBox::ControlMove>
# Creation : Decembre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Deplacement d'un point de controle courant.
#
# Parametres :
#   <Canvas> : Identificateur du canvas
#   <Id>     : Identificateur du point de controle
#   <X>      : Coordonnee en X
#
# Retour     :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc MapBox::ControlMove { Canvas Id X } {
   variable Control
   variable Data

   set idx [expr $X-$Control(X0)]
   set x   [expr $X-$Control(X)]

   if { $idx>=0 && $idx<256 } {
      if { [colormap control $Data(Map) -move $idx $Control($Id)] } {

         $Canvas move CTRL$Id $x 0

         set Control($Id) $idx
         set Control(X)   $X
         MapBox::Update
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <MapBox::ControlPopup>
# Creation : Decembre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer le menu de controle des points de controles.
#
# Parametres :
#   <Canvas> : Identificateur du canvas
#   <X>      : Coordonnee X de la souris
#   <Y>      : Coordonnee Y de la souris
#   <XI>     : Coordonnee X de la souris dans la palette
#   <Id>     : Identificateur du dernier point de controle
#
# Retour     :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc MapBox::ControlPopup { Canvas X Y XI Id } {
   global GDefs
   variable Lbl
   variable Control

   if { ![winfo exists .mapbox.popup] } {
      menu .mapbox.popup
      .mapbox.popup add command -label [lindex $Lbl(Add) $GDefs(Lang)] \
         -command { MapBox::ControlAdd .mapbox.fr.edit.map $MapBox::Control(X) }
      .mapbox.popup add command -label [lindex $Lbl(Del) $GDefs(Lang)] \
         -command { MapBox::ControlDel .mapbox.fr.edit.map $MapBox::Control(Current) }
      .mapbox.popup add separator
      .mapbox.popup add command -label [lindex $Lbl(Color) $GDefs(Lang)] \
         -command { MapBox::ControlColor .mapbox.fr.edit.map $MapBox::Control(Current) }
   }

   if { $Id==-1 } {
      .mapbox.popup entryconfigure 0 -state normal
      .mapbox.popup entryconfigure 1 -state disabled
      .mapbox.popup entryconfigure 3 -state disabled
   } else {
      .mapbox.popup entryconfigure 0 -state disabled
      .mapbox.popup entryconfigure 1 -state normal
      .mapbox.popup entryconfigure 3 -state normal
   }

   MapBox::ControlSet $Id $XI
   tk_popup .mapbox.popup $X $Y 0
}

#-------------------------------------------------------------------------------
# Nom      : <MapBox::ControlSet>
# Creation : Decembre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Specification du point de controle courant.
#
# Parametres :
#   <Id>     : Identificateur du point de controle
#   <X>      : Coordonnee en X
#
# Retour     :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc MapBox::ControlSet { Id X } {
   variable Control

   set Control(Current) $Id
   set Control(X) $X
}

#-------------------------------------------------------------------------------
# Nom      : <MapBox::ControlValue>
# Creation : Decembre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Afficher la valeur correspondant a l'index de la palette.
#
# Parametres :
#   <Canvas> : Identificateur du canvas
#   <X>      : Coordonnee en X
#
# Retour     :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc MapBox::ControlValue { Canvas X } {
   variable Data
   variable Control

   set idx [expr $X-$Control(X0)]

   if { $idx<0 || $idx>255 || ![fstdfield is $Data(Field)] } {
      set val ""
   } else {
      set val [FSTD::FieldFormat $Data(Field) [fstdfield configure $Data(Field) -map2val $idx]]
   }

   $Canvas itemconfigure CTRLVALUE -text $val
   $Canvas coords CTRLVALUE $X 20
}

#-------------------------------------------------------------------------------
# Nom      : <MapBox::Create>
# Creation : Mars 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer l'interface de selection des parametres de la palette.
#
# Parametres :
#   <Parent> : Frame Parent
#   <Map>    : Palette a editer
#   <Apply>  : Commande a effectuer pour appliquer les changements
#
# Retour     :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc MapBox::Create { Parent Apply Map args } {
   global GDefs
   variable Lbl
   variable Bubble
   variable Data
   variable Control

   if { ![colormap is $Map] } {
      return
   }

   if { [winfo exists .mapbox] } {
      raise .mapbox
      return
   }

   #----- Creer une fenetre sans frame en bas a gauche du parent

   toplevel     .mapbox
   wm geom      .mapbox =300x350+[winfo rootx $Parent]+[expr [winfo rooty $Parent]+[winfo height $Parent]]
   wm transient .mapbox .
   wm resizable .mapbox 0 0
   wm title     .mapbox "MapBox 3.0"

   set Data(Command) $Apply
   set Data(Map)     $Map
   set Data(Init)    True

   colormap image $Data(Map) MAPBOXIMG

   frame .mapbox.fr -relief raised -bd 1

   set fr .mapbox.fr.list
   labelframe $fr -text [lindex $Lbl(Map) $GDefs(Lang)]
      ComboBox::Create $fr.name MapBox::Data(Name) edit sorted nodouble -1 {} 1 8 { MapBox::Select $MapBox::Data(Name) }
      button $fr.name.img -relief raised -bd 1 -image COLORMAP -command "MapBox::ListImg $fr.name" -width 16
      button $fr.save -image FOLDIN -bd 1 -command "MapBox::Save $fr.name" -relief flat -overrelief raised
      button $fr.del -image FOLDOUT -bd 1 -command "MapBox::Delete $fr.name" -relief flat -overrelief raised
      pack $fr.name -side left -fill x -expand true -padx 2
      pack $fr.name.img -side left -fill y
      pack $fr.save $fr.del -side left -padx 2

      Bubble::Create $$fr.name     $Bubble(List)
      Bubble::Create $fr.sel.save  $Bubble(Save)
      Bubble::Create $fr.sel.del   $Bubble(Del)
      Bubble::Create $fr.sel.name  $Bubble(Name)

      MapBox::List $fr.name
   pack .mapbox.fr.list -side top -fill both -padx 5 -pady 5

   set fr .mapbox.fr.params
   labelframe $fr -text [lindex $Lbl(Params) $GDefs(Lang)]
       frame $fr.interp
         label $fr.interp.lbl -text [lindex $Lbl(Color) $GDefs(Lang)] -width 7 -anchor w
         radiobutton $fr.interp.sc1 -value 1 -variable MapBox::Data(Interp) -indicatoron False \
            -relief sunken -bd 1 -overrelief raised -offrelief flat -command "MapBox::Config" -text [lindex $Lbl(Smooth) $GDefs(Lang)]
         radiobutton $fr.interp.sc0 -value 0 -variable MapBox::Data(Interp) -indicatoron False \
            -relief sunken -bd 1 -overrelief raised -offrelief flat -command "MapBox::Config" -text [lindex $Lbl(Fix) $GDefs(Lang)]
         checkbutton $fr.interp.inv -offvalue 0 -onvalue 1 -variable MapBox::Data(Invert) -indicatoron False \
            -relief sunken -bd 1 -overrelief raised -offrelief flat -command "MapBox::Config" -text [lindex $Lbl(Invert) $GDefs(Lang)]
         pack $fr.interp.lbl -side left
         pack $fr.interp.sc1 $fr.interp.sc0 $fr.interp.inv  -side left -fill both -expand true
     frame $fr.min
         label $fr.min.lbl -text [lindex $Lbl(Min) $GDefs(Lang)] -width 7 -anchor w
         label $fr.min.ent -width 3 -relief sunken -bd 1 -bg $GDefs(ColorLight) -textvariable MapBox::Data(Min) -anchor w
         scale $fr.min.sc -from 0 -to 100 -resolution 1 -variable MapBox::Data(Min) -showvalue false \
            -relief flat -bd 1 -orient horizontal -width 15 -sliderlength 10 -command "MapBox::Config"
         pack $fr.min.lbl $fr.min.ent -side left
         pack $fr.min.sc -side left -fill both -expand true
      frame $fr.max
         label $fr.max.lbl -text [lindex $Lbl(Max) $GDefs(Lang)] -width 7 -anchor w
         label $fr.max.ent -width 3 -relief sunken -bd 1 -bg $GDefs(ColorLight) -textvariable MapBox::Data(Max) -anchor w
         scale $fr.max.sc -from 0 -to 100 -resolution 1 -variable MapBox::Data(Max) -showvalue false \
            -relief flat -bd 1 -orient horizontal -width 15 -sliderlength 10 -command "MapBox::Config"
         pack $fr.max.lbl $fr.max.ent -side left
         pack $fr.max.sc -side left -fill both -expand true
      frame $fr.red
         label $fr.red.lbl -text [lindex $Lbl(Red) $GDefs(Lang)] -width 7 -anchor w
         label $fr.red.ent -width 3 -relief sunken -bd 1 -bg $GDefs(ColorLight) -textvariable MapBox::Data(Red) -anchor w
         scale $fr.red.sc -from 0 -to 100 -resolution 1 -variable MapBox::Data(Red) -showvalue false \
            -relief flat -bd 1 -orient horizontal -width 15 -sliderlength 10 -command "MapBox::Config"
         pack $fr.red.lbl $fr.red.ent -side left
         pack $fr.red.sc -side left -fill both -expand true
      frame $fr.green
         label $fr.green.lbl -text [lindex $Lbl(Green) $GDefs(Lang)] -width 7 -anchor w
         label $fr.green.ent -width 3 -relief sunken -bd 1 -bg $GDefs(ColorLight) -textvariable MapBox::Data(Green) -anchor w
         scale $fr.green.sc -from 0 -to 100 -resolution 1 -variable MapBox::Data(Green) -showvalue false \
            -relief flat -bd 1 -orient horizontal -width 15 -sliderlength 10 -command "MapBox::Config"
         pack $fr.green.lbl $fr.green.ent -side left
         pack $fr.green.sc -side left -fill both -expand true
      frame $fr.blue
         label $fr.blue.lbl -text [lindex $Lbl(Blue) $GDefs(Lang)] -width 7 -anchor w
         label $fr.blue.ent -width 3 -relief sunken -bd 1 -bg $GDefs(ColorLight) -textvariable MapBox::Data(Blue) -anchor w
         scale $fr.blue.sc -from 0 -to 100 -resolution 1 -variable MapBox::Data(Blue) -showvalue false \
            -relief flat -bd 1 -orient horizontal -width 15 -sliderlength 10 -command "MapBox::Config"
         pack $fr.blue.lbl $fr.blue.ent -side left
         pack $fr.blue.sc -side left -fill both  -expand true
      frame $fr.alpha
         label $fr.alpha.lbl -text [lindex $Lbl(Alpha) $GDefs(Lang)] -width 7 -anchor w
         label $fr.alpha.ent -width 3 -relief sunken -bd 1 -bg $GDefs(ColorLight) -textvariable MapBox::Data(Alpha) -anchor w
         scale $fr.alpha.sc -from 0 -to 100 -resolution 1 -variable MapBox::Data(Alpha) -showvalue false \
            -relief flat -bd 1 -orient horizontal -width 15 -sliderlength 10 -command "MapBox::Config"
         pack $fr.alpha.lbl $fr.alpha.ent -side left
         pack $fr.alpha.sc -side left -fill both -expand true
      frame $fr.curve
         label $fr.curve.lbl -text [lindex $Lbl(Curve) $GDefs(Lang)] -width 7 -anchor w
         label $fr.curve.ent -width 11 -relief sunken -bd 1 -bg $GDefs(ColorLight) -textvariable MapBox::Data(Curve) -anchor w
         scale $fr.curve.sc -from 0 -to 6 -resolution 1 -variable MapBox::Data(CurveIdx) -showvalue false \
            -relief flat -bd 1 -orient horizontal -width 15 -sliderlength 10 \
            -command { set MapBox::Data(Curve) [lindex $MapBox::Data(Curves) $MapBox::Data(CurveIdx)] ; MapBox::Config }
         pack $fr.curve.lbl $fr.curve.ent -side left
         pack $fr.curve.sc -side left -fill both -expand true
      pack $fr.interp $fr.curve $fr.min $fr.max $fr.red $fr.green $fr.blue $fr.alpha -fill x -padx 2
   pack .mapbox.fr.params -side top -fill both -padx 5 -pady 5 -ipady 2

   Bubble::Create $fr.min   $Bubble(Min)
   Bubble::Create $fr.max   $Bubble(Max)
   Bubble::Create $fr.red   $Bubble(Red)
   Bubble::Create $fr.green $Bubble(Green)
   Bubble::Create $fr.blue  $Bubble(Blue)
   Bubble::Create $fr.alpha $Bubble(Alpha)
   Bubble::Create $fr.curve $Bubble(Curve)

   #----- Edition de palette

   set fr .mapbox.fr.edit
   labelframe $fr -text [lindex $Lbl(Edit) $GDefs(Lang)]
      canvas $fr.map -height 40 -relief flat
      $fr.map create image 5 10 -image MAPBOXIMG -tags MAPBOXIMG -anchor nw
      pack $fr.map -side top -padx 2 -fill x
   pack .mapbox.fr.edit -side top -fill both -padx 5 -pady 5 -ipady 2
   pack .mapbox.fr -side top -padx 5 -pady 5 -fill x

   .mapbox.fr.edit.map create text 0 5 -anchor s -fill black -tags "CTRLVALUE" -text ""
#   .mapbox.fr.edit.map bind MAPBOXIMG <Motion> "MapBox::ControlValue .mapbox.fr.edit.map %x"

   MapBox::ControlInit .mapbox.fr.edit.map
   .mapbox.fr.edit.map bind MAPBOXIMG <ButtonPress-3> { MapBox::ControlPopup .mapbox.fr.edit.map %X %Y %x -1 }
   Bubble::Create .mapbox.fr.edit.map $Bubble(Map)

   #----- Commandes

   frame .mapbox.cmd
      checkbutton .mapbox.cmd.real -image DOCSEL -bd 1 -relief raised \
         -variable MapBox::Data(RealTime) -onvalue 1 -offvalue 0 -indicatoron false
      button .mapbox.cmd.close -text [lindex $Lbl(Close) $GDefs(Lang)] -bd 1 -relief raised -command "destroy .mapbox"
      button .mapbox.cmd.apply -text [lindex $Lbl(Apply) $GDefs(Lang)] -bd 1 -relief raised -command "$Apply"
      pack .mapbox.cmd.real -side left -fill y
      pack .mapbox.cmd.apply .mapbox.cmd.close -side left -fill x -expand true
   pack .mapbox.cmd -side bottom -fill x -padx 5 -pady 5

   Bubble::Create .mapbox.cmd.real  $Bubble(Real)
   Bubble::Create .mapbox.cmd.apply $Bubble(Apply)
   Bubble::Create .mapbox.cmd.close $Bubble(Close)

   #----- Afficher les valeurs si presentes

   if { [llength $args]>0 } {
      set Data(Field) [lindex $args 0]
   }

   MapBox::Select ""
   TabFrame::Select .mapbox.tab 0

   update
   set Data(Init) False
}

#-------------------------------------------------------------------------------
# Nom      : <MapBox::Delete>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Sauvegarder la palette courante.
#
# Parametres :
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc MapBox::Delete { Widget } {
   global GDefs
   variable Data
   variable Lbl
   variable Msg

   set idx [lsearch -exact $Data(List) $Data(Name)]

   if { $idx!=-1 } {
      if { [Dialog::Default .mapbox 200 WARNING $Msg(Delete) "" 0 $Lbl(Yes) $Lbl(No)] } {
         return
      }

      file delete -force $Data(Dir)/$Data(Name).rgba
      MapBox::List $Widget
   }
}

#-------------------------------------------------------------------------------
# Nom      : <MapBox::List>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Recupere la liste des definitions de palettes.
#
# Parametres :
#   <Widget> : Identificateur de la boite
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc MapBox::List { Widget } {
   global GDefs
   variable Data

   set Data(List) ""

   #----- Lire toutes les definitions et inserer les noms dans la liste
   foreach file [glob $Data(Dir)/*] {
      regsub ".rgba" [file tail $file] "" pal
      lappend Data(List) $pal
   }
   set  Data(List) [lsort $Data(List)]

   ComboBox::DelAll  $Widget False
   ComboBox::AddList $Widget $Data(List)
}

#-------------------------------------------------------------------------------
# Nom      : <MapBox::ListImg>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Liste des definitions de palettes en images.
#
# Parametres :
#   <Widget> : Identificateur de la boite
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc MapBox::ListImg { Widget } {
   global GDefs
   variable Data

   #---- Create Window
   if { ![winfo exists .mapboxmaps] } {
      toplevel .mapboxmaps
      wm withdraw .mapboxmaps
      wm overrideredirect .mapboxmaps 1
      canvas .mapboxmaps.maps -relief sunken -bd 1 -yscrollcommand ".mapboxmaps.scroll set" \
            -scrollregion "1 1 500 5000" -width 200 -height 1
      scrollbar .mapboxmaps.scroll -orient vertical -bd 1 -width 10 -command ".mapboxmaps.maps yview"
      bind .mapboxmaps.maps <Button-4> ".mapboxmaps.maps yview scroll -1 units"
      bind .mapboxmaps.maps <Button-5> ".mapboxmaps.maps yview scroll 1 units"

      pack .mapboxmaps.maps -side left -fill both -expand true
      pack .mapboxmaps.scroll -side right -fill y
   }
   .mapboxmaps.maps delete all

   #----- Activate/Deactivate
   if { [winfo ismapped .mapboxmaps] } {
      wm withdraw .mapboxmaps
      grab release .mapboxmaps
   } else {
      set yloc [expr [winfo rooty $Widget] + [winfo height $Widget]]
      set xloc [winfo rootx $Widget]
      wm geometry .mapboxmaps 500x400+$xloc+$yloc
      wm deiconify .mapboxmaps
      raise .mapboxmaps
      grab .mapboxmaps
   }

   colormap create CMAPTMP
   set x 10
   set y 10
   set no 0

   #----- Create button for each colormap
   foreach map $Data(List) {
      if { [lsearch -exact [image names] MAPBOXIMG$map]==-1 } {
         colormap read CMAPTMP $Data(Dir)/$map.rgba
         image create photo MAPBOXIMG$map -width 128 -height 15
         colormap image CMAPTMP MAPBOXIMG$map

         button .mapboxmaps.maps.m$no -text $map -image MAPBOXIMG$map -compound bottom -anchor w -bd 0 \
            -command "MapBox::Select $map; grab release .mapboxmaps; wm withdraw .mapboxmaps "
         bind .mapboxmaps.maps.m$no  <Button-4> ".mapboxmaps.maps yview scroll -1 units"
         bind .mapboxmaps.maps.m$no  <Button-5> ".mapboxmaps.maps yview scroll 1 units"
      }
      .mapboxmaps.maps create window $x $y -anchor nw -window .mapboxmaps.maps.m$no

      incr no
      incr x 150
      if { $x>400 } {
         set x  10
         incr y 50
      }
   }

   colormap free CMAPTMP
   update idletasks
#   set bbox [.mapboxmaps.maps bbox all]
#   puts $bbox
#   .mapboxmaps.maps postscript -x 0 -y 0 -width [expr [lindex $bbox 2]+10] -height [expr [lindex $bbox 3]+10] -rotate false \
#      -pageanchor c -colormode color -pagewidth 8i -file ~/test.ps -fontmap PrintBox::Map
}

#-------------------------------------------------------------------------------
# Nom      : <MapBox::Save>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Sauvegarder la palette courante.
#
# Parametres :
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc MapBox::Save { Widget } {
   global GDefs
   variable Data
   variable Lbl
   variable Msg
   variable Bubble


   if { [set name [Dialog::Get .mapbox $Bubble(Save) $Msg(Save)]]!="" } {
      set idx [lsearch -exact $Data(List) $name]

      if { $idx!=-1 } {
         if { [Dialog::Default .mapbox 200 WARNING $Msg(Exist) "" 0 $Lbl(Yes) $Lbl(No)] } {
            return
         }
      }
      colormap write $Data(Map) $Data(Dir)/$name.rgba
      set Data(Name) $name

      MapBox::List $Widget
      Dialog::Info .mapbox $Msg(Saved) "\n\n\t $name"
   }
}

#-------------------------------------------------------------------------------
# Nom      : <MapBox::Select>
# Creation : Mars 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectue la selection de la palette de couleur en l'inserant
#            dans la palette en cours.
#
# Parametres :
#   <Map>    : Palette a selectionar
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc MapBox::Select { Map } {
   global GDefs
   variable Data
   variable Control

   if { $Map!="" } {
      set Data(Name) $Map
      colormap read $Data(Map) $Data(Dir)/$Data(Name).rgba
      MapBox::ControlInit .mapbox.fr.edit.map
   } else {
      set Data(Name) ""
   }

   set list [colormap configure $Data(Map) -RGBAratio]
   set Data(Red)    [lindex $list 0]
   set Data(Green)  [lindex $list 1]
   set Data(Blue)   [lindex $list 2]
   set Data(Alpha)  [lindex $list 3]

   set list [colormap configure $Data(Map) -MMratio]
   set Data(Min)      [lindex $list 0]
   set Data(Max)      [lindex $list 1]
   set Data(Curve)    [colormap configure $Data(Map) -curve rgba]
   set Data(CurveIdx) [lsearch -exact $Data(Curves) $Data(Curve)]
   set Data(Interp)   [colormap configure $Data(Map) -interp]
   set Data(Invert)   [lindex [colormap configure $Data(Map) -invertx] 0]

   MapBox::Update
}

#-------------------------------------------------------------------------------
# Nom      : <MapBox::Update>
# Creation : Decembre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Mise a jour des parametres de la palette
#
# Parametres :
#
# Retour     :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc MapBox::Update { } {
   variable Data

   colormap image $Data(Map) MAPBOXIMG True

   if { $Data(RealTime) && !$Data(Init) } {
      eval $Data(Command)
   }
}
