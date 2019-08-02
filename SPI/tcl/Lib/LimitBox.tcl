#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie de "Widget" Tk.
# Fichier  : LimitBox.tcl
# Creation : Juin 2019 - A. Germain
#
# Description:
#    Boite de selection des parametres d'affichage des limites.
#
# Fonctions:
#
#   LimitBox::Create   { Parent Apply }
#
# Remarques :
#
#===============================================================================

package provide LimitBox 1.0
package require FSTD

catch { SPI::Splash "Loading Widget Package LimitBox 1.0" }

namespace eval LimitBox {
   variable Lbl
   variable Bubble
   variable Data

   set Data(limits)   {0.0 0.0 0.0 1.0 1.0 1.0}
   set Data(Top)      0.0
   set Data(North)    0.0
   set Data(South)    0.0
   set Data(East)     0.0
   set Data(West)     0.0

   set Data(RealTime) [expr $OpenGL::Param(Res)<=1] ;#Reaffichage interactif

   set Data(NI)       0
   set Data(NJ)       0
   set Data(NK)       0

   set Lbl(Params)    { "Paramètres" "Parameters" }
   set Lbl(Close)     { "Fermer" "Close" }
   set Lbl(Apply)     { "Appliquer" "Apply" }
   set Lbl(Top)       { "Plan dessus" "Top Plane" }
   set Lbl(North)     { "Plan nord" "North Plane" }
   set Lbl(South)     { "Plan sud" "South Plane" }
   set Lbl(East)      { "Plan est" "East Plane" }
   set Lbl(West)      { "Plan ouest" "West Plane" }

   set Bubble(Apply)      { "Appliquer les paramêtres" "Apply the parameters" }
   set Bubble(Close)      { "Fermer sans appliquer les paramêtres"  "Close without applying the parameters" }
   set Bubble(Mode)       { "Mode de sélection des limites" "Limit selection mode" }
   set Bubble(Top)        { "Change l'emplacement du plan de coupe au dessus" "Change position of the top clip plane" }
   set Bubble(North)      { "Change l'emplacement du plan de coupe au nord" "Change position of the north clip plane" }
   set Bubble(South)      { "Change l'emplacement du plan de coupe au sud" "Change position of the south clip plane" }
   set Bubble(East)       { "Change l'emplacement du plan de coupe a l'est" "Change position of the east clip plane" }
   set Bubble(West)       { "Change l'emplacement du plan de coupe a l'ouest" "Change position of the west clip plane" }
   set Bubble(Real)       { "Applique les parametres interactivement" "Apply parameters interactively" }
   
}

#----------------------------------------------------------------------------
# Nom      : <LimitBox::Create>
# Creation : Juin 2019 - A. Germain
#
# But      : Interface des parametres de limites
#
# Parametres :
#   <Parent> : Widget parent
#   <Apply>  : Apply button
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc LimitBox::Create { Parent Apply } {
   global GDefs
   variable Lbl
   variable Bubble
   variable Data
   
   LimitBox::GetDimensions

   set Data(South)    0.0
   set Data(West)     0.0
   set Data(Top)      [expr $Data(NK) - 1]
   set Data(North)    [expr $Data(NJ) - 1]
   set Data(East)     [expr $Data(NI) - 1]

   if { [winfo exist .limbox] } {
      raise .limbox
      return
   }

   toplevel     .limbox
   wm geom      .limbox =300x400+[winfo rootx $Parent]+[expr [winfo rooty $Parent]+[winfo height $Parent]]
   wm transient .limbox .
   wm resizable .limbox 0 0
   wm title     .limbox "LimitBox 1.0"

   TabFrame::Create .limbox.tab 1 ""
   pack .limbox.tab -side top -fill both -expand true -padx 5 -pady 2

   set fr [TabFrame::Add .limbox.tab 1 [lindex $Lbl(Params) $GDefs(Lang)] False ""]

      
      
   labelframe $fr.top -text "[lindex $Lbl(Top) $GDefs(Lang)]"
      scale $fr.top.scale -orient horizontal -from 0 -to [ expr $Data(NK) - 1 ] \
         -showvalue true -variable LimitBox::Data(Top) -relief flat -command "LimitBox::SetLimits \$LimitBox::Data(West) \$LimitBox::Data(South) 0 \$LimitBox::Data(East) \$LimitBox::Data(North) \$LimitBox::Data(Top); LimitBox::GetLimits; if { \$LimitBox::Data(RealTime) } { $Apply }; LimitBox::ignore" -width 14 -sliderlength 8 -bd 1 -resolution 1
      pack $fr.top.scale -side left -fill x -expand true -padx 2 -pady 0
   pack $fr.top -side top -fill x -pady 1
   
   labelframe $fr.north -text "[lindex $Lbl(North) $GDefs(Lang)]"
      scale $fr.north.scale -orient horizontal -from 0 -to [ expr $Data(NJ) - 1 ] \
         -showvalue true -variable LimitBox::Data(North) -relief flat -command "LimitBox::SetLimits \$LimitBox::Data(West) \$LimitBox::Data(South) 0 \$LimitBox::Data(East) \$LimitBox::Data(North) \$LimitBox::Data(Top); LimitBox::GetLimits; if { \$LimitBox::Data(RealTime) } { $Apply }; LimitBox::ignore" -width 14 -sliderlength 8 -bd 1 -resolution 1
      pack $fr.north.scale -side left -fill x -expand true -padx 2 -pady 0
   pack $fr.north -side top -fill x -pady 1
   
   labelframe $fr.south -text "[lindex $Lbl(South) $GDefs(Lang)]"
      scale $fr.south.scale -orient horizontal -from 0 -to [ expr $Data(NJ) - 1 ] \
         -showvalue true -variable LimitBox::Data(South) -relief flat -command "LimitBox::SetLimits \$LimitBox::Data(West) \$LimitBox::Data(South) 0 \$LimitBox::Data(East) \$LimitBox::Data(North) \$LimitBox::Data(Top); LimitBox::GetLimits; if { \$LimitBox::Data(RealTime) } { $Apply }; LimitBox::ignore" -width 14 -sliderlength 8 -bd 1 -resolution 1
      pack $fr.south.scale -side left -fill x -expand true -padx 2 -pady 0
   pack $fr.south -side top -fill x -pady 1   

   labelframe $fr.east -text "[lindex $Lbl(East) $GDefs(Lang)]"
      scale $fr.east.scale -orient horizontal -from 0 -to [ expr $Data(NI) - 1 ] \
         -showvalue true -variable LimitBox::Data(East) -relief flat -command "LimitBox::SetLimits \$LimitBox::Data(West) \$LimitBox::Data(South) 0 \$LimitBox::Data(East) \$LimitBox::Data(North) \$LimitBox::Data(Top); LimitBox::GetLimits; if { \$LimitBox::Data(RealTime) } { $Apply }; LimitBox::ignore" -width 14 -sliderlength 8 -bd 1 -resolution 1
      pack $fr.east.scale -side left -fill x -expand true -padx 2 -pady 0
   pack $fr.east -side top -fill x -pady 1
   
   labelframe $fr.west -text "[lindex $Lbl(West) $GDefs(Lang)]"
      scale $fr.west.scale -orient horizontal -from 0 -to [ expr $Data(NI) - 1 ] \
         -showvalue true -variable LimitBox::Data(West) -relief flat -command "LimitBox::SetLimits \$LimitBox::Data(West) \$LimitBox::Data(South) 0 \$LimitBox::Data(East) \$LimitBox::Data(North) \$LimitBox::Data(Top); LimitBox::GetLimits; if { \$LimitBox::Data(RealTime) } { $Apply }; LimitBox::ignore" -width 14 -sliderlength 8 -bd 1 -resolution 1
      pack $fr.west.scale -side left -fill x -expand true -padx 2 -pady 0
   pack $fr.west -side top -fill x -pady 1   
   
   Bubble::Create $fr.top.scale $Bubble(Top)
   Bubble::Create $fr.north.scale $Bubble(North)
   Bubble::Create $fr.south.scale $Bubble(South)
   Bubble::Create $fr.east.scale $Bubble(East)
   Bubble::Create $fr.west.scale $Bubble(West)

   frame .limbox.cmd
      checkbutton .limbox.cmd.real -image DOCSEL -bd 1 -relief raised \
         -overrelief raised -offrelief flat \
         -variable LimitBox::Data(RealTime) -onvalue 1 -offvalue 0 -indicatoron false
      checkbutton .limbox.cmd.pick -variable Page::Data(ToolMode) -relief raised -bd 1 -overrelief raised -offrelief flat \
         -onvalue Limit -offvalue SPI -selectcolor "" -image ARROW -indicatoron false -command { SPI::ToolMode $Page::Data(ToolMode) Data True }
      button .limbox.cmd.close -text [lindex $Lbl(Close) $GDefs(Lang)] -bd 1 -relief raised -command "LimitBox::Close"
      button .limbox.cmd.apply -text [lindex $Lbl(Apply) $GDefs(Lang)] -bd 1 -relief raised -command "LimitBox::SetLimits \$LimitBox::Data(West) \$LimitBox::Data(South) 0 \$LimitBox::Data(East) \$LimitBox::Data(North) \$LimitBox::Data(Top); LimitBox::GetLimits; $Apply"
      pack .limbox.cmd.real .limbox.cmd.pick -side left
      pack .limbox.cmd.apply .limbox.cmd.close -side left -fill x -expand true
   pack .limbox.cmd -side bottom -fill x -padx 5 -pady 5

   Bubble::Create .limbox.cmd.apply $Bubble(Apply)
   Bubble::Create .limbox.cmd.close $Bubble(Close)
   Bubble::Create .limbox.cmd.real  $Bubble(Real)
   Bubble::Create .limbox.cmd.pick  $Bubble(Mode)
   
   
   TabFrame::Select .limbox.tab 0
}

proc LimitBox::ignore {data} {
}

#----------------------------------------------------------------------------
# Nom      : <LimitBox::Close>
# Creation : Aout 2019 - A. Germain
#
# But      : Ferme la fenetre
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc LimitBox::Close { } {
   destroy .limbox
}

#----------------------------------------------------------------------------
# Nom      : <LimitBox::SetLimits>
# Creation : Juin 2019 - A. Germain
#
# But      : pour definir les limites
#
# Parametres :
#   <X0> : Limite minimum x
#   <X1> : Limite maximum x 
#   <Y0> : Limite minimum y
#   <Y1> : Limite maximum y
#   <Z0> : Limite minimum z
#   <Z1> : Limite maximum z
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------    

proc LimitBox::SetLimits { X0 Y0 Z0 X1 Y1 Z1 } {
   foreach field [concat $FSTD::Data(List) $FSTD::Data(ListTool)] {
      if { [FSTD::ParamGetMode $field]==$FSTD::Param(Spec) } {
         set temp [list [expr int($X0)] [expr int($Y0)] [expr int($Z0)] [expr int($X1)] [expr int($Y1)] [expr int($Z1)] ]
         fstdfield stats $field -limits $temp 
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <LimitBox::GetLimits>
# Creation : Juin 2019 - A. Germain
#
# But      : pour recupere les limites
#
# Parametres :
#   
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------    

proc LimitBox::GetLimits { } {
   foreach field [concat $FSTD::Data(List) $FSTD::Data(ListTool)] {
      if { [FSTD::ParamGetMode $field]==$FSTD::Param(Spec) } {
         set LimitBox::Data(limits) [fstdfield stats $field -limits]
         break
      }

   }
}

#----------------------------------------------------------------------------
# Nom      : <LimitBox::GetDimensions>
# Creation : Juin 2019 - A. Germain
#
# But      : pour recupere les valeurs de NI NJ et NK
#
# Parametres :
#   
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------    

proc LimitBox::GetDimensions { } {
   foreach field [concat $FSTD::Data(List) $FSTD::Data(ListTool)] {
      if { [FSTD::ParamGetMode $field]==$FSTD::Param(Spec) } {
         set LimitBox::Data(NI) [fstdfield define $field -NI]
         set LimitBox::Data(NJ) [fstdfield define $field -NJ]
         set LimitBox::Data(NK) [fstdfield define $field -NK]
         break
      }
   }
}
