#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie de "Widget" Tk.
# Fichier  : VolumeBox.tcl
# Creation : Juin 2019 - A. Germain
#
# Description:
#    Boite de selection des parametres d'affichage des donnees de volume.
#
# Fonctions:
#
#   VolumeBox::Create   { Parent Apply }
#
# Remarques :
#
#===============================================================================

package provide VolumeBox 1.0
package require FSTD

catch { SPI::Splash "Loading Widget Package VolumeBox 1.0" }

namespace eval VolumeBox {
   variable Lbl
   variable Bubble
   variable Data

   set Data(limits)   {0.0 0.0 0.0 1.0 1.0 1.0}
   set Data(Top)      0.0
   set Data(North)    0.0
   set Data(South)    0.0
   set Data(East)     0.0
   set Data(West)     0.0

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
   set Bubble(Top)        { "Change l'emplacement du plan de coupe au dessus" "Change position of the top clip plane" }
   set Bubble(North)      { "Change l'emplacement du plan de coupe au nord" "Change position of the north clip plane" }
   set Bubble(South)      { "Change l'emplacement du plan de coupe au sud" "Change position of the south clip plane" }
   set Bubble(East)       { "Change l'emplacement du plan de coupe a l'est" "Change position of the east clip plane" }
   set Bubble(West)       { "Change l'emplacement du plan de coupe a l'ouest" "Change position of the west clip plane" }
   
}

#----------------------------------------------------------------------------
# Nom      : <VolumeBox::Create>
# Creation : Juin 2019 - A. Germain
#
# But      : Interface des parametres d'affichage de volume
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

proc VolumeBox::Create { Parent Apply } {
   global GDefs
   variable Lbl
   variable Bubble
   variable Data
   
   VolumeBox::GetDimensions;

   if { [winfo exist .volbox] } {
      raise .volbox
      return
   }

   toplevel     .volbox
   wm geom      .volbox =300x400+[winfo rootx $Parent]+[expr [winfo rooty $Parent]+[winfo height $Parent]]
   wm transient .volbox .
   wm resizable .volbox 0 0
   wm title     .volbox "VolumeBox 1.0"

   TabFrame::Create .volbox.tab 1 ""
   pack .volbox.tab -side top -fill both -expand true -padx 5 -pady 2

   set fr [TabFrame::Add .volbox.tab 1 [lindex $Lbl(Params) $GDefs(Lang)] False ""]

      
      
   labelframe $fr.top -text "[lindex $Lbl(Top) $GDefs(Lang)]"
      scale $fr.top.scale -orient horizontal -from 0 -to [ expr $Data(NK) - 1 ] \
         -showvalue true -variable VolumeBox::Data(Top) -relief flat -command "" -width 14 -sliderlength 8 -bd 1 -resolution 1
      pack $fr.top.scale -side left -fill x -expand true -padx 2 -pady 0
   pack $fr.top -side top -fill x -pady 1
   
   labelframe $fr.north -text "[lindex $Lbl(North) $GDefs(Lang)]"
      scale $fr.north.scale -orient horizontal -from 0 -to [ expr $Data(NJ) - 1 ] \
         -showvalue true -variable VolumeBox::Data(North) -relief flat -command "" -width 14 -sliderlength 8 -bd 1 -resolution 1
      pack $fr.north.scale -side left -fill x -expand true -padx 2 -pady 0
   pack $fr.north -side top -fill x -pady 1
   
   labelframe $fr.south -text "[lindex $Lbl(South) $GDefs(Lang)]"
      scale $fr.south.scale -orient horizontal -from 0 -to [ expr $Data(NJ) - 1 ] \
         -showvalue true -variable VolumeBox::Data(South) -relief flat -command "" -width 14 -sliderlength 8 -bd 1 -resolution 1
      pack $fr.south.scale -side left -fill x -expand true -padx 2 -pady 0
   pack $fr.south -side top -fill x -pady 1   

   labelframe $fr.east -text "[lindex $Lbl(East) $GDefs(Lang)]"
      scale $fr.east.scale -orient horizontal -from 0 -to [ expr $Data(NI) - 1 ] \
         -showvalue true -variable VolumeBox::Data(East) -relief flat -command "" -width 14 -sliderlength 8 -bd 1 -resolution 1
      pack $fr.east.scale -side left -fill x -expand true -padx 2 -pady 0
   pack $fr.east -side top -fill x -pady 1
   
   labelframe $fr.west -text "[lindex $Lbl(West) $GDefs(Lang)]"
      scale $fr.west.scale -orient horizontal -from 0 -to [ expr $Data(NI) - 1 ] \
         -showvalue true -variable VolumeBox::Data(West) -relief flat -command "" -width 14 -sliderlength 8 -bd 1 -resolution 1
      pack $fr.west.scale -side left -fill x -expand true -padx 2 -pady 0
   pack $fr.west -side top -fill x -pady 1   
   
   Bubble::Create $fr.top.scale $Bubble(Top)
   Bubble::Create $fr.north.scale $Bubble(North)
   Bubble::Create $fr.south.scale $Bubble(South)
   Bubble::Create $fr.east.scale $Bubble(East)
   Bubble::Create $fr.west.scale $Bubble(West)

   frame .volbox.cmd
      button .volbox.cmd.close -text [lindex $Lbl(Close) $GDefs(Lang)] -bd 1 -relief raised -command "destroy .volbox"
      button .volbox.cmd.apply -text [lindex $Lbl(Apply) $GDefs(Lang)] -bd 1 -relief raised -command "VolumeBox::SetLimits \$VolumeBox::Data(West) \$VolumeBox::Data(South) 0 \$VolumeBox::Data(East) \$VolumeBox::Data(North) \$VolumeBox::Data(Top); VolumeBox::GetLimits; $Apply"
      pack .volbox.cmd.apply .volbox.cmd.close -side left -fill x -expand true
   pack .volbox.cmd -side bottom -fill x -padx 5 -pady 5

   Bubble::Create .volbox.cmd.apply $Bubble(Apply)
   Bubble::Create .volbox.cmd.close $Bubble(Close)

   
   
   TabFrame::Select .volbox.tab 0
}

#----------------------------------------------------------------------------
# Nom      : <VolumeBox::SetLimits>
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

proc VolumeBox::SetLimits { X0 Y0 Z0 X1 Y1 Z1 } {
   foreach field [concat $FSTD::Data(List) $FSTD::Data(ListTool)] {
      if { [FSTD::ParamGetMode $field]==$FSTD::Param(Spec) } {
         set temp [list $X0 $Y0 $Z0 $X1 $Y1 $Z1]
         fstdfield stats $field -limits $temp 
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <VolumeBox::GetLimits>
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

proc VolumeBox::GetLimits { } {
   foreach field [concat $FSTD::Data(List) $FSTD::Data(ListTool)] {
      if { [FSTD::ParamGetMode $field]==$FSTD::Param(Spec) } {
         set VolumeBox::Data(limits) [fstdfield stats $field -limits]
         break
      }

   }
}

#----------------------------------------------------------------------------
# Nom      : <VolumeBox::GetDimensions>
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

proc VolumeBox::GetDimensions { } {
   foreach field [concat $FSTD::Data(List) $FSTD::Data(ListTool)] {
      if { [FSTD::ParamGetMode $field]==$FSTD::Param(Spec) } {
         set VolumeBox::Data(NI) [fstdfield define $field -NI]
         set VolumeBox::Data(NJ) [fstdfield define $field -NJ]
         set VolumeBox::Data(NK) [fstdfield define $field -NK]
         break
      }
   }
}
