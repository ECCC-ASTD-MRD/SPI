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
   set Data(Canvas)   ""
   set Data(VP)       ""

   set Data(NI)       0
   set Data(NJ)       0
   set Data(NK)       0

   set Data(P0)       0          ;#Coordonnees des 4 coins en lat lon
   set Data(P1)       0
   set Data(P2)       0
   set Data(P3)       0

   set Data(PG0)      0         ;#Coordonnees des 4 coins en point de grille
   set Data(PG1)      0
   set Data(PG2)      0
   set Data(PG3)      0

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
   set Bubble(Reset)      { "Réinitialiser les limites" "Reset limits" }
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
         -onvalue LimitBox -offvalue SPI -selectcolor "" -image ARROW -indicatoron false -command "LimitBox::ToggleTool"
      button .limbox.cmd.reset -bitmap "@$GDefs(Dir)/share/bitmap/CLEAR.xbm" -relief raised \
         -bd 1 -command "LimitBox::Reset; $Apply"
      button .limbox.cmd.close -text [lindex $Lbl(Close) $GDefs(Lang)] -bd 1 -relief raised -command "LimitBox::Close"
      button .limbox.cmd.apply -text [lindex $Lbl(Apply) $GDefs(Lang)] -bd 1 -relief raised -command "LimitBox::SetLimits \$LimitBox::Data(West) \$LimitBox::Data(South) 0 \$LimitBox::Data(East) \$LimitBox::Data(North) \$LimitBox::Data(Top); LimitBox::GetLimits; $Apply"
      pack .limbox.cmd.real .limbox.cmd.pick -side left
      pack .limbox.cmd.reset -side left -fill y
      pack .limbox.cmd.apply .limbox.cmd.close -side left -fill x -expand true
   pack .limbox.cmd -side bottom -fill x -padx 5 -pady 5

   Bubble::Create .limbox.cmd.apply $Bubble(Apply)
   Bubble::Create .limbox.cmd.close $Bubble(Close)
   Bubble::Create .limbox.cmd.real  $Bubble(Real)
   Bubble::Create .limbox.cmd.pick  $Bubble(Mode)
   Bubble::Create .limbox.cmd.reset $Bubble(Reset)
   
   
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
   if { $Page::Data(ToolMode)=="LimitBox" } {
      SPI::ToolMode SPI Zoom
   }
   if { $LimitBox::Data(Canvas)!="" } {
      $LimitBox::Data(Canvas) delete LIMIT
   }
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

#----------------------------------------------------------------------------
# Nom      : <LimitBox::Draw...>
# Creation : Aout 2019 - A. Germain - CMC
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

proc LimitBox::DrawInit { Frame VP } {
   variable Data

   if { $Viewport::Map(Type)=="grid" } {
      set LimitBox::Data(West)   $Viewport::Map(GridICursor)
      set LimitBox::Data(South)   $Viewport::Map(GridJCursor)
   } else {
      foreach field [concat $FSTD::Data(List) $FSTD::Data(ListTool)] {
         if { [FSTD::ParamGetMode $field]==$FSTD::Param(Spec) } {
            set temp [fstdfield stats $field -coordpoint $Viewport::Map(LatCursor) $Viewport::Map(LonCursor)]
         }
      }
      set LimitBox::Data(West)   [lindex $temp 0]
      set LimitBox::Data(South)   [lindex $temp 1]
   }
}

proc LimitBox::Draw { Frame VP } {
   variable Data

   if { $Data(Canvas)!="" } {
      $Data(Canvas) delete LIMIT
   }

   if { $Viewport::Map(Type)=="grid" } {
      set LimitBox::Data(East)   $Viewport::Map(GridICursor)
      set LimitBox::Data(North)   $Viewport::Map(GridJCursor)
   } else {
      foreach field [concat $FSTD::Data(List) $FSTD::Data(ListTool)] {
         if { [FSTD::ParamGetMode $field]==$FSTD::Param(Spec) } {
            set temp [fstdfield stats $field -coordpoint $Viewport::Map(LatCursor) $Viewport::Map(LonCursor)]
         }
      }
      set LimitBox::Data(East)   [lindex $temp 0]
      set LimitBox::Data(North)   [lindex $temp 1]
   }
   set Data(Canvas) $Frame.page.canvas
   set Data(Frame)  $Frame
   set Data(VP)     $VP

   LimitBox::UpdateItems $Frame
}

proc LimitBox::DrawDone { Frame VP } {
   variable Data

   foreach field [concat $FSTD::Data(List) $FSTD::Data(ListTool)] {
      if { [FSTD::ParamGetMode $field]==$FSTD::Param(Spec) } {
         if { $LimitBox::Data(Top) == 0 } {
            set LimitBox::Data(Top) [expr [fstdfield define $field -NK] - 1]
         }
         if { $LimitBox::Data(RealTime) } {
            LimitBox::SetLimits $LimitBox::Data(West) $LimitBox::Data(South) 0 $LimitBox::Data(East) $LimitBox::Data(North) $LimitBox::Data(Top)
            Page::Update $Page::Data(Frame)
            Page::UpdateCommand $Page::Data(Frame)
         }
      }
   }
}

proc LimitBox::MoveInit { Frame VP } {
   variable Data

   if { $Viewport::Map(Type)=="grid" } {
      set Data(XD)   $Viewport::Map(GridICursor)
      set Data(YD)   $Viewport::Map(GridJCursor)
   } else {
      foreach field [concat $FSTD::Data(List) $FSTD::Data(ListTool)] {
         if { [FSTD::ParamGetMode $field]==$FSTD::Param(Spec) } {
            set temp [fstdfield stats $field -coordpoint $Viewport::Map(LatCursor) $Viewport::Map(LonCursor)]
         }
      }
      set Data(XD)   [lindex $temp 0]
      set Data(YD)   [lindex $temp 1]
   }
}

proc LimitBox::Move { Frame VP } {
   variable Data

   #----- Effectuer la translation

   if { $Viewport::Map(Type)=="grid" } {
      set deltaX [expr $Viewport::Map(GridICursor) - $Data(XD)]
      set deltaY [expr $Viewport::Map(GridJCursor) - $Data(YD)]
   } else {
      foreach field [concat $FSTD::Data(List) $FSTD::Data(ListTool)] {
         if { [FSTD::ParamGetMode $field]==$FSTD::Param(Spec) } {
            set temp [fstdfield stats $field -coordpoint $Viewport::Map(LatCursor) $Viewport::Map(LonCursor)]
         }
      }
      set deltaX [expr [lindex $temp 0] - $Data(XD)]
      set deltaY [expr [lindex $temp 1] - $Data(YD)]
   }

   if { $Data(Canvas)!="" } {
      $Data(Canvas) delete LIMIT
   }

   if { $Viewport::Map(Type)=="grid" } {
      set Data(XD)   $Viewport::Map(GridICursor)
      set Data(YD)   $Viewport::Map(GridJCursor)
   } else {
      foreach field [concat $FSTD::Data(List) $FSTD::Data(ListTool)] {
         if { [FSTD::ParamGetMode $field]==$FSTD::Param(Spec) } {
            set temp [fstdfield stats $field -coordpoint $Viewport::Map(LatCursor) $Viewport::Map(LonCursor)]
         }
      }
      set Data(XD)   [lindex $temp 0]
      set Data(YD)   [lindex $temp 1]
   }
   set Data(Canvas) $Frame.page.canvas
   set Data(Frame)  $Frame
   set Data(VP)     $VP
   LimitBox::MoveItems $Frame $deltaX $deltaY
}

proc LimitBox::MoveDone { Frame VP } {
   variable Data

   set LimitBox::Data(West) [lindex $Data(PG0) 0]
   set LimitBox::Data(South) [lindex $Data(PG0) 1]
   set LimitBox::Data(East) [lindex $Data(PG2) 0]
   set LimitBox::Data(North) [lindex $Data(PG2) 1]
   LimitBox::DrawDone $Frame $VP
}

#-------------------------------------------------------------------------------
# Nom      : <LimitBox::Update>
# Creation : Juin 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectuer le "Refresh" de l'outils apres une mise a jour dans SPI
#
# Parametres :
#   <Frame>  : Identificateur de Page
#
# Remarques :
#    - Cette fonctions est appele par SPI au besoin.
#
#-------------------------------------------------------------------------------

proc LimitBox::Update { Frame } {
   variable Data
}

#-------------------------------------------------------------------------------
# Nom      : <LimitBox::UpdateItems>
# Creation : Aout 2019 - A. Germain - CMC
#
# But      : Effectuer le "Refresh" des items relatifs a cet outils sur
#            la projection.
#
# Parametres :
#   <Frame>  : Identificateur de Page
#
# Remarques :
#    - Cette fonctions est appele par SPI au besoin.
#
#-------------------------------------------------------------------------------

proc LimitBox::UpdateItems { Frame } {
   global   GDefs
   variable Data
   if { $Page::Data(ToolMode)=="LimitBox" } {
      if { $Data(Canvas)!="" } {
         $Data(Canvas) delete LIMIT
      }
      foreach field [concat $FSTD::Data(List) $FSTD::Data(ListTool)] {
         if { [FSTD::ParamGetMode $field]==$FSTD::Param(Spec) } {

            set Data(P0) [fstdfield stats $field -gridpoint $LimitBox::Data(West) $LimitBox::Data(South)]
            set Data(P1) [fstdfield stats $field -gridpoint $LimitBox::Data(East) $LimitBox::Data(South)]
            set Data(P2) [fstdfield stats $field -gridpoint $LimitBox::Data(East) $LimitBox::Data(North)]
            set Data(P3) [fstdfield stats $field -gridpoint $LimitBox::Data(West) $LimitBox::Data(North)]

         }
      }
      if { $Data(VP)!="" } {
         Viewport::DrawLine $Data(Frame) $Data(VP) "$Data(P0) 0 $Data(P1) 0 $Data(P2) 0 $Data(P3) 0 $Data(P0) 0" LIMIT blue 2 TRUE
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <LimitBox::MoveItems>
# Creation : Aout 2019 - A. Germain - CMC
#
# But      : Effectuer le "Refresh" des items relatifs a cet outils sur
#            la projection quand il bouge.
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <deltaX> : Deplacement en X
#   <deltaY> : Deplacement en Y
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc LimitBox::MoveItems { Frame deltaX deltaY } {
   global   GDefs
   variable Data

   foreach field [concat $FSTD::Data(List) $FSTD::Data(ListTool)] {
      if { [FSTD::ParamGetMode $field]==$FSTD::Param(Spec) } {

         set Data(PG0) [fstdfield stats $field -coordpoint [lindex $Data(P0) 0] [lindex $Data(P0) 1]]
         set Data(PG1) [fstdfield stats $field -coordpoint [lindex $Data(P1) 0] [lindex $Data(P1) 1]]
         set Data(PG2) [fstdfield stats $field -coordpoint [lindex $Data(P2) 0] [lindex $Data(P2) 1]]
         set Data(PG3) [fstdfield stats $field -coordpoint [lindex $Data(P3) 0] [lindex $Data(P3) 1]]

         lset Data(PG0) 0 [expr [lindex $Data(PG0) 0] + $deltaX]
         lset Data(PG0) 1 [expr [lindex $Data(PG0) 1] + $deltaY]
         lset Data(PG1) 0 [expr [lindex $Data(PG1) 0] + $deltaX]
         lset Data(PG1) 1 [expr [lindex $Data(PG1) 1] + $deltaY]
         lset Data(PG2) 0 [expr [lindex $Data(PG2) 0] + $deltaX]
         lset Data(PG2) 1 [expr [lindex $Data(PG2) 1] + $deltaY]
         lset Data(PG3) 0 [expr [lindex $Data(PG3) 0] + $deltaX]
         lset Data(PG3) 1 [expr [lindex $Data(PG3) 1] + $deltaY]

         set Data(P0) [fstdfield stats $field -gridpoint [lindex $Data(PG0) 0] [lindex $Data(PG0) 1]]
         set Data(P1) [fstdfield stats $field -gridpoint [lindex $Data(PG1) 0] [lindex $Data(PG1) 1]]
         set Data(P2) [fstdfield stats $field -gridpoint [lindex $Data(PG2) 0] [lindex $Data(PG2) 1]]
         set Data(P3) [fstdfield stats $field -gridpoint [lindex $Data(PG3) 0] [lindex $Data(PG3) 1]]

      }
   }

   if { $Data(Canvas)!="" } {
      $Data(Canvas) delete LIMIT
   }

   if { $Data(VP)!="" } {
      Viewport::DrawLine $Data(Frame) $Data(VP) "$Data(P0) 0 $Data(P1) 0 $Data(P2) 0 $Data(P3) 0 $Data(P0) 0" LIMIT blue 2 TRUE
   }
}

#-------------------------------------------------------------------------------
# Nom      : <LimitBox::Reset>
# Creation : Aout 2019 - A. Germain - CMC
#
# But      : Remettre les limites pour avoir tous les points de grilles
#
# Parametres :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc LimitBox::Reset {  } {
   set LimitBox::Data(South)    0.0
   set LimitBox::Data(West)     0.0
   set LimitBox::Data(Top)      [expr $LimitBox::Data(NK) - 1]
   set LimitBox::Data(North)    [expr $LimitBox::Data(NJ) - 1]
   set LimitBox::Data(East)     [expr $LimitBox::Data(NI) - 1]
   LimitBox::SetLimits $LimitBox::Data(West) $LimitBox::Data(South) 0 $LimitBox::Data(East) $LimitBox::Data(North) $LimitBox::Data(Top)
   if { $LimitBox::Data(Canvas)!="" } {
      $LimitBox::Data(Canvas) delete LIMIT
   }
}

#-------------------------------------------------------------------------------
# Nom      : <LimitBox::ToggleTool>
# Creation : Aout 2019 - A. Germain - CMC
#
# But      : Efface ou trace le carré de l'outil dependament si l'outil
#            est activé ou désactivé
#
# Parametres :
#
# Remarques : Appeler quand on clique sur le bouton de l'outil
#
#-------------------------------------------------------------------------------

proc LimitBox::ToggleTool {  } {
   variable Data
   SPI::ToolMode $Page::Data(ToolMode) Data True
   if { $LimitBox::Data(Canvas)!="" } {
      $LimitBox::Data(Canvas) delete LIMIT
   }
   if { $Data(VP)!="" && $Page::Data(ToolMode) == "LimitBox" } {
      Viewport::DrawLine $Data(Frame) $Data(VP) "$Data(P0) 0 $Data(P1) 0 $Data(P2) 0 $Data(P3) 0 $Data(P0) 0" LIMIT blue 2 TRUE
   }
}
