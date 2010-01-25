#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie de "Widget" Tk.
# Fichier  : VectorBox.tcl
# Creation : Decembre 2006 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Boite de selection des parametres d'affichage des donnees vectorielles.
#
# Fonctions:
#
#   VectorBox::Create   { Parent Apply }
#   VectorBox::Lock     { Axis From To }
#   VectorBox::Draw     { Frame VP }
#   VectorBox::DrawDone { Frame VP }
#   VectorBox::DrawInit { Frame VP }
#   VectorBox::Move     { Frame VP }
#   VectorBox::MoveDone { Frame VP }
#   VectorBox::MoveInit { Frame VP }
#
# Remarques :
#
#===============================================================================

package provide VectorBox 1.0

catch { SPI::Splash "Loading Widget Package VectorBox 1.0" }

namespace eval VectorBox {
   variable Lbl
   variable Bubble
   variable Data

   set Data(Field)    ""
   set Data(RealTime) [expr $OpenGL::Param(Res)<=1]
   set Data(X0)       0
   set Data(Y0)       0
   set Data(LockX)    False
   set Data(LockY)    False
   set Data(LockZ)    False

   set Lbl(Geo)     { "Geospatial" "Geospatial" }
   set Lbl(Params)  { "Parametres" "Parameters" }
   set Lbl(Size)    { "Dimension" "Dimension" }
   set Lbl(Sample)  { "Echantillonage" "Sampling" }
   set Lbl(Step)    { "Pas" "Step" }
   set Lbl(Start)   { "Départ" "Start" }
   set Lbl(Close)   { "Fermer" "Close" }
   set Lbl(Apply)   { "Appliquer" "Apply" }
   set Lbl(Stream)  { "Ligne de courant" "Streamline" }

   set Bubble(Size)   { "Largeur des lignes de courants" "Streamline width" }
   set Bubble(Step)   { "Pas de temps du deplacment des lignes de courants" "Streamline displacement step" }
   set Bubble(Sample) { "Espacement des lignes de courants\npixel(2D) / grille(3D)" "Streamline sampling\npixel(2D) / gridpt(3D)" }
   set Bubble(Geo)    { "Orientation des vecteurs\ngéographique (N-S,E,W) ou grille (X-Y)" "Vector orientation\ngeographic (N-S,E,W) ou grid (X-Y)" }
   set Bubble(Cube)   { "Point de départ du plan des lignes de courants" "Streamline plane starting point" }
   set Bubble(Select) { "Sélection interactive du cube de départ\nBoutton Gauche: Sélection du cube\nBoutton Centre: Déplacement du cube" "Interactive starting cube selection\nLeft button  : Select cube\nMiddle button: Move cube" }
   set Bubble(Real)   { "Applique les paramêtres interactivement" "Apply parameters interactively" }
   set Bubble(Apply)  { "Appliquer les paramêtres" "Apply the parameters" }
   set Bubble(Close)  { "Fermer sans appliquer les paramêtres"  "Close without applying the parameters" }
}

#----------------------------------------------------------------------------
# Nom      : <VectorBox::Create>
# Creation : Decemnbre 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Interface des parametres d'affichagfe vectoriels
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

proc VectorBox::Create { Parent Apply } {
   global GDefs
   variable Lbl
   variable Bubble
   variable Data

   if { [winfo exist .vecbox] } {
      raise .vecbox
      return
   }

   toplevel     .vecbox
   wm geom      .vecbox =235x215+[winfo rootx $Parent]+[expr [winfo rooty $Parent]+[winfo height $Parent]]
   wm transient .vecbox .
   wm resizable .vecbox 0 0
   wm title     .vecbox "VectorBox 1.0"

   TabFrame::Create .vecbox.tab 1 ""
   pack .vecbox.tab -side top -fill both -expand true -padx 5 -pady 5

   set fr [TabFrame::Add .vecbox.tab 1 [lindex $Lbl(Stream) $GDefs(Lang)] False ""]

      labelframe $fr.step -text [lindex $Lbl(Step) $GDefs(Lang)]
         scale $fr.step.sc -from 0.001 -to 1.0 -resolution 0.01 -width 14 -sliderlength 8 -variable FSTD::Param(Step) -length 150 -relief flat -bd 1 -orient horizontal \
            -command "if { \$VectorBox::Data(RealTime) } { $Apply }; catch "
         pack $fr.step.sc -fill x -padx 2 -expand true
#      pack $fr.step -side top -padx 5 -pady 5 -fill x
      labelframe $fr.plane -text [lindex $Lbl(Start) $GDefs(Lang)]
          checkbutton $fr.plane.mode -variable Page::Data(ToolMode) -onvalue VectorBox -offvalue SPI \
            -image ARROW -indicatoron 0 -relief sunken -bd 1 -overrelief raised -offrelief flat -selectcolor $GDefs(ColorFrame) \
            -command { SPI::ToolMode $Page::Data(ToolMode) Data True }
         pack $fr.plane.mode -side right -anchor n -fill x -expand True -padx 2  -ipady 8
         frame $fr.plane.x
            label $fr.plane.x.lbl -text "X " -width 2 -anchor w
            spinbox $fr.plane.x.x0 -textvariable FSTD::Param(X0) -width 4 -from 0 -to 1000 -wrap 1 -bd 1 -command "VectorBox::Lock X 0 1; if { \$VectorBox::Data(RealTime) } { $Apply }" -bg $GDefs(ColorLight)
            spinbox $fr.plane.x.x1 -textvariable FSTD::Param(X1) -width 4 -from 0 -to 1000 -wrap 1 -bd 1 -command "VectorBox::Lock X 1 0; if { \$VectorBox::Data(RealTime) } { $Apply }" -bg $GDefs(ColorLight)
            checkbutton $fr.plane.x.lock -variable VectorBox::Data(LockX) -onvalue True -offvalue False -image VCRLOCK -indicatoron 0 -relief sunken -bd 1 -overrelief raised -offrelief flat -selectcolor $GDefs(ColorFrame) \
               -command { VectorBox::Lock X 0 1 }
            pack  $fr.plane.x.lbl $fr.plane.x.x0 $fr.plane.x.x1 $fr.plane.x.lock -side left -fill y
         pack  $fr.plane.x -side top -fill x -expand True
         frame $fr.plane.y
            label $fr.plane.y.lbl -text "Y " -width 2 -anchor w
            spinbox $fr.plane.y.y0 -textvariable FSTD::Param(Y0) -width 4 -from 0 -to 1000 -wrap 1 -bd 1 -command "VectorBox::Lock Y 0 1; if { \$VectorBox::Data(RealTime) } { $Apply }" -bg $GDefs(ColorLight)
            spinbox $fr.plane.y.y1 -textvariable FSTD::Param(Y1) -width 4 -from 0 -to 1000 -wrap 1 -bd 1 -command "VectorBox::Lock Y 1 0; if { \$VectorBox::Data(RealTime) } { $Apply }" -bg $GDefs(ColorLight)
            checkbutton $fr.plane.y.lock -variable VectorBox::Data(LockY) -onvalue True -offvalue False -image VCRLOCK -indicatoron 0 -relief sunken -bd 1 -overrelief raised -offrelief flat -selectcolor $GDefs(ColorFrame) \
               -command { VectorBox::Lock Y 0 1 }
            pack  $fr.plane.y.lbl $fr.plane.y.y0 $fr.plane.y.y1 $fr.plane.y.lock -side left -fill y
         pack  $fr.plane.y -side top -fill x -expand True
         frame $fr.plane.z
            label $fr.plane.z.lbl -text "Z " -width 2 -anchor w
            spinbox $fr.plane.z.z0 -textvariable FSTD::Param(Z0) -width 4 -from 0 -to 1000 -wrap 1 -bd 1 -command "VectorBox::Lock Z 0 1; if { \$VectorBox::Data(RealTime) } { $Apply }" -bg $GDefs(ColorLight)
            spinbox $fr.plane.z.z1 -textvariable FSTD::Param(Z1) -width 4 -from 0 -to 1000 -wrap 1 -bd 1 -command "VectorBox::Lock Z 1 0; if { \$VectorBox::Data(RealTime) } { $Apply }" -bg $GDefs(ColorLight)
            checkbutton $fr.plane.z.lock -variable VectorBox::Data(LockZ) -onvalue True -offvalue False -image VCRLOCK -indicatoron 0 -relief sunken -bd 1 -overrelief raised -offrelief flat -selectcolor $GDefs(ColorFrame) \
               -command { VectorBox::Lock Z 0 1 }
            pack  $fr.plane.z.lbl $fr.plane.z.z0 $fr.plane.z.z1 $fr.plane.z.lock -side left -fill y
         pack  $fr.plane.z -side top -fill x -expand True
      pack $fr.plane -side top -padx 5 -pady 5 -fill x

   Bubble::Create $fr.plane      $Bubble(Cube)
   Bubble::Create $fr.plane.mode $Bubble(Select)

   set fr [TabFrame::Add .vecbox.tab 1 [lindex $Lbl(Params) $GDefs(Lang)] False ""]

      labelframe $fr.size -text [lindex $Lbl(Size) $GDefs(Lang)]
         scale $fr.size.sc -from 1 -to 30 -resolution 1 -width 14 -sliderlength 8 -variable FSTD::Param(Size) -length 150 -relief flat -bd 1 -orient horizontal \
            -command "if { \$VectorBox::Data(RealTime) } { $Apply }; catch "
         pack $fr.size.sc -fill x -padx 2 -expand true
      labelframe $fr.sample -text [lindex $Lbl(Sample) $GDefs(Lang)]
         scale $fr.sample.sc -from 1 -to 10 -resolution 1 -width 14 -sliderlength 8 -variable FSTD::Param(Sample) -length 150 -relief flat -bd 1 -orient horizontal \
            -command "if { \$VectorBox::Data(RealTime) } { $Apply }; catch "
         pack $fr.sample.sc -fill x -padx 2 -expand true
         checkbutton $fr.geo -text [lindex $Lbl(Geo) $GDefs(Lang)] -variable FSTD::Param(Geo) -onvalue 1 -offvalue 0 -relief raised -bd 1 -indicatoron false
      pack $fr.size $fr.sample $fr.geo -side top -padx 5 -pady 5 -fill x

   Bubble::Create $fr.size.sc   $Bubble(Size)
   Bubble::Create $fr.sample.sc $Bubble(Sample)
   Bubble::Create $fr.geo       $Bubble(Geo)

   #----- Commandes

   frame .vecbox.cmd
      checkbutton .vecbox.cmd.real -image DOCSEL -bd 1 -relief raised \
         -variable VectorBox::Data(RealTime) -onvalue 1 -offvalue 0 -indicatoron false
      button .vecbox.cmd.close -text [lindex $Lbl(Close) $GDefs(Lang)] -bd 1 -relief raised -command "destroy .vecbox"
      button .vecbox.cmd.apply -text [lindex $Lbl(Apply) $GDefs(Lang)] -bd 1 -relief raised -command "$Apply"
      pack .vecbox.cmd.real -side left -fill y
      pack .vecbox.cmd.apply .vecbox.cmd.close -side left -fill x -expand true
   pack .vecbox.cmd -side bottom -fill x -padx 5 -pady 5

   Bubble::Create .vecbox.cmd.real  $Bubble(Real)
   Bubble::Create .vecbox.cmd.apply $Bubble(Apply)
   Bubble::Create .vecbox.cmd.close $Bubble(Close)
}

#----------------------------------------------------------------------------
# Nom      : <VectorBox::Lock>
# Creation : Decembre 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Lier les deux coordonnees debut-fin du cube de selection
#
# Parametres :
#   <Axis>   : Axe a lier
#   <From>   : Veleur reference
#   <To>     : Valeur destination
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc VectorBox::Lock { Axis From To } {
   variable Data

   if { $Data(Lock$Axis) } { set FSTD::Param(${Axis}${To}) $FSTD::Param(${Axis}${From}) }
}

#-------------------------------------------------------------------------------
# Nom      : <VectorBox::Draw...>
# Creation : Decembre 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Fonctions de manipulations de la selection sur la projection.
#
# Parametres :
#   <Frame   : Identificateur de Page
#   <VP>     : Identificateur du Viewport
#
# Remarques :
#    - Ces fonctions sont appele par le package Page au besoin.
#
#-------------------------------------------------------------------------------

proc VectorBox::Draw     { Frame VP } {
   variable Data

   if { $Data(Field)!="" } {
      set xy [fstdfield stats $Data(Field) -coordpoint $Viewport::Map(LatCursor) $Viewport::Map(LonCursor)]
      set FSTD::Param(X1) [expr round([lindex $xy 0])]
      set FSTD::Param(Y1) [expr round([lindex $xy 1])]

      VectorBox::Lock X 0 1
      VectorBox::Lock Y 0 1

      if  { $Data(RealTime) } {
         VectorBox::DrawDone $Frame $VP
      }
   }
}

proc VectorBox::DrawDone { Frame VP } {

   FSTD::ParamSet
   Page::Update $Page::Data(Frame)
   Page::UpdateCommand $Page::Data(Frame)
}

proc VectorBox::DrawInit { Frame VP } {
   variable Data

   #----- Find the first vectorial field
   set Data(Field) ""
   foreach fld [Viewport::Assigned $Page::Data(Frame) $Viewport::Data(VP) fstdfield] {
      if { [fstdfield stats $fld -component]>1 } {
         set Data(Field) $fld
         break
      }
   }

   if { $Data(Field)!="" } {
      set xy [fstdfield stats $Data(Field) -coordpoint $Viewport::Map(LatCursor) $Viewport::Map(LonCursor)]
      set FSTD::Param(X0) [expr round([lindex $xy 0])]
      set FSTD::Param(Y0) [expr round([lindex $xy 1])]
   }
}

proc VectorBox::Move { Frame VP } {
   variable Data

   if { $Data(Field)!="" } {
      set xy [fstdfield stats $Data(Field) -coordpoint $Viewport::Map(LatCursor) $Viewport::Map(LonCursor)]
      set Data(X1) [expr round([lindex $xy 0])]
      set Data(Y1) [expr round([lindex $xy 1])]
      set Data(DX) [expr $Data(X1)-$Data(X0)]
      set Data(DY) [expr $Data(Y1)-$Data(Y0)]

      incr FSTD::Param(X0) $Data(DX)
      incr FSTD::Param(Y0) $Data(DY)
      incr FSTD::Param(X1) $Data(DX)
      incr FSTD::Param(Y1) $Data(DY)

      set Data(X0) $Data(X1)
      set Data(Y0) $Data(Y1)

      VectorBox::Lock X 0 1
      VectorBox::Lock Y 0 1

      if  { $Data(RealTime) } {
         VectorBox::DrawDone $Frame $VP
      }
   }
}

proc VectorBox::MoveDone { Frame VP } {
   VectorBox::DrawDone $Frame $VP
}

proc VectorBox::MoveInit { Frame VP } {
   variable Data

   #----- Find the first vectorial field
   set Data(Field) ""
   foreach fld [Viewport::Assigned $Page::Data(Frame) $Viewport::Data(VP) fstdfield] {
      if { [fstdfield stats $fld -component]>1 } {
         set Data(Field) $fld
         break
      }
   }

   if { $Data(Field)!="" } {
      set xy [fstdfield stats $Data(Field) -coordpoint $Viewport::Map(LatCursor) $Viewport::Map(LonCursor)]
      set Data(X0) [expr round([lindex $xy 0])]
      set Data(Y0) [expr round([lindex $xy 1])]
   }
}
