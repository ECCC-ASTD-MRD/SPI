#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie d'objects visuel interactifs
# Fichier  : Miniport.tcl
# Creation : Mai 2000 - J.P. Gauthier - CMC/CMOE
#
# Description: Ce package s'occupe de l'affichage et de la manipulation
#              de miniport (Loupe/Encart) geographiques
#
# Fonctions:
#
#    Miniport::Create     { Frame X0 Y0 Width Height Active Z { Lat -999 } { Lon -999 } }
#    Miniport::Coverage   { Frame }
#    Miniport::Lens       { Frame }
#    Miniport::Lock       { Frame }
#    Miniport::UpdateData { Frame VP }
#    Miniport::Destroy    { Frame }
#    Miniport::Write      { Frame File }
#
#===============================================================================

package provide Miniport 2.2

catch { SPI::Splash "Loading Canvas Package Miniport 2.2" }

namespace eval Miniport { } {
   variable Params
   variable Lbl

   set Params(Lens)    { 1 2 4 8 16 32 64 128 256 512 1024 2048 4096 8192 16384 32768 65536 }

   set Lbl(Zoom)       { "Zoom ..." "Zoom ..." }
   set Lbl(Location)   { "Afficher la localisation" "Display location" }
   set Lbl(Data)       { "Afficher les données" "Display data" }
   set Lbl(Relative)   { "Relatif" "Relative" }
   set Lbl(Cursor)     { "Suivre le curseur" "Follow mouse" }
   set Lbl(Color)      { "Couleur de la région visible" "Color of view area" }
   set Lbl(Ortho)      { "Projection Orthographique" "Orthographic projection" }
   set Lbl(Cylin)      { "Projection Cylindrique" "Cylindric projection" }
   set Lbl(Same)       { "Projection maitre" "Master projection" }
}

#----------------------------------------------------------------------------
# Nom      : <Miniport::Create>
# Creation : Octobre 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer un viewport encart
#
# Parametres :
#   <Frame>  : Indentificateur de Page
#   <X0>     : Coordonee X du coin superieur gauche
#   <Y0>     : Coordonee Y du coin superieur gauche
#   <Width>  : Largeur du Viewport
#   <Height> : Hauteur du Viewport
#   <Active> : Fonction active (Deplacement,Agrandisement)
#   <Z>      : Facteur de zoom out
#   <Lat>    : Latitude focus (Optionel)
#   <Lon>    : Longitude focus (Optionel)
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Miniport::Create { Frame X0 Y0 Width Height Active Z { Lat -999 } { Lon -999 } } {
   global   GDefs
   variable Data
   variable Params
   variable Lbl

   set Data(Mini$Frame)                   0            ;#Miniport
   set Data(VP$Frame)                     ""           ;#Viewport
   set Viewport::Data(XMINI$Frame)        $X0          ;#Offset en x
   set Viewport::Data(YMINI$Frame)        $Y0          ;#Offset en y
   set Viewport::Data(ZMINI$Frame)        $Z           ;#Zoom
   set Viewport::Data(LMINI$Frame)        [expr log10($Z)/log10(2)] ;#log2(Zoom)
   set Viewport::Data(WidthMINI$Frame)    $Width       ;#Largeur de la projection
   set Viewport::Data(HeightMINI$Frame)   $Height      ;#Hauteur de la projection
   set Viewport::Data(ActiveMINI$Frame)   $Active      ;#Mode Active (Manipulation in place)
   set Viewport::Data(DataMINI$Frame)     True         ;#Donnees associees
   set Viewport::Data(RelativeMINI$Frame) False        ;#Zoom relatif
   set Viewport::Data(CursorMINI$Frame)   True         ;#Suivre le curseur en mode zoom in (Magnifier)
   set Viewport::Data(ColorMINI$Frame)    black        ;#Couleur du rectangle de position
   set Viewport::Data(LocationMINI$Frame) True         ;#Afficher le rectangle de position
   set Viewport::Data(TypeMINI$Frame)     same         ;#Projection

   $Frame.page.canvas configure -cursor watch
   update idletasks

   set ctag $Page::Data(Tag)MINI$Frame
   set wtag $Page::Data(Tag)MINI

   #----- Initialiser les variables du viewport
   set x0 $Viewport::Data(XMINI$Frame)
   set y0 $Viewport::Data(YMINI$Frame)
   set x1 [expr $Viewport::Data(WidthMINI$Frame)+$x0]
   set y1 [expr $Viewport::Data(HeightMINI$Frame)+$y0]

   #----- Creer le viewport et son pourtour
   if { ![projection is  MINI$Frame] } {
      projcam create MINI$Frame
      projection create MINI$Frame
      projection configure MINI$Frame -type orthographic
   }
   $Frame.page.canvas create viewport -x $x0 -y $y0 -width $Viewport::Data(WidthMINI$Frame) -height $Viewport::Data(HeightMINI$Frame) \
      -anchor nw -tags "MINI$Frame $ctag VPINTRUDE" -projection MINI$Frame -camera MINI$Frame -command MINI$Frame -secondary True

   Viewport::ConfigSet $Frame

   $Frame.page.canvas create line $x0 $y0 $x0 $y0 -fill black -width 2 -tags "MINIAREA LOCK$ctag"

   if { [info exists Viewport::Data(Data$Frame)] } {
      set Data(VP$Frame) [lindex $Viewport::Data(Data$Frame) 0]
   }

   #----- Centrer sur les coordonnees specifies
   if { $Lat!=-999 && $Lon!=-999 } {
      set Viewport::Data(CursorMINI$Frame)   False
      projection configure MINI$Frame -location $Lat $Lon
   }

   #----- Creer les fonction du miniport
   if { $Active } {
      scale $Frame.sc$wtag -bg white -relief raised -bd 1 -width 8 -sliderlength 15  -orient horizontal -showvalue False -resolution 0.01 \
         -from [expr log10([lindex $Params(Lens) 0])/log10(2)] -to [expr log10([lindex $Params(Lens) end])/log10(2)] \
         -variable Viewport::Data(LMINI$Frame) -command "set Viewport::Data(ZMINI$Frame) \[expr pow(2,\$Viewport::Data(LMINI$Frame))\];Page::Update $Frame; catch"
      label $Frame.bs$wtag -bg $GDefs(ColorFrame) -bitmap @$GDefs(Dir)/Resources/Bitmap/cvscale.xbm -cursor sizing -bd 1 -relief raised
      label $Frame.bm$wtag -bg $GDefs(ColorFrame) -bitmap @$GDefs(Dir)/Resources/Bitmap/cvmove.xbm -cursor fleur -bd 1 -relief raised
      menubutton $Frame.bf$wtag -bg $GDefs(ColorFrame) -bitmap @$GDefs(Dir)/Resources/Bitmap/cvmenu.xbm -cursor hand1 -bd 1 -relief raised \
         -menu $Frame.bf$wtag.menu

      menu $Frame.bf$wtag.menu -bg $GDefs(ColorFrame)
      $Frame.bf$wtag.menu add cascade -label [lindex $Lbl(Zoom) $GDefs(Lang)] -menu $Frame.bf$wtag.menu.lens
      $Frame.bf$wtag.menu add checkbutton -label [lindex $Lbl(Relative) $GDefs(Lang)] -variable Viewport::Data(RelativeMINI$Frame) \
         -onvalue True -offvalue False -command "Page::Update $Frame"
      $Frame.bf$wtag.menu add separator
      $Frame.bf$wtag.menu add radiobutton -label [lindex $Lbl(Same) $GDefs(Lang)] -value same -variable Viewport::Data(TypeMINI$Frame) \
         -command "Miniport::Projection $Frame"
      $Frame.bf$wtag.menu add radiobutton -label [lindex $Lbl(Ortho) $GDefs(Lang)] -value orthographic -variable Viewport::Data(TypeMINI$Frame) \
         -command "Miniport::Projection $Frame"
      $Frame.bf$wtag.menu add radiobutton -label [lindex $Lbl(Cylin) $GDefs(Lang)] -value cylindric -variable Viewport::Data(TypeMINI$Frame) \
         -command "Miniport::Projection $Frame"
      $Frame.bf$wtag.menu add separator
      $Frame.bf$wtag.menu add checkbutton -label [lindex $Lbl(Data) $GDefs(Lang)] -variable Viewport::Data(DataMINI$Frame) \
         -onvalue True -offvalue False -command "Viewport::UpdateData $Frame"
      $Frame.bf$wtag.menu add checkbutton -label [lindex $Lbl(Location) $GDefs(Lang)] -variable Viewport::Data(LocationMINI$Frame) \
         -onvalue True -offvalue False -command "Miniport::Coverage $Frame"
      $Frame.bf$wtag.menu add command -label [lindex $Lbl(Color) $GDefs(Lang)] -command "set Viewport::Data(ColorMINI$Frame) \[ColorBox::Create . \$Viewport::Data(ColorMINI$Frame)\]"
      $Frame.bf$wtag.menu add separator
      $Frame.bf$wtag.menu add checkbutton -label [lindex $Lbl(Cursor) $GDefs(Lang)] -variable Viewport::Data(CursorMINI$Frame) \
         -onvalue True -offvalue False

      menu $Frame.bf$wtag.menu.lens -bg $GDefs(ColorFrame)
      foreach size $Params(Lens) {
         $Frame.bf$wtag.menu.lens add radiobutton -label "${size}x" -variable Viewport::Data(ZMINI$Frame) -value [expr double(${size}.0)] \
            -command "set Viewport::Data(LMINI$Frame) \[expr log10(\$Viewport::Data(ZMINI$Frame))/log10(2)\];Page::Update $Frame"
      }

      $Frame.page.canvas create window [expr $x1-150-35] [expr $y1-1] -window $Frame.sc$wtag -anchor sw -tags "SC$ctag NOPRINT" -width 151
      $Frame.page.canvas create window $x1 [expr $y1-1]               -window $Frame.bs$wtag -anchor se -tags "BS$ctag NOPRINT"
      $Frame.page.canvas create window [expr $x1-11] [expr $y1-1]     -window $Frame.bm$wtag -anchor se -tags "BM$ctag NOPRINT"
      $Frame.page.canvas create window [expr $x1-22] [expr $y1-1]     -window $Frame.bf$wtag -anchor se -tags "BF$ctag NOPRINT"

      #----- bindings de deplacement

      bind $Frame.bm$wtag <ButtonPress-1>      "Page::SnapRef $Frame %X %Y"
      bind $Frame.bm$wtag <B1-Motion>          "Page::ActiveMove Viewport $Frame MINI$Frame %X %Y;Miniport::Coverage $Frame"

      #----- bindings de scaling

      bind $Frame.bs$wtag <ButtonPress-1>      "Page::SnapRef $Frame %X %Y"
      bind $Frame.bs$wtag <B1-Motion>          "Page::ActiveScale Viewport $Frame MINI$Frame %X %Y 1;Miniport::Coverage $Frame"
      bind $Frame.bs$wtag <ButtonRelease-1>    "Page::ActiveScale Viewport $Frame MINI$Frame %X %Y 0;Miniport::Coverage $Frame"
   }
   $Frame.page.canvas bind MINI$Frame <Button-3> "tk_popup .mapmenu %X %Y 0"

   Page::MaskItem $Frame
   Page::WidgetBind $Frame $ctag
   Page::ModeZoom $Frame MINI$Frame
   Page::Update   $Frame

   update idletasks
   $Frame.page.canvas configure -cursor left_ptr
}

#----------------------------------------------------------------------------
# Nom      : <Miniport::Projection>
# Creation : Octobre 2011 - J.P. Gauthier - CMC/CMOE
#
# But      : Ajuster les parametres relatifs a la projection
#
# Parametres :
#   <Frame>  : Indentificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Miniport::Projection { Frame } {

   set wtag $Page::Data(Tag)MINI

   if { $Viewport::Data(TypeMINI$Frame)=="same" } {
      projection configure MINI$Frame -type [projection configure $Frame -type] -georef [projection configure $Frame -georef]
      catch { $Frame.bf$wtag.menu entryconfigure 7 -state normal }
  } else {
      #----- We can only show the data when the projection is the same
      set type [projection configure $Frame -type]
      if { $type!="$Viewport::Data(TypeMINI$Frame)" } {
         set Viewport::Data(DataMINI$Frame) False
         $Frame.page.canvas itemconf MINI$Frame -data {}
         catch { $Frame.bf$wtag.menu entryconfigure 7 -state disabled }
      }

      projection configure MINI$Frame -type $Viewport::Data(TypeMINI$Frame) -data {} -georef ""
   }
}

#----------------------------------------------------------------------------
# Nom      : <Miniport::Coverage>
# Creation : Octobre 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Calcul de la region de l'encart de projection
#
# Parametres :
#   <Frame>  : Indentificateur de Page
#   <VP>     : Viewport
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Miniport::Coverage { Frame { VP "" } } {

   if { $Viewport::Data(LocationMINI$Frame) } {

      if { $VP=="" } {
         set VP $Miniport::Data(VP$Frame)
      }

      $Frame.page.canvas itemconfigure MINIAREA -fill $Viewport::Data(ColorMINI$Frame)

      if { [projcam configure MINI$Frame -lens]<[projcam configure $Frame -lens] } {

         #----- Coverage dans le miniport

         set x0  [expr $Viewport::Data(X$VP)+1]
         set y0  [expr $Viewport::Data(Y$VP)+1]
         set x1  [expr $x0+$Viewport::Data(Width$VP)-1]
         set y1  [expr $y0+$Viewport::Data(Height$VP)-1]

         set ll0 [$VP -unproject $x0 $y0]
         set ll1 [$VP -unproject $x1 $y0]
         set ll2 [$VP -unproject $x1 $y1]
         set ll3 [$VP -unproject $x0 $y1]

         set xy  [MINI$Frame -projectline TRUE [list [lindex $ll0 0] [lindex $ll0 1] 0.0 [lindex $ll1 0] [lindex $ll1 1] 0.0 \
            [lindex $ll2 0] [lindex $ll2 1] 0.0 [lindex $ll3 0] [lindex $ll3 1] 0.0 [lindex $ll0 0] [lindex $ll0 1] 0.0]]
         catch { $Frame.page.canvas coords MINIAREA [lindex $xy 0] }
         $Frame.page.canvas raise MINIAREA MINI$Frame
      } else {

         #----- Coverage dans le viewport

         set x0  [expr $Viewport::Data(XMINI$Frame)+1]
         set y0  [expr $Viewport::Data(YMINI$Frame)+1]
         set x1  [expr $x0+$Viewport::Data(WidthMINI$Frame)-1]
         set y1  [expr $y0+$Viewport::Data(HeightMINI$Frame)-1]

         set ll0 [MINI$Frame -unproject $x0 $y0]
         set ll1 [MINI$Frame -unproject $x1 $y0]
         set ll2 [MINI$Frame -unproject $x1 $y1]
         set ll3 [MINI$Frame -unproject $x0 $y1]

         set xy  [$VP -projectline TRUE [list [lindex $ll0 0] [lindex $ll0 1] 0.0 [lindex $ll1 0] [lindex $ll1 1] 0.0 \
            [lindex $ll2 0] [lindex $ll2 1] 0.0 [lindex $ll3 0] [lindex $ll3 1] 0.0 [lindex $ll0 0] [lindex $ll0 1] 0.0]]
         catch { $Frame.page.canvas coords MINIAREA [lindex $xy 0] }
         $Frame.page.canvas lower MINIAREA MINI$Frame
      }
   } else {
      $Frame.page.canvas coords MINIAREA $Viewport::Data(XMINI$Frame) $Viewport::Data(YMINI$Frame) \
         $Viewport::Data(XMINI$Frame) $Viewport::Data(YMINI$Frame)
   }
}

#----------------------------------------------------------------------------
# Nom      : <Miniport::Lens>
# Creation : Fevrier 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Ajuster la lentille (Zoom) du miniport
#
# Parametres :
#   <Frame>  : Indentificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Miniport::Lens { Frame } {

   if { $Viewport::Data(RelativeMINI$Frame) } {
      set lens [expr ([projcam configure $Frame -lens]*$Viewport::Data(ZMINI$Frame))]
   } else {
      set lens $Viewport::Data(ZMINI$Frame)
   }

   projcam configure MINI$Frame -lens $lens

   if { $lens<[projcam configure $Frame -lens] || $Viewport::Data(CursorMINI$Frame) } {
      eval projection configure MINI$Frame -location [projection configure $Frame -location]
   }
   if { [projcam configure MINI$Frame -lens]>[projcam configure $Frame -lens] && $Viewport::Data(CursorMINI$Frame) } {
      projection configure MINI$Frame -location $Viewport::Map(LatCursor) $Viewport::Map(LonCursor)
   }
}

#----------------------------------------------------------------------------
# Nom      : <Miniport::Lock>
# Creation : Fevrier 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Fixer la position de la loupe
#
# Parametres :
#   <Frame>  : Indentificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Miniport::Lock { Frame } {

   set Viewport::Data(CursorMINI$Frame) False
   projection configure MINI$Frame -location $Viewport::Map(LatCursor) $Viewport::Map(LonCursor)
   $Frame.page.canvas itemconf MINI$Frame -update True
   update idletasks
}

#----------------------------------------------------------------------------
# Nom      : <Miniport::UpdateData>
# Creation : Novembre 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectue la mise a jours des donnees.
#
# Parametres   :
#  <Frame>     : Identificateur de Page
#  <VP>        : Viewport
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Miniport::UpdateData { Frame { VP "" } } {
   variable Data

   if { [info exists Miniport::Data(Mini$Frame)] } {
      if { $Viewport::Data(DataMINI$Frame) } {
         if { $VP=="" } {
            set VP $Data(VP$Frame)
         } else {
            set Data(VP$Frame) $VP
         }
         projection configure MINI$Frame -data [projection configure $Frame -data]
         if { $VP!="" } {
            $Frame.page.canvas itemconf MINI$Frame -data [lindex [$Frame.page.canvas itemconf $VP -data] end]
         }
      } else {
         $Frame.page.canvas itemconf MINI$Frame -data {}
      }
   }
   Miniport::Projection $Frame
}

#----------------------------------------------------------------------------
# Nom      : <Miniport::Destroy>
# Creation : Octobre 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer un viewport encart
#
# Parametres :
#   <Frame>  : Indentificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Miniport::Destroy { Frame } {
   variable Data


   if { [info exists Miniport::Data(Mini$Frame)] } {

      $Frame.page.canvas configure -cursor watch
      update idletasks

      set ctag $Page::Data(Tag)MINI$Frame
      set wtag $Page::Data(Tag)MINI

      $Frame.page.canvas delete $ctag BS$ctag BM$ctag BF$ctag BD$ctag MINIAREA
      destroy $Frame.sc$wtag $Frame.bs$wtag $Frame.bm$wtag $Frame.bf$wtag $Frame.bf$wtag.menu
      projcam destroy MINI$Frame
      projection destroy MINI$Frame

      unset Data(Mini$Frame)
      unset Data(VP$Frame)
      unset Viewport::Data(XMINI$Frame)
      unset Viewport::Data(YMINI$Frame)
      unset Viewport::Data(ZMINI$Frame)
      unset Viewport::Data(LMINI$Frame)
      unset Viewport::Data(WidthMINI$Frame)
      unset Viewport::Data(HeightMINI$Frame)
      unset Viewport::Data(ActiveMINI$Frame)
      unset Viewport::Data(DataMINI$Frame)
      unset Viewport::Data(RelativeMINI$Frame)
      unset Viewport::Data(CursorMINI$Frame)
      unset Viewport::Data(LocationMINI$Frame)

      update idletasks
      $Frame.page.canvas configure -cursor left_ptr
   }
}

#------------------------------------------------------------------------------
# Nom      : <Miniport::Write>
# Creation : Novembre 2003 - J.P. Gauthier - CMC/CMOE -
#
# But     : Engeristrer les parametres du Miniport dans un fichier Layout
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <File>   : Identificateur de Fichier
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Miniport::Write { Frame File } {

   puts $File "   #----- Affichage de l'encart"
   puts $File ""

   if { !$Viewport::Data(CursorMINI$Frame) } {
      set coords [projection configure MINI$Frame -location]
   } else {
      set coords { -999 -999 }
   }
   puts $File "   Miniport::Create \$Frame $Viewport::Data(XMINI$Frame) $Viewport::Data(YMINI$Frame) $Viewport::Data(WidthMINI$Frame)\
                        $Viewport::Data(HeightMINI$Frame) $Viewport::Data(ActiveMINI$Frame) $Viewport::Data(ZMINI$Frame) [lindex $coords 0] [lindex $coords 1]"
}
