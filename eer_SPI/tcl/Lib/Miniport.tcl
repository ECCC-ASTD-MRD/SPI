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

package provide Miniport 2.3

catch { SPI::Splash "Loading Canvas Package Miniport 2.3" }

namespace eval Miniport { } {
   variable Params
   variable Lbl

   set Params(Lens)    { 1 2 4 8 16 32 64 128 256 512 1024 2048 4096 8192 16384 32768 65536 }
   set Params(Width)   250
   set Params(Height)  250
   set Params(Zoom)    32
   set Params(MiniNo)  0

   set Params(Current) ""

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

proc Miniport::Create { Frame { X0 0 } { Y0 0 } { Width 0 } { Height 0 } { Active 1 } { Z 0 } { Lat -999 } { Lon -999 } } {
   global   GDefs
   variable Data
   variable Params
   variable Lbl

   #----- Set default parameters based on last miniport
   if { $Width==0 } {
      set mini [lindex $Data(Mini$Frame) end]
      if { [projection is $mini] } {
         set Width  $Viewport::Data(Width$mini)
         set Height $Viewport::Data(Height$mini)
         set Z      $Viewport::Data(Z$mini)
         set X0     [expr $Viewport::Data(X$mini)+10]
         set Y0     [expr $Viewport::Data(Y$mini)+10]
         set Active $Viewport::Data(Active$mini)
      } else {
         set Width  $Params(Width)
         set Height $Params(Height)
         set Z      $Params(Zoom)
         set X0 10
         set Y0 10
         set Active 1
      }
   }

   set mini MINI$Frame[incr Params(MiniNo)]

   lappend Data(Mini$Frame)          $mini        ;#Miniport
   set Data(VP$Frame)                ""           ;#Viewport

   set Viewport::Data(X$mini)        $X0          ;#Offset en x
   set Viewport::Data(Y$mini)        $Y0          ;#Offset en y
   set Viewport::Data(Z$mini)        $Z           ;#Zoom
   set Viewport::Data(L$mini)        [expr log10($Z)/log10(2)] ;#log2(Zoom)
   set Viewport::Data(Width$mini)    $Width       ;#Largeur de la projection
   set Viewport::Data(Height$mini)   $Height      ;#Hauteur de la projection
   set Viewport::Data(Active$mini)   $Active      ;#Mode Active (Manipulation in place)
   set Viewport::Data(Data$mini)     True         ;#Donnees associees
   set Viewport::Data(Relative$mini) False        ;#Zoom relatif
   set Viewport::Data(Cursor$mini)   True         ;#Suivre le curseur en mode zoom in (Magnifier)
   set Viewport::Data(Color$mini)    black        ;#Couleur du rectangle de position
   set Viewport::Data(Location$mini) True         ;#Afficher le rectangle de position
   set Viewport::Data(Type$mini)     same         ;#Projection

   $Frame.page.canvas configure -cursor watch
   update idletasks

   set tag  PAGE[string map { . "" } $mini]
   set ctag PAGE$mini

   #----- Initialiser les variables du viewport
   set x0 $Viewport::Data(X$mini)
   set y0 $Viewport::Data(Y$mini)
   set x1 [expr $Viewport::Data(Width$mini)+$x0]
   set y1 [expr $Viewport::Data(Height$mini)+$y0]

   #----- Creer le viewport et son pourtour
   if { ![projection is $mini] } {
      projcam create $mini
      projection create $mini
      projection configure $mini -type orthographic
   }
   $Frame.page.canvas create viewport -x $x0 -y $y0 -width $Viewport::Data(Width$mini) -height $Viewport::Data(Height$mini) \
      -tags "$mini $ctag VPINTRUDE" -projection $mini -camera $mini -command $mini -secondary True
   $Frame.page.canvas bind $mini <ButtonPress-1>  "+Miniport::Select $Frame $mini"

   Viewport::ConfigSet $Frame

   $Frame.page.canvas create line $x0 $y0 $x0 $y0 -fill black -width 2 -tags "AREA$mini"

   if { [info exists Viewport::Data(Data$Frame)] } {
      set Data(VP$Frame) [lindex $Viewport::Data(Data$Frame) 0]
   }

   #----- Centrer sur les coordonnees specifies
   if { $Lat!=-999 && $Lon!=-999 } {
      set Viewport::Data(Cursor$mini)   False
      projection configure $mini -location $Lat $Lon
   }

   #----- Creer les fonction du miniport
   if { $Active } {
      scale $Frame.sc$tag -bg white -relief raised -bd 1 -width 8 -sliderlength 15  -orient horizontal -showvalue False -resolution 0.01 \
         -from [expr log10([lindex $Params(Lens) 0])/log10(2)] -to [expr log10([lindex $Params(Lens) end])/log10(2)] \
         -variable Viewport::Data(L$mini) -command "Miniport::Select $Frame $mini; set Viewport::Data(Z$mini) \[expr pow(2,\$Viewport::Data(L$mini))\];Page::Update $Frame; catch"
      label  $Frame.bs$tag -bg $GDefs(ColorFrame) -bitmap @$GDefs(Dir)/share/bitmap/cvscale.xbm -cursor sizing -bd 1 -relief raised
      label  $Frame.bm$tag -bg $GDefs(ColorFrame) -bitmap @$GDefs(Dir)/share/bitmap/cvmove.xbm -cursor fleur -bd 1 -relief raised
      button $Frame.bd$tag -bg $GDefs(ColorFrame) -bitmap @$GDefs(Dir)/share/bitmap/cvdel.xbm -cursor pirate -bd 1 -relief raised -command "Miniport::Destroy $Frame $mini"
      menubutton $Frame.bf$tag -bg $GDefs(ColorFrame) -bitmap @$GDefs(Dir)/share/bitmap/cvmenu.xbm -cursor hand1 -bd 1 -relief raised \
         -menu $Frame.bf$tag.menu

      menu $Frame.bf$tag.menu -bg $GDefs(ColorFrame)
      $Frame.bf$tag.menu add cascade -label [lindex $Lbl(Zoom) $GDefs(Lang)] -menu $Frame.bf$tag.menu.lens
      $Frame.bf$tag.menu add checkbutton -label [lindex $Lbl(Relative) $GDefs(Lang)] -variable Viewport::Data(Relative$mini) \
         -onvalue True -offvalue False -command "Page::Update $Frame"
      $Frame.bf$tag.menu add separator
      $Frame.bf$tag.menu add radiobutton -label [lindex $Lbl(Same) $GDefs(Lang)] -value same -variable Viewport::Data(Type$mini) \
         -command "Miniport::Projection $Frame $mini"
      $Frame.bf$tag.menu add radiobutton -label [lindex $Lbl(Ortho) $GDefs(Lang)] -value orthographic -variable Viewport::Data(Type$mini) \
         -command "Miniport::Projection $Frame $mini"
      $Frame.bf$tag.menu add radiobutton -label [lindex $Lbl(Cylin) $GDefs(Lang)] -value cylindric -variable Viewport::Data(Type$mini) \
         -command "Miniport::Projection $Frame $mini"
      $Frame.bf$tag.menu add separator
      $Frame.bf$tag.menu add checkbutton -label [lindex $Lbl(Data) $GDefs(Lang)] -variable Viewport::Data(Data$mini) \
         -onvalue True -offvalue False -command "Viewport::UpdateData $Frame $mini"
      $Frame.bf$tag.menu add checkbutton -label [lindex $Lbl(Location) $GDefs(Lang)] -variable Viewport::Data(Location$mini) \
         -onvalue True -offvalue False -command "Miniport::Coverage $Frame $mini"
      $Frame.bf$tag.menu add command -label [lindex $Lbl(Color) $GDefs(Lang)] -command "ColorBox::Create . Viewport::Data(Color$mini)"
      $Frame.bf$tag.menu add separator
      $Frame.bf$tag.menu add checkbutton -label [lindex $Lbl(Cursor) $GDefs(Lang)] -variable Viewport::Data(Cursor$mini) \
         -onvalue True -offvalue False

      menu $Frame.bf$tag.menu.lens -bg $GDefs(ColorFrame)
      foreach size $Params(Lens) {
         $Frame.bf$tag.menu.lens add radiobutton -label "${size}x" -variable Viewport::Data(Z$mini) -value [expr double(${size}.0)] \
            -command "set Viewport::Data(L$mini) \[expr log10(\$Viewport::Data(Z$mini))/log10(2)\];Page::Update $Frame"
      }

      $Frame.page.canvas create window [expr $x1-150-35] [expr $y1-1] -window $Frame.sc$tag -anchor sw -tags "SC$ctag NOPRINT" -width 151
      $Frame.page.canvas create window $x1 [expr $y1-1]               -window $Frame.bs$tag -anchor se -tags "BS$ctag NOPRINT"
      $Frame.page.canvas create window [expr $x1-11] [expr $y1-1]     -window $Frame.bm$tag -anchor se -tags "BM$ctag NOPRINT"
      $Frame.page.canvas create window [expr $x1-22] [expr $y1-1]     -window $Frame.bf$tag -anchor se -tags "BF$ctag NOPRINT"
      $Frame.page.canvas create window $x1 $y0                        -window $Frame.bd$tag -anchor ne -tags "BD$ctag NOPRINT"

      #----- bindings de deplacement
      bind $Frame.bm$tag <ButtonPress-1>      "Page::SnapRef $Frame %X %Y"
      bind $Frame.bm$tag <B1-Motion>          "Page::ActiveMove Viewport $Frame $mini %X %Y; Miniport::Coverage $Frame $mini"

      #----- bindings de scaling
      bind $Frame.bs$tag <ButtonPress-1>      "Page::SnapRef $Frame %X %Y"
      bind $Frame.bs$tag <B1-Motion>          "Page::ActiveScale Viewport $Frame $mini %X %Y 1; Miniport::Coverage $Frame $mini"
      bind $Frame.bs$tag <ButtonRelease-1>    "Page::ActiveScale Viewport $Frame $mini %X %Y 0; Miniport::Coverage $Frame $mini"

      Shape::BindWidget $Frame.page.canvas $mini
   }

   $Frame.page.canvas bind $mini <Button-3> "tk_popup $Frame.bf$tag.menu %X %Y 0"

   Page::MaskItem $Frame
   Page::ModeZoom $Frame $mini
   Page::Update   $Frame

   Miniport::Select $Frame $mini

   update idletasks
   $Frame.page.canvas configure -cursor left_ptr

   return $mini
}

proc Miniport::Select { Frame Mini } {

   Page::ActiveUnWrap $Frame $Miniport::Params(Current)
   set Miniport::Params(Current) $Mini
   Page::ActiveWrap $Frame $Miniport::Params(Current)
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

proc Miniport::Projection { Frame Mini } {

   set tag PAGE[string map { . "" } $Mini]

   if { $Viewport::Data(Type$Mini)=="same" } {
      projection configure $Mini -type [projection configure $Frame -type] -georef [projection configure $Frame -georef]
      catch { $Frame.bf$tag.menu entryconfigure 7 -state normal }
  } else {
      #----- We can only show the data when the projection is the same
      set type [projection configure $Frame -type]
      if { $type!="$Viewport::Data(Type$Mini)" } {
         set Viewport::Data(Data$Mini) False
         $Frame.page.canvas itemconf $Mini -data {}
         catch { $Frame.bf$tag.menu entryconfigure 7 -state disabled }
      }

      projection configure $Mini -type $Viewport::Data(Type$Mini) -data {} -georef ""
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

proc Miniport::Coverage { Frame Mini { VP "" } } {

   if { $Viewport::Data(Location$Mini) } {

      if { $VP=="" } {
         set VP $Miniport::Data(VP$Frame)
      }

      #----- Make sure viewport is well initialiazed
      if { ![llength [info command $VP]] } {
         return
      }

      $Frame.page.canvas itemconfigure AREA$Mini -fill $Viewport::Data(Color$Mini)

      if { [projcam configure $Mini -lens]>[projcam configure $Frame -lens] } {

         #----- Coverage dans le viewport

         set x0  [expr $Viewport::Data(X$Mini)+1]
         set y0  [expr $Viewport::Data(Y$Mini)+1]
         set x1  [expr $x0+$Viewport::Data(Width$Mini)-1]
         set y1  [expr $y0+$Viewport::Data(Height$Mini)-1]

         set ll0 [$Mini -unproject $x0 $y0]
         set ll1 [$Mini -unproject $x1 $y0]
         set ll2 [$Mini -unproject $x1 $y1]
         set ll3 [$Mini -unproject $x0 $y1]

         set xy [lindex  [$VP -projectline TRUE [list [lindex $ll0 0] [lindex $ll0 1] 0.0 [lindex $ll1 0] [lindex $ll1 1] 0.0 \
            [lindex $ll2 0] [lindex $ll2 1] 0.0 [lindex $ll3 0] [lindex $ll3 1] 0.0 [lindex $ll0 0] [lindex $ll0 1] 0.0]] 0]

      } else {

         #----- Coverage dans le miniport

         set x0  [expr $Viewport::Data(X$VP)+1]
         set y0  [expr $Viewport::Data(Y$VP)+1]
         set x1  [expr $x0+$Viewport::Data(Width$VP)-1]
         set y1  [expr $y0+$Viewport::Data(Height$VP)-1]

         set ll0 [$VP -unproject $x0 $y0]
         set ll1 [$VP -unproject $x1 $y0]
         set ll2 [$VP -unproject $x1 $y1]
         set ll3 [$VP -unproject $x0 $y1]

         set xy  [lindex [$Mini -projectline TRUE [list [lindex $ll0 0] [lindex $ll0 1] 0.0 [lindex $ll1 0] [lindex $ll1 1] 0.0 \
            [lindex $ll2 0] [lindex $ll2 1] 0.0 [lindex $ll3 0] [lindex $ll3 1] 0.0 [lindex $ll0 0] [lindex $ll0 1] 0.0]] 0]
      }

      if { [llength $xy]<4 } {
         $Frame.page.canvas coords AREA$Mini $Viewport::Data(X$Mini) $Viewport::Data(Y$Mini) $Viewport::Data(X$Mini) $Viewport::Data(Y$Mini)
      } else {
         $Frame.page.canvas coords AREA$Mini $xy
      }
      $Frame.page.canvas raise AREA$Mini $Mini
   } else {
      $Frame.page.canvas coords AREA$Mini $Viewport::Data(X$Mini) $Viewport::Data(Y$Mini) $Viewport::Data(X$Mini) $Viewport::Data(Y$Mini)
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

proc Miniport::Lens { Frame Mini } {

   if { $Viewport::Data(Relative$Mini) } {
      set lens [expr ([projcam configure $Frame -lens]*$Viewport::Data(Z$Mini))]
   } else {
      set lens $Viewport::Data(Z$Mini)
   }

   projcam configure $Mini -lens $lens

   if { $Viewport::Data(Cursor$Mini) } {
      if { $lens<[projcam configure $Frame -lens] } {
         eval projection configure $Mini -location [projection configure $Frame -location]
      } else {
         projection configure $Mini -location $Viewport::Map(LatCursor) $Viewport::Map(LonCursor)
      }
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

proc Miniport::Lock { Frame Mini } {

   set Viewport::Data(Cursor$Mini) False
   projection configure $Mini -location $Viewport::Map(LatCursor) $Viewport::Map(LonCursor)
   $Frame.page.canvas itemconf $Mini -update True
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

proc Miniport::UpdateData { Frame Mini { VP "" } } {
   variable Data

   if { $Viewport::Data(Data$Mini) } {
      if { $VP=="" } {
         set VP $Data(VP$Frame)
      } else {
         set Data(VP$Frame) $VP
      }
      projection configure $Mini -data [projection configure $Frame -data]
      if { $VP!="" } {
         $Frame.page.canvas itemconf $Mini -data [lindex [$Frame.page.canvas itemconf $VP -data] end]
      }
   } else {
      projection configure $Mini -data {}
      $Frame.page.canvas itemconf $Mini -data {}
   }
   Miniport::Projection $Frame $Mini
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

proc Miniport::Destroy { Frame { Mini {} } } {
   variable Data

   if { ![llength $Mini] } {
      set Mini $Data(Mini$Frame)
   }

   foreach mini $Mini {

      $Frame.page.canvas configure -cursor watch
      update idletasks

      set tag PAGE[string map { . "" } $mini]
      set ctag PAGE$mini

      $Frame.page.canvas delete $ctag BS$ctag BM$ctag BF$ctag BD$ctag AREA$Mini
      destroy $Frame.sc$tag $Frame.bs$tag $Frame.bm$tag $Frame.bf$tag Frame.bd$tag $Frame.bf$tag.menu

      projcam destroy $mini
      projection destroy $mini

      unset Data(VP$Frame)
      unset Viewport::Data(X$mini)
      unset Viewport::Data(Y$mini)
      unset Viewport::Data(Z$mini)
      unset Viewport::Data(L$mini)
      unset Viewport::Data(Width$mini)
      unset Viewport::Data(Height$mini)
      unset Viewport::Data(Active$mini)
      unset Viewport::Data(Data$mini)
      unset Viewport::Data(Relative$mini)
      unset Viewport::Data(Cursor$mini)
      unset Viewport::Data(Location$mini)

      set idx [lsearch -exact $Data(Mini$Frame) $mini]
      set Data(Mini$Frame) [lreplace $Data(Mini$Frame) $idx $idx]
   }
   update idletasks
   $Frame.page.canvas configure -cursor left_ptr
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

   if { ![llength  $Miniport::Data(Mini$Frame)] } {
      return
   }

   puts $File "   #----- Affichage de l'encart"
   puts $File ""

   foreach mini $Miniport::Data(Mini$Frame) {
      if { !$Viewport::Data(Cursor$mini) } {
         set coords [projection configure $mini -location]
      } else {
         set coords { -999 -999 }
      }
      puts $File "   Miniport::Create \$Frame $Viewport::Data(X$mini) $Viewport::Data(Y$mini) $Viewport::Data(Width$mini)\
                           $Viewport::Data(Height$mini) $Viewport::Data(Active$mini) $Viewport::Data(Z$mini) [lindex $coords 0] [lindex $coords 1]"
   }
}
