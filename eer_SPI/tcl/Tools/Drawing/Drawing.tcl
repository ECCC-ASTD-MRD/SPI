# #===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Widget de librairie.
# Fichier  : Drawing.tcl
# Creation : Novembre 1999 - J.P. Gauthier - CMC/CMOE
#
# Description:
#    Definitions d'une boite de dialogue pour le dessin sur les projections
#
# Fonctions:
#
#   Drawing::PageActivate { Frame }
#   Drawing::Clear        { Frame }
#   Drawing::Close        { }
#   Drawing::Create       { }
#   Drawing::Dock         { }
#   Drawing::DrawMode     { Frame Clear }
#   Drawing::Draw         { Frame Params }
#   Drawing::Draw3D       { Frame VP Vertex Lat Lon Color Tags }
#   Drawing::DrawImg      { Frame VP Vertex Image Tag }
#   Drawing::DrawBitm     { Frame VP Vertex Color Bitmap Tag }
#   Drawing::DrawDist     { Frame VP Vertex Color Width Font Mode Tag }
#   Drawing::DrawHead     { Frame VP Vertex Color Width Font Tag }
#   Drawing::DrawLine     { Frame VP Vertex Color Width Line Arrow Tag }
#   Drawing::DrawOval     { Frame VP Vertex Color Width Pattern Fill Info Tag Fix }
#   Drawing::DrawPoly     { Frame VP Vertex Color Width Line Pattern Fill Info Tag }
#   Drawing::DrawRect     { Frame VP Vertex Color Width Pattern Fill Info Tag }
#   Drawing::DrawText     { Frame VP Vertex Color Text Font Angle Tag }
#   Drawing::DrawValu     { Frame VP Vertex Color Date Font Grid Coord Tag }
#   Drawing::DrawStream   { Frame VP Vertex Color Width Step Res Tag }
#   Drawing::DrawVert     { Frame VP Vertex Color Text Font Tag }
#   Drawing::FileLoad     { Frame File }
#   Drawing::FileSave     { File }
#   Drawing::ImageAdd     { File }
#   Drawing::ImageSetup   { }
#   Drawing::InitBitm     { }
#   Drawing::InitImg      { }
#   Drawing::InitDist     { }
#   Drawing::InitHead     { }
#   Drawing::InitFont     { Font }
#   Drawing::InitLine     { }
#   Drawing::InitPoly     { }
#   Drawing::InitRect     { }
#   Drawing::InitText     { }
#   Drawing::InitValu     { }
#   Drawing::InitStream   { }
#   Drawing::InitVert     { }
#   Drawing::ItemAdd      { Frame Type { Coords { } }
#   Drawing::ItemDel      { Frame }
#   Drawing::ItemIndex    { Frame Dir }
#   Drawing::ItemSel      { Frame }
#   Drawing::SetIndex     { Frame Index Value }
#   Drawing::SetArrow     { Frame Arrow }
#   Drawing::SetColor     { Frame }
#   Drawing::SetFill      { Frame }
#   Drawing::SetLine      { Frame Line }
#   Drawing::SetPattern   { Frame Pattern }
#   Drawing::SetFont      { Frame Font }
#   Drawing::SetText      { Frame Text }
#   Drawing::SetValu      { Frame }
#   Drawing::SetWidth     { Frame Width }
#   Drawing::UpdateItems  { Frame }
#   Drawing::VertexAdd    { Frame VP X Y args }
#   Drawing::VertexDelete { Frame VP }
#   Drawing::VertexFollow { Frame VP X Y Scan }
#   Drawing::Write        { Frame File }
#
#===============================================================================

#----- Lire les sources d'execution
source $GDefs(Dir)/tcl/Tools/Drawing/Drawing.ctes
source $GDefs(Dir)/tcl/Tools/Drawing/Drawing.txt
source $GDefs(Dir)/tcl/Tools/Drawing/Drawing.int

proc Drawing::PageActivate { Frame } {
   variable Data
   variable Current

   #----- Si la page courante n'a pas de dessin

   if { ![info exist Drawing::Data(Params$Frame)] } {
      set Data(Params$Frame) {}
   }

   #----- Assigner le dessin de la page a celui courant

   set Data(Params) $Data(Params$Frame)
   set Data(Frame) $Frame
   set Current(Mode) ""

   Drawing::Insert
   Drawing::UpdateItems $Frame
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::ImageSetup>
# Creation : Septembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer le menu des images disponibles
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::ImageSetup { { Path "" } } {
   global   GDefs
   variable Resources
   variable Lbl
   variable Data
   
   if { $Path=="" } {
      set Path $GDefs(Dir)/share/image/Symbol
   }
   
   set path .drawing.params.image.lbl.opt
   if { ![winfo exists $path.menu] } {
      menu $path.menu
      $path.menu add cascade -label [lindex $Lbl(New) $GDefs(Lang)] -menu $path.menu.new
      menu $path.menu.new
      $path.menu.new add command -label [lindex $Lbl(Open) $GDefs(Lang)] \
         -command { Drawing::ImageAdd [FileBox::Create .drawing "" Load [list {Graphic Interchange Format {*.gif}}]] }
      $path.menu.new add separator
   }
   
   $path.menu add separator
   
   set Resources(Image) {}
   foreach group [lsort -dictionary [glob $Path/*]] {

      set menu $path.menu.[string tolower [file tail $group]]
      $path.menu add cascade -label [file tail $group] -menu $menu
      menu $menu

      set i 0
      foreach image [lsort -dictionary [glob $group/*.gif]] {
         if { [incr i]==10 } {
            set i 0
            set br 1
         } else {
            set br 0
         }

         regsub -all "\[^a-zA-Z0-9\]"  [file tail $image] _ name
         #----- if image does not exists
         if  { [catch { image width IMG$name }] } {
            image create photo IMG$name -file $image
         }
         $menu add radiobutton -image IMG$name -variable Drawing::Current(Image) -value IMG$name \
            -indicatoron false -columnbreak $br \
            -command "Drawing::SetImage \$Page::Data(Frame) \$Drawing::Current(Image) ; $path configure -image \$Drawing::Current(Image)"
         lappend Resources(Image) IMG$name
      }
   }
   set Drawing::Current(Image) [lindex $Resources(Image) 0]
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::ImageAdd>
# Creation : Septembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Ajouter une image au menu des images disponibles
#
# Parametres :
#  <File>    : Identificateur du menu
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::ImageAdd { File } {
   global   GDefs
   variable Resources
   variable Data

   set menu .drawing.params.image.lbl.opt

   if { $File!="" } {
      regsub -all "\[^a-zA-Z0-9\]"  [file tail $File] _ name
      if { [lsearch -exact $Resources(Image) IMG$name]==-1 } {
         image create photo IMG$name -file $File
         $menu.menu.new add radiobutton -image IMG$name -variable Drawing::Current(Image) -value IMG$name -indicatoron false \
            -command "Drawing::SetImage $Page::Data(Frame) \$Drawing::Current(Image) ; $menu configure -image \$Drawing::Current(Image)"
         lappend Resources(Image) IMG$name
         
         #----- Select this new image
         set Drawing::Current(Image) IMG$name
         Drawing::SetImage $Page::Data(Frame) $Drawing::Current(Image) 
         $menu configure -image $Drawing::Current(Image)
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::Clear>
# Creation : Septembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Supprimer toutes les primitives.
#
# Parametres :
#  <Frame>   : Identificateurs de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::Clear { { Frames {} } } {
   variable Resources
   variable Data

   if { ![llength $Frames] } {
      set Frames $Page::Data(Frames)
   }
   
   foreach frame $Frames {
      foreach font $Resources(Font) {
         font delete $font
      }

      set Data(Params)     [set Data(Params$frame) ""]
      set Data(NoItem)     0
      set Resources(Font)  ""

      Drawing::Insert
      Drawing::ItemSel $frame

      $frame.page.canvas delete $Data(Tag)
   }
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::Close>
# Creation : Novembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Fermer le "Plug-In" de dessin.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::Close { } {
   variable Data
   variable Current

   set Current(Mode) ""
#   set Data(Active) 0

   #----- Retour de la projection en mode Zoom

   SPI::ToolMode SPI Zoom

   destroy .drawing
   if { [winfo exists $Page::Data(Canvas)] } {
      $Page::Data(Canvas) config -cursor left_ptr
   }

   if { !$SPI::Param(Window) } { SPI::Quit }
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::Insert>
# Creation : Mars 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Inserer les objets dans la liste
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::Insert { } {
   variable Data

   if { [winfo exists .drawing] } {

      $Data(Tab).items.list.box delete 0 end

      foreach params $Data(Params) {
         if { [lindex $params 2] == -1 } {
            set mode static
         } else {
            set mode georef
         }
         $Data(Tab).items.list.box insert end "[lindex $params 1] [lindex $params 0] $mode"
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::DrawMode>
# Creation : Juillet 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Selectionner le mode de dessin (Georeference ou statique)
#
# Parametres :
#  <Frame>  : Identificateur du canvas
#  <Clear>   : Enlever la selection de la primitive
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::DrawMode { Frame Clear } {
   variable Data
   variable Current

   #----- Deselectionner toute primitive pour ne pas causer
   #      de conflit de mode dans une meme primitive

   if { $Clear } {
      set Current(Mode) ""
      $Data(Tab).items.list.box selection clear 0 end
      Drawing::ItemSel $Frame
   }

   if { $Data(GeoRef) } {
      $Data(Tab).items.def.head configure -state normal
      $Data(Tab).items.def.dist configure -state normal
      $Data(Tab).items.def.valu configure -state normal
      $Data(Tab).items.def.strm configure -state normal
      $Data(Tab).items.def.vert configure -state normal

      $Data(Tab).head.grid configure -state disabled

      .drawing.dock.lat configure -state normal -fg black
      .drawing.dock.lon configure -state normal -fg black
      .drawing.dock.ele configure -state normal -fg black
      .drawing.dock.add configure -state normal
   } else {
      $Data(Tab).items.def.head configure -state disabled
      $Data(Tab).items.def.dist configure -state disabled
      $Data(Tab).items.def.valu configure -state disabled
      $Data(Tab).items.def.strm configure -state disabled
      $Data(Tab).items.def.vert configure -state disabled

      $Data(Tab).head.grid configure -state normal

      .drawing.dock.lat configure -state disabled -fg gray70
      .drawing.dock.lon configure -state disabled -fg gray70
      .drawing.dock.ele configure -state disabled -fg gray70
      .drawing.dock.add configure -state disabled
   }
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::Draw>
# Creation : Novembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectuer l'affichage de ce qui a ete dessiner jusqu'a maintenant.
#
# Parametres :
#  <Frame>  : Identificateur du canvas
#  <VP>      : Identificateur du Viewport
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::Draw { Frame Params } {
   variable Data
   variable Current
   variable Resources

   #----- Extraire les parametres

   set mode    [lindex $Params 0]
   set no      [lindex $Params 1]
   set vp      [lindex $Params 2]
   set vertex  [lindex $Params 3]
   set color   [lindex $Params 4]
   set width   [lindex $Params 5]
   set line    [lindex $Params 6]
   set pattern [lindex $Params 7]
   set extra   [lindex $Params 8]
   set info    [lindex $Params 9]

   $Frame.page.canvas delete $Data(Tag)$no D$Data(Tag)$no VERTEXFOLLOW DVERTEXFOLLOW

   #----- Est-ce que l'item est localise

   if { [llength $vertex] == 0 } {
      return
   }

   if { $info=="" } {
      set info False
   }
   #----- Est-ce que le viewport existe toujours


   if { $vp==-1 || [Page::Registered $Page::Data(Frame) Viewport $vp]!=-1 } {

      switch $mode {
        "imag" { Drawing::DrawImag   $Frame $vp $vertex $color $Data(Tag)$no }
        "bitm" { Drawing::DrawBitm   $Frame $vp $vertex $color $width $Data(Tag)$no }
        "line" { Drawing::DrawLine   $Frame $vp $vertex $color $width $line $pattern $Data(Tag)$no }
        "poly" { Drawing::DrawPoly   $Frame $vp $vertex $color $width $line $pattern $extra $info $Data(Tag)$no }
        "rect" { Drawing::DrawRect   $Frame $vp $vertex $color $width $pattern $extra $info $Data(Tag)$no }
        "oval" { Drawing::DrawOval   $Frame $vp $vertex $color $width $pattern $extra $info $Data(Tag)$no 0 }
        "circ" { Drawing::DrawOval   $Frame $vp $vertex $color $width $pattern $extra $info $Data(Tag)$no 1 }
        "text" { Drawing::DrawText   $Frame $vp $vertex $color $width $line $pattern $Data(Tag)$no }
        "dist" { Drawing::DrawDist   $Frame $vp $vertex $color $width $line $pattern $Data(Tag)$no }
        "head" { Drawing::DrawHead   $Frame $vp $vertex $color $width $line $Data(Tag)$no }
        "valu" { Drawing::DrawValu   $Frame $vp $vertex $color $width $line $pattern $extra $Data(Tag)$no }
        "strm" { Drawing::DrawStream $Frame $vp $vertex $color $width $line $pattern $Data(Tag)$no }
        "vert" { Drawing::DrawVert   $Frame $vp $vertex $color $width $line $Data(Tag)$no }
        "strk" { Drawing::DrawStrk   $Frame $vp $vertex $color $width $Data(Tag)$no }
      }
   }

   if { $vp==-1 } {
      Shape::BindAllMove $Frame.page.canvas $Data(Tag)$no "Drawing::VertexSet $Frame $Data(Tag)$no $no"
   }
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::Draw3D>
# Creation : Decembre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche la troisieme dimension
#
# Parametres :
#  <Frame>  : Identificateur du canvas
#  <VP>      : Identificateur du Viewport
#  <Vertex>  : Pixel en elevation
#  <Lat>     : Latitude du point
#  <Lon>     : Longitude du point
#  <Color>   : Couleur
#  <Tags>     : Tag associe a l'item dans le canvas
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::Draw3D { Frame VP Vertex Lat Lon Color Tags } {
   variable Data

   if { [set pt [$VP -project $Lat $Lon 0.0]]!="" } {
      $Frame.page.canvas create line [lindex $Vertex 0] [lindex $Vertex 1] [lindex $pt 0] [lindex $pt 1] \
         -fill $Color -tags "PAGE$VP $Data(Tag) D$Tags" -arrow none -width 1 -smooth 0
   }
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::DrawBitm>
# Creation : Septembre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche un bitmap
#
# Parametres :
#  <Frame>  : Identificateur du canvas
#  <VP>      : Identificateur du Viewport
#  <Vertex>  : Liste des coordonnees
#  <Color>   : Couleur
#  <Bitmap>  : Bitmap
#  <Tag>     : Tag associe a l'item dans le canvas
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::DrawBitm { Frame VP Vertex Color Bitmap Tag } {
   variable Data
   variable Current

   if { $VP>0 } {

      foreach { la lo lz } $Vertex {

         set l [$VP -project $la $lo $lz]

         if { $l != "" && [lindex $l 2] > 0 } {

            $Frame.page.canvas create bitmap [lindex $l 0] [lindex $l 1] -bitmap $Bitmap \
               -foreground $Color -tags "PAGE$VP $Data(Tag) $Tag"

            if { $Current(3D) && [lindex $Vertex 2] != 0.0 } {
               Drawing::Draw3D $Frame $VP $l [lindex $Vertex 0] [lindex $Vertex 1] $Color $Tag
            }
         }
      }
   } else {
     foreach { x y } $Vertex {
        $Frame.page.canvas create bitmap $x $y -bitmap $Bitmap \
           -foreground $Color -tags "PAGE $Data(Tag) $Tag"
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::DrawImag>
# Creation : Mai 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche une image
#
# Parametres :
#  <Frame>  : Identificateur du canvas
#  <VP>      : Identificateur du Viewport
#  <Vertex>  : Liste des coordonnees
#  <Image>   : Image
#  <Tag>     : Tag associe a l'item dans le canvas
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::DrawImag { Frame VP Vertex Image Tag } {
   variable Data
   variable Current

   if { $VP>0 } {

      foreach { la lo lz } $Vertex {

         set l [$VP -project $la $lo $lz]

         if { $l != "" && [lindex $l 2] > 0 } {

            $Frame.page.canvas create image [lindex $l 0] [lindex $l 1] -image $Image \
               -tags "PAGE$VP $Data(Tag) $Tag"

            if { $Current(3D) && [lindex $Vertex 2] != 0.0 } {
               Drawing::Draw3D $Frame $VP $l [lindex $Vertex 0] [lindex $Vertex 1] black $Tag
            }
         }
      }
   } else {
     foreach { x y } $Vertex {
        $Frame.page.canvas create image $x $y -image $Image -tags "PAGE $Data(Tag) $Tag"
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::DrawHead>
# Creation : Novembre 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche un compas et direction
#
# Parametres :
#  <Frame>  : Identificateur du canvas
#  <VP>      : Identificateur du Viewport
#  <Vertex>  : Liste des coordonnees
#  <Color>   : Couleur
#  <Width>   : Dimension de la ligne
#  <Font>    : Police
#  <Tag>     : Tag associe a l'item dans le canvas
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::DrawHead { Frame VP Vertex Color Width Font Tag } {
   variable Current
   variable Data

   set c    {}
   set cir  {}
   set cdir {}
   set d    [expr 35*[$VP -distpix]]

   foreach { la lo lz } $Vertex {

      if { ![llength $c] } {
         set cll "$la $lo $lz"
         if { [set c [$VP -project $la $lo $lz]]==""  || [lindex $c 2]<0} {
            break
         }
         foreach th { 0 90 180 270 } dir { N E S W } {
            set ll [$VP -circle [lindex $cll 0] [lindex $cll 1] $d $th]
            if { [set cdir [$VP -project [lindex $ll 0] [lindex $ll 1] $lz]]!="" && [lindex $cdir 2]>0 } {
               lappend cir [lindex $cdir 0] [lindex $cdir 1]
               $Frame.page.canvas create text [lindex $cdir 0] [lindex $cdir 1] -text $dir \
                  -tags "PAGE$VP $Data(Tag) $Tag" -fill $Color -font $Font -anchor c
            }
         }
         lappend cir [lindex $cir 0] [lindex $cir 1]
         if { [llength $cir]==10 } {
            eval $Frame.page.canvas create line $cir -dash . -fill \$Color -width $Width -smooth 1 -tags \"PAGE$VP $Data(Tag) $Tag\"
         }
      } else {
         set h [expr -[$VP -bearing [lindex $cll 0] [lindex $cll 1] $la $lo]]
         set h [expr $h<0?360+$h:$h]
         set vr  [$VP -projectline TRUE [list [lindex $cll 0] [lindex $cll 1] $lz $la $lo $lz]]
         set vr0 [lindex $vr 0]
         set vr1 [lindex $vr 1]
         if { [llength $vr0]>2 } {
            $Frame.page.canvas create text [lindex $vr0 end-1] [lindex $vr0 end] -text [format " %.2f°" $h] \
               -tags "PAGE$VP $Data(Tag) $Tag" -fill $Color -font $Font -anchor w
            eval $Frame.page.canvas create line $vr0 -fill \$Color -width $Width -arrow last -tags \"PAGE$VP $Data(Tag) $Tag\"
         }
         if { [llength $vr1]>2 && $Viewport::Map(Type$Page::Data(Frame)) != "orthographic" } {
            eval $Frame.page.canvas create line $vr1 -fill \$Color -width $Width -arrow none -tags \"PAGE$VP $Data(Tag) $Tag\"
         }
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::DrawDist>
# Creation : Septembre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche une distance
#
# Parametres :
#  <Frame>  : Identificateur du canvas
#  <VP>      : Identificateur du Viewport
#  <Vertex>  : Liste des coordonnees
#  <Color>   : Couleur
#  <Width>   : Dimension de la ligne
#  <Font>    : Police
#  <Mode>    : Mode nautique
#  <Tag>     : Tag associe a l'item dans le canvas
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::DrawDist { Frame VP Vertex Color Width Font Mode Tag } {
   variable Current
   variable Data

   #----- Projeter les lignes

   set dist   0
   set px     ""
   set py     ""

   foreach { la lo lz } $Vertex {

      if { $px!="" } {
         set dist [expr $dist + [$VP -distll $py $px $la $lo $lz]]
      }
      set px $lo
      set py $la

      if { $Current(3D) && $lz != 0.0 } {
         if { [set l [$VP -project $la $lo $lz]]!="" } {
            Drawing::Draw3D $Frame $VP $l $la $lo $Color $Tag
         }
      }
   }

   set vr  [$VP -projectline TRUE $Vertex]
   set vr0 [lindex $vr 0]
   set vr1 [lindex $vr 1]

   if { [llength $vr0]>2 } {
      $Frame.page.canvas create text [lindex $vr0 end-1] [lindex $vr0 end] -text " [Convert::FormatDist $dist 2 $Mode]" \
         -tags "PAGE$VP $Data(Tag) $Tag" -fill $Color -font $Font -anchor w
      eval $Frame.page.canvas create line $vr0 -fill \$Color -width $Width -arrow both -tags \"PAGE$VP $Data(Tag) $Tag\"
   }
   if { [llength $vr1]>2 && $Viewport::Map(Type$Page::Data(Frame)) != "orthographic" } {
      eval $Frame.page.canvas create line $vr1 -fill \$Color -width $Width -arrow both -tags \"PAGE$VP $Data(Tag) $Tag\"
   }
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::DrawStrk>
# Creation : Novembre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche une inscription continue
#
# Parametres :
#  <Frame>  : Identificateur du canvas
#  <VP>      : Identificateur du Viewport
#  <Vertex>  : Liste des coordonnees
#  <Color>   : Couleur
#  <Width>   : Dimension de la police
#  <Tag>     : Tag associe a l'item dans le canvas
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::DrawStrk { Frame VP Vertex Color Width Tag } {
   variable Current
   variable Data

   #----- Separer les segments

   set segs [split $Vertex "|"]

   foreach seg $segs {

      if { $VP > 0 } {

         set vr  [$VP -projectline NONE $seg]
         set vr0 [lindex $vr 0]
         set vr1 [lindex $vr 1]

         if { [llength $vr0]>2 } {
            eval $Frame.page.canvas create line $vr0 -fill \$Color -width $Width -tags \"PAGE$VP $Data(Tag) $Tag\"
         }

         if { [llength $vr1]>2 && $Viewport::Map(Type$Page::Data(Frame)) != "orthographic" } {
            eval $Frame.page.canvas create line $vr1 -fill \$Color -width $Width -tags \"PAGE$VP $Data(Tag) $Tag\"
         }
      } else {
         if { [llength $seg]>2 } {
            eval $Frame.page.canvas create line $seg -fill \$Color -width $Width -tags \"PAGE $Data(Tag) $Tag\"
         }
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::DrawLine>
# Creation : Novembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche une ligne
#
# Parametres :
#  <Frame>  : Identificateur du canvas
#  <VP>      : Identificateur du vIewport
#  <Vertex>  : Liste des coordonnees
#  <Color>   : Couleur
#  <Width>   : Dimension de la police
#  <Arrow>   : Type de fleche
#  <Tag>     : Tag associe a l'item dans le canvas
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::DrawLine { Frame VP Vertex Color Width Line Arrow Tag } {
   variable Data
   variable Current

   if { $VP > 0 } {

      if { $Current(3D) } {
         foreach { la lo lz } $Vertex {
            if { $lz != 0.0 && [set l [$VP -project $la $lo $lz]]!="" } {
               Drawing::Draw3D $Frame $VP $l $la $lo $Color $Tag
            }
         }
      }

      set vr  [$VP -projectline TRUE $Vertex]
      set vr0 [lindex $vr 0]
      set vr1 [lindex $vr 1]

      if { [llength $vr0]>2 } {
         eval $Frame.page.canvas create line $vr0 -fill \$Color -smooth $Line -width $Width -arrow $Arrow -tags \"PAGE$VP $Data(Tag) $Tag\"
      }
      if { [llength $vr1]>2 && $Viewport::Map(Type$Page::Data(Frame)) != "orthographic" } {
         eval $Frame.page.canvas create line $vr1 -fill \$Color -smooth $Line -width $Width -arrow $Arrow -tags \"PAGE$VP $Data(Tag) $Tag\"
      }
   } else {
      if { [llength $Vertex]>2 } {
         eval $Frame.page.canvas create line $Vertex -fill \$Color -smooth $Line -width $Width -arrow $Arrow -tags \"PAGE $Data(Tag) $Tag\"
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::DrawOval>
# Creation : Novembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche une ellipse
#
# Parametres :
#  <Frame>  : Identificateur du canvas
#  <VP>      : Identificateur du Viewport
#  <Vertex>  : Liste des coordonnees
#  <Color>   : Couleur
#  <Width>   : Dimension de la police
#  <Pattern> : Bitmap de remplissage
#  <Fill>    : Couleur de remplissage
#  <Info>    : Afficher l'info
#  <Tag>     : Tag associe a l'item dans le canvas
#  <Fix>     : Fixer les dimensions egale (Cercle)
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::DrawOval { Frame VP Vertex Color Width Pattern Fill Info Tag Fix } {
   global   GDefs
   variable Data
   variable Current
   variable Resources

   if { $VP > 0 } {

      foreach { la0 lo0 lz0 la1 lo1 lz1 } $Vertex {

         if { $lz1 == "" } {
            break
         }

         set l0 [$VP -project $la0 $lo0 $lz0]
         set l1 [$VP -project $la1 $lo1 $lz1]

         if { $l0 != "" && $l1 != "" && ([lindex $l0 2]>0 || [lindex $l1 2]>0) } {

            set lx0 [lindex $l0 0]
            set lx1 [lindex $l1 0]
            set ly0 [lindex $l0 1]
            set ly1 [lindex $l1 1]

            set dx [expr abs($lx1-$lx0)]
            set dy [expr abs($ly1-$ly0)]

            if { $Fix } {
               if { $dx<$dy } {
                  set dx $dy
               } else {
                  set dy $dx
               }
            }

            set x0 [expr $lx0-$dx]
            set x1 [expr $lx0+$dx]
            set y0 [expr $ly0-$dy]
            set y1 [expr $ly0+$dy]

            $Frame.page.canvas create oval $x0 $y0 $x1 $y1 -fill $Fill -outline $Color -width $Width -stipple $Pattern \
               -tags "PAGE$VP $Data(Tag) $Tag"

            if { $Tag=="VERTEXFOLLOW" || $Info } {
               set dista [$VP -distxy $lx0 $ly0 $x1 $ly0]

               $Frame.page.canvas create text $x1 $ly0 -font XFont14 -text " [Convert::FormatDist $dista]" -fill $Color -anchor w -tags "PAGE  PAGE$VP $Data(Tag) $Tag"
               $Frame.page.canvas create line $lx0 $ly0 $x1 $ly0 -width 1 -arrow last -fill $Color -tags "PAGE  PAGE$VP $Data(Tag) $Tag"
               if { !$Fix } {
                  set distb [$VP -distxy $lx0 $ly0 $lx0 $y1]
                  $Frame.page.canvas create text $lx0 $y1 -font XFont14 -text "\n[Convert::FormatDist $distb]" -fill $Color -anchor n -tags "PAGE  PAGE$VP $Data(Tag) $Tag"
                  $Frame.page.canvas create line $lx0 $ly0 $lx0 $y1 -width 1 -arrow last -fill $Color -tags "PAGE  PAGE$VP $Data(Tag) $Tag"
               }
            }
         }
      }
   } else {
      foreach { la0 lo0 la1 lo1 } $Vertex {

         if { $lo1 == "" } {
            break
         }

         set dx [expr abs($la1-$la0)]
         set dy [expr abs($lo1-$lo0)]

         if { $Fix } {
            if { $dx<$dy } {
               set dx $dy
            } else {
               set dy $dx
            }
         }
         set x0 [expr $la0-$dx]
         set x1 [expr $la0+$dx]
         set y0 [expr $lo0-$dy]
         set y1 [expr $lo0+$dy]

         $Frame.page.canvas create oval $x0 $y0 $x1 $y1 -fill $Fill -outline $Color -width $Width -stipple $Pattern \
            -tags "PAGE $Data(Tag) $Tag"
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::DrawPoly>
# Creation : Novembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche un polygone
#
# Parametres :
#  <Frame>  : Identificateur du canvas
#  <VP>      : Identificateur du Viewport
#  <Vertex>  : Liste des coordonnees
#  <Color>   : Couleur
#  <Width>   : Dimension de la police
#  <Line>    : Type de ligne
#  <Pattern> : Bitmap de remplissage
#  <Fill>    : Couleur du contour
#  <Info>    : Afficher l'info
#  <Tag>     : Tag associe a l'item dans le canvas
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::DrawPoly { Frame VP Vertex Color Width Line Pattern Fill Info Tag } {
   global   GDefs
   variable Data
   variable Resources

   if { $VP > 0 } {

      lappend Vertex [lindex $Vertex 0] [lindex $Vertex 1] [lindex $Vertex 2]
      set vr  [$VP -projectline TRUE $Vertex]
      set vr0 [lindex $vr 0]
      set vr1 [lindex $vr 1]
      
      if { [llength $vr0]>4 } {
         eval $Frame.page.canvas create polygon $vr0 -stipple \$Pattern -tags \"PAGE$VP $Data(Tag) $Tag\" \
           -fill \$Fill -outline \$Color -smooth $Line -width $Width
      } elseif { [llength $vr0]==4 } {
         eval $Frame.page.canvas create line $vr0 -fill \$Color -width $Width -tags \"PAGE$VP $Data(Tag) $Tag\"
      }

      if { $Viewport::Map(Type$Page::Data(Frame))!="orthographic" } {
         if { [llength $vr1]>4 } {
            eval $Frame.page.canvas create polygon $vr1 -stipple \$Pattern -tags \"PAGE$VP $Data(Tag) $Tag\" \
              -fill \$Fill -outline \$Color -smooth $Line -width $Width
         } elseif { [llength $vr1]==4 } {
            eval $Frame.page.canvas create line $vr1 -fill \$Color -width $Width -tags \"PAGE$VP $Data(Tag) $Tag \"
         }
      }
      
      if { ($Tag=="VERTEXFOLLOW" || $Info) && [llength $vr0]>2} {
         foreach { x y z } $Vertex {
            lappend ll $x $y
         }
         set area [projection function $Frame -area $ll 0.0]
         $Frame.page.canvas create text [lindex $vr0 end-3] [lindex $vr0 end-2] -font XFont14 -text " [Convert::FormatArea $area]" -fill $Color -anchor w -tags "PAGE PAGE$VP $Data(Tag) $Tag"
      }
   } else {
      eval $Frame.page.canvas create polygon $Vertex [lindex $Vertex 0] [lindex $Vertex 1] -fill \$Fill -outline \$Color -smooth $Line -width $Width  -stipple \$Pattern \
         -tags \"PAGE $Data(Tag) $Tag\"
   }
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::DrawRect>
# Creation : Novembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche un rectangle
#
# Parametres :
#  <Frame>  : Identificateur du canvas
#  <VP>      : Identificateur du Viewport
#  <Vertex>  : Liste des coordonnees
#  <Color>   : Couleur
#  <Width>   : Dimension de la police
#  <Pattern> : Bitmap de remplissage
#  <Fill>    : Couleur de remplissage
#  <Info>    : Afficher l'info
#  <Tag>     : Tag associe a l'item dans le canvas
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::DrawRect { Frame VP Vertex Color Width Pattern Fill Info Tag } {
   global   GDefs
   variable Data
   variable Resources

   if { $VP > 0 } {

      foreach { la0 lo0 lz0 la1 lo1 lz1 } $Vertex {

         if { $lz1 == "" } {
            break
         }

         set l0 [$VP -project $la0 $lo0 $lz0]
         set l1 [$VP -project $la1 $lo1 $lz1]

         if { $l0 != "" && $l1 != "" && ([lindex $l0 2]>0 || [lindex $l1 2]>0) } {
            $Frame.page.canvas create rectangle [lindex $l0 0] [lindex $l0 1] [lindex $l1 0] [lindex $l1 1] \
               -fill $Fill -outline $Color -width $Width -stipple $Pattern \
               -tags "PAGE$VP $Data(Tag) $Tag"
               
            if { $Tag=="VERTEXFOLLOW" || $Info } {
               set x0 [lindex $l0 0]
               set x1 [lindex $l1 0]
               set y0 [lindex $l0 1]
               set y1 [lindex $l1 1]

               set dx [expr abs($x1-$x0)/2]
               set dy [expr abs($y1-$y0)/2]

               set dista [$VP -distxy $x0 $y1 $x1 $y1]
               $Frame.page.canvas create text [expr $x0+$dx] $y1 -font XFont14 -text "\n[Convert::FormatDist $dista]" -fill $Color -anchor n -tags "PAGE PAGE$VP $Data(Tag) $Tag"
               set distb [$VP -distxy $x1 $y0 $x1 $y1]
               $Frame.page.canvas create text $x1 [expr $y0+$dy] -font XFont14 -text "\n[Convert::FormatDist $distb]" -fill $Color -anchor n -tags "PAGE PAGE$VP $Data(Tag) $Tag" -angle 90
            }
         }
      }
   } else {
      foreach { la0 lo0 la1 lo1 } $Vertex {

         if { $lo1 == "" } {
            break
         }

         $Frame.page.canvas create rectangle $la0 $lo0 $la1 $lo1 \
            -fill $Fill -outline $Color -width $Width -stipple $Pattern \
            -tags "PAGE $Data(Tag) $Tag"
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::DrawText>
# Creation : Novembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche une ligne de texte
#
# Parametres :
#  <Frame>  : Identificateur du canvas
#  <VP>      : Identificateur du Viewport
#  <Vertex>  : Liste des coordonnees
#  <Color>   : Couleur
#  <Text>    : Ligne de texte
#  <Font>    : Police
#  <Angle>   : Angle
#  <Tag>     : Tag associe a l'item dans le canvas
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::DrawText { Frame VP Vertex Color Text Font Angle Tag } {
   variable Data

   if { $VP > 0  } {
      foreach { la lo lz } $Vertex {

          if { [set l [$VP -project $la $lo $lz]]!="" } {

            if { [lindex $l 2] > 0 } {
               $Frame.page.canvas create text [lindex $l 0] [lindex $l 1] -text "$Text" -anchor w \
                  -tags "PAGE$VP $Data(Tag) $Tag" -fill $Color -font $Font -angle $Angle
            }
         }
      }
   } else {
      foreach { x y } $Vertex {
         $Frame.page.canvas create text $x $y -text "$Text" -anchor w \
            -tags "PAGE $Data(Tag) $Tag" -fill $Color -font $Font -angle $Angle
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::DrawValu>
# Creation : Septembre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche une valeur pointee
#
# Parametres :
#  <Frame>  : Identificateur du canvas
#  <VP>      : Identificateur du Viewport
#  <Vertex>  : Liste des coordonnees
#  <Color>   : Couleur
#  <Date>    : Affichage de la date de validite
#  <Font>    : Police
#  <Grid>    : Affichage de la coordonne point de grille
#  <Coord>   : Affichage de la coordonne latlon
#  <Tag>     : Tag associe a l'item dans le canvas
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::DrawValu { Frame VP Vertex Color Date Font Grid Coord Tag } {
   variable Current
   variable Data

   foreach { la lo lz } $Vertex {

       if { [set l [$VP -project $la $lo $lz]]!="" } {

         if { [lindex $l 2] > 0 } {

            set x    [lindex $l 0]
            set y    [lindex $l 1]
            set text ""

            foreach field [lindex [$Frame.page.canvas itemconfigure $VP -data] 4] {

               if { [fstdfield is $field] && [fstdfield define $field -GRTYP]!="V" } {
                  set value [fstdfield stats $field -coordvalue $la $lo]
                  set spd [lindex $value 0]
                  set dir [lindex $value 1]

                  if { $spd != "-" && $spd != "" } {

                     if { $FSTD::Param(Mantisse) == 0 } {
                        set p 2
                     } else {
                        set p $FSTD::Param(Mantisse)
                     }

                     if { $FSTD::Param(Order) == "AUTO" || $FSTD::Param(Order) == "EXPONENTIAL" } {
                         eval set spd \[format \"%1.${p}e\" $spd\]
                     } elseif { $FSTD::Param(Order) == "INTEGER" } {
                         eval set spd \[format \"%i\" [expr int($spd)]\]
                     } elseif { $FSTD::Param(Order) == "FLOAT" } {
                         eval set spd \[format \"%1.${p}f\" $spd\]
                     }
                  }

                  set info "[fstdfield configure $field -desc]:$spd"

                  if { $dir != "" } {
                     set info "$info@[format "%1.2f" $dir]"
                  }

                  if { $Grid } {
                     set grid [fstdfield stats $field -coordpoint $la $lo]
                     set info "$info \[[format "%0.2f,%0.2f" [lindex $grid 0] [lindex $grid 1]]\]"
                  }

                  if { $Date } {
                     set date [MetData::FormatDATEV $field]
                     set info "$info ($date)"
                  }
                  lappend text "$info"
               }
            }

            set text [join $text \n]

            #----- Afficher la valeur pointee

            $Frame.page.canvas create text [expr $x+6] $y -text $text -fill $Color -font $Font -tags "PAGE$VP $Data(Tag) $Tag" -anchor w
            $Frame.page.canvas create oval [expr $x-2] [expr $y-2] [expr $x+2] [expr $y+2] -fill $Color -outline $Color \
               -tags "PAGE$VP $Data(Tag) $Tag"

            if { $Coord } {
               $Frame.page.canvas create text [expr $x-5] $y -text "([Convert::FormatCoord $la $lo DEG])" -fill $Color -font $Font \
                  -tags "PAGE$VP $Data(Tag) $Tag" -anchor e
            }
         }
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::DrawStream>
# Creation : Juin 2005 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche une ligne de courant
#
# Parametres :
#  <Frame>  : Identificateur du canvas
#  <VP>      : Identificateur du Viewport
#  <Vertex>  : Liste des coordonnees
#  <Color>   : Couleur
#  <Width>   : Largeur
#  <Step>    : Pas d'echantillonage
#  <Res>     : Resolution
#  <Tag>     : Tag associe a l'item dans le canvas
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::DrawStream { Frame VP Vertex Color Width Step Res Tag } {
   variable Current
   variable Data

   foreach { la lo lz } $Vertex {

       if { [set l [$VP -project $la $lo $lz]]!="" && [lindex $l 2]>0 } {

         foreach field [lindex [$Frame.page.canvas itemconfigure $VP -data] 4] {

            if { [fstdfield is $field] && [fstdfield define $field -GRTYP]!="V" } {

               set ij [fstdfield stats $field -coordpoint $la $lo]
               if { [lindex $ij 0]!=-1 } {
                  set coords [fstdfield stats $field -coordstream [lindex $ij 0] [lindex $ij 1] 256 $Step 0 $Res]
                  if { [llength $coords] && [llength [set path [lindex [$VP -projectline NONE $coords] 0]]>2] } {
                     eval $Frame.page.canvas create line $path -fill \$Color -width $Width -tags \"PAGE$VP $Data(Tag) $Tag\" -arrow last
                  }
               }
            }
         }
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::DrawVert>
# Creation : Avril 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche une echelle verticale
#
# Parametres :
#  <Frame>  : Identificateur du canvas
#  <VP>      : Identificateur du Viewport
#  <Vertex>  : Liste des coordonnees
#  <Color>   : Couleur
#  <Text>    : Liste des elevations
#  <Font>    : Police
#  <Tag>     : Tag associe a l'item dans le canvas
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::DrawVert { Frame VP Vertex Color Text Font Tag } {
   variable Data

   if { $VP > 0  } {
      foreach { la lo lz } $Vertex {

         set vr  [$VP -projectline NONE "$la $lo [lindex $Text 0] $la $lo [lindex $Text end]"]
         set vr0 [lindex $vr 0]
         set vr1 [lindex $vr 1]


         if { [llength $vr0]>2 } {

            eval $Frame.page.canvas create line $vr0 -fill \$Color -width 2 -tags \"PAGE$VP $Data(Tag) $Tag\"

            #----- Afficher les elevations

            foreach elev $Text {
               if { [set l [$VP -project $la $lo $elev]]!="" } {
                  set x [lindex $l 0]
                  set y [lindex $l 1]
                  $Frame.page.canvas create text $x $y -text "$elev " -anchor e \
                     -tags "PAGE$VP $Data(Tag) $Tag" -fill $Color -font $Font
                  $Frame.page.canvas create line $x $y [expr $x-5] $y \
                     -fill $Color -width 2 -tags "PAGE$VP $Data(Tag) $Tag"
               }
            }
         }
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::FileSave>
# Creation : Septembre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Enregistrer les items du dessin.
#
# Parametres :
#   <File>   : Path du fichier
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::FileSave { File } {
   variable Data
   variable Resources

   if { $File == "" } {
      return
   }

   set file [open $File w]

   puts $file "Drawing 3.1"
   puts $file $Data(NoItem)

   #----- Enregistrer les primitives

   foreach params $Data(Params) {

   #----- Patch temporaire pour sortir les coordonnees georeferencees en coordonnees statiques
#      set v {}
#      foreach { lat lon elev } [lindex $params 3] {
#         set pix [VP1 -project $lat $lon 0]
#         if { [lindex $pix 2] <= 0 } {
#            puts stderr "problem"
#         }
#         lappend v [lindex $pix 0] [lindex $pix 1]
#      }
#      puts $file "[lrange $params 0 1] -1 { $v } [lrange $params 4 end]"

      #----- Si il y a des coordonnees

      if { [llength [lindex $params 3]] } {

         #----- Enregistrer les parametres specifiques (polices, images)

         switch -regexp [lindex $params 0] {
            "text|head|dist|valu|vert" {
               set item [lindex $params 6]
               puts $file "font create $item -family [font actual $item -family] -weight [font actual $item -weight]\
                  -size -[font actual $item -size] -slant [font actual $item -slant] -underline [font actual $item -underline]\
                  -overstrike [font actual $item -overstrike]"
               regsub -all "\n"  $params "\\n" params
               }
            "imag"  {
               set item [lindex $params 4]
               puts $file "image create photo $item -file [lindex [$item configure -file] end]"
               }
         }

         #----- Remplacement du viewport par son index

         if { [set vp [lindex $params 2]]!=-1 } {
            lset params 2 [lsearch -exact [Page::Registered $Data(Frame) Viewport] $vp]
         }
         puts $file $params
      }
   }

   close $file
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::FileLoad>
# Creation : Septembre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Lire un fichier de dessin.
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <File>   : Path du fichier
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::FileLoad { Frame File } {
   global GDefs
   variable Error
   variable Data
   variable Resources

   if { $File == "" } {
      return
   }

   set file [open $File r]

   gets $file version

   if { $version!="Drawing 3.01" && $version!="Drawing 3.1"} {
      Dialog::Error .drawing $Error(File)
      return
   }

   Drawing::Clear $Frame

   gets $file Data(NoItem)

   while { ![eof $file] } {

      gets $file params

      if { $params != "" } {
         switch [lindex $params 0] {
            "font"  { eval $params ; lappend Resources(Font)  [lindex $params 2] }
            "image" { eval $params ; lappend Resources(Image) [lindex $params 3] }
            default { #----- Remplacement de l'index par son viewport

                      if { [lindex $params 0]=="text" } {
                         set params [lreplace $params 5 5 [join [lindex $params 5]]]
                      }
                      if { [set idx [lindex $params 2]]!=-1 } {
                         set vps [Page::Registered $Frame Viewport]
                         if { ![llength $vps] } {
                            continue
                         }
                         if { $idx>=[llength $vps] } {
                            set vp [lindex $vps end]
                         } else {
                            set vp [lindex $vps $idx]
                         }
                         lset params 2 $vp
                      }
                      lappend Data(Params) $params
                    }
         }
      }
   }

   set Data(Params$Frame) $Data(Params)

   Drawing::Insert
   Drawing::ItemSel $Frame
   Drawing::UpdateItems $Frame
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::InitBitm>
# Creation : Septembre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialise le mode "Bitmap"
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::InitBitm { } {
   variable Current
   variable Data

   set Current(Color)   [lindex $Current(Params) 4]
   set Current(Bitmap)  [lindex $Current(Params) 5]

   .drawing.params.bitmap.lbl.opt configure -bitmap $Current(Bitmap)
   pack .drawing.params.color -side top -fill x
   pack .drawing.params.bitmap -side top -fill both -expand true
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::InitImag>
# Creation : Mai 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialise le mode "Image"
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::InitImag { } {
   variable Current
   variable Data

   set Current(Image)   [lindex $Current(Params) 4]

   .drawing.params.image.lbl.opt configure -image $Current(Image)
   pack .drawing.params.image -side top -fill both -expand true
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::InitDist>
# Creation : Septembre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialise le mode "Distance"
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::InitDist { } {
   variable Current
   variable Data

   set Current(Color)   [lindex $Current(Params) 4]
   set Current(Width)   [lindex $Current(Params) 5]
   set Current(Font)    [lindex $Current(Params) 6]
   set Current(Nautic)  [lindex $Current(Params) 7]

   Drawing::InitFont $Current(Font)

   .drawing.params.width.opt.menu invoke $Current(Width)
   pack .drawing.params.color .drawing.params.width .drawing.params.font .drawing.params.nau -side top -fill x
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::InitHead>
# Creation : Septembre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialise le mode "Compas"
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::InitHead { } {
   variable Current
   variable Data

   set Current(Color)   [lindex $Current(Params) 4]
   set Current(Width)   [lindex $Current(Params) 5]
   set Current(Font)    [lindex $Current(Params) 6]

   Drawing::InitFont $Current(Font)

   .drawing.params.width.opt.menu invoke $Current(Width)
   pack .drawing.params.color .drawing.params.width .drawing.params.font -side top -fill x
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::InitFont>
# Creation : Juillet 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialise la police de caractere courante
#
# Parametres :
#  <Font>    : Identificateur de la police
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::InitFont { Font } {
   variable Resources

   if { [lsearch -exact $Resources(Font) $Font]==-1 } {
      font create $Font -family courier -size -12

      set old [lindex $Resources(Font) end]
      lappend Resources(Font) $Font

      if { $old=="" } {
         set old  XFont18
      }
      font configure $Font -family [font actual $old -family] -weight [font actual $old -weight] \
         -size [font actual $old -size] -slant [font actual $old -slant] -underline [font actual $old -underline] \
         -overstrike [font actual $old -overstrike]
   }
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::InitLine>
# Creation : Novembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialise le mode "Line"
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::InitLine { } {
   variable Current
   variable Data

   set Current(Color)   [lindex $Current(Params) 4]
   set Current(Width)   [lindex $Current(Params) 5]
   set Current(Line)    [lindex $Current(Params) 6]
   set Current(Arrow)   [lindex $Current(Params) 7]

   .drawing.params.width.opt.menu  invoke $Current(Width)
   .drawing.params.type.opt.menu   invoke $Current(Line)
   .drawing.params.arrow.opt.menu  invoke [lsearch -exact "none first last both" $Current(Arrow)]
   pack .drawing.params.color .drawing.params.arrow .drawing.params.width .drawing.params.type -side top -fill x
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::InitPoly>
# Creation : Novembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialise le mode "Polygon"
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::InitPoly { } {
   variable Current
   variable Data

   set Current(Color)   [lindex $Current(Params) 4]
   set Current(Width)   [lindex $Current(Params) 5]
   set Current(Line)    [lindex $Current(Params) 6]
   set Current(Pattern) [lindex $Current(Params) 7]
   set Current(Fill)    [lindex $Current(Params) 8]
   set Current(Info)    [lindex $Current(Params) 9]

   .drawing.params.width.opt.menu             invoke $Current(Width)
   .drawing.params.type.opt.menu              invoke $Current(Line)
   IcoMenu::Set .drawing.params.pattern.opt $Current(Pattern)
   pack .drawing.params.color .drawing.params.fill .drawing.params.width .drawing.params.type .drawing.params.pattern .drawing.params.info -side top -fill x
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::InitRect>
# Creation : Novembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialise le mode "Rectangle"
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::InitRect { } {
   variable Current
   variable Data

   set Current(Color)   [lindex $Current(Params) 4]
   set Current(Width)   [lindex $Current(Params) 5]
   set Current(Line)    [lindex $Current(Params) 6]
   set Current(Pattern) [lindex $Current(Params) 7]
   set Current(Fill)    [lindex $Current(Params) 8]
   set Current(Info)    [lindex $Current(Params) 9]

   .drawing.params.width.opt.menu             invoke $Current(Width)
   .drawing.params.type.opt.menu              invoke $Current(Line)
   IcoMenu::Set .drawing.params.pattern.opt $Current(Pattern)

   pack .drawing.params.color .drawing.params.fill .drawing.params.width .drawing.params.pattern .drawing.params.info -side top -fill x
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::InitStrk>
# Creation : Novembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialise le mode "Stroke"
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::InitStrk { } {
   variable Current
   variable Data

   set Current(Color)   [lindex $Current(Params) 4]
   set Current(Width)   [lindex $Current(Params) 5]

   .drawing.params.width.opt.menu  invoke $Current(Width)
   pack .drawing.params.color .drawing.params.width -side top -fill x
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::InitText>
# Creation : Novembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialise le mode "text"
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::InitText { } {
   variable Current
   variable Data

   set Current(Color)   [lindex $Current(Params) 4]
   set Current(Text)    [lindex $Current(Params) 5]
   set Current(Font)    [lindex $Current(Params) 6]
   set Current(Angle)   [lindex $Current(Params) 7]

   Drawing::InitFont $Current(Font)

   pack .drawing.params.color .drawing.params.font .drawing.params.angle -side top -fill x
   pack .drawing.params.text -side top -fill both -expand true
   .drawing.params.text.lbl.opt delete 0.0 end
   .drawing.params.text.lbl.opt insert 0.0 $Current(Text)
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::InitValu>
# Creation : Septembre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialise le mode Pointage
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::InitValu { } {
   variable Current
   variable Data

   set Current(Color) [lindex $Current(Params) 4]
   set Current(Date)  [lindex $Current(Params) 5]
   set Current(Font)  [lindex $Current(Params) 6]
   set Current(Grid)  [lindex $Current(Params) 7]
   set Current(Coord) [lindex $Current(Params) 8]

   Drawing::InitFont $Current(Font)

   pack .drawing.params.color .drawing.params.font .drawing.params.date .drawing.params.coord .drawing.params.grid -side top -fill x
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::InitStream>
# Creation : Juin 2005 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialise le mode "Stream"
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::InitStream { } {
   variable Current
   variable Data

   set Current(Color)   [lindex $Current(Params) 4]
   set Current(Width)   [lindex $Current(Params) 5]
   set Current(Step)    [lindex $Current(Params) 6]
   set Current(Res)     [lindex $Current(Params) 7]

   .drawing.params.width.opt.menu  invoke $Current(Width)
   pack .drawing.params.color .drawing.params.width -side top -fill x
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::InitVert>
# Creation : Avril 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialise le mode "vert"
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::InitVert { } {
   variable Current
   variable Data

   set Current(Color)   [lindex $Current(Params) 4]
   set Current(Text)    [lindex $Current(Params) 5]
   set Current(Font)    [lindex $Current(Params) 6]

   Drawing::InitFont $Current(Font)

   pack .drawing.params.color .drawing.params.font .drawing.params.line -side top -fill x
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::ItemAdd>
# Creation : Novembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Ajoute un nouvel item.
#
# Parametres :
#  <Frame>   : Identificateur du canvas
#  <Type>    : Type d'item
#  <Coords>  : Coordonnees
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::ItemAdd { Frame Type { Coords { } } } {
   variable Data
   variable Current

   incr Data(NoItem)
   set no [format "%03i" $Data(NoItem)]

   if { !$Data(GeoRef) } {
      set vp -1
      set mode static
   } else {
      set vp $Viewport::Data(VP)
      set mode georef
   }

   switch $Type {
      "imag" { set params [list $Type $no $vp $Coords $Current(Image) ""] }
      "bitm" { set params [list $Type $no $vp $Coords $Current(Color) $Current(Bitmap) ""] }
      "text" { set params [list $Type $no $vp $Coords $Current(Color) "" font$no $Current(Angle)] }
      "line" { set params [list $Type $no $vp $Coords $Current(Color) $Current(Width) $Current(Line) $Current(Arrow)] }
      "poly" { set params [list $Type $no $vp $Coords $Current(Color) $Current(Width) $Current(Line) $Current(Pattern) $Current(Fill) $Current(Info)] }
      "rect" { set params [list $Type $no $vp $Coords $Current(Color) $Current(Width) $Current(Line) $Current(Pattern) $Current(Fill)] }
      "oval" { set params [list $Type $no $vp $Coords $Current(Color) $Current(Width) $Current(Line) $Current(Pattern) $Current(Fill)] }
      "circ" { set params [list $Type $no $vp $Coords $Current(Color) $Current(Width) $Current(Line) $Current(Pattern) $Current(Fill)] }
      "dist" { set params [list $Type $no $vp $Coords $Current(Color) $Current(Width) font$no $Current(Nautic)] }
      "head" { set params [list $Type $no $vp $Coords $Current(Color) $Current(Width) font$no] }
      "valu" { set params [list $Type $no $vp $Coords $Current(Color) $Current(Date) font$no $Current(Grid) $Current(Coord)] }
      "strm" { set params [list $Type $no $vp $Coords $Current(Color) $Current(Width) $Current(Step) $Current(Res) $Current(Coord)] }
      "vert" { set params [list $Type $no $vp $Coords $Current(Color) "0 10000" font$no] }
      "strk" { set params [list $Type $no $vp $Coords $Current(Color) $Current(Width)] }
   }
   set Current(Vertex) $Coords
   lappend Data(Params) $params
   set Data(Params$Frame) $Data(Params)

   $Data(Tab).items.list.box insert end "$no $Type $mode"
   $Data(Tab).items.list.box selection clear 0 end
   $Data(Tab).items.list.box selection set end

   Drawing::ItemSel $Frame

   if { ![llength $Coords] && $Page::Data(ToolMode)!="Drawing" } {
      SPI::ToolMode Drawing Draw
   }
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::ItemDel>
# Creation : Novembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Efface un item.
#
# Parametres :
#  <Frame>  : Identificateur du canvas
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::ItemDel { Frame } {
   variable Data
   variable Current

   #----- Si un item est selectionnee

   if { $Current(Item)!="" } {

      $Frame.page.canvas delete $Data(Tag)$Current(NoItem) D$Data(Tag)$Current(NoItem) VERTEXFOLLOW DVERTEXFOLLOW

      $Data(Tab).items.list.box delete $Current(Item)
      $Data(Tab).head.del configure -state disabled

      set Data(Params$Frame) [set Data(Params)  [lreplace $Data(Params) $Current(Item) $Current(Item)]]
      set Current(Vertex) ""
      set Current(Mode)   ""

      if { $Current(Item)>=[$Data(Tab).items.list.box size] } {
         set Current(Item)   ""
      } else {
         $Data(Tab).items.list.box selection set $Current(Item)
      }

      Drawing::ItemSel $Frame
   }
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::ItemIndex>
# Creation : Juillet 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Deplace un objet dans la liste.
#
# Parametres :
#  <Frame>   : Identificateur de page
#  <Dir>     : Direction du deplacement
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::ItemIndex { Frame Dir } {
   variable Data
   variable Current

   set Current(Item) [$Data(Tab).items.list.box curselection]

   if { $Current(Item) != "" } {

      switch -- $Dir {
         "F" { set item [expr [$Data(Tab).items.list.box index end]-1] }
         "B" { set item 0 }
         default { set item [expr $Current(Item) + $Dir] }
      }

      if { $item<0 || $item>[expr [$Data(Tab).items.list.box index end]-1] } {
         return
      }

      set params [lindex $Data(Params) $Current(Item)]
      set line   [$Data(Tab).items.list.box get $Current(Item)]

      set Data(Params) [lreplace $Data(Params) $Current(Item) $Current(Item)]
      $Data(Tab).items.list.box delete $Current(Item)

      set Current(Item) $item

      set Data(Params) [linsert $Data(Params) $Current(Item) $params]
      set Data(Params$Frame) $Data(Params)
      $Data(Tab).items.list.box insert $Current(Item) $line

      $Data(Tab).items.list.box selection clear 0 end
      $Data(Tab).items.list.box selection set $Current(Item)

      Drawing::UpdateItems $Frame
   }
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::ItemSel>
# Creation : Novembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialise les parametres pour le type d'item selectionne.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::ItemSel { Frame } {
   global GDefs
   variable Data
   variable Current

   if { ![winfo exists .drawing] } {
      return
   }

   pack forget .drawing.params.color .drawing.params.angle .drawing.params.fill .drawing.params.width .drawing.params.font\
      .drawing.params.text .drawing.params.line .drawing.params.arrow .drawing.params.pattern \
      .drawing.params.type .drawing.params.bitmap .drawing.params.image .drawing.params.coord \
       .drawing.params.grid .drawing.params.date .drawing.params.nau .drawing.params.info

   set Current(Item) [$Data(Tab).items.list.box curselection]

   if { $Current(Item)!="" } {

      #----- Definir les parametres courants selon les parametres de l'item

      set Current(Params)  [lindex $Data(Params) $Current(Item)]
      set Current(Mode)    [lindex $Current(Params) 0]
      set Current(NoItem)  [lindex $Current(Params) 1]
      set vp               [lindex $Current(Params) 2]
      set Current(Vertex)  [lindex $Current(Params) 3]

      #----- Verifier le mode de la primitive a changer si necessaire
      #      vp = -1  , primitive statique
      #      vp != -1 , primitive georeferencee

      if { $vp==-1 && $Data(GeoRef) } {
         set Data(GeoRef) 0
         DrawMode $Frame 0
      } elseif { $vp!=-1 && !$Data(GeoRef) } {
         set Data(GeoRef) 1
         DrawMode $Frame 0
      }

      #----- Mettre disponible les bonnes options

      switch $Current(Mode) {
         "imag" { Drawing::InitImag   }
         "bitm" { Drawing::InitBitm   }
         "text" { Drawing::InitText   }
         "line" { Drawing::InitLine   }
         "poly" { Drawing::InitPoly   }
         "rect" { Drawing::InitRect   }
         "oval" { Drawing::InitRect   }
         "circ" { Drawing::InitRect   }
         "dist" { Drawing::InitDist   }
         "head" { Drawing::InitHead   }
         "valu" { Drawing::InitValu   }
         "strm" { Drawing::InitStream }
         "vert" { Drawing::InitVert   }
         "strk" { Drawing::InitStrk   }
      }
      $Data(Tab).head.del configure -state normal
      if { $Current(Color)!="" } {
         .drawing.params.color.opt configure -fg $Current(Color)
         $Data(Tab).items.list.box itemconfigure $Current(Item) -fg $Current(Color)
      } else {
         .drawing.params.color.opt configure -fg $GDefs(ColorFrame)
      }
      if { $Current(Fill)!="" } {
         .drawing.params.fill.opt   configure -fg $Current(Fill)
      } else {
         .drawing.params.fill.opt configure -fg $GDefs(ColorFrame)
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::SetIndex>
# Creation : Novembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Changer les informations de la liste de primitives.
#
# Parametres :
#  <Frame>   : Identificateur de page
#  <Index>   : Index dans la liste des parametres
#  <Value>   : Valeur du parametre
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::SetIndex { Frame Index Value } {
   variable Data
   variable Current

   lset Current(Params) $Index $Value
   lset Data(Params) $Current(Item) $Current(Params)
   set Data(Params$Frame) $Data(Params)
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::SetArrow>
# Creation : Novembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Changer le type de fleches.
#
# Parametres :
#  <Frame>  : Identificateur du canvas
#  <Arrow>   : Type de fleche
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::SetArrow { Frame Arrow } {
   variable Data
   variable Current

   Drawing::SetIndex $Frame 7 $Arrow

   catch { $Frame.page.canvas itemconf $Data(Tag)$Current(NoItem) -arrow $Arrow }
   catch { $Frame.page.canvas itemconf VERTEXFOLLOW -arrow $Arrow }
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::SetBitmap>
# Creation : Novembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Changer le bitmap.
#
# Parametres :
#  <Frame>  : Identificateur du canvas
#  <bitmap>  : Bitmap
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::SetBitmap { Frame Bitmap } {
   global   GDefs
   variable Data
   variable Current
   variable Resources

   set Current(Bitmap)  @$GDefs(Dir)/share/bitmap/[lindex $Resources(Bitmap) $Bitmap]

   Drawing::SetIndex $Frame 5 $Current(Bitmap)

   catch { $Frame.page.canvas itemconf $Data(Tag)$Current(NoItem) -bitmap $Current(Bitmap) }
   catch { $Frame.page.canvas itemconf VERTEXFOLLOW -bitmap $Current(Bitmap) }
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::SetImage>
# Creation : Mai 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Changer l'image.
#
# Parametres :
#  <Frame>  : Identificateur du canvas
#  <Image>   : Image
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::SetImage { Frame Image } {
   global   GDefs
   variable Data
   variable Current
   variable Resources

   Drawing::SetIndex $Frame 4 $Current(Image)

   catch { $Frame.page.canvas itemconf $Data(Tag)$Current(NoItem) -image $Current(Image) }
   catch { $Frame.page.canvas itemconf VERTEXFOLLOW -image $Current(Image) }
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::SetColor>
# Creation : Novembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Changer la couleur du dessin.
#
# Parametres :
#  <Frame>  : Identificateur du canvas
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::SetColor { Frame } {
   variable Data
   variable Current

   Drawing::SetIndex $Frame 4 $Current(Color)

   $Data(Tab).items.list.box itemconfigure $Current(Item) -fg $Current(Color)

   catch { $Frame.page.canvas itemconf $Data(Tag)$Current(NoItem) -outline $Current(Color) }
   catch { $Frame.page.canvas itemconf $Data(Tag)$Current(NoItem) -foreground $Current(Color) }
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::SetFill>
# Creation : Novembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Changer la couleur de remplissage du dessin.
#
# Parametres :
#  <Frame>  : Identificateur du canvas
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::SetFill { Frame } {
   variable Data
   variable Current

   Drawing::SetIndex $Frame 8 $Current(Fill)

   catch { $Frame.page.canvas itemconf $Data(Tag)$Current(NoItem) -fill $Current(Fill) }
   catch { $Frame.page.canvas itemconf VERTEXFOLLOW -fill $Current(Color) }
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::SetInfo>
# Creation : Fevrier 2014 - J.P. Gauthier - CMC/CMOE
#
# But      : Activer/Desactiver l'affichage de l'info.
#
# Parametres :
#  <Frame>  : Identificateur du canvas
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::SetInfo { Frame } {
   variable Data
   variable Current

   Drawing::SetIndex $Frame 9 $Current(Info)

   Drawing::Draw $Frame $Current(Params)
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::SetValu>
# Creation : Juillet 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Activer/Desactiver l'affichage des coordonnes latlon et grille.
#
# Parametres :
#  <Frame>  : Identificateur du canvas
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::SetValu { Frame } {
   variable Data
   variable Current

   Drawing::SetIndex $Frame 5 $Current(Date)
   Drawing::SetIndex $Frame 7 $Current(Grid)
   Drawing::SetIndex $Frame 8 $Current(Coord)

   Drawing::Draw $Frame $Current(Params)
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::SetLine>
# Creation : Novembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Changer le type de ligne du dessin (droit ou courbe).
#
# Parametres :
#  <Frame>  : Identificateur du canvas
#  <Line>    : Type de ligne selectionnee
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::SetLine { Frame Line } {
   variable Data
   variable Current

   Drawing::SetIndex $Frame 6 $Line

   catch { $Frame.page.canvas itemconf $Data(Tag)$Current(NoItem) -smooth $Line }
   catch { $Frame.page.canvas itemconf VERTEXFOLLOW -smooth $Line }
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::SetNautic>
# Creation : Novembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Changer le mode de distance pour miles nautiques
#
# Parametres :
#  <Frame>   : Identificateur du canvas
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::SetNautic { Frame } {
   variable Data
   variable Current

   Drawing::SetIndex $Frame 7 $Current(Nautic)
   Drawing::UpdateItems $Frame
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::SetPattern>
# Creation : Novembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Changer le type de pattern de remplissage du dessin.
#
# Parametres :
#  <Frame>  : Identificateur du canvas
#  <Pattern> : Type de rempplissage selectionnee
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::SetPattern { Frame Pattern } {
   global   GDefs
   variable Data
   variable Current
   variable Resources

   Drawing::SetIndex $Frame 7 $Pattern

   catch { $Frame.page.canvas itemconf $Data(Tag)$Current(NoItem) -stipple $Pattern }
   catch { $Frame.page.canvas itemconf VERTEXFOLLOW -stipple $Pattern }
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::SetFont>
# Creation : Juillet 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Changer les parametres de la police.
#
# Parametres :
#  <Frame>  : Identificateur du canvas
#  <Font>    : Police
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::SetFont { Frame Font } {
   variable Data
   variable Current

   Drawing::SetIndex $Frame 6 $Font

   catch { $Frame.page.canvas itemconf $Data(Tag)$Current(NoItem) -font $Font }
   catch { $Frame.page.canvas itemconf VERTEXFOLLOW -font $Font }
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::SetAngle>
# Creation : Octobre 2005 - J.P. Gauthier - CMC/CMOE
#
# But      : Changer l'angle des textes.
#
# Parametres :
#  <Frame>  : Identificateur du canvas
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::SetAngle { Frame Angle } {
   variable Data
   variable Current

   Drawing::SetIndex $Frame 7 $Angle

   catch { $Frame.page.canvas itemconf $Data(Tag)$Current(NoItem) -angle $Angle }
   catch { $Frame.page.canvas itemconf VERTEXFOLLOW -angle $Angle }
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::SetText>
# Creation : Novembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Changer le texte.
#
# Parametres :
#  <Frame>   : Identificateur de page
#  <Text>    : Texte a ecrire
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::SetText { Frame Text } {
   variable Data
   variable Current

   Drawing::SetIndex $Frame 5 $Text
   Drawing::UpdateItems $Frame
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::SetWidth>
# Creation : Novembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Changer la largeur des segments du dessin.
#
# Parametres :
#  <Frame>  : Identificateur du canvas
#  <Width>   : Largeur de la ligne
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::SetWidth { Frame Width } {
   variable Data
   variable Current

   Drawing::SetIndex $Frame 5 $Width
   Drawing::UpdateItems $Frame
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::UpdateItems>
# Creation : Novembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectue le refresh de tout ce que l'usager a dessine sur les
#            projection
#
# Parametres :
#  <Frames>  : Identificateur de page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::UpdateItems { Frame } {
   variable Data

   if { [info exist Drawing::Data(Params$Frame)] } {
      foreach params $Data(Params$Frame) {
         Drawing::Draw $Frame $params
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::VertexAdd>
# Creation : Novembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Ajouter un point a la liste des points definissant le dessin.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <VP>      : Identificateur du Viewport
#  <X>       : Coordonnee X du nouveau Point
#  <Y>       : Coordonnee Y du nouveau Point
#  <args>    : Type de coordonnees
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::VertexAdd { Frame VP X Y args } {
   variable Data
   variable Current

   if { $Current(Mode)=="" } {
      return
   }

   if { $Frame!=$Data(Frame) } {
      return
   }

   if { $Data(GeoRef) } {

      if { $VP==-1 } {
         return
      }

      #----- Si les coordonnees sont en pixel

      if { $args!=0 } {
         set loc [$VP -unproject $X $Y]
         set X [lindex $loc 0]
         set Y [lindex $loc 1]
      }

      #----- Si le vertex est valide on l'ajoute a la liste

      if { $X!=-999 && $Y!=-999 } {
         set Current(Vertex) "$Current(Vertex) $X $Y $Current(Elev)"
         lset Current(Params) 2 $VP
      }
   } else {
      Page::Snap $Frame X Y
      set Current(Vertex) "$Current(Vertex) $X $Y"
   }

   if { !$Data(Stroke) && $Current(Mode)=="strk" } {
      set Current(Vertex) "$Current(Vertex) |"
   }
   lset Current(Params) 3 $Current(Vertex)
   lset Data(Params) $Current(Item) $Current(Params)
   set Data(Params$Frame) $Data(Params)

   Draw $Frame $Current(Params)
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::VertexDelete>
# Creation : Novembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectuer l'affichage de ce qui a ete dessiner jusqu'a maintenant.
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

proc Drawing::VertexDelete { Frame VP } {
   variable Data
   variable Current

   if { $Current(Mode) == "" } {
      return
   }

   if { $Frame!=$Data(Frame) } {
      return
   }

   if { $Data(GeoRef) && $VP==-1 } {
      return
   }

   if { !$Data(GeoRef) && $VP!=-1 } {
      return
   }

   if { [llength $Current(Vertex)] > 0 } {

      #----- On recherche le separtateur de segments

      if { [lsearch -exact $Current(Vertex) "|"]!=-1 } {
         set Current(Vertex) "[join [lrange [split $Current(Vertex) "|"] 0 end-2] "|"]|"
      } else {

         #----- On supprime les coordonnees du dernier points

         set Current(Vertex) [lreplace $Current(Vertex) end-1 end]

         if { $Data(GeoRef) } {

            #----- On supprime la coordonnee Z

            set Current(Vertex) [lreplace $Current(Vertex) end end]

            if { [llength $Current(Vertex)] > 0 } {
               set Current(Elev) [lindex $Current(Vertex) end]
            }
         }
      }

      lset Current(Params) 3 $Current(Vertex)
      lset Data(Params) $Current(Item) $Current(Params)
      set Data(Params$Frame) $Data(Params)
      Draw $Frame $Current(Params)
   }
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::VertexFollow>
# Creation : Novembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche une ligne entre le dernier vertex creer et la position du
#            curseur de la souris.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <VP>      : Identificateur du Viewport
#  <X>       : Coordonnee X de la souris
#  <Y>       : Coordonnee Y de la souris
#  <Scan>    : Mode Scan
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::VertexFollow { Frame VP X Y Scan } {
   variable Data
   variable Current
   variable Resources

   if { $Data(GeoRef) && $VP==-1 } {
      return
   }

   if { $Frame!=$Data(Frame) } {
      return
   }

   $Frame.page.canvas delete VERTEXFOLLOW DVERTEXFOLLOW

   if { $Data(GeoRef) } {

      #----- Transformer en latlon pour passer le tout a la projection

      set loc [$VP -unproject $X $Y]
      set x [lindex $loc 0]
      set y [lindex $loc 1]
      if { $x==-999 || $y==-999 } {
         return
      }

      set tmp        "$x $y $Current(Elev)"
      set tmplist     $Current(Vertex)
      lappend tmplist $x $y $Current(Elev)
      $Frame.page.canvas create line [expr $x-1] [expr $y-1] [expr $x+1] [expr $y+1] -fill $Current(Color) \
         -tags "PAGE VERTEXFOLLOW"
   } else {
      $Frame.page.canvas create line [expr $X-1] [expr $Y-1] [expr $X+1] [expr $Y+1] -fill $Current(Color) \
         -tags "PAGE VERTEXFOLLOW"

      set VP          -1

      if { !$Scan } {
         Page::Snap $Frame X Y
      }
      set tmp        "$X $Y"
      set tmplist     $Current(Vertex)
      lappend tmplist $X $Y
   }

   switch $Current(Mode) {
     "imag" { Drawing::DrawImag $Frame $VP $tmp \
              $Current(Image) VERTEXFOLLOW }
     "bitm" { Drawing::DrawBitm $Frame $VP $tmp \
              $Current(Color) $Current(Bitmap) VERTEXFOLLOW }
     "line" { Drawing::DrawLine $Frame $VP $tmplist \
              $Current(Color) $Current(Width) $Current(Line) $Current(Arrow) VERTEXFOLLOW }
     "poly" { Drawing::DrawPoly $Frame $VP $tmplist \
              $Current(Color) $Current(Width) $Current(Line) $Current(Pattern) $Current(Fill) $Current(Info) VERTEXFOLLOW }
     "rect" { Drawing::DrawRect $Frame $VP $tmplist \
              $Current(Color) $Current(Width)  $Current(Pattern) $Current(Fill) $Current(Info) VERTEXFOLLOW }
     "oval" { Drawing::DrawOval $Frame $VP $tmplist \
              $Current(Color) $Current(Width) $Current(Pattern) $Current(Fill) $Current(Info) VERTEXFOLLOW 0 }
     "circ" { Drawing::DrawOval $Frame $VP $tmplist \
              $Current(Color) $Current(Width) $Current(Pattern) $Current(Fill) $Current(Info) VERTEXFOLLOW 1 }
     "text" { Drawing::DrawText $Frame $VP $tmp \
              $Current(Color) $Current(Text) $Current(Font) $Current(Angle) VERTEXFOLLOW }
     "dist" { Drawing::DrawDist $Frame $VP $tmplist \
              $Current(Color) $Current(Width) $Current(Font) $Current(Nautic) VERTEXFOLLOW }
     "head" { Drawing::DrawHead $Frame $VP $tmplist \
              $Current(Color) $Current(Width) $Current(Font) VERTEXFOLLOW }
     "valu" { Drawing::DrawValu $Frame $VP $tmp \
              $Current(Color) $Current(Date) $Current(Font) $Current(Grid) $Current(Coord) VERTEXFOLLOW }
     "strm" { Drawing::DrawStream $Frame $VP $tmp \
              $Current(Color) $Current(Width) $Current(Step) $Current(Res) VERTEXFOLLOW }
     "vert" { Drawing::DrawVert $Frame $VP $tmp \
              $Current(Color) $Current(Text) $Current(Font) VERTEXFOLLOW }
     "strk" { if { $Scan } { set Drawing::Data(Stroke) 1 ; Drawing::VertexAdd $Frame $VP $X $Y $Data(GeoRef) ; set Drawing::Data(Stroke) 0 } }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Drawing::VertexSet>
# Creation : Novembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Reassigner les coordonnees des objets deplaces dans
#            la liste des parametres des objets.
#
# Parametres :
#  <Frame>  : Identificateur du canvas
#  <Tag>     : Tag associe aux objets
#  <No>      : Numero d'objet dans la liste
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Drawing::VertexSet { Frame Tag No } {
   variable Data
   variable Current

   if { [llength $Data(Params)] } {

      #----- Recuperer tout les items correspondants

      set items [$Frame.page.canvas find withtag $Tag]

      #----- Recuperer les coordonnees

      set vertex ""
      foreach item $items {
         append vertex " [$Frame.page.canvas coords $item]"
      }

      #----- Recuperer la liste de ses parametres

      set no 0
      foreach params $Data(Params) {
         if { [lindex $params 1]==$No } {
            break
         }
         incr no
      }

      #----- Modifier la liste des parametres

      lset params 3 $vertex
      lset Data(Params) $no $params
      set Data(Params$Frame) $Data(Params)

      #----- Modiffier l'item courant si necessaire

      if { $Current(NoItem)==$No } {
         set Current(Vertex) $vertex
         set Current(Params) $params
      }
   }
}

#------------------------------------------------------------------------------
# Nom      : <Drawing::Parse>
# Creation : Fevrier 2004 - J.P. Gauthier - CMC/CMOE -
#
# But     : Parcourir la liste de primitives pour y assigner le bon viewport
#
# Parametres :
#
# Remarques :
#     - Cette fonction sert a reassigner les viewports apres une lecture de
#       layout sauvegardee
#
#-------------------------------------------------------------------------------

proc Drawing::Parse { } {
   variable Data

   for { set i 0 } { $i < [llength $Data(Params)] } { incr i } {
      if { [set vp [lindex [lindex $Data(Params) $i] 2]]!="" } {
        eval eval lset Drawing::Data(Params) $i 2 \$$vp
      }
   }
   Log::Print DEBUG "$Drawing::Data(Params)"
}

#------------------------------------------------------------------------------
# Nom      : <Drawing::Write>
# Creation : Novembre 2003 - J.P. Gauthier - CMC/CMOE -
#
# But     : Engeristrer les parametres des dessins dans un fichier Layout
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <File>   : Identificateur de Fichier
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Drawing::Write { Frame File } {
   variable Data

   if { [info exists Drawing::Data(Params$Frame)] && [llength $Data(Params$Frame)] } {
      puts $File "   #-----  Primitives de dessin"
      puts $File ""
      puts $File "   Drawing::Clear \$Frame"
      puts $File ""
      puts $File "   set Drawing::Data(NoItem)     $Drawing::Data(NoItem)"
      puts $File "   set Drawing::Data(Params\$Frame) \[list \\"
      foreach param $Data(Params$Frame) {
         if { [set vp [lindex $param 2]]!="" && $vp!=-1 } {
            lset param 2 $Viewport::Data(Alias$vp)
         }
         puts $File "      \{ $param \} \\"
      }

      puts $File "   \]"

      puts $File ""
      puts $File "   for { set i 0 } { \$i < \[llength \$Drawing::Data(Params\$Frame)\] } { incr i } {"
      puts $File "      if { \[set vp \[lindex \[lindex \$Drawing::Data(Params\$Frame) \$i\] 2\]\]!=\"\" && \$vp!=-1 } {"
      puts $File "        eval eval lset Drawing::Data(Params\$Frame) \$i 2 \\\$\$vp"
      puts $File "      }"
      puts $File "   }"


      puts $File "   set Drawing::Resources(Font)  \[list $Drawing::Resources(Font)\]"
      puts $File ""

      foreach font $Drawing::Resources(Font) {
         puts $File "   font create $font -family \"[font actual $font -family]\" -weight [font actual $font -weight] -size [font actual $font -size] -slant [font actual $font -slant] -underline [font actual $font -underline] -overstrike [font actual $font -overstrike]"
      }

      foreach params $Drawing::Data(Params$Frame) {
         if { [lindex $params 0]=="imag" } {
            set item [lindex $params 4]
            puts $File "   image create photo $item -file [lindex [$item configure -file] end]"
         }
      }
      puts $File ""
      puts $File "   Drawing::PageActivate \$Frame"
      puts $File ""
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Drawing::AsProject>
# Creation : Aout 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Sauvegarder l'etat de l'outils dans un projet SPI.
#
# Parametres :
#   <File>   : Descripteur de fichier ou ecrire les commandes
#
# Remarques :
#    - Le fichier est deja ouvert, il suffit d'y ecrire les commandes a executer
#      afin de re-instaurer l'outils dans son etat actuel.
#
#-------------------------------------------------------------------------------

proc Drawing::AsProject { File } {
   variable Data
   variable Param

   if { [winfo exists .drawing] } {
      puts $File "#----- Tool: Drawing\n"
      puts $File "set Drawing::Param(Dock)   $Param(Dock)"
      puts $File "set Drawing::Param(Geom)   [winfo geometry .drawing]"
      puts $File "Drawing::Window"
      puts $File "\n"
   }
}
