#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie de projection.
# Fichier  : Page.tcl
# Creation : Mai 2000 - J.P. Gauthier - CMC/CMOE
#
# Description:
#   -Definit un "widget" de projection contenant:
#
#       -Les projection
#       -Le menu contextuel de la projection (Boutton 3 sur la projection)
#       -Les constantes relatives a la projection
#       -Les fonctionnalites de zoom
#       -Les fonctionnalites de rotation (Normale et relative)
#       -La fonctionnalite de changement de mode (Manipulation/Selection/Dessin,Viewpoint/...)
#       -La fonctionnalite d'impression de la projection
#       -Les touches d'acces rapide de la configuration
#           -F1           Mode Zoom
#           -F2           Mode Camera
#           -F9           Parametres
#           -Print Screen Impression
#   -Ce qui doit etre fournit par l'utilisateur de ce "widget"
#
#       -Fonction Page::UpdateItems { Frame }
#          Effectue le reaffichage (refresh) des items du canvas apres
#          manipulation de la projection.
#
#       -Fonction Page::UpdateCommand { Frame }
#          Effectue le reaffichage (refresh) des items du canvas apres
#          un changement de parametres.
#
#       -Fonctions de selection.
#          Si on ne desire utiliser cette fonctionnalite, on doit creer les fonctions
#          suivantes et activer l'option de selection du menu de projection qui est descativee
#          par defaut ou space est le mode courant (Page::Data(ToolMode)).
#
#             "space"::DrawInit { Frame VP }    #;Bouton 1 presse
#             "space"::Draw     { Frame VP      #;Bouton 1 + Mouvement
#             "space"::DrawDone { Frame VP }    #;Bouton 1 relache
#             "space"::MoveInit { Frame VP }    #;Bouton 2 presse
#             "space"::Move     { Frame VP }    #;Bouton 2 + Mouvement
#             "space"::MoveDone { Frame VP }    #;Bouton 2 Relache
#
#       -Fonctions de dessin.
#          Si on ne desire utiliser cette fonctionnalite, on doit creer les fonctions
#          suivantes et activer l'option de selection du menu de projection qui est descativee
#          par defaut ou space est le mode courant (Page::Data(DrawMode)).
#
#             "space"::VertexAdd    { Frame VP X Y }      #;Bouton 1 presse
#             "space"::VertexFollow { Frame VP X Y Scan } #;Mouvement && Bouton 1 presse + Mouvement (Scan)
#             "space"::VertexDelete { Frame VP }          #;Bouton 2 presse
#
# Fonctions:
#
#    Page::ActiveFull      { Type Frame Id Full }
#    Page::ActiveMove      { Type Frame Id X Y }
#    Page::ActiveScale     { Type Frame Id X Y U }
#    Page::ActiveTag       { Type Frame Id X Y { Args {} } }
#    Page::ActiveUnTag     { Type Frame Id } }
#    Page::ActiveUnWrap    { Frame Object }
#    Page::ActiveUnWrapper { Type Frame Id }
#    Page::ActiveWrap      { Frame Object }
#    Page::ActiveWrapper   { Type Frame Id X0 Y0 X1 Y1 }
#
#    Page::Assign          { Frame Ids { Force 0 } }
#    Page::UnAssign        { Frame { Ids "" } { Force 0 } }

#    Page::Canvas          { Frame }
#    Page::CanvasHeight    { Frame }
#    Page::CanvasWidth     { Frame }
#    Page::Create          { Frame Width Height }
#    Page::CursorInfo      { Frame X Y Info { Graph "" } }
#    Page::Destroy         { Frame }
#    Page::MaskItem        { Frame }
#    Page::ModeCam         { Frame VP }
#    Page::ModeData        { Frame VP }
#    Page::ModeDraw        { Frame VP }
#    Page::ModeSelect      { Mode { Frames {} } }
#    Page::ModeNone        { Frame VP }
#    Page::ModeZoom        { Frame VP }
#    Page::ParamApply      { Frame Apply }
#    Page::ParamFrame      { Frame Apply }
#    Page::Resize          { Frame }
#    Page::Register        { Frame Type Id }
#    Page::Registered      { Frame Type Id }
#    Page::UnRegister      { Frame Type Id }
#    Page::Scale           { Frame { Incr 0 } }
#    Page::ScaleSet        { Frame }
#    Page::Size            { Frame Width Height { Intrusion -1 } }
#    Page::Update          { Frame { VP True } }
#    Page::UpdateCommand   { Frame }
#    Page::UpdateItems     { Frame }
#    Page::SnapGrid        { Frame }
#    Page::Snap            { Frame X Y }
#    Page::SnapRef         { Frame X Y }
#    Page::Play            { }
#    Page::Pause           { }
#
#    VertexAdd    { Frame VP X Y }
#    VertexDelete { Frame VP }
#    VertexFollow { Frame VP X Y Scan }
#
# Binding utilisees
#       -canvas <ButtonPress-1>
#       -canvas <B1-Motion>
#       -canvas <ButtonRelease-1>
#       -canvas <ButtonPress-2>
#       -canvas <B2-Motion>
#       -canvas <ButtonRelease-2>
#       -canvas <Configure>
#       -canvas <Button-3>
#       - .     <Key-F1>
#       - .     <Key-F2>
#       - .     <Key-F9>
#       - .     <Key-Print>
#===============================================================================

package provide Page 5.1

catch { SPI::Splash "Loading Canvas Package Page 5.1" }

package require OpenGL
package require TkGeoEER $env(SPI_VERSION)

package require PrintBox
package require ProjCam
package require Convert
package require TabFrame
package require ColorBox
package require ColorBar
package require DataBar
package require FontBox
package require MapBox

namespace eval Page {
   variable Data
   variable Param
   variable Lbl

   set Param(Grid)      0           ;#Grille de snap
   set Param(Snap)      5           ;#intervalle de snap
   set Param(WidgetOpt) False       ;#Affichage des widgets
   set Param(Intrusion) 0           ;#Allow for viewport intrusions
   set Param(Square)    0           ;#Dimension du curseur carre

   #----- Definitions des constantes pour la projection

   set Data(Frames)     {}          ;#Liste des Pages
   set Data(Frame)      ""          ;#Frame courant
   set Data(Canvas)     ""          ;#Canvas courant
   set Data(VP)         ""          ;#Viewport courant
   set Data(Scale)      100         ;#Scaling du frame courant
   set Data(Full)       1           ;#Scaling continue du frame courant

   set Data(Width)      0           ;#Largeur de la page courante
   set Data(Height)     0           ;#Hauteur de la page courante
   set Data(X)          0           ;#Coordonnee de base
   set Data(Y)          0           ;#Coordonnee de base

   set Data(Previous)   ""          ;#Previously active tag

   set Data(Coord)      ""          ;#Coordonnees a l'interieur de la page
   set Data(CoordUnit)  DEG         ;#Type d'unite des coordonnees
   set Data(CoordLink)  False       ;#Curseur commun
   set Data(CoordPrec)  10          ;#Precision des coordonnees
   set Data(Value)      ""          ;#Valeur a l'interieur de la page
   set Data(Altitude)   ""          ;#Altitude a l'interieur de la page
   set Data(Mode)       Zoom        ;#Mode de manipulation
   set Data(ToolMode)   SPI         ;#Outils de selection courant
   set Data(DrawMode)   ""          ;#Outils de dessin courant

   #----- Definitions des labels

   set Lbl(Grid)           { "Grille d'attache"    "Snap grid" }
   set Lbl(Display)        { "Afficher"  "Display" }
   set Lbl(Size)           { "Dimensions" "Size" }
   set Lbl(Intrusion)      { "Intrusion dans les vues" "Allow viewport intrusions" }
   set Lbl(Widget)         { "Affichage des boutons de contrôle" "Display control widgets" }
   set Lbl(Others)         { "Autres" "Others" }
}

proc VertexAdd    { Frame VP X Y } { }
proc VertexDelete { Frame VP }     { }
proc VertexFollow { Frame VP X Y Scan } { }

proc Page::Activate { Frame { Force 0 } } {
}

#----------------------------------------------------------------------------
# Nom      : <Page::ActiveTag>
# Creation : Mars 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Selectionner un objet pour manipulation (deplacement/scaling)
#
# Parametres :
#   <Type>   : Type d'object (namespace)
#   <Frame>  : Indentificateur de Page
#   <Id>     : Indentificateur de l'object
#   <X>      : Coordonnee en X du deplacement
#   <Y>      : Coordonnee en Y du deplacement
#   <Args>   : Commande speciales
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Page::ActiveTag { Type Frame Id X Y { Args {} } } {

   glrender -xexpose -1 -resolution 2

   Page::Activate $Frame
   if { [llength $Args] } {
      ${Type}::Activate $Frame $Id $Args
   } else {
      ${Type}::Activate $Frame $Id
   }
   catch { $Frame.page.canvas itemconfigure PAGE$Id -transparency 50 }

   Page::SnapRef $Frame $X $Y
}

#----------------------------------------------------------------------------
# Nom      : <Page::ActiveUnTag>
# Creation : Mars 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : De-selectionner un objet pour manipulation (deplacement/scaling)
#
# Parametres :
#   <Type>   : Type d'object (namespace)
#   <Frame>  : Indentificateur de Page
#   <Id>     : Indentificateur de l'object
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Page::ActiveUnTag { Type Frame Id } {

   glrender -xexpose 1 -resolution 1
   catch { $Frame.page.canvas itemconfigure PAGE$Id -transparency 100 }
   eval Page::ActiveFull $Type $Frame $Id \$${Type}::Data(Full$Id)
}

#----------------------------------------------------------------------------
# Nom      : <Page::ActiveWrapper>
# Creation : Mars 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Ajoute les boutons de manipulation des objets de pages
#
# Parametres :
#   <Type>   : Type d'object (namespace)
#   <Frame>  : Indentificateur de Page
#   <Id>     : Indentificateur de l'object
#   <X0>     : Coordonnee en X inferieure
#   <Y0>     : Coordonnee en Y inferieure
#   <X1>     : Coordonnee en X superieure
#   <Y1>     : Coordonnee en Y superieure
#   <Args>   : Commande speciales
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Page::ActiveWrapper { Type Frame Id X0 Y0 X1 Y1 { Args { } } } {
   global GDefs
   variable Data

   set tag PAGE$Id

   label $Frame.bs$tag -bg $GDefs(ColorFrame) -bitmap @$GDefs(Dir)/share/bitmap/cvscale.xbm -cursor sizing -bd 1 -relief raised
   label $Frame.bm$tag -bg $GDefs(ColorFrame) -bitmap @$GDefs(Dir)/share/bitmap/cvmove.xbm -cursor fleur -bd 1 -relief raised
   if { [llength $Args] } {
      checkbutton $Frame.bf$tag -bg $GDefs(ColorFrame) -bitmap @$GDefs(Dir)/share/bitmap/cvfull.xbm -cursor hand1 -bd 1 \
         -indicatoron false -variable ${Type}::Data(Full$Id) -onvalue 1 -offvalue 0 -command "Page::Activate $Frame; ${Type}::Activate $Frame $Id $Args; Page::ActiveFull $Type $Frame $Id \$${Type}::Data(Full$Id)"
      button $Frame.bd$tag -bg $GDefs(ColorFrame) -bitmap @$GDefs(Dir)/share/bitmap/cvdel.xbm -cursor pirate -bd 1 -relief raised -command "Page::Activate $Frame; ${Type}::Destroy $Frame $Id $Args"
   } else {
      checkbutton $Frame.bf$tag -bg $GDefs(ColorFrame) -bitmap @$GDefs(Dir)/share/bitmap/cvfull.xbm -cursor hand1 -bd 1 \
         -indicatoron false -variable ${Type}::Data(Full$Id) -onvalue 1 -offvalue 0 -command "Page::Activate $Frame;  ${Type}::Activate $Frame $Id; Page::ActiveFull $Type $Frame $Id \$${Type}::Data(Full$Id)"
      button $Frame.bd$tag -bg $GDefs(ColorFrame) -bitmap @$GDefs(Dir)/share/bitmap/cvdel.xbm -cursor pirate -bd 1 -relief raised -command "Page::Activate $Frame; ${Type}::Destroy $Frame $Id"
   }

   $Frame.page.canvas create window $X1 $Y1           -window $Frame.bs$tag -anchor se -tags "BS$tag NOPRINT"
   $Frame.page.canvas create window [expr $X1-11] $Y1 -window $Frame.bm$tag -anchor se -tags "BM$tag NOPRINT"
   $Frame.page.canvas create window [expr $X1-22] $Y1 -window $Frame.bf$tag -anchor se -tags "BF$tag NOPRINT"
   $Frame.page.canvas create window $X1 $Y0           -window $Frame.bd$tag -anchor ne -tags "BD$tag NOPRINT"

   #----- bindings de fullscreen
   eval Page::ActiveFull $Type $Frame $Id \$${Type}::Data(Full$Id)

   #----- bindings de placement des bouttons
   Shape::BindWidget $Frame.page.canvas $Id

   #----- bindings de deplacement
   bind $Frame.bm$tag <ButtonPress-1>   "Page::ActiveTag   $Type $Frame $Id %X %Y $Args"
   bind $Frame.bm$tag <B1-Motion>       "Page::ActiveMove  $Type $Frame $Id %X %Y"
   bind $Frame.bm$tag <ButtonRelease-1> "Page::ActiveUnTag $Type $Frame $Id"

   #----- bindings de scaling
   bind $Frame.bs$tag <ButtonPress-1>   "Page::ActiveTag   $Type $Frame $Id %X %Y $Args"
   bind $Frame.bs$tag <B1-Motion>       "Page::ActiveScale $Type $Frame $Id %X %Y 1"
   bind $Frame.bs$tag <ButtonRelease-1> "Page::ActiveUnTag $Type $Frame $Id; Page::ActiveScale ${Type} $Frame $Id %X %Y 0"

}

#----------------------------------------------------------------------------
# Nom      : <Page::ActiveWrapper>
# Creation : Mars 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Supprime les boutons de manipulation des objets de pages
#
# Parametres :
#   <Type>   : Type d'object (namespace)
#   <Frame>  : Indentificateur de Page
#   <Id>     : Indentificateur de l'object
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Page::ActiveUnWrapper { Type Frame Id } {
   variable Data

   upvar #0 ${Type}::Data(Full$Id) full

   set tag PAGE$Id

   destroy $Frame.bs$tag $Frame.bm$tag $Frame.bf$tag $Frame.bd$tag $Frame.sc$tag
   $Frame.page.canvas delete BS$tag BM$tag BF$tag BD$tag SC$tag

   if { $full } {
      bind $Frame.page.canvas <Configure> ""
   }
}

#----------------------------------------------------------------------------
# Nom      : <Page::ActiveWrap>
# Creation : Mars 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Active les boutons de manipulation des objets de pages
#
# Parametres :
#   <Frame>  : Indentificateur de Page
#   <Object> : Indentificateur de l'object
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Page::ActiveWrap { Frame Object } {
   variable Data

   if { $Object!="" } {
      set tag PAGE[string map { . "" } $Object]

      catch {
         $Frame.bs$tag configure -fg red
         $Frame.bm$tag configure -fg red
         $Frame.bf$tag configure -fg red
         $Frame.bd$tag configure -fg red
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Page::ActiveUnWrap>
# Creation : Mars 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Desactive les boutons de manipulation des objets de pages
#
# Parametres :
#   <Frame>  : Indentificateur de Page
#   <Object> : Indentificateur de l'object
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Page::ActiveUnWrap { Frame Object } {
   global   GDefs
   variable Data

   set tag PAGE[string map { . "" } $Object]

   catch {
      $Frame.bs$tag configure -fg black
      $Frame.bm$tag configure -fg black
      $Frame.bf$tag configure -fg black
      $Frame.bd$tag configure -fg black
   }
}

#----------------------------------------------------------------------------
# Nom      : <Page::ActiveMove>
# Creation : Octobre 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Deplacement d'un object actif
#
# Parametres :
#   <Type>   : Type d'object (namespace)
#   <Frame>  : Indentificateur de Page
#   <Id>     : Indentificateur de l'object
#   <X>      : Coordonnee en X du deplacement
#   <Y>      : Coordonnee en Y du deplacement
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Page::ActiveMove { Type Frame Id X Y } {
   variable Param
   variable Data
   variable Map

   set x [winfo rootx $Frame.page.canvas]
   set y [winfo rooty $Frame.page.canvas]

   if { $X<=$x || $Y<=$y || $X>=[expr [winfo width $Frame.page.canvas]+$x] || $Y>=[expr [winfo height $Frame.page.canvas]+$y] } {
      return
   }

   set tag PAGE$Id

   set X [$Frame.page.canvas canvasx $X $Param(Snap)]
   set Y [$Frame.page.canvas canvasy $Y $Param(Snap)]

   set dx [expr $X-$Page::Data(X)]
   set dy [expr $Y-$Page::Data(Y)]

   $Frame.page.canvas move $tag   $dx $dy
   $Frame.page.canvas move BS$tag $dx $dy
   $Frame.page.canvas move BM$tag $dx $dy
   $Frame.page.canvas move BF$tag $dx $dy
   $Frame.page.canvas move BD$tag $dx $dy
   $Frame.page.canvas move SC$tag $dx $dy

   set Page::Data(X) $X
   set Page::Data(Y) $Y

   set ${Type}::Data(Full$Id) 0
   eval set ${Type}::Data(X$Id) [expr \$${Type}::Data(X$Id)+$dx]
   eval set ${Type}::Data(Y$Id) [expr \$${Type}::Data(Y$Id)+$dy]

   if { $Type=="Viewport" } {
      Viewport::MoveDepend $Frame $Id $dx $dy
   }

   Page::Update $Frame False
}

#----------------------------------------------------------------------------
# Nom      : <Page::ActiveScale>
# Creation : Janvier 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Redimenssionement d'un viewport
#
# Parametres :
#   <Type>   : Type d'object (namespace)
#   <Frame>  : Identificateur de Page
#   <GR>     : Indentificateur du Graph
#   <X>      : Coordonnee en X du deplacement
#   <Y>      : Coordonnee en Y du deplacement
#   <U>      : Mise a jour des coordonnees
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Page::ActiveScale { Type Frame Id X Y U } {
   variable Param
   variable Data

   set x [winfo rootx $Frame.page.canvas]
   set y [winfo rooty $Frame.page.canvas]

   if { $X<=$x || $Y<=$y || $X>=[expr [winfo width $Frame.page.canvas]+$x] || $Y>=[expr [winfo height $Frame.page.canvas]+$y] } {
      return
   }

   set X [$Frame.page.canvas canvasx [expr $X-$x] $Param(Snap)]
   set Y [$Frame.page.canvas canvasy [expr $Y-$y] $Param(Snap)]

   set ${Type}::Data(Full$Id) 0
   eval ${Type}::Resize $Frame $Id -999 -999 $X $Y $U
}

#----------------------------------------------------------------------------
# Nom      : <Page::ActiveFull>
# Creation : Decembre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Redimenssionner la page au maximum.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <Id>      : Identificateur de l'objet
#  <Full>    : Full ???
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Page::ActiveFull { Type Frame Id Full } {
   variable Map

   if { $Full } {
      #----- We've 1 pixel offset ???? and it's in X axis for HW mode and Y for Mesa ???
      if { [glrender -direct] } {
         bind $Frame.page.canvas <Configure> "update idletasks; ${Type}::Resize $Frame $Id 1 0 \[Page::CanvasWidth $Frame\] \[expr \[Page::CanvasHeight $Frame\]-1\] 0"
         eval ${Type}::Resize $Frame $Id 1 0 [Page::CanvasWidth $Frame] [expr [Page::CanvasHeight $Frame]-1] 0
      } else {
         bind $Frame.page.canvas <Configure> "update idletasks; ${Type}::Resize $Frame $Id 0 1 \[expr \[Page::CanvasWidth $Frame\]-1\] \[Page::CanvasHeight $Frame\] 0"
         eval ${Type}::Resize $Frame $Id 0 1 [expr [Page::CanvasWidth $Frame]-1] [Page::CanvasHeight $Frame] 0
      }
   } else {
      bind $Frame.page.canvas <Configure> ""
   }
}

#----------------------------------------------------------------------------
# Nom      : <Page::Assign>
# Creation : Mars 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Assigner une donnee a une page (projection)
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <Ids>     : Identificateurs des donnees
#  <Force>   : Forcer le reaffichage
#
# Retour:
#  <ok>      : Ajout effectue
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Page::Assign { Frame Ids { Force 0 } } {
   variable Data

   set idx 0

   if { [info exists Viewport::Data(Data$Frame)] } {
      foreach id $Ids {
         if { [set idx [lsearch -exact $Viewport::Data(Data$Frame) $id]]==-1 } {
            lappend Viewport::Data(Data$Frame) $id

            #----- Definir les tags aux emplacements

            if { [ogrlayer is $id] } {
            } elseif { [gdalband  is $id] } {
            }
            set Force [expr $Force==-1?0:1]
         }
      }
      if { $Force } {
         Page::Update $Frame
      }
   }
   if { $idx==-1 } {
      return 1
   } else {
      return 0
   }
}

#----------------------------------------------------------------------------
# Nom      : <Page::UnAssign>
# Creation : Mars 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Desassigner une donnee a une page (projection)
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <Ids>     : Identificateurs des donnees
#
# Retour:
#  <ok>      : Suppression effectue
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Page::UnAssign { Frame { Ids "" } { Force 0 } } {
   variable Data

   set ok 0

   if { [info exists Viewport::Data(Data$Frame)] } {
      if { $Ids=="" } {
         set Viewport::Data(Data$Frame) {}
         set ok 1
      } else {
         foreach id $Ids {
            if { [set idx [lsearch -exact $Viewport::Data(Data$Frame) $id]]!=-1 } {
               set Viewport::Data(Data$Frame) [lreplace $Viewport::Data(Data$Frame) $idx $idx]
               set ok 1
            }
         }
      }
   }

   if { ($ok && $Force!=-1) || $Force==1 } {
      Page::Update $Frame
   }

   return $ok
}

#----------------------------------------------------------------------------
# Nom      : <Page::CanvasWidth>
# Creation : Septembre 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Determiner la largeur de la page
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Page::CanvasWidth { Frame } {
   variable Data

   if { $Data(Width$Frame)<=0 } {
      return [winfo width $Frame.page.canvas]
   } else {
      return [expr int($Data(Width$Frame)*$Data(Scale)/100.0)]
   }
}

#----------------------------------------------------------------------------
# Nom      : <Page::CanvasHeight>
# Creation : Septembre 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Determiner la hauteur de la page
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Page::CanvasHeight { Frame } {
   variable Data

   if { $Data(Height$Frame)<=0 } {
      return [winfo height $Frame.page.canvas]
   } else {
      return [expr int($Data(Height$Frame)*$Data(Scale)/100.0)]
   }
}

#----------------------------------------------------------------------------
# Nom      : <Page::Canvast>
# Creation : Septembre 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Determiner la hauteur de la page
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Page::Canvas { Frame } {
   variable Data

   return $Frame.page.canvas
}

#----------------------------------------------------------------------------
# Nom      : <Page::Create>
# Creation : Octobre 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer une nouvelle page.
#
# Parametres :
#   <Frame>  : Indentification de Page
#   <Width>  : Largeur de la page
#   <Height> : Hauteur de la page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Page::Create { Frame Width Height { Active True } } {
   variable Data

   . configure -cursor watch
   update idletasks

   set Data(Active$Frame)  $Active    ;#Mode Active (Manipulation in place)
   set Data(Scale$Frame)   100
   set Data(Lock$Frame)    False
   set Data(Full$Frame)    False
   set Data(Width$Frame)   $Width
   set Data(Height$Frame)  $Height
   set Data(Data$Frame)    {}
   set Data(L$Frame)       1

   lappend Data(Frames) $Frame

   if { $Data(Frame)=="" && $Data(Active$Frame) } {
      set Data(Frame)   $Frame
      set Data(Canvas)  $Frame.page.canvas
   }

   #----- For some misterious reasons, we have to force aliasing once cause FSAA does not
   #      always kicks in. We then remove it at the end cause it's too slow in software mode
   glrender -fsaa $OpenGL::Param(FSAA)
   glrender -aliasing 1

   #----- Creation de la fenetre de projection

   frame $Frame -relief sunken -bd 1
      frame $Frame.page
      pack $Frame.page -side top -fill both -expand true

      #----- Mode dynamique ou page

      if { $Width<=0 && $Height<=0 } {
         glcanvas $Frame.page.canvas -relief flat -closeenough 0.0 -bd 0 -bg white -width 0 -height 0
         pack $Frame.page.canvas -side top -fill both -expand true
      } else {
         glcanvas $Frame.page.canvas -relief flat -closeenough 0.0 -bd 0 -bg white -width $Width -height $Height \
            -yscrollcommand "$Frame.page.v set" -xscrollcommand "$Frame.page.h set" -scrollregion "0 0 [expr $Width-1] [expr $Height-1]"
         pack $Frame.page.canvas -side top -anchor nw
         scrollbar $Frame.page.v -orient vertical   -command "$Frame.page.canvas yview" -bd 1 -width 10
         scrollbar $Frame.page.h -orient horizontal -command "$Frame.page.canvas xview" -bd 1 -width 10
         bind $Frame <Configure> "if { \$Page::Data(Full$Frame) } { Page::ScaleSet $Frame }"
      }
   pack $Frame -side top -fill both -expand true

   bind $Frame.page.canvas      <Button> "Page::Activate $Frame"
   bind [winfo toplevel $Frame] <Visibility> { if { "%s"!="VisibilityUnobscured" } { glrender -xexpose -1 } else { glrender -xexpose 1 } }

   Viewport::Setup $Frame
   CVText::Init $Frame.page.canvas

   update idletasks
   glrender -aliasing 0
   . configure -cursor left_ptr
}

#----------------------------------------------------------------------------
# Nom      : <Page::CursorInfo>
# Creation : Mai 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Afficher une bulle d'information
#
# Parametres :
#   <Frame>  : Indentificateur de Page
#   <X>      : Coordonnee en X
#   <Y>      : Coordonnee en Y
#   <Info>   : Information sur la localisation a afficher
#   <Graph>  : Graph a inserer dans la bulle
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Page::CursorInfo { Frame X Y Info { Graph "" } } {

   if { $Info!="" } {
      if { ![llength [$Frame.page.canvas find withtag PAGECURSORINFO]] } {
         $Frame.page.canvas create polygon -999 -999  -tags "PAGECURSORINFO PAGECURSORINFOSHADOW" -fill black -transparency 50
         $Frame.page.canvas create polygon -999 -999  -tags "PAGECURSORINFO PAGECURSORINFOFRAME" -fill white -outline black -width 1
         $Frame.page.canvas create text -999 -999 -tags "PAGECURSORINFO PAGECURSORINFOSTUFF PAGECURSORINFOTEXT" -text 333 -font XFont12 -fill black -anchor sw

         $Frame.page.canvas create graph -x -999 -y -999 -command "" -bd 0 -fg black -bg white -fill white -font XFont10\
            -tags "PAGECURSORINFO PAGECURSORINFOSTUFF PAGECURSORINFOGRAPH" -legend True -xlegend -30 -ylegend -25 -bdlegend 0

         $Frame.page.canvas bind PAGECURSORINFO <Enter> "Page::CursorInfo $Frame %X %Y \"\""
      }

      if { [graphitem is $Graph] } {
         $Frame.page.canvas itemconfigure PAGECURSORINFOGRAPH -item $Graph -x [expr $X+10] -y [expr $Y+2] -width 210 -height 100
      } else {
         $Frame.page.canvas itemconfigure PAGECURSORINFOGRAPH -item {} -x -999 -y -999 -width 0 -height 0
      }

      $Frame.page.canvas coords PAGECURSORINFOTEXT [expr $X+10] $Y
      $Frame.page.canvas itemconfigure PAGECURSORINFOTEXT -text $Info

      set bbox [$Frame.page.canvas bbox PAGECURSORINFOSTUFF]
      set x0 [lindex $bbox 0]
      set y0 [lindex $bbox 1]
      set x1 [lindex $bbox 2]
      set y1 [lindex $bbox 3]
      set coords [list [expr $x0-2] [expr $y1+2] [expr $x0-2] $y0 $x0 [expr $y0-2] $x1 [expr $y0-2] [expr $x1+2] $y0 \
                       [expr $x1+2] $y1 $x1 [expr $y1+2] [expr $x0-2] [expr $y1+2]]
      $Frame.page.canvas coords PAGECURSORINFOFRAME $coords

      set coords [list [expr $x0+3] [expr $y1+7] [expr $x0+3] [expr $y0+5] [expr $x0+5] [expr $y0+3] [expr $x1+5] [expr $y0+3] [expr $x1+7] [expr $y0+5] \
                       [expr $x1+7] [expr $y1+5] [expr $x1+5] [expr $y1+7] [expr $x0+3] [expr $y1+7]]
      $Frame.page.canvas coords PAGECURSORINFOSHADOW $coords
      $Frame.page.canvas raise PAGECURSORINFO
      $Frame.page.canvas raise PAGECURSORINFOSTUFF
   } else {
      $Frame.page.canvas coords PAGECURSORINFOSHADOW -999 -999
      $Frame.page.canvas coords PAGECURSORINFOFRAME -999 -999
      $Frame.page.canvas coords PAGECURSORINFOTEXT -999 -999
      $Frame.page.canvas itemconfigure PAGECURSORINFOGRAPH -item {} -x -999 -y -999 -width 0 -height 0

   }
   update idletasks
}

#----------------------------------------------------------------------------
# Nom      : <Page::Destroy>
# Creation : Octobre 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Supprimer une page et toutes sees structures associees
#
# Parametres :
#   <Frame>  : Indentificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Page::Destroy { Frame } {
   variable Data

   if { [llength [set objects [Page::Registered $Frame]]] } {

      foreach object $objects {
         if { [set object [lindex [split [lindex $object 0] ":"] 0]]!="" } {
            eval [lindex [split [lindex $object 0] ":"] 0]::Destroy $Frame
         }
      }

      Viewport::UnSetup $Frame

      if { [set idx [lsearch -exact $Data(Frames) $Frame]]<0 } {
         return
      }
      set Data(Frames) [lreplace $Data(Frames) $idx $idx]

      #----- Activer la page precedente
      if { $Data(Frame)==$Frame } {
         set Data(Canvas)  ""
         set Data(Frame)  ""
         Page::Activate [lindex $Data(Frames) end] True
      }


      unset Data(Scale$Frame)
      unset Data(Full$Frame)
      unset Data(Width$Frame)
      unset Data(Height$Frame)

      #----- Suppression des objects

      destroy $Frame
   }
}

#----------------------------------------------------------------------------
# Nom      : <Page::MaskItem>
# Creation : Novembre 2010 - J.P. Gauthier - CMC/CMOE
#
# But      : Assigner les items de masquage intrusif dans la page aux vues
#
# Parametres :
#   <Frame>  : Indentificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Page::MaskItem { Frame } {
   variable Param

   set items [$Frame.page.canvas find withtag VPINTRUDE]

   foreach vp [Page::Registered $Frame Viewport] {
      $Frame.page.canvas itemconfigure $vp -maskwidth $Param(Intrusion) -maskitem $items
   }
}

#----------------------------------------------------------------------------
# Nom      : <Page::ModeCam>
# Creation : Aout 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Entrer dans le mode point de vue de la souris.
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

proc Page::ModeCam { Frame VP } {
   variable Data

   set c $Frame.page.canvas

   #----- Evenements de rotation
   $c bind PAGE$VP <ButtonPress-1>   "$c config -cursor hand1; Viewport::Activate $Frame $VP; Viewport::RotateInit $Frame $VP \[$c canvasx %x\] \[$c canvasy %y\]"
   $c bind PAGE$VP <B1-Motion>       "Viewport::RotateDo $Frame $VP \[$c canvasx %x\] \[$c canvasy %y\]"
   $c bind PAGE$VP <ButtonRelease-1> " $c config -cursor left_ptr; Viewport::RotateDone $Frame $VP True"

   $c bind PAGE$VP <Double-ButtonRelease-1> "set Viewport::Map(Grabbed) \[clock click -milliseconds\]; $c config -cursor left_ptr; Viewport::GoTo $Frame \$Viewport::Map(LatCursor) \$Viewport::Map(LonCursor) -2"

   #----- Evenements de manipulation sur l'axe X-Y
   $c bind PAGE$VP <ButtonPress-2>       "$c config -cursor exchange; Viewport::Activate $Frame $VP; ProjCam::XYInit $Frame $Frame $VP \[$c canvasx %x\] \[$c canvasy %y\]"
   $c bind PAGE$VP <B2-Motion>           "ProjCam::XYDo $Frame $Frame $VP \[$c canvasx %x\] \[$c canvasy %y\]"
   $c bind PAGE$VP <ButtonRelease-2>     "$c config -cursor left_ptr; ProjCam::XYDone $Frame $Frame True"

   $c config -cursor left_ptr
}

#----------------------------------------------------------------------------
# Nom      : <Page::ModeFly>
# Creation : Aout 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Entrer dans le survol de la souris.
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

proc Page::ModeFly { Frame VP } {
   variable Data

   set c $Frame.page.canvas

   #----- Evenements de manipulation sur l'axe X-Y
   $c bind PAGE$VP <ButtonPress-1>       "Viewport::Activate $Frame $VP; ProjCam::XYInit $Frame $Frame $VP \[$c canvasx %x\] \[$c canvasy %y\]; $c config -cursor fleur"
   $c bind PAGE$VP <B1-Motion>           "ProjCam::ToDo $Frame $Frame $VP \[$c canvasx %x\] \[$c canvasy %y\]"
#   $c bind PAGE$VP <ButtonRelease-1>     "ProjCam::XYDone $Frame $Frame"

   #----- Evenements de rotation
   $c bind PAGE$VP <ButtonPress-2>       ""
   $c bind PAGE$VP <B2-Motion>           ""
   $c bind PAGE$VP <ButtonRelease-2>     ""

   #----- Evenements de manipulation sur l'axe Z
   $c bind PAGE$VP <B1-B2-ButtonPress>   ""
   $c bind PAGE$VP <B1-B2-Motion>        ""
   $c bind PAGE$VP <B1-B2-ButtonRelease> ""


   bind $c <ButtonPress-4>   { set ProjCam::Param(Speed) [expr $ProjCam::Param(Speed)==0?0.00001:$ProjCam::Param(Speed)*2.0] }
   bind $c <ButtonPress-5>   { set ProjCam::Param(Speed) [expr $ProjCam::Param(Speed)<=0.00001?0.0:$ProjCam::Param(Speed)/2.0] }
#   bind . <KP_Add>      { set ProjCam::Param(Speed) [expr $ProjCam::Param(Speed)+1.0] }
#   bind . <KP_Subtract> { set ProjCam::Param(Speed) [expr $ProjCam::Param(Speed)-1.0] }

   ProjCam::Fly $Frame $Frame
   $c config -cursor left_ptr
}

#----------------------------------------------------------------------------
# Nom      : <Page::ModeData>
# Creation : Avril 1998 - J.P. Gauthier - CMC/CMOE
#
# But      : Enlever le mode zoom de la souris.
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

proc Page::ModeData { Frame VP } {
   variable Data

   set c $Frame.page.canvas

   #----- Evenements de selection de region
   $c bind PAGE$VP <ButtonPress-1>   "Viewport::Activate $Frame $VP; \$Page::Data(ToolMode)::DrawInit $Frame $VP"
   $c bind PAGE$VP <B1-Motion>       "$c config -cursor sizing; if { \[Viewport::Follow $Frame $VP \[$c canvasx %x\] \[$c canvasy %y\]\] } { \$Page::Data(ToolMode)::Draw $Frame $VP }"
   $c bind PAGE$VP <ButtonRelease-1> "$c config -cursor hand1; \$Page::Data(ToolMode)::DrawDone $Frame $VP"

   #----- Evenement de deplacement de region (A implementer chez le client)
   $c bind PAGE$VP <ButtonPress-2>   "set Viewport::Map(Lat0) \$Viewport::Map(LatCursor); set Viewport::Map(Lon0) \$Viewport::Map(LonCursor); $c config -cursor fleur; Viewport::Activate $Frame $VP; \$Page::Data(ToolMode)::MoveInit $Frame $VP"
   $c bind PAGE$VP <B2-Motion>       "set Viewport::Map(LatD) \[expr \$Viewport::Map(LatCursor)-\$Viewport::Map(Lat0)\]; set Viewport::Map(LonD) \[expr \$Viewport::Map(LonCursor)-\$Viewport::Map(Lon0)\]; if { \[Viewport::Follow $Frame $VP \[$c canvasx %x\] \[$c canvasy %y\]\] } { \$Page::Data(ToolMode)::Move $Frame $VP }"
   $c bind PAGE$VP <ButtonRelease-2> "set Viewport::Map(LatD) 0.0; set Viewport::Map(LonD) 0.0; $c config -cursor hand1; \$Page::Data(ToolMode)::MoveDone $Frame $VP"

   #----- Evenements de selection de region
   $c bind PAGE$VP <Control-ButtonPress-1>   "Viewport::Activate $Frame $VP; \$Page::Data(ToolMode)::DrawInit $Frame $VP Control"
   $c bind PAGE$VP <Control-B1-Motion>       "$c config -cursor sizing; if { \[Viewport::Follow $Frame $VP \[$c canvasx %x\] \[$c canvasy %y\]\] } { \$Page::Data(ToolMode)::Draw $Frame $VP Control }"
   $c bind PAGE$VP <Control-ButtonRelease-1> "$c config -cursor hand1; \$Page::Data(ToolMode)::DrawDone $Frame $VP Control"

   #----- Evenement de deplacement de region (A implementer chez le client)
   $c bind PAGE$VP <Control-ButtonPress-2>   "set Viewport::Map(Lat0) \$Viewport::Map(LatCursor); set Viewport::Map(Lon0) \$Viewport::Map(LonCursor); $c config -cursor fleur; Viewport::Activate $Frame $VP; \$Page::Data(ToolMode)::MoveInit $Frame $VP Control"
   $c bind PAGE$VP <Control-B2-Motion>       "set Viewport::Map(LatD) \[expr \$Viewport::Map(LatCursor)-\$Viewport::Map(Lat0)\]; set Viewport::Map(LonD) \[expr \$Viewport::Map(LonCursor)-\$Viewport::Map(Lon0)\]; if { \[Viewport::Follow $Frame $VP \[$c canvasx %x\] \[$c canvasy %y\]\] } { \$Page::Data(ToolMode)::Move $Frame $VP Control }"
   $c bind PAGE$VP <Control-ButtonRelease-2> "set Viewport::Map(LatD) 0.0; set Viewport::Map(LonD) 0.0; $c config -cursor hand1; \$Page::Data(ToolMode)::MoveDone $Frame $VP Control"

   $c config -cursor hand1
}

#----------------------------------------------------------------------------
# Nom      : <Page::ModeDraw>
# Creation : Avril 1998 - J.P. Gauthier - CMC/CMOE
#
# But      : Enlever le mode zoom de la souris.
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

proc Page::ModeDraw { Frame VP } {
   variable Data

   set c $Frame.page.canvas

   $c bind PAGE$VP <Motion>          "if { \[Viewport::Follow $Frame $VP \[$c canvasx %x\] \[$c canvasy %y\]\] } { \$Page::Data(DrawMode)::VertexFollow $Frame $VP \[$c canvasx %x\] \[$c canvasy %y\] 0 }"

   #----- Evenements de creation de vertex
   $c bind PAGE$VP <ButtonPress-1>   "$c config -cursor pencil; Viewport::Activate $Frame $VP"
   $c bind PAGE$VP <B1-Motion>       "if { \[Viewport::Follow $Frame $VP \[$c canvasx %x\] \[$c canvasy %y\]\] } { \$Page::Data(DrawMode)::VertexFollow $Frame $VP \[$c canvasx %x\] \[$c canvasy %y\] 1 }"
   $c bind PAGE$VP <ButtonRelease-1> "$c config -cursor left_ptr; if { \[Viewport::Follow $Frame $VP \[$c canvasx %x\] \[$c canvasy %y\]\] } { \$Page::Data(DrawMode)::VertexAdd $Frame $VP \[$c canvasx %x\] \[$c canvasy %y\] }"

   #----- Evenement de suppression de vertex
   $c bind PAGE$VP <ButtonPress-2>   ""
   $c bind PAGE$VP <B2-Motion>       ""
   $c bind PAGE$VP <ButtonRelease-2> "\$Page::Data(DrawMode)::VertexDelete $Frame $VP"
}

#----------------------------------------------------------------------------
# Nom      : <Page::ModeSelect>
# Creation : Mai 1998 - J.P. Gauthier - CMC/CMOE
#
# But      : Changer la fonction des boutons de la souris.
#
# Parametres :
#   <Mode>   : Mode de manipulation (Zoom,Data,Cam,Draw)
#   <Frame>  : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Page::ModeSelect { Mode { Frames {} } } {
   variable Data

   set Data(Mode) $Mode

   if { [llength $Frames] } {
      set frames $Frames
   } else {
      set frames $Page::Data(Frames)
   }

   foreach frame $frames {

      Page::UpdateItems $frame

      if { [winfo exists [set c $frame.page.canvas]] } {

         bind $frame.page <ButtonPress-4>     "$frame.page.canvas yview scroll -1 units"
         bind $frame.page <ButtonPress-5>     "$frame.page.canvas yview scroll 1 units"

         #----- Suppression des evenement globaux

         bind $c <Motion>          ""
         bind $c <B1-Motion>       ""
         bind $c <ButtonPress-1>   ""
         bind $c <ButtonRelease-1> ""
         bind $c <ButtonPress-2>   ""
         bind $c <ButtonRelease-2> ""

         bind $c <ButtonPress-4>   "set Page::Data(L$frame) \[expr \$Page::Data(L$frame)+0.05\]; ProjCam::ZoomScroll $frame $frame \$Page::Data(VP) %x %y \[expr pow(2,\$Page::Data(L$frame))\] True"
         bind $c <ButtonPress-5>   "set Page::Data(L$frame) \[expr \$Page::Data(L$frame)-0.05\]; ProjCam::ZoomScroll $frame $frame \$Page::Data(VP) %x %y \[expr pow(2,\$Page::Data(L$frame))\] False"

         #----- Cas statique du mode Draw

         if { $Data(Mode)=="Draw" } {
            bind $c <Motion>          "\$Page::Data(DrawMode)::VertexFollow $frame -1 \[$c canvasx %x\] \[$c canvasy %y\] 0"
            bind $c <B1-Motion>       "\$Page::Data(DrawMode)::VertexFollow $frame -1 \[$c canvasx %x\] \[$c canvasy %y\] 1"
            bind $c <ButtonRelease-1> "\$Page::Data(DrawMode)::VertexAdd    $frame -1 \[$c canvasx %x\] \[$c canvasy %y\]"
            bind $c <ButtonRelease-2> "\$Page::Data(DrawMode)::VertexDelete $frame -1"
         }

         foreach vp [Page::Registered $frame Viewport] {

            $c bind PAGE$vp <Motion>                 "Viewport::Follow $frame $vp \[$c canvasx %x\] \[$c canvasy %y\]"
            $c bind PAGE$vp <B1-Motion>              ""
            $c bind PAGE$vp <ButtonPress-1>          ""
            $c bind PAGE$vp <ButtonRelease-1>        ""
            $c bind PAGE$vp <ButtonPress-2>          ""
            $c bind PAGE$vp <ButtonRelease-2>        ""
            $c bind PAGE$vp <B1-B2-ButtonPress>      ""
            $c bind PAGE$vp <B1-B2-Motion>           ""
            $c bind PAGE$vp <B1-B2-ButtonRelease>    ""
            $c bind PAGE$vp <Double-ButtonRelease-1> ""

            switch $Data(Mode) {
               Zoom  { Page::ModeZoom  $frame $vp }
               Data  { Page::ModeData  $frame $vp }
               Fly   { Page::ModeFly   $frame $vp }
               Cam   { Page::ModeCam   $frame $vp }
               Draw  { Page::ModeDraw  $frame $vp }
               None  { Page::ModeNone  $frame $vp }
               default {}
            }
         }
         $c delete VERTEXFOLLOW

         if { $Data(Mode)=="Mag" } {
            CVMagnifier::Create $c
         }
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Page::ModeNone>
# Creation : Mai 1998 - J.P. Gauthier - CMC/CMOE
#
# But      : Supprimer tout les mode de manipulations
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

proc Page::ModeNone { Frame VP } {
   variable Data

   set c $Frame.page.canvas

   #----- Suppression des evenement globaux

   bind $c <ButtonPress-1>   ""
   bind $c <Motion>          ""
   bind $c <ButtonRelease-2> ""

   foreach vp [Page::Registered $Frame Viewport] {

      $c bind PAGE$vp <ButtonPress-1>       ""
      $c bind PAGE$vp <B1-Motion>           ""
      $c bind PAGE$vp <ButtonRelease-1>     ""
      $c bind PAGE$vp <Motion>              ""
      $c bind PAGE$vp <ButtonPress-2>       ""
      $c bind PAGE$vp <B2-Motion>           ""
      $c bind PAGE$vp <ButtonRelease-2>     ""
      $c bind PAGE$vp <B1-B2-ButtonPress>   ""
      $c bind PAGE$vp <B1-B2-Motion>        ""
      $c bind PAGE$vp <B1-B2-ButtonRelease> ""
      $c bind PAGE$VP <ButtonPress-4>       ""
      $c bind PAGE$VP <ButtonPress-5>       ""
   }
}

#----------------------------------------------------------------------------
# Nom      : <Page::ModeZoom>
# Creation : Avril 1998 - J.P. Gauthier - CMC/CMOE
#
# But      : Entrer dans mode zoom de la souris.
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

proc Page::ModeZoom { Frame VP } {
   variable Data

   set c $Frame.page.canvas

   #----- Evenements de zoom
   $c bind PAGE$VP <ButtonPress-2>   "$c config -cursor crosshair; Viewport::Activate $Frame $VP; ProjCam::ZoomInit $Frame $VP \[$c canvasx %x\] \[$c canvasy %y\]"
   $c bind PAGE$VP <ButtonRelease-2> " ProjCam::ZoomIn $Frame $Frame $VP; $c config -cursor left_ptr"

   #----- Evenements de rotation
   $c bind PAGE$VP <ButtonPress-1>   "$c config -cursor hand1; Viewport::Activate $Frame $VP; Viewport::RotateInit $Frame $VP \[$c canvasx %x\] \[$c canvasy %y\]"
   $c bind PAGE$VP <B1-Motion>       "Viewport::RotateDo $Frame $VP \[$c canvasx %x\] \[$c canvasy %y\]"
   $c bind PAGE$VP <ButtonRelease-1> " $c config -cursor left_ptr; Viewport::RotateDone $Frame $VP True"

   $c bind PAGE$VP <Double-ButtonRelease-1> "set Viewport::Map(Grabbed) \[clock click -milliseconds\]; $c config -cursor left_ptr; Viewport::GoTo $Frame \$Viewport::Map(LatCursor) \$Viewport::Map(LonCursor) $Viewport::Map(ClickFactor)"

   $c config -cursor left_ptr
}

#----------------------------------------------------------------------------
# Nom      : <Page::ParamApply>
# Creation : Novembre 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Appliquer les parametres de la page
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Page::ParamApply { } {
   variable Data

   Page::Size $Data(Frame) $Data(Width) $Data(Height)
   Page::MaskItem $Data(Frame)
   Shape::Widget $Data(Canvas) {} 0 0 0
}

#----------------------------------------------------------------------------
# Nom      : <Page::ParamFrame>
# Creation : Novembre 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Definition des parametres des options.
#
# Parametres :
#  <Frame>   : Identificateur du frame
#  <Apply>   : Commande d'update de l'etat
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Page::ParamFrame { Frame Apply } {
   global GDefs
   variable Lbl

   #----- Inserer l'onglet du canvas

   set frame [TabFrame::Add $Frame 1 "Page" False ""]
   labelframe $frame.size -text [lindex $Lbl(Size) $GDefs(Lang)]
      frame $frame.size.pix
         entry $frame.size.pix.width -textvariable Page::Data(Width) -width 7 -bd 1 -bg $GDefs(ColorLight) -validate key -vcmd "string is integer %P"
         entry $frame.size.pix.height  -textvariable Page::Data(Height) -width 7 -bd 1 -bg $GDefs(ColorLight) -validate key -vcmd "string is integer %P"
         label $frame.size.pix.x -text " X "
         label $frame.size.pix.lbl -text " pixels"
         pack $frame.size.pix.width $frame.size.pix.x $frame.size.pix.height $frame.size.pix.lbl -side left -fill x -expand true
      pack  $frame.size.pix -side top -padx 5 -ipady 2 -fill x -expand true

   labelframe $frame.grid -text [lindex $Lbl(Grid) $GDefs(Lang)]
      checkbutton $frame.grid.show -text [lindex $Lbl(Display) $GDefs(Lang)] \
         -variable Page::Param(Grid) -command "Page::SnapGrid \$Page::Data(Frame)" -indicatoron false -bd 1
      scale $frame.grid.size -orient horizontal -from 1 -to 100 \
         -showvalue true -variable Page::Param(Snap) -relief flat -width 14 -sliderlength 8 -length 150 -bd 1 -resolution 1 \
         -command "Page::SnapGrid  \$Page::Data(Frame) ; catch"
      pack $frame.grid.show $frame.grid.size -side top -fill x -expand true -padx 2
   pack $frame.size $frame.grid -side top -anchor w -padx 5 -pady 5 -fill x

   labelframe $frame.oth -text [lindex $Lbl(Others) $GDefs(Lang)]
      frame $frame.oth.def -relief sunken -bd 1
         checkbutton $frame.oth.def.intr  -text [lindex $Lbl(Intrusion) $GDefs(Lang)] -variable Page::Param(Intrusion) \
            -indicatoron false -command "$Apply configure -state normal" -onvalue 10 -offvalue 0 -bd 1
         checkbutton $frame.oth.def.widget  -text [lindex $Lbl(Widget) $GDefs(Lang)] -variable Page::Param(WidgetOpt) \
            -indicatoron false -command "$Apply configure -state normal" -onvalue True -offvalue False -bd 1
         pack $frame.oth.def.intr $frame.oth.def.widget -side top -fill x
      pack $frame.oth.def -side top -fill x
   pack  $frame.oth -padx 5 -pady 2 -side top -fill x

   bind $frame.size.pix.width  <Key> "$Apply configure -state normal"
   bind $frame.size.pix.height <Key> "$Apply configure -state normal"
}

#----------------------------------------------------------------------------
# Nom      : <Page::Register>
# Creation : Octobre 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Ajouter un nouvel object dans une page
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <Type>   : Type d'object
#   <Id>     : Identificateur de l'object
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Page::Register { Frame Type Id } {
   variable Data

   lappend Data(Data$Frame$Type) $Id
}

#----------------------------------------------------------------------------
# Nom      : <Page::Registered>
# Creation : Octobre 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Verification de l'assignation d'un object(s) dans une page(s)
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <Type>   : Type d'object
#   <Id>     : Identificateur de l'object
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Page::Registered { Frame { Type "" } { Id "" } } {
   variable Data

   #----- Recherche dans tous les frame

   if { $Frame=="All" } {

       #----- Recherche un identificateur specifique
       if { $Id!="" } {
         set idx -1
         foreach frame $Page::Data(Frames) {
            if { [info exists Data(Data$frame$Type)] } {
               set idx [lsearch -exact $Data(Data$frame$Type) $Id]
               if { $idx!=-1 } { break }
            }
         }
         return $idx

      #----- Recherche de tout les identificateurs d'un type specifique
      } else {
         set lst {}
         foreach frame $Page::Data(Frames) {
            if { [info exists Data(Data$frame$Type)] } {
               set lst [concat $lst $Data(Data$frame$Type)]
            }
         }
         return $lst
      }
   }

   #----- Recherche de tout les identificateurs d'un frame

   if { $Type=="" } {
      set lst {}
      foreach { index value } [array get Data Data$Frame*] {
         lappend lst [list [string trimleft $index Data$Frame] $value]
      }
      return $lst
   }

   #----- Recherche un identificateur specifique
   if { $Id!="" } {
      if { [info exists Data(Data$Frame$Type)] } {
         return [lsearch -exact $Data(Data$Frame$Type) $Id]
      } else {
         return -1
      }
      #----- Recherche de tout les identificateurs d'un type specifique
   } else {
      if { [info exists Data(Data$Frame$Type)] } {
         return $Data(Data$Frame$Type)
      } else {
         return ""
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Page::UnRegister>
# Creation : Octobre 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Supprimner la persence d'un objectdans une page
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <Type>   : Type d'object
#   <Id>     : Identificateur de l'object
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Page::UnRegister { Frame Type Id } {
   variable Data

   if { [info exists Data(Data$Frame$Type)] } {
      set idx [lsearch $Data(Data$Frame$Type) "$Id" ]
      if { $idx!=-1 } {
         set Data(Data$Frame$Type) [lreplace $Data(Data$Frame$Type) $idx $idx]
      }
      return [lindex $Data(Data$Frame$Type) end]
   }
   return ""
}

#----------------------------------------------------------------------------
# Nom      : <Page::Resize>
# Creation : Septembre 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Afficher et ajuster lse scrollbars pour les canvas fixes
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Page::Resize { Frame } {
   variable Data

   if { ![winfo exist $Frame.page.v] || ![winfo exist $Frame.page.h]} {
      return
   }

   #----- Visibilite en largeur

   set ws [expr [winfo ismapped $Frame.page.v]?[winfo width  $Frame.page.v]:0]
   if { [expr $Data(Width$Frame)*$Data(Scale$Frame)/100.0]>[expr [winfo width $Frame.page]-$ws] } {
      place $Frame.page.h -rely 1.0 -relx 0.0 -relwidth 1.0 -width -11 -anchor sw
   } else {
      place forget $Frame.page.h
   }

   #----- Visibilite en hauteur

   set hs [expr [winfo ismapped $Frame.page.h]?[winfo height $Frame.page.h]:0]
   if { [expr $Data(Height$Frame)*$Data(Scale$Frame)/100.0]>[expr [winfo height $Frame.page]-$hs] } {
      place  $Frame.page.v -relx 1.0 -rely 0.0 -relheight 1.0 -height -11 -anchor ne
   } else {
      place forget $Frame.page.v
   }
}

#----------------------------------------------------------------------------
# Nom      : <Page::Scale>
# Creation : Septembre 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Redimensionner la canvas et tout ce qu'il contient
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <Incr>    : Increment de scale
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Page::Scale { Frame { Incr 0 } } {
   variable Data

   set scale [expr 100.0*100.0/$Data(Scale$Frame)]

   if { !$Incr } {
      set Data(Scale$Frame) $Data(Scale)
   } else {
      set Data(Scale) [set Data(Scale$Frame) [expr ($Data(Scale$Frame)+$Incr)<0?0:$Data(Scale$Frame)+$Incr]]
   }

   if { $Data(Scale$Frame)>0 } {
      $Frame.page.canvas configure -width [expr $Data(Width$Frame)*$Data(Scale$Frame)/100.0] \
         -height [expr $Data(Height$Frame)*$Data(Scale$Frame)/100.0] \
         -scrollregion "1 1 [expr $Data(Width$Frame)*$Data(Scale$Frame)/100.0] [expr $Data(Height$Frame)*$Data(Scale$Frame)/100.0]"

      set scale [expr $Data(Scale$Frame)/100.0*$scale/100.0]
      $Frame.page.canvas scale all 0 0 $scale $scale
      Page::Resize $Frame
   }
}

#----------------------------------------------------------------------------
# Nom      : <Page::ScaleSet>
# Creation : Septembre 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Calculer le scaling pour que la page soit entierement visible
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Page::ScaleSet { Frame } {
   variable Data

   if { $Data(Full$Frame) } {
      set w [winfo width $Frame.page]
      set h [winfo height $Frame.page]

      set wr [expr double($w)/$Data(Width$Frame)]
      set hr [expr double($h)/$Data(Height$Frame)]

      if { $hr>$wr } {
         set Data(Scale$Frame) [expr $wr*100.0]
      } else {
         set Data(Scale$Frame) [expr $hr*100.0]
      }

      Page::Scale $Frame
   }
}

#----------------------------------------------------------------------------
# Nom      : <Page::Size>
# Creation : Septembre 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Redimensionner la canvas et tout ce qu'il contient
#
# Parametres :
#  <Frame>     : Identificateur de Page
#  <Width>     : Largeur de la page
#  <Height     : Hauteur de la page
#  <Intrusion> : Largeur des intrusions
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Page::Size { Frame Width Height { Intrusion -1 } } {
   variable Data
   variable Param

   if { $Intrusion>=0 } {
      set Param(Intrusion) $Intrusion
   }

   if { $Data(Width$Frame)!=$Width || $Data(Height$Frame)!=$Height } {

      set Data(Width)  [set Data(Width$Frame)  $Width]
      set Data(Height) [set Data(Height$Frame) $Height]

      if { $Width<=0 && $Height<=0 } {
         $Frame.page.canvas configure -width 0 -height 0 -yscrollcommand "" -xscrollcommand "" -scrollregion ""
         $Frame.page.canvas yview moveto 0
         $Frame.page.canvas xview moveto 0
         destroy $Frame.page.v $Frame.page.h
         pack $Frame.page.canvas -side top -fill both -expand true
         bind $Frame <Configure> "update idletasks; SPI::LayoutFit $Frame; QuickLayout::LayoutGrid False"
      } else {
         $Frame.page.canvas configure -yscrollcommand "$Frame.page.v set" -xscrollcommand "$Frame.page.h set"
         pack $Frame.page.canvas -side top -anchor nw -expand false -fill none

         if { ![winfo exist $Frame.page.v] } {
            scrollbar $Frame.page.v -orient vertical   -bd 1 -width 10 -command "$Frame.page.canvas yview"
            scrollbar $Frame.page.h -orient horizontal -bd 1 -width 10 -command "$Frame.page.canvas xview"
         }
         bind $Frame <Configure> "update idletasks; if { \$Page::Data(Full$Frame) } { Page::ScaleSet $Frame } ; Page::Resize $Frame"
         Page::Scale $Frame
      }

      #----- Si la page est plus grande que la fenetre l'evenement configure ne se fait pas
      #      alors on le force

      event generate $Frame.page.canvas <Configure>
      update idletasks
   }
}

#----------------------------------------------------------------------------
# Nom      : <Page::Update>
# Creation : Janvier 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Raffraichir la projection et l'affichage des objets internes
#            ou optionnel de la projection.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <VP>      : Identificateur de Viewport
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Page::Update { { Frame "" } { VP True } } {
   variable Data

   #----- Est-ce que la page est valide
   if { $Frame=="" } {
      set Frame $Page::Data(Frame)
   }

   #----- Est-ce que la page existe encore
   if { ![winfo exists $Frame] } {
      if { [set idx [lsearch -exact $Data(Frames) $Frame]]<0 } {
         set Data(Frame) [lreplace $Data(Frames) $idx $idx]
      }
      return
   }

   #----- Get the log2 lens
   catch { 
      set l [format "%.2f" [expr log10([projcam configure $Frame -lens])/log10(2)]]
      if { $l!=$Page::Data(L$Frame) } {
         set Page::Data(L$Frame) $l
      }
   }

   #----- Faire un update de tous les viewports
   if { $VP } {
      set vp ""
      foreach vp [Page::Registered $Frame Viewport] {
         $Frame.page.canvas itemconf $vp -projection $Frame -frame 0
      }

      if { $vp!="" } {
         foreach mini $Miniport::Data(Mini$Frame) {
            Miniport::Lens $Frame $mini
            Miniport::Coverage $Frame $mini $vp
            $Frame.page.canvas itemconf $mini -update True
         }
         $Frame.page.canvas raise PAGEMINI$Frame
      }
   }

   #----- Refresh de l'usager

   Page::UpdateItems $Frame
   OpenGL::Update
}

#----------------------------------------------------------------------------
# Nom      : <Page::UpdateCommand>
# Creation : Octobre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Effecture la mise a jour de fonctions usagers.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour:
#
# Remarques :
#    A redefinir par l'usager
#
#----------------------------------------------------------------------------

proc Page::UpdateCommand { { Frame "" } } {
}

#----------------------------------------------------------------------------
# Nom      : <Page::UpdateItems>
# Creation : Octobre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Effecture la mise a jour de fonctions usagers.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour:
#
# Remarques :
#    A redefinir par l'usager
#
#----------------------------------------------------------------------------

proc Page::UpdateItems { { Frame "" } } {
}

#----------------------------------------------------------------------------
# Nom      : <Page::SnapGrid>
# Creation : Janvier 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Affichage de la grille de "snap"
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Page::SnapGrid { Frame } {
   variable Param

   $Frame.page.canvas delete PAGEGRID

   if { $Param(Grid) } {

      if { $Param(Snap)>1 } {

         for { set x 0 } { $x < [winfo width $Frame.page.canvas] } { incr x $Param(Snap) } {
            for { set y 0 } { $y < [winfo height $Frame.page.canvas] } { incr y $Param(Snap) } {
               $Frame.page.canvas create line $x $y [expr $x+1] $y -tags "PAGEGRID NOPRINT" -fill black
            }
         }
      }
      $Frame.page.canvas lower PAGEGRID
   }
}

#----------------------------------------------------------------------------
# Nom      : <Page::Snap>
# Creation : Janvier 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Appliquer le "snap" a une parire de coordonnee
#
# Parametres :
#  <Frame>   : Identificateur de Page
#   <X>      : Variable contenant la valeur X
#   <Y>      : Variable contenant la valeur Y
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Page::Snap { Frame X Y } {
   variable Param

   upvar $X x
   upvar $Y y

   set x [$Frame.page.canvas canvasx $x $Param(Snap)]
   set y [$Frame.page.canvas canvasy $y $Param(Snap)]
}

#----------------------------------------------------------------------------
# Nom      : <Page::SnapRef>
# Creation : Janvier 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialisation des coordonees de references pour les deplacement
#            et redimenssionement d'un viewport
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <X>      : Coordonnee en X du deplacement
#   <Y>      : Coordonnee en Y du deplacement
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Page::SnapRef { Frame X Y } {
   variable Param
   variable Data

   set Data(X) [$Frame.page.canvas canvasx $X $Param(Snap)]
   set Data(Y) [$Frame.page.canvas canvasy $Y $Param(Snap)]
}

proc Page::Pause { } {

   set OpenGL::Param(PrevDelay) [glrender -delay 1000]
   glrender -delay 1000
}
proc Page::Play { } {

   glrender -delay $OpenGL::Param(PrevDelay)
}

