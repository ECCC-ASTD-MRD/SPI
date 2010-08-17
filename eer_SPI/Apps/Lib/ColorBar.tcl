#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Widget de selection de couleur.
# Fichier  : ColorBar.tcl
# Creation : Mars 2001 - J.P. Gauthier - CMC/CMOE
#
# Description:
#   -Definition d'un widget de selection de police.
#
# Fonctions:
#    ColorBar::Active     { Frame }
#    ColorBar::Create     { Frame VP X0 Y0 Width Height { Fields { } } }
#    ColorBar::Set        { Frame VP No Id Field }
#    ColorBar::Destroy    { Frame VP No }
#    ColorBar::DestroyAll { Frame { VP "" }}
#    ColorBar::Full       { Canvas Tag VP { Pix 0 } }
#    ColorBar::Move       { Canvas Tag }
#    ColorBar::Scale      { Canvas Tag X Y }
#    ColorBar::Update     { Frame { State -1 } }
#    ColorBar::UpdateVP   { Frame VP List }
#    proc ColorBar::Write { Frame File }
#
#===============================================================================

package provide ColorBar 1.1

catch { SPI::Splash "Loading Canvas Package ColorBar 1.1" }

namespace eval ColorBar {
   variable Data
   variable Param
   variable Lbl

   set Lbl(BarFrame)  { "Cadre" "Frame" }
   set Lbl(BarAlpha)  { "Transparence" "Transparency" }
   set Lbl(BarSplit)  { "Séparation des intervalles" "Split intervals" }
   set Lbl(BarFactor) { "Afficher les facteurs" "Show factors" }
   set Lbl(BarBorder) { "Bordure des intervalles" "Border intervals" }
   set Lbl(BarThin)   { "Barre mince" "Thin bar" }
   set Lbl(BarMedium) { "Barre moyenne" "Medium bar" }
   set Lbl(BarWide)   { "Barre large" "Wide bar" }
   set Lbl(BarLeft)   { "Barre à gauche" "Bar to the left" }
   set Lbl(BarRight)  { "Barre à droite" "Bar to the right" }

   set Param(Full)   0
   set Param(BG)     white
   set Param(Alpha)  100
   set Param(Split)  0
   set Param(Factor) True
   set Param(Border) 0
   set Param(Width)  15
   set Param(Side)   right
}

#------------------------------------------------------------------------------
# Nom      : <ColorBar::Active>
# Creation : Octobre 20013 - J.P. Gauthier - CMC/CMOE -
#
# But     : Detrminer l'etat de la colorbar
#
# Parametres :
#   <Frame>  : Identificateur de Page
#
# Retour:
#   <Active> : Active ou non
#
# Remarques  :
#
#-------------------------------------------------------------------------------

proc ColorBar::Active { Frame } {
   variable Data

   if { [info exists Data(Active$Frame)] } {
      return $Data(Active$Frame)
   } else {
      return 0
   }
}

proc ColorBar::Id { VP Id } {
  return [string map { : "-" "/" "-" . "-" } CB$VP[join $Id ""]]
}

#------------------------------------------------------------------------------
# Nom      : <ColorBar::Create>
# Creation : Fevrier 2002 - J.P. Gauthier - CMC/CMOE -
#
# But     : Creer un widget de selection de palette
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <VP>     : Identificateur du Viewport
#   <Field>  : Champs associe
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc ColorBar::Create { Frame VP X0 Y0 Width Height } {
   variable Data
   variable Param

   set No 0
   set Id -1
   set tag [ColorBar::Id $VP $Id]:$VP$No

   set Data(Active$Frame) 1
   set Data(Full$tag)     0
   set Data(BG$tag)       $Param(BG)
   set Data(Alpha$tag)    $Param(Alpha)
   set Data(Split$tag)    $Param(Split)
   set Data(Factor$tag)   $Param(Factor)
   set Data(Border$tag)   $Param(Border)
   set Data(Width$tag)    $Param(Width)
   set Data(Side$tag)     $Param(Side)
   set Data($VP$No)       [list $X0 $Y0 $Width $Height $tag]

   set Data(Alpha$tag)   25
   set SPI::Data(ShowColorBar$Frame) 1

   return $tag
}

#------------------------------------------------------------------------------
# Nom      : <ColorBar::Set>
# Creation : Fevrier 2002 - J.P. Gauthier - CMC/CMOE -
#
# But     : Creer un widget de selection de palette
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <VP>     : Identificateur du Viewport
#   <Field>  : Champs associe
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc ColorBar::Set { Frame VP No Id Field } {
   global GDefs
   variable Data
   variable Param
   variable Lbl

   if { ![info exists Data(Active$Frame)] } {
      set Data(Active$Frame) 0
   }

   if { !$Data(Active$Frame) } {
      return
   }

   set tag [ColorBar::Id $VP $Id]:$VP$No

   if { ![info exists Data(List$Frame)] } {
      set Data(List$Frame) {}
      set idx -1
   } else {
      set idx [lsearch -exact $Data(List$Frame) $tag]
   }

   if { $idx!=-1 } {
      $Frame.page.canvas itemconfigure $tag -data $Field
   } else {

      if { [info exists ColorBar::Data($VP$No)] } {
         set x  [lindex $Data($VP$No) 0]
         set y  [lindex $Data($VP$No) 1]
         set w  [lindex $Data($VP$No) 2]
         set h  [lindex $Data($VP$No) 3]
      } else {
         set x  [$Frame.page.canvas itemcget $VP -x]
         set y  [$Frame.page.canvas itemcget $VP -y]
         set h  [expr [$Frame.page.canvas itemcget $VP -height] -1]
         set w  90

         incr h
         set Data($VP$No) [list $x $y $w $h $tag]
      }

      if { ![info exists ColorBar::Data(Split$tag)] } {
         set Data(Full$tag)   $Param(Full)
         set Data(BG$tag)     $Param(BG)
         set Data(Alpha$tag)  $Param(Alpha)
         set Data(Split$tag)  $Param(Split)
         set Data(Factor$tag) $Param(Factor)
         set Data(Border$tag) $Param(Border)
         set Data(Width$tag)  $Param(Width)
         set Data(Side$tag)   $Param(Side)
      }

      $Frame.page.canvas create colorbar -x $x -y $y -width $w -height $h \
         -data $Field -tags "$Page::Data(Tag) CB$VP $tag" -anchor nw -barsplit $Data(Split$tag) -barside $Data(Side$tag) \
         -barborder $Data(Border$tag) -barwidth $Data(Width$tag) -bg $Data(BG$tag) -transparency $Data(Alpha$tag) -showfactor $Data(Factor$tag)

      menubutton $Frame.bo$tag -bg $GDefs(ColorFrame) -bitmap @$GDefs(Dir)/Resources/Bitmap/cvmenu.xbm -cursor hand1 -bd 1 -relief raised \
         -menu $Frame.bo$tag.menu
      menu $Frame.bo$tag.menu -bg $GDefs(ColorFrame)
         $Frame.bo$tag.menu add checkbutton -label [lindex $Lbl(BarFrame) $GDefs(Lang)] -variable ColorBar::Data(BG$tag) -onvalue white -offvalue "" \
            -command "$Frame.page.canvas itemconfigure $tag -bg \$ColorBar::Data(BG$tag); Page::Update $Frame"
         $Frame.bo$tag.menu add checkbutton -label [lindex $Lbl(BarAlpha) $GDefs(Lang)] -variable ColorBar::Data(Alpha$tag) -onvalue 50 -offvalue 100 \
            -command "$Frame.page.canvas itemconfigure $tag -transparency \$ColorBar::Data(Alpha$tag); Page::Update $Frame"
         $Frame.bo$tag.menu add separator
         $Frame.bo$tag.menu add checkbutton -label [lindex $Lbl(BarSplit) $GDefs(Lang)] -variable ColorBar::Data(Split$tag) -onvalue 5 -offvalue 0 \
            -command "$Frame.page.canvas itemconfigure $tag -barsplit \$ColorBar::Data(Split$tag); Page::Update $Frame"
         $Frame.bo$tag.menu add checkbutton -label [lindex $Lbl(BarBorder) $GDefs(Lang)] -variable ColorBar::Data(Border$tag) -onvalue 1 -offvalue 0 \
            -command "$Frame.page.canvas itemconfigure $tag -barborder \$ColorBar::Data(Border$tag); Page::Update $Frame"
         $Frame.bo$tag.menu add separator
         $Frame.bo$tag.menu add radiobutton -label [lindex $Lbl(BarThin) $GDefs(Lang)] -variable ColorBar::Data(Width$tag) -value 15 \
            -command "$Frame.page.canvas itemconfigure $tag -barwidth \$ColorBar::Data(Width$tag); Page::Update $Frame"
         $Frame.bo$tag.menu add radiobutton -label [lindex $Lbl(BarMedium) $GDefs(Lang)] -variable ColorBar::Data(Width$tag) -value 30 \
            -command "$Frame.page.canvas itemconfigure $tag -barwidth \$ColorBar::Data(Width$tag); Page::Update $Frame"
         $Frame.bo$tag.menu add radiobutton -label [lindex $Lbl(BarWide) $GDefs(Lang)] -variable ColorBar::Data(Width$tag) -value 50 \
            -command "$Frame.page.canvas itemconfigure $tag -barwidth \$ColorBar::Data(Width$tag); Page::Update $Frame"
         $Frame.bo$tag.menu add separator
         $Frame.bo$tag.menu add radiobutton -label [lindex $Lbl(BarLeft) $GDefs(Lang)] -variable ColorBar::Data(Side$tag) -value left \
            -command "$Frame.page.canvas itemconfigure $tag -barside \$ColorBar::Data(Side$tag); Page::Update $Frame"
         $Frame.bo$tag.menu add radiobutton -label [lindex $Lbl(BarRight) $GDefs(Lang)] -variable ColorBar::Data(Side$tag) -value right \
            -command "$Frame.page.canvas itemconfigure $tag -barside \$ColorBar::Data(Side$tag); Page::Update $Frame"
         $Frame.bo$tag.menu add separator
         $Frame.bo$tag.menu add checkbutton -label [lindex $Lbl(BarFactor) $GDefs(Lang)] -variable ColorBar::Data(Factor$tag) -onvalue True -offvalue False \
            -command "$Frame.page.canvas itemconfigure $tag -showfactor \$ColorBar::Data(Factor$tag); Page::Update $Frame"
      $Frame.page.canvas create window [expr $x+$w-22] [expr $y+$h] -window $Frame.bo$tag -anchor se -tags "BO$tag NOPRINT"

      Shape::BindMove  $Frame.page.canvas $tag ColorBar::Move $Frame.page.canvas $tag
      Shape::BindScale $Frame.page.canvas $tag [expr $x+$w] [expr $y+$h] "ColorBar::Scale $Frame.page.canvas $tag"
      Shape::BindFull  $Frame.page.canvas $tag [expr $x+$w-11] [expr $y+$h] ColorBar::Data(Full$tag) "ColorBar::Full $Frame.page.canvas $tag $VP"

      lappend Data(List$Frame) $tag
   }
   return $tag
}

#------------------------------------------------------------------------------
# Nom      : <ColorBar::Destroy>
# Creation : Fevrier 2002 - J.P. Gauthier - CMC/CMOE -
#
# But     : Supprimer un Colorbar
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <VP>     : Identificateur du Viewport
#   <No>     : Numero de champs
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc ColorBar::Destroy { Frame VP No } {
   variable Data

   if { [info exists ColorBar::Data($VP$No)] } {

      set id  [lindex $Data($VP$No) 4]
      set idx [lsearch -exact $Data(List$Frame) $id]

      if { $idx!=-1 } {
         Shape::UnBindScale $Frame.page.canvas $id
         Shape::UnBindFull  $Frame.page.canvas $id
         $Frame.page.canvas delete $id BF$id BO$id
         destroy $Frame.bo$id $Frame.bo$id.menu
         unset Data(Active$Frame)

         set Data(List$Frame) [lreplace $Data(List$Frame) $idx $idx]
      }
   }
}

#------------------------------------------------------------------------------
# Nom      : <ColorBar::DestroyAll>
# Creation : Fevrier 2002 - J.P. Gauthier - CMC/CMOE -
#
# But     : Supprimer toutes les Colorbars
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <VP>     : Identificateur du Viewport
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc ColorBar::DestroyAll { Frame { VP "" } } {
   variable Data

   if { [info exist Data(List$Frame)] } {
      foreach idx [lsort -decreasing -integer [lsearch -all $Data(List$Frame) CB$VP*]] {
         set cb [lindex $Data(List$Frame) $idx]
         Shape::UnBindScale $Frame.page.canvas $cb
         Shape::UnBindFull  $Frame.page.canvas $cb
         $Frame.page.canvas delete $cb
         destroy $Frame.bo$cb $Frame.bo$cb.menu
         set Data(List$Frame) [lreplace $Data(List$Frame) $idx $idx]
      }
   }
}

#------------------------------------------------------------------------------
# Nom      : <ColorBar::Move>
# Creation : Fevrier 2002 - J.P. Gauthier - CMC/CMOE -
#
# But     : Enregistrer le changement de position de la colorbar
#
# Parametres :
#   <Canvas> : Path du canvas
#   <Tag>    : Identificateur de la colorbar
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc ColorBar::Move { Canvas Tag } {
   variable Data

   set x [$Canvas itemcget $Tag -x]
   set y [$Canvas itemcget $Tag -y]
   set w [$Canvas itemcget $Tag -width]
   set h [$Canvas itemcget $Tag -height]

   set tag [lindex [split $Tag :] end]
   set Data($tag) [list $x $y $w $h $Tag]
   set Data(Full$Tag) False
}

#------------------------------------------------------------------------------
# Nom      : <ColorBar::Full>
# Creation : Avril 2008 - J.P. Gauthier - CMC/CMOE -
#
# But     : Enregistrer le changement de position/dimension de la colorbar
#
# Parametres :
#   <Canvas> : Path du canvas
#   <Tag>    : Identificateur de la colorbar
#   <VP>     : Identificateur du viewport associe
#   <Pix>    : Delta en translation
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc ColorBar::Full { Canvas Tag VP { Pix 0 } } {
   variable Data

   set xc [$Canvas itemcget $Tag -x]
   set wc [$Canvas itemcget $Tag -width]

   set yv [$Canvas itemcget $VP -y]
   set xv [$Canvas itemcget $VP -x]
   set hv [$Canvas itemcget $VP -height]
   set wv [$Canvas itemcget $VP -width]

   if { $xc>[expr $xv+$wv*0.5] } {
      set xc [expr $xc-$Pix]
   }

   set tag [lindex [split $Tag :] end]
   set Data($tag) [list $xc $yv $wc $hv $Tag]
   $Canvas itemconfigure $Tag -x $xc -y $yv -width $wc -height $hv

   return [list [expr $xc+$wc] [expr $yv+$hv]]
}

#------------------------------------------------------------------------------
# Nom      : <ColorBar::Scale>
# Creation : Fevrier 2002 - J.P. Gauthier - CMC/CMOE -
#
# But     : Changement de dimension de la colorbar
#
# Parametres :
#   <Canvas> : Path du canvas
#   <Tag>    : Identificateur de la colorbar
#   <X>      : Dimension en X
#   <Y>      : Dimension en Y
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc ColorBar::Scale { Canvas Tag X Y } {
   variable Data

   set x [$Canvas itemcget $Tag -x]
   set y [$Canvas itemcget $Tag -y]
   set w [expr $X-$x]
   set h [expr $Y-$y]

   if { $w>25 && $h>25 } {
      $Canvas itemconfigure $Tag -width $w -height $h
      set tag [lindex [split $Tag :] end]
      set Data($tag) [list $x $y $w $h $Tag]
      set Data(Full$Tag) False
      return True
   } else {
      return False
   }
}

#------------------------------------------------------------------------------
# Nom      : <ColorBar::Update>
# Creation : Fevrier 2002 - J.P. Gauthier - CMC/CMOE -
#
# But     : Mise-a-jour des proprietes d'une colorbar
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <State>  : Etat d'activation de la colorbar
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc ColorBar::Update { Frame { State -1 } } {
   variable Data

   set lst ""

   if { ![info exists Data(Active$Frame)] } {
      set Data(Active$Frame) 0
   }

   if { $State!=-1 } {
      set Data(Active$Frame) $State
   }

   if { $Data(Active$Frame) } {
      foreach vp [Page::Registered $Frame Viewport] {
         set i -1
         foreach field [lindex [$Frame.page.canvas itemconfigure $vp -data] 4] {
            if { [fstdfield is $field] } {
               if { [fstdfield configure $field -rendertexture] || [fstdfield configure $field -mapall] || [fstdfield configure $field -rendervector]!="NONE" || [fstdfield configure $field -renderparticle] } {
                  set id [fstdfield configure $field -dataspec]
                  lappend lst [ColorBar::Set $Frame $vp [incr i] $id $field]
               }
            } elseif { [observation is $field] } {
               if { [observation configure $field -rendertexture] || [observation configure $field -rendervector]!="NONE" } {
                  set id [observation configure $field -dataspec]
                  lappend lst [ColorBar::Set $Frame $vp [incr i] $id $field]
               }
            } elseif { [metobs is $field] } {
               set model [metobs define $field -MODEL]
               set id METOBS$field
               set specs {}
               foreach item [metmodel define $model -items] {
                  set spec [metmodel configure $model [lindex $item 2] -dataspec]
                  if { [lsearch -exact $lst $id]==-1 && ([dataspec configure $spec -rendertexture] || [dataspec configure $spec -rendervector]!="NONE" ) } {
                     lappend specs $spec
                  }
               }
               if { [llength $specs] } {
                  lappend lst [ColorBar::Set $Frame $vp [incr i] $id $specs]
               }
            }
         }
      }

      foreach field [projection configure $Frame -data] {
         if { [ogrlayer is $field] } {
            if { [ogrlayer configure $field -colormap]!="" && [ogrlayer define $field -map]!="" } {
               lappend lst [ColorBar::Set $Frame $vp [incr i] $field $field]
            }
         } elseif { [gdalband is $field] } {
            if { [gdalband configure $field -colormap]!="" && [gdalband define $field -nb]==1 } {
#               lappend lst [ColorBar::Set $Frame $vp [incr i] $field $field]
            }
         }
      }
   }


   if { [info exist Data(List$Frame)] } {
      foreach id $Data(List$Frame) {
         if { [lsearch -exact $lst $id]==-1 } {
            Shape::UnBindScale $Frame.page.canvas $id
            Shape::UnBindFull  $Frame.page.canvas $id
            $Frame.page.canvas delete $id
            destroy $Frame.bo$id $Frame.bo$id.menu
         }
      }
      set Data(List$Frame) $lst
   }
}

#------------------------------------------------------------------------------
# Nom      : <ColorBar::UpdateVP>
# Creation : Fevrier 2002 - J.P. Gauthier - CMC/CMOE -
#
# But     : Mise-a-jour des proprietes d'une colorbar pour un set de donnees
#           specifiques
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <VP>     : Identificateur du Viewport
#   <List>   : Liste des donnees
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc ColorBar::UpdateVP { Frame VP List } {
   variable Data

   set i 0
   foreach field $List {
      if { [fstdfield is $field] } {
         set spec [fstdfield configure $field -dataspec]
      } elseif { [observation is $field] } {
         set spec [observation configure $field -dataspec]
      }

      if { [dataspec is $spec] && ([dataspec configure $spec -rendertexture] || [dataspec configure $spec -rendervector]!="NONE" || [ataspec configure $spec -renderparticle]) } {
         lappend lst [ColorBar::Set $Frame $VP $i $id $field]
         incr i
      }
   }
}

#------------------------------------------------------------------------------
# Nom      : <ColorBar::Write>
# Creation : Novembre 2003 - J.P. Gauthier - CMC/CMOE -
#
# But     : Engeristrer les parametres des ColorBar dans un fichier Layout
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <File>   : Identificateur de Fichier
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc ColorBar::Write { Frame File } {
   variable Data

   puts $File "   #-----  Positionnement des ColorBars"
   puts $File ""
   puts $File "   set ColorBar::Data(Active\$Frame) 1"

   foreach vp [Page::Registered $Frame Viewport] {
      set i 0
      foreach field [Viewport::Assigned $Frame $vp { fstdfield observation }] {
         if { [info exists ColorBar::Data($vp$i)] } {
            puts $File "   set ColorBar::Data(\$\{$Viewport::Data(Alias$vp)\}$i) \[list $ColorBar::Data($vp$i)\]"
         }
         incr i
      }
   }
   puts $File ""
}
