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
#    ColorBar::Tag        { VP Id }
#    ColorBar::Create     { Frame VP X0 Y0 Width Height { Fields { } } }
#    ColorBar::Set        { Frame VP No Id Field }
#    ColorBar::Destroy    { Frame VP No }
#    ColorBar::DestroyAll { Frame { VP "" }}
#    ColorBar::Full       { Canvas Tag VP { Pix 0 } }
#    ColorBar::Move       { Canvas Tag }
#    ColorBar::Scale      { Canvas Tag X Y }
#    ColorBar::Update     { Frame { State -1 } }
#    ColorBar::UpdateVP   { Frame VP List }
#    ColorBar::Write      { Frame File }
#
#===============================================================================

package provide ColorBar 1.2

catch { SPI::Splash "Loading Canvas Package ColorBar 1.2" }

namespace eval ColorBar {
   variable Data
   variable Param
   variable Lbl

   set Lbl(BarFrame)  { "Cadre" "Frame" }
   set Lbl(BarAlpha)  { "Transparence" "Transparency" }
   set Lbl(BarSingle) { "Une seule échelle" "Single colorbar" }
   set Lbl(BarSplit)  { "Séparation des intervalles" "Split intervals" }
   set Lbl(BarFactor) { "Afficher les facteurs" "Show factors" }
   set Lbl(BarBorder) { "Bordure des intervalles" "Border intervals" }
   set Lbl(BarThin)   { "Barre mince" "Thin bar" }
   set Lbl(BarMedium) { "Barre moyenne" "Medium bar" }
   set Lbl(BarWide)   { "Barre large" "Wide bar" }
   set Lbl(BarLeft)   { "Barre à gauche/bas" "Bar to the left/bottom" }
   set Lbl(BarRight)  { "Barre à droite/haut" "Bar to the right/top" }

   set Param(Full)   0
   set Param(BG)     white
   set Param(Alpha)  100
   set Param(Split)  0
   set Param(Factor) True
   set Param(Border) 0
   set Param(Width)  15
   set Param(Single) False
   set Param(Side)   right
}

#------------------------------------------------------------------------------
# Nom      : <ColorBar::Active>
# Creation : Octobre 2001 - J.P. Gauthier - CMC/CMOE -
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

#------------------------------------------------------------------------------
# Nom      : <ColorBar::Tag>
# Creation : Octobre 2001 - J.P. Gauthier - CMC/CMOE -
#
# But     : Creer le tag unique pour la donnee
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

proc ColorBar::Tag { VP Id } {
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
   set tag [ColorBar::Tag $VP $Id]:$VP$No

   set Data(Active$Frame) 1
   set Data(Full$tag)     $Param(Full)
   set Data(BG$tag)       $Param(BG)
   set Data(Alpha$tag)    $Param(Alpha)
   set Data(Split$tag)    $Param(Split)
   set Data(Factor$tag)   $Param(Factor)
   set Data(Border$tag)   $Param(Border)
   set Data(Width$tag)    $Param(Width)
   set Data(Side$tag)     $Param(Side)
   set Data($VP$No)       [list $X0 $Y0 $Width $Height $tag $Param(Full) $Param(BG) $Param(Alpha) $Param(Split) $Param(Factor) $Param(Border) $Param(Width) $Param(Side)]

   set Data(Alpha$tag)   25
   set SPI::Data(ShowColorBar$Frame) 1

   return $tag
}

proc ColorBar::SetParams { Frame VP No Tag } {
   variable Data

   $Frame.page.canvas itemconfigure $Tag -bg $Data(BG$Tag) -transparency $Data(Alpha$Tag) -barsplit $Data(Split$Tag) -barborder $Data(Border$Tag) \
      -barwidth $Data(Width$Tag) -barside $Data(Side$Tag) -showfactor $Data(Factor$Tag)

   set Data($VP$No) [lreplace $Data($VP$No) 5 end $Data(Full$Tag) $Data(BG$Tag) $Data(Alpha$Tag) $Data(Split$Tag) $Data(Factor$Tag) $Data(Border$Tag) $Data(Width$Tag) $Data(Side$Tag)]
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
   
   set tag [ColorBar::Tag $VP $Id]:$VP$No

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
         set Data($VP$No) [list $x $y $w $h $tag $Param(Full) $Param(BG) $Param(Alpha) $Param(Split) $Param(Factor) $Param(Border) $Param(Width) $Param(Side)]
      }

      if { ![info exists ColorBar::Data(Split$tag)] } {
      
         #----- Check if this is a previous version of colorbar definitions
         if { [llength $Data($VP$No)]>5 } {
            set Data(Full$tag)   [lindex $Data($VP$No) 5]
            set Data(BG$tag)     [lindex $Data($VP$No) 6]
            set Data(Alpha$tag)  [lindex $Data($VP$No) 7]
            set Data(Split$tag)  [lindex $Data($VP$No) 8]
            set Data(Factor$tag) [lindex $Data($VP$No) 9]
            set Data(Border$tag) [lindex $Data($VP$No) 10]
            set Data(Width$tag)  [lindex $Data($VP$No) 11]
            set Data(Side$tag)   [lindex $Data($VP$No) 12]        
         } else {
            set Data(Full$tag)   $Param(Full)
            set Data(BG$tag)     $Param(BG)
            set Data(Alpha$tag)  $Param(Alpha)
            set Data(Split$tag)  $Param(Split)
            set Data(Factor$tag) $Param(Factor)
            set Data(Border$tag) $Param(Border)
            set Data(Width$tag)  $Param(Width)
            set Data(Side$tag)   $Param(Side)

            set Data($VP$No) [list $x $y $w $h $tag $Param(Full) $Param(BG) $Param(Alpha) $Param(Split) $Param(Factor) $Param(Border) $Param(Width) $Param(Side)]
         }
      }

      $Frame.page.canvas create colorbar -x $x -y $y -width $w -height $h \
         -data $Field -tags "PAGE CB$VP VPINTRUDE $tag" -barsplit $Data(Split$tag) -barside $Data(Side$tag) \
         -barborder $Data(Border$tag) -barwidth $Data(Width$tag) -bg $Data(BG$tag) -transparency $Data(Alpha$tag) -showfactor $Data(Factor$tag)

      menubutton $Frame.bo$tag -bg $GDefs(ColorFrame) -bitmap @$GDefs(Dir)/share/bitmap/cvmenu.xbm -cursor hand1 -bd 1 -relief raised \
         -menu $Frame.bo$tag.menu
      menu $Frame.bo$tag.menu -bg $GDefs(ColorFrame)
         $Frame.bo$tag.menu add checkbutton -label [lindex $Lbl(BarFrame) $GDefs(Lang)] -variable ColorBar::Data(BG$tag) -onvalue white -offvalue "" \
            -command "ColorBar::SetParams $Frame $VP $No $tag; Page::Update $Frame"
         $Frame.bo$tag.menu add checkbutton -label [lindex $Lbl(BarAlpha) $GDefs(Lang)] -variable ColorBar::Data(Alpha$tag) -onvalue 50 -offvalue 100 \
            -command "ColorBar::SetParams $Frame $VP $No $tag; Page::Update $Frame"
         $Frame.bo$tag.menu add checkbutton -label [lindex $Lbl(BarSingle) $GDefs(Lang)] -variable ColorBar::Param(Single) -onvalue True -offvalue False \
            -command "ColorBar::Update $Frame"
         $Frame.bo$tag.menu add separator
         $Frame.bo$tag.menu add checkbutton -label [lindex $Lbl(BarSplit) $GDefs(Lang)] -variable ColorBar::Data(Split$tag) -onvalue 5 -offvalue 0 \
            -command "ColorBar::SetParams $Frame $VP $No $tag; Page::Update $Frame"
         $Frame.bo$tag.menu add checkbutton -label [lindex $Lbl(BarBorder) $GDefs(Lang)] -variable ColorBar::Data(Border$tag) -onvalue 1 -offvalue 0 \
            -command "ColorBar::SetParams $Frame $VP $No $tag; Page::Update $Frame"
         $Frame.bo$tag.menu add separator
         $Frame.bo$tag.menu add radiobutton -label [lindex $Lbl(BarThin) $GDefs(Lang)] -variable ColorBar::Data(Width$tag) -value 15 \
            -command "ColorBar::SetParams $Frame $VP $No $tag; Page::Update $Frame"
         $Frame.bo$tag.menu add radiobutton -label [lindex $Lbl(BarMedium) $GDefs(Lang)] -variable ColorBar::Data(Width$tag) -value 30 \
            -command "ColorBar::SetParams $Frame $VP $No $tag; Page::Update $Frame"
         $Frame.bo$tag.menu add radiobutton -label [lindex $Lbl(BarWide) $GDefs(Lang)] -variable ColorBar::Data(Width$tag) -value 50 \
            -command "ColorBar::SetParams $Frame $VP $No $tag; Page::Update $Frame"
         $Frame.bo$tag.menu add separator
         $Frame.bo$tag.menu add radiobutton -label [lindex $Lbl(BarLeft) $GDefs(Lang)] -variable ColorBar::Data(Side$tag) -value left \
            -command "ColorBar::SetParams $Frame $VP $No $tag; Page::Update $Frame"
         $Frame.bo$tag.menu add radiobutton -label [lindex $Lbl(BarRight) $GDefs(Lang)] -variable ColorBar::Data(Side$tag) -value right \
            -command "ColorBar::SetParams $Frame $VP $No $tag; Page::Update $Frame"
         $Frame.bo$tag.menu add separator
         $Frame.bo$tag.menu add checkbutton -label [lindex $Lbl(BarFactor) $GDefs(Lang)] -variable ColorBar::Data(Factor$tag) -onvalue True -offvalue False \
            -command "ColorBar::SetParams $Frame $VP $No $tag; Page::Update $Frame"
      $Frame.page.canvas create window [expr $x+$w-22] [expr $y+$h] -window $Frame.bo$tag -anchor se -tags "BO$tag NOPRINT"
      $Frame.page.canvas bind $tag <Button-3> "tk_popup $Frame.bo$tag.menu %X %Y 0"

      Shape::BindAllMove $Frame.page.canvas $tag "ColorBar::Move $Frame.page.canvas $tag"
      Shape::BindScale   $Frame.page.canvas $tag "ColorBar::Scale $Frame.page.canvas $tag"
      Shape::BindFull    $Frame.page.canvas $tag ColorBar::Data(Full$tag) "ColorBar::Full $Frame.page.canvas $tag $VP"
      Shape::BindWidget  $Frame.page.canvas $tag

      Page::MaskItem $Frame

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
         Shape::UnBind $Frame.page.canvas $id
         $Frame.page.canvas delete $id
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
         Shape::UnBind $Frame.page.canvas $cb
         $Frame.page.canvas delete $cb
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
   set Data($tag) [lreplace $Data($tag) 0 3 $x $y $w $h]
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
   set Data($tag) [lreplace $Data($tag) 0 3 $xc $yv $wc $hv]
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
      set Data($tag) [lreplace $Data($tag) 0 3 $x $y $w $h]
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
   variable Param
   variable Data

   set lst    ""
   
   if { ![info exists Data(Active$Frame)] } {
      set Data(Active$Frame) 0
   }

   if { $State!=-1 } {
      set Data(Active$Frame) $State
   }

   if { $Data(Active$Frame) } {
      #----- Check for data assigned to viewports
      foreach vp [Page::Registered $Frame Viewport] {
         set fields ""
         set i -1
         foreach field [lindex [$Frame.page.canvas itemconfigure $vp -data] 4] {
            if { [fstdfield is $field True] } {
               if { [fstdfield configure $field -active] && [fstdfield configure $field -showmap] && ([fstdfield configure $field -rendertexture] || [fstdfield configure $field -mapall] || [fstdfield configure $field -rendervector]!="NONE" || [fstdfield configure $field -renderparticle]) } {
                  set id [fstdfield configure $field -dataspec]
                  lappend fields $field
                  if { !$Param(Single) } {
                     lappend lst [ColorBar::Set $Frame $vp [incr i] $id $field]
                  }
               }
            } elseif { [observation is $field] } {
               if { [observation configure $field -active] && [observation configure $field -showmap] && ([observation configure $field -rendertexture] || [observation configure $field -mapall] || [observation configure $field -rendervector]!="NONE") } {
                  set id [observation configure $field -dataspec]
                  lappend fields $field
                  if { !$Param(Single) } {
                     lappend lst [ColorBar::Set $Frame $vp [incr i] $id $field]
                  }
               }
            } elseif { [metobs is $field] } {
               set model [metobs define $field -MODEL]
               set id METOBS$field
               set specs {}
               foreach item [metmodel define $model -items] {
                  set spec [metmodel configure $model [lindex $item 2] -dataspec]
                  if { [lsearch -exact $lst $id]==-1 && [dataspec configure $spec -showmap] && ([dataspec configure $spec -rendertexture] || [dataspec configure $spec -rendervector]!="NONE" ) } {
                     lappend specs $spec
                  }
               }
               if { [llength $specs] } {
                  lappend fields $field
                  if { !$Param(Single) } {
                     lappend lst [ColorBar::Set $Frame $vp [incr i] $id $specs]
                  }
               }
            } elseif { [ogrlayer is $field] } {
               if { [ogrlayer configure $field -active] && [ogrlayer configure $field -showmap] && [ogrlayer configure $field -colormap]!="" && [ogrlayer configure $field -mapvar]!="" } {
                  lappend fields $field
                  if { !$Param(Single) } {
                     lappend lst [ColorBar::Set $Frame $vp [incr i] $field $field]
                  }
               }
            } elseif { [gdalband is $field] } {
               if { [gdalbandr configure $field -active] && [gdalband configure $field -showmap] && [gdalband configure $field -colormap]!="" && [gdalband define $field -nb]==1  } {
                  lappend fields $field
                  if { !$Param(Single) } {
                     lappend lst [ColorBar::Set $Frame $vp [incr i] $field $field]
                  }
               }
            }
         }
         if { $Param(Single) && [llength $fields] } {
            lappend lst [ColorBar::Set $Frame $vp [incr i] 0 $fields]
         }
      }

      #----- Check for data assigned directly to projection
      foreach field [projection configure $Frame -data] {
         if { [ogrlayer is $field] } {
            if { [ogrlayer configure $field -colormap]!="" && [ogrlayer configure $field -showmap] && [ogrlayer configure $field -mapvar]!="" } {
               lappend lst [ColorBar::Set $Frame $vp [incr i] $field $field]
            }
         } elseif { [gdalband is $field] } {
            if { [gdalband configure $field -colormap]!="" && [gdalband configure $field -showmap] && [gdalband define $field -nb]==1  } {
               lappend lst [ColorBar::Set $Frame $vp [incr i] $field $field]
            }
         }
      }
   }


   if { [info exist Data(List$Frame)] } {
      foreach id $Data(List$Frame) {
         if { [lsearch -exact $lst $id]==-1 } {
            Shape::UnBind $Frame.page.canvas $id
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
      if { [fstdfield is $field True] } {
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
   variable Param
   variable Data

   puts $File "   #-----  Positionnement des ColorBars"
   puts $File ""
   puts $File "   set ColorBar::Param(Full)   $Param(Full)"
   puts $File "   set ColorBar::Param(BG)     $Param(BG)"
   puts $File "   set ColorBar::Param(Alpha)  $Param(Alpha)"
   puts $File "   set ColorBar::Param(Split)  $Param(Split)"
   puts $File "   set ColorBar::Param(Factor) $Param(Factor)"
   puts $File "   set ColorBar::Param(Border) $Param(Border)"
   puts $File "   set ColorBar::Param(Width)  $Param(Width)"
   puts $File "   set ColorBar::Param(Single) $Param(Single)"
   puts $File "   set ColorBar::Param(Side)   $Param(Side)"
   puts $File "   set ColorBar::Data(Active\$Frame) 1"

   foreach vp [Page::Registered $Frame Viewport] {
      set i 0
      foreach item [$Frame.page.canvas find withtag CB$vp] {
         set tag [lindex [split [lindex [$Frame.page.canvas itemcget $item -tags] end] :] end]
         if { [info exists ColorBar::Data($tag)] } {
            puts $File "   set ColorBar::Data(\$\{$Viewport::Data(Alias$vp)\}$i) \[list $ColorBar::Data($tag)\]"
         }
         incr i
      }
   }

   puts $File ""
}
