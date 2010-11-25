#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Widget de selection de couleur.
# Fichier  : DataBar.tcl
# Creation : Mars 2001 - J.P. Gauthier - CMC/CMOE
#
# Description:
#   -Definition d'un widget de selection de police.
#
# Fonctions:
#    DataBar::Active     { Frame }
#    DataBar::Create     { Frame VP No Id }
#    DataBar::Destroy    { Frame VP No }
#    DataBar::DestroyAll { Frame }
#    DataBar::Move       { Canvas Tag0 Tag1 }
#    DataBar::Scale      { Canvas Tag0 Tag1 X Y }
#    DataBar::SetTitle   { Frame VP Title }
#    DataBar::Update     { Frame { State -1 } }
#    DataBar::UpdateVP   { Frame VP List }
#    DataBar::Write      { Frame File }
#
#===============================================================================

package provide DataBar 1.1

catch { SPI::Splash "Loading Canvas Package DataBar 1.1" }

namespace eval DataBar {
   global GDefs
   variable Data
   variable Param

   set Param(Title) "Title"
   set Param(Full)  1

   image create photo DATABARLOGO -file $GDefs(Dir)/Resources/Image/Symbol/Logo/Flag.gif
}

#------------------------------------------------------------------------------
# Nom      : <DataBar::Active>
# Creation : Octobre 20013 - J.P. Gauthier - CMC/CMOE -
#
# But     : Detrminer l'etat de la databar
#
# Parametres :
#   <Frame>  : Identificateur de Page
#
# Retour     :
#
# Remarques  :
#   <Active> : Active ou non
#
#-------------------------------------------------------------------------------

proc DataBar::Active { Frame } {
   variable Data

   if { [info exists Data(Active$Frame)] } {
      return $Data(Active$Frame)
   } else {
      return 0
   }
}

#------------------------------------------------------------------------------
# Nom      : <DataBar::Create>
# Creation : Fevrier 2002 - J.P. Gauthier - CMC/CMOE -
#
# But     : Creer un widget de selection de police
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <VP>     : Identificateur du Viewport
#   <X0>     : Coordonee X du coin superieur gauche
#   <Y0>     : Coordonee Y du coin superieur gauche
#   <Width>  : Largeur du Viewport
#   <Height> : Hauteur du Viewport
#   <Title>  : Titre
#
# Retour     :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc DataBar::Create { Frame VP X0 Y0 Width Height { Title "" } } {
   variable Data
   variable Param

   if { $Title!="" } {
      set Data(Title$Frame) $Title
   } else {
      set Data(Title$Frame) $Param(Title)
   }

   set x0  $X0
   set y0  $Y0
   set x1  [expr $X0+$Width]
   set y1  [expr $Y0+$Height]

   set Data(Active$Frame) 1
   set Data($VP)          [list $x0 $y0 $x1 $y1]
   set Data(Full$VP)   $Param(Full)

   if { ![info exists Data(List$Frame)] } {
      set Data(List$Frame) {}
      set idx -1
   } else {
      set idx [lsearch -exact $Data(List$Frame) DB$VP]
   }

   if { $idx==-1 } {
      lappend Data(List$Frame) DB$VP
   }

   DataBar::Draw $Frame $VP $x0 $y0 $x1 $y1

   Shape::BindMove  $Frame.page.canvas DB$VP DataBar::Move $Frame $VP DB$VP
   Shape::BindScale $Frame.page.canvas DB$VP $x1 $y1 "DataBar::Scale $Frame $VP DB$VP"
   Shape::BindFull  $Frame.page.canvas DB$VP [expr $x1-11] $y1 DataBar::Data(Full$VP) "DataBar::Full $Frame DB$VP $VP"

   Page::MaskItem $Frame
}

proc DataBar::SetTitle { Frame VP Title } {
   variable Data

   set Data(Title$Frame) $Title

   $Frame.page.canvas itemconfigure TXTDB$VP -text $Data(Title$Frame)
}

proc DataBar::Draw { Frame VP X0 Y0 X1 Y1 } {
   global GDefs
   variable Param
   variable Data

   if { [llength [$Frame.page.canvas find withtag TXTDB$VP]] } {
      set Data(Title$Frame) [$Frame.page.canvas itemcget TXTDB$VP -text]
   }

   $Frame.page.canvas delete DBDEL$VP

   if { [llength [$Frame.page.canvas find withtag $VP]] } {
      $Frame.page.canvas create rectangle $X0 $Y0 $X1 $Y1 -tags "$Page::Data(Tag) DB$VP FRDB$VP VPINTRUDE" -fill white -outline black
   } else {
      $Frame.page.canvas create rectangle $X0 $Y0 $X1 $Y1 -tags "$Page::Data(Tag) DB$VP FRDB$VP VPINTRUDE" -fill white -outline black
   }

   $Frame.page.canvas create line [expr $X0+20] [expr $Y0+20] [expr $X0+20] $Y1 -tags "$Page::Data(Tag) DB$VP DBDEL$VP" -fill #CCCCCC
   $Frame.page.canvas create image [expr $X0+2] [expr $Y0+2] -image DATABARLOGO -anchor nw -tags "$Page::Data(Tag) DB$VP DBDEL$VP"
   $Frame.page.canvas create text [expr $X0+50] [expr $Y0+1] -text $Data(Title$Frame) -anchor nw -font XFont16 -tags "$Page::Data(Tag) DB$VP DBDEL$VP TXTDB$VP CVTEXT"

   set y [expr $Y0+21]
   set x [expr $X0+25]
   set i 0
   set n 0

   set ids "A B C D E F G H I J K L M N O P Q R S T U V W X Y Z"

   if { [info exist Animator::Play(Data$VP)] } {
      set datas  $Animator::Play(Data$VP)
   } else {
      set datas $Viewport::Data(Data$VP)
   }

   foreach data $datas {
      if { [fstdfield is $data] }  {
         set id   [lindex $ids $i]
         incr i
         set lbl  [DataBar::IdField $data]
         set font XFont12
         set col  [fstdfield configure $data -color]
      } elseif { [observation is $data] } {
         set id   ""
         set lbl  [DataBar::IdObs $data]
         set font XFont12
         set col  [observation configure $data -color]
      } elseif { [metobs is $data] } {
         set id   ""
         set lbl  [DataBar::IdMetObs $data]
         set font XFont12
         set col  black
      } elseif { [trajectory is $data] } {
         set id   ""
         set lbl  [DataBar::IdTraj $data]
         set font XFont12
         set col  [trajectory configure $data -color]
      }
      set h  [font metrics $font -linespace]
      set y [expr $y+$h]
      if { $y>$Y1 } {
         break
      }

      $Frame.page.canvas create text $x $y -text $lbl -tags "$Page::Data(Tag) DB$VP DBDEL$VP" -anchor sw -font $font -fill $col
      $Frame.page.canvas create text [expr $X0+10] $y -text $id -tags "$Page::Data(Tag) DB$VP DBDEL$VP" -anchor s -font $font
      $Frame.page.canvas create line $X0 $y $X1 $y -tags "$Page::Data(Tag) DB$VP DBDEL$VP" -fill #CCCCCC

      if { ![winfo exists $Frame.page.canvas.up$n] } {
         button $Frame.page.canvas.up$n -bg $GDefs(ColorFrame) -bitmap @$GDefs(Dir)/Resources/Bitmap/up.xbm -bd 0 -cursor hand1 -bd 1 -relief raised -height 9 -width 9
         button $Frame.page.canvas.dn$n -bg $GDefs(ColorFrame) -bitmap @$GDefs(Dir)/Resources/Bitmap/down.xbm -bd 0 -cursor hand1 -bd 1 -relief raised -height 9 -width 9
      }
      $Frame.page.canvas.up$n configure -command "set Viewport::Data(Data$VP) \[linsert \[lreplace \$Viewport::Data(Data$VP) $n $n\] [expr $n-1] $data\] ;Viewport::UpdateData $Frame $VP; Page::UpdateCommand $Frame"
      $Frame.page.canvas.dn$n configure -command "set Viewport::Data(Data$VP) \[linsert \[lreplace \$Viewport::Data(Data$VP) $n $n\] [expr $n+1] $data\] ;Viewport::UpdateData $Frame $VP; Page::UpdateCommand $Frame"
      $Frame.page.canvas create window $X1 $y           -window $Frame.page.canvas.up$n -anchor se -tags "$Page::Data(Tag) DB$VP DBDEL$VP NOPRINT"
      $Frame.page.canvas create window [expr $X1-12] $y -window $Frame.page.canvas.dn$n -anchor se -tags "$Page::Data(Tag) DB$VP DBDEL$VP NOPRINT"
      incr n
   }

   #----- Memo quelle est la formule ???
   if { [string trim $Viewport::Data(Operand$VP)]!="" } {
      set h  [font metrics XFont14 -linespace]
      set y [expr $y+$h]
      if { $y<=$Y1 } {
         $Frame.page.canvas create text $x $y -text "$Viewport::Data(Operand$VP)" -tags "$Page::Data(Tag) DB$VP DBDEL$VP" -anchor sw -font XFont14
         $Frame.page.canvas create text [expr $X0+10] $y -text "=" -tags "$Page::Data(Tag) DB$VP DBDEL$VP" -anchor s -font XFont14
      }
   }
}

proc DataBar::IdField { Field } {
   global GDefs

   set lbl [fstdfield define $Field -NOMVAR]

   if { [info exists MetStat::Rec(Desc$lbl)] } {
      append lbl " $MetStat::Rec(Desc$lbl)"
   }
   append lbl " ([lindex [fstdfield stats $Field -levels] [fstdfield stats $Field -level]] [fstdfield stats $Field -leveltype])"

   if { [set unit [fstdfield configure $Field -unit]]!="" } {
      append lbl " ($unit)"
   }
   append lbl " ([fstdfield define $Field -ETIKET])"
   append lbl " [lindex { "a" "at" } $GDefs(Lang)] [clock format [fstdstamp toseconds [fstdfield define $Field -DATEV]] -format "%H:%M %Y%m%d" -gmt true]"
   append lbl " ([fstdfield define $Field -IP2])"

   return $lbl
}

proc DataBar::IdObs { Obs } {
   global GDefs

   set lbl    "Obs [observation configure $Obs -desc]([observation define $Obs -NB])"
   append lbl " [lindex { "a" "at" } $GDefs(Lang)] [clock format [observation define $Obs -DATE] -format "%H:%M %Y%m%d" -gmt true]"

   return $lbl
}

proc DataBar::IdMetObs { MetObs } {
   global GDefs

   set lbl    "$MetObs"
   append lbl " [lindex { "a" "at" } $GDefs(Lang)] [clock format [metobs define $MetObs -VALID] -format "%H:%M %Y%m%d" -gmt True]"

   return $lbl
}

proc DataBar::IdTraj { Traj } {
   global GDefs

   if { [trajectory define $Traj -BACKWARD] } {
      set info { "arrivant" "arriving" }

      switch [trajectory define $Traj -MODE] {
         "0" { set lbl [lindex { "Prevision de retrotrajectoires" "Back trajectory forecasts" } $GDefs(Lang)] }
         "1" { set lbl [lindex { "Prevision a posteriori de retrotrajectoires" "Back trajectory hindcasts" } $GDefs(Lang)] }
         "2" { set lbl [lindex { "Retro-trajectoires mixtes" "Mixed mode back trajectories" } $GDefs(Lang)] }
         "3" { set lbl [lindex { "Retro-trajectoires" "Back trajectories" } $GDefs(Lang)] }
      }
   } else {
      set info { "debutant" "starting" }

      switch [trajectory define $Traj -MODE] {
         "0" { set lbl [lindex { "Prevision de trajectoires" "Trajectory forecasts" } $GDefs(Lang)] }
         "1" { set lbl [lindex { "Prevision a posteriori de trajectoires" "Trajectory hindcasts" } $GDefs(Lang)] }
         "2" { set lbl [lindex { "Trajectoires mixtes" "Mixed mode trajectories" } $GDefs(Lang)] }
         "3" { set lbl [lindex { "Trajectoires" "Trajectories" } $GDefs(Lang)] }
      }
   }

   append lbl " [trajectory define $Traj -ID]"
   set pr [trajectory define $Traj -PARCEL 0]
   append lbl " [Convert::FormatCoord [lindex $pr 1] [lindex $pr 2] DEG] [lindex $pr 5]"

   if { [trajectory define $Traj -LEVELTYPE]=="P" } {
      append lbl " ASL"
   } else {
       append lbl " AGL"
   }

   append lbl " [lindex $info $GDefs(Lang)] [clock format [lindex $pr 0] -format "%H:%M %Y%m%d" -gmt true]]"

   return $lbl
}

#------------------------------------------------------------------------------
# Nom      : <DataBar::Destroy>
# Creation : Fevrier 2002 - J.P. Gauthier - CMC/CMOE -
#
# But     : Supprimer un Colorbar
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <VP>     : Identificateur du Viewport
#   <No>     : Numero de champs
#
# Retour     :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc DataBar::Destroy { Frame VP } {
   variable Data

   if { [info exists DataBar::Data($VP)] } {

      set idx [lsearch -exact $Data(List$Frame) DB$VP]

      if { $idx!=-1 } {
         Shape::UnBindScale $Frame.page.canvas DB$VP
         Shape::UnBindFull  $Frame.page.canvas DB$VP
         $Frame.page.canvas delete DB$VP
         unset Data(Active$Frame)

         set Data(List$Frame) [lreplace $Data(List$Frame) $idx $idx]
      }
   }
}

#------------------------------------------------------------------------------
# Nom      : <DataBar::DestroyAll>
# Creation : Fevrier 2002 - J.P. Gauthier - CMC/CMOE -
#
# But     : Supprimer toutes les Colorbars
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <VP>     : Identificateur du viewport
#
# Retour     :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc DataBar::DestroyAll { Frame { VP "" } } {
   variable Data

   if { [info exist Data(List$Frame)] } {
      foreach idx [lsort -decreasing -integer [lsearch -all $Data(List$Frame) DB$VP*]] {
         set db [lindex $Data(List$Frame) $idx]
         Shape::UnBindScale $Frame.page.canvas $db
         Shape::UnBindFull  $Frame.page.canvas $db
         $Frame.page.canvas delete $db
         set Data(List$Frame) [lreplace $Data(List$Frame) $idx $idx]
      }
   }
}

#------------------------------------------------------------------------------
# Nom      : <DataBar::Full>
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

proc DataBar::Full { Frame Tag VP { Pix 0 } } {
   variable Data

   set co [$Frame.page.canvas coords $Tag]
   set yc [lindex $co 1]
   set hc [expr [lindex $co 3]- [lindex $co 1]]

   set yv [$Frame.page.canvas itemcget $VP -y]
   set xv [$Frame.page.canvas itemcget $VP -x]
   set hv [$Frame.page.canvas itemcget $VP -height]
   set wv [$Frame.page.canvas itemcget $VP -width]

   if { $yc>[expr $yv+$hv*0.5] } {
      set yc [expr $yc-$Pix]
   }

   set Data($VP) [list $xv $yc [expr $xv+$wv] [expr $yc+$hc]]
   eval DataBar::Draw $Frame $VP $Data($VP)

   return [list [expr $xv+$wv] [expr $yc+$hc]]
}

#------------------------------------------------------------------------------
# Nom      : <DataBar::Move>
# Creation : Fevrier 2002 - J.P. Gauthier - CMC/CMOE -
#
# But     : Enregistrer le changement de position de la colorbar
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <VP>     : Identificateur du Viewport
#   <Tag>    : Identificateur de la barre
#
# Retour     :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc DataBar::Move { Frame VP Tag } {
   variable Data

   set Data(Full$VP) False
   set Data($VP) [$Frame.page.canvas coords FR$Tag]
}

#------------------------------------------------------------------------------
# Nom      : <DataBar::Scale>
# Creation : Fevrier 2002 - J.P. Gauthier - CMC/CMOE -
#
# But     : Changement de dimension de la colorbar
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <VP>     : Identificateur du Viewport
#   <Tag>    : Identificateur de la barre
#   <X>      : Dimension en X
#   <Y>      : Dimension en Y
#
# Retour     :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc DataBar::Scale { Frame VP Tag X Y } {
   variable Data

   set x0 [lindex $Data($VP) 0]
   set y0 [lindex $Data($VP) 1]
   set x1 [lindex $Data($VP) 2]
   set y1 [lindex $Data($VP) 3]

   if { $X>[expr $x0+25] && $Y>[expr $y0+25] } {
      set dx [expr (double($X)-$x0)/(double($x1)-$x0)]
      set dy [expr (double($Y)-$y0)/(double($y1)-$y0)]

      $Frame.page.canvas scale $Tag $x0 $y0 $dx $dy
      set Data($VP) [$Frame.page.canvas coords FR$Tag]
      set Data(Full$VP) False
      eval DataBar::Draw $Frame $VP $Data($VP)
      return True
   } else {
      return False
   }
}

#------------------------------------------------------------------------------
# Nom      : <DataBar::Update>
# Creation : Fevrier 2002 - J.P. Gauthier - CMC/CMOE -
#
# But     : Mise-a-jour des proprietes d'une colorbar
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <State>  : Etat d'activation de la colorbar
#
# Retour     :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc DataBar::Update { Frame { State -1 } } {
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
         if { [info exists Data($vp)] } {
            set x [lindex $Data($vp) 0]
            set y [lindex $Data($vp) 1]
            set w  [expr [lindex $Data($vp) 2]-[lindex $Data($vp) 0]]
            set h  [expr [lindex $Data($vp) 3]-[lindex $Data($vp) 1]]
         } else {
            set x [$Frame.page.canvas itemcget $vp -x]
            set y [$Frame.page.canvas itemcget $vp -y]
            set w [$Frame.page.canvas itemcget $vp -width]
            set h 50
         }
         DataBar::Create $Frame $vp $x $y $w $h
         lappend lst DB$vp
      }
   }

   if { [info exist Data(List$Frame)] } {
      foreach id $Data(List$Frame) {
         if { [lsearch -exact $lst $id]==-1 } {
            Shape::UnBindScale $Frame.page.canvas $id
            Shape::UnBindFull  $Frame.page.canvas $id
            $Frame.page.canvas delete $id
         }
      }
      set Data(List$Frame) $lst
   }
}

#------------------------------------------------------------------------------
# Nom      : <DataBar::UpdateVP>
# Creation : Fevrier 2002 - J.P. Gauthier - CMC/CMOE -
#
# But     : Mise-a-jour des proprietes d'une colorbar pour un set de donnees
#           specifiques
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <VP>     : Identificateur du viewport
#
# Retour     :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc DataBar::UpdateVP { Frame VP } {
   variable Data

   if { $Data(Active$Frame) && [info exist Data(List$Frame)] } {
      set idx [lsearch -exact $Data(List$Frame) DB$VP]
      if { $idx!=-1 } {
         if { [info exists Data($VP)] } {
            set x [lindex $Data($VP) 0]
            set y [lindex $Data($VP) 1]
            set w  [expr [lindex $Data($VP) 2]-[lindex $Data($VP) 0]]
            set h  [expr [lindex $Data($VP) 3]-[lindex $Data($VP) 1]]
         } else {
            set x [$Frame.page.canvas itemcget $VP -x]
            set y [$Frame.page.canvas itemcget $VP -y]
            set w [$Frame.page.canvas itemcget $VP -width]
            set h 50
         }
         DataBar::Create $Frame $VP
      }
   }
}

#------------------------------------------------------------------------------
# Nom      : <DataBar::Write>
# Creation : Novembre 2003 - J.P. Gauthier - CMC/CMOE -
#
# But     : Engeristrer les parametres des DataBar dans un fichier Layout
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <File>   : Identificateur de Fichier
#
# Retour     :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc DataBar::Write { Frame File } {
   variable Data

   puts $File "   #-----  Positionnement des DataBars"
   puts $File ""
   puts $File "   set DataBar::Data(Active\$Frame) 1"

   foreach vp [Page::Registered $Frame Viewport] {
      if { [info exists DataBar::Data($vp)] } {
         puts $File "   set DataBar::Data(\$$Viewport::Data(Alias$vp)) \[list $DataBar::Data($vp)\]"
      }
   }
   puts $File ""
}
