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
#    DataBar::SetTitle   { Frame VP Title }
#    DataBar::Draw       { Frame VP X0 Y0 X1 Y1 }
#    DataBar::IdField    { Field }
#    DataBar::IdObs      { Obs }
#    DataBar::IdMetObs   { MetObs }
#    DataBar::IdTraj     { Traj }
#    DataBar::Destroy    { Frame VP No }
#    DataBar::DestroyAll { Frame }
#    DataBar::Full       { Frame Tag VP { Pix 0 } }
#    DataBar::Move       { Canvas Tag0 Tag1 }
#    DataBar::Scale      { Canvas Tag0 Tag1 X Y }
#    DataBar::Update     { Frame { State -1 } }
#    DataBar::UpdateVP   { Frame VP List }
#    DataBar::Write      { Frame File }
#
#===============================================================================

package provide DataBar 1.2

catch { SPI::Splash "Loading Canvas Package DataBar 1.2" }

namespace eval DataBar {
   global GDefs
   variable Data
   variable Param
   variable Lbl

   set Param(Title)  "Title"
   set Param(Full)   1
   set Param(Font)   XFont12
   
   set Param(NOMVAR) True
   set Param(DATEV)  True
   set Param(DATEO)  False
   set Param(ETIKET) False
   set Param(IP1)    False
   set Param(IP2)    False
   set Param(IP3)    False
   set Param(Desc)   True
   set Param(Unit)   True
   set Param(Level)  True
   set Param(Range)  False

   set Lbl(Desc)     { "Description" "Description" }
   set Lbl(Unit)     { "Unité" "Unit" }
   set Lbl(Level)    { "Niveau" "Level" }
   set Lbl(Range)    { "Intervalle" "Interval" }
   set Lbl(Valid)    { "valide à" "valid at" }
   set Lbl(Run)      { "passe de" "run of" }
   set Lbl(Between)  { "entre" "between" }
   set Lbl(And)      { "et" "and" }
   set Lbl(On)       { "sur" "on" }
   set Lbl(Elements) { "éléments" "elements" }
   

   image create photo DATABARLOGO -file $GDefs(Dir)/share/image/Symbol/Logo/Flag.gif
}

#------------------------------------------------------------------------------
# Nom      : <DataBar::Active>
# Creation : Octobre 2001 - J.P. Gauthier - CMC/CMOE -
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
#   <Full>   : Make it full width of the viewport
#
# Retour     :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc DataBar::Create { Frame VP X0 Y0 Width Height { Title "" } { Full "" } } {
   global GDefs
   variable Data
   variable Param
   variable Lbl

   if { $Title!="" } {
      set Data(Title$VP) $Title
   } else {
      set Data(Title$VP) $Param(Title)
   }

   if { $Full!="" } {
      set Data(Full$VP) $Full
   } else {
      set Data(Full$VP) $Param(Full)
   }

   set x0  $X0
   set y0  $Y0
   set x1  [expr $X0+$Width]
   set y1  [expr $Y0+$Height]

   set Data(Active$Frame) 1
   set Data($VP)          [list $x0 $y0 $x1 $y1 $Data(Title$VP) $Data(Full$VP)]

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

   if { ![winfo exists $Frame.page.canvas.bo$VP] } {
      menubutton $Frame.page.canvas.bo$VP -bg $GDefs(ColorFrame) -bitmap @$GDefs(Dir)/share/bitmap/cvmenu.xbm -cursor hand1 -bd 1 -relief raised \
         -menu $Frame.page.canvas.bo$VP.menu
      menu $Frame.page.canvas.bo$VP.menu -bg $GDefs(ColorFrame)
         $Frame.page.canvas.bo$VP.menu add checkbutton -label [lindex $Lbl(Desc) $GDefs(Lang)] -variable DataBar::Param(Desc)     -onvalue True -offvalue False -command "DataBar::UpdateVP $Frame $VP"
         $Frame.page.canvas.bo$VP.menu add checkbutton -label [lindex $Lbl(Unit) $GDefs(Lang)] -variable DataBar::Param(Unit)     -onvalue True -offvalue False -command "DataBar::UpdateVP $Frame $VP"
         $Frame.page.canvas.bo$VP.menu add checkbutton -label [lindex $Lbl(Level) $GDefs(Lang)] -variable DataBar::Param(Level)   -onvalue True -offvalue False -command "DataBar::UpdateVP $Frame $VP"
         $Frame.page.canvas.bo$VP.menu add checkbutton -label [lindex $Lbl(Range) $GDefs(Lang)] -variable DataBar::Param(Range)   -onvalue True -offvalue False -command "DataBar::UpdateVP $Frame $VP"
         $Frame.page.canvas.bo$VP.menu add checkbutton -label NOMAR -variable DataBar::Param(NOMVAR)  -onvalue True -offvalue False -command "DataBar::UpdateVP $Frame $VP"
         $Frame.page.canvas.bo$VP.menu add checkbutton -label ETIKET -variable DataBar::Param(ETIKET) -onvalue True -offvalue False -command "DataBar::UpdateVP $Frame $VP"
         $Frame.page.canvas.bo$VP.menu add checkbutton -label IP1 -variable DataBar::Param(IP1)       -onvalue True -offvalue False -command "DataBar::UpdateVP $Frame $VP"
         $Frame.page.canvas.bo$VP.menu add checkbutton -label IP2 -variable DataBar::Param(IP2)       -onvalue True -offvalue False -command "DataBar::UpdateVP $Frame $VP"
         $Frame.page.canvas.bo$VP.menu add checkbutton -label IP3 -variable DataBar::Param(IP3)       -onvalue True -offvalue False -command "DataBar::UpdateVP $Frame $VP"
         $Frame.page.canvas.bo$VP.menu add checkbutton -label DATEV -variable DataBar::Param(DATEV)   -onvalue True -offvalue False -command "DataBar::UpdateVP $Frame $VP"
         $Frame.page.canvas.bo$VP.menu add checkbutton -label DATEO -variable DataBar::Param(DATEO)   -onvalue True -offvalue False -command "DataBar::UpdateVP $Frame $VP"
   }
   $Frame.page.canvas create window [expr $x1-22] $y1 -window $Frame.page.canvas.bo$VP -anchor se -tags "BODB$VP NOPRINT"
   $Frame.page.canvas bind DB$VP <Button-3> "tk_popup $Frame.page.canvas.bo$VP.menu %X %Y 0"
               
   Shape::BindAllMove $Frame.page.canvas DB$VP "DataBar::Move $Frame $VP DB$VP"
   Shape::BindScale   $Frame.page.canvas DB$VP "DataBar::Scale $Frame $VP DB$VP"
   Shape::BindFull    $Frame.page.canvas DB$VP DataBar::Data(Full$VP) "DataBar::Full $Frame FRDB$VP $VP"
   Shape::BindWidget  $Frame.page.canvas DB$VP

   Page::MaskItem $Frame
   
   return DB$VP
}

#------------------------------------------------------------------------------
# Nom      : <DataBar::SetTitle>
# Creation : Octobre 20012- J.P. Gauthier - CMC/CMOE -
#
# But     : Definir le titre de la legende
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <VP>     : Identificateur du Viewport
#   <Title>  : Titre
#
# Retour     :
#
# Remarques  :
#
#-------------------------------------------------------------------------------

proc DataBar::SetTitle { Frame VP Title } {
   variable Data

   set Data(Title$VP) $Title
   lset Data($VP) 3 $Data(Title$VP)

   $Frame.page.canvas itemconfigure TXTDB$VP -text $Data(Title$VP)
}

#------------------------------------------------------------------------------
# Nom      : <DataBar::Draw>
# Creation : Octobre 20012- J.P. Gauthier - CMC/CMOE -
#
# But     : Afficher la legende
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <VP>     : Identificateur du Viewport
#   <X0>     : Coordonee X du coin superieur gauche
#   <Y0>     : Coordonee Y du coin superieur gauche
#   <X1>     : Coordonee X du coin inferieur droit
#   <Y1>     : Coordonee Y du coin inferieur droit
#
# Retour     :
#
# Remarques  :
#
#-------------------------------------------------------------------------------

proc DataBar::Draw { Frame VP X0 Y0 X1 Y1 { Title "" } { Full "" } } {
   global GDefs
   variable Param
   variable Data

   if { $Title!="" } {
      set Data(Title$VP) $Title
   }

   if { $Full!="" } {
      set Data(Full$VP) $Full
   }
   
   if { [llength [$Frame.page.canvas find withtag TXTDB$VP]] } {
      set Data(Title$VP) [$Frame.page.canvas itemcget TXTDB$VP -text]
   }

   $Frame.page.canvas delete DB$VP

   if { [llength [$Frame.page.canvas find withtag $VP]] } {
      $Frame.page.canvas create rectangle $X0 $Y0 $X1 $Y1 -tags "PAGE DB$VP FRDB$VP VPINTRUDE" -fill white -outline black
   } else {
      $Frame.page.canvas create rectangle $X0 $Y0 $X1 $Y1 -tags "PAGE DB$VP FRDB$VP VPINTRUDE" -fill white -outline black
   }

   $Frame.page.canvas create line [expr $X0+20] [expr $Y0+20] [expr $X0+20] $Y1 -tags "PAGE DB$VP" -fill #CCCCCC
   $Frame.page.canvas create image [expr $X0+2] [expr $Y0+2] -image DATABARLOGO -anchor nw -tags "PAGE DB$VP"
   $Frame.page.canvas create text [expr $X0+50] [expr $Y0+1] -text $Data(Title$VP) -anchor nw -font XFont16 -tags "PAGE DB$VP TXTDB$VP CVTEXT"

   set h [font metrics $Param(Font) -linespace]
   set y [expr $Y0+21]
   set x [expr $X0+25]
   set i 0
   set n 0

   set ids "A B C D E F G H I J K L M N O P Q R S T U V W X Y Z"

   if { [info exist Animator::Play(Data$VP)] } {
      set datas $Animator::Play(Data$VP)
   } else {
      set datas $Viewport::Data(Data$VP)
   }

   foreach data $datas {
       set lbl ""

      if { [fstdfield is $data] }  {
         set id   [lindex $ids $i]
         incr i
         set lbl  [DataBar::IdField $data]
         set typ  fstdfield
      } elseif { [gribfield is $data] } {
         set id   ""
         set lbl  [DataBar::IdGrib $data]
         set typ  gribfield
      } elseif { [observation is $data] } {
         set id   ""
         set lbl  [DataBar::IdObs $data]
         set typ  observation
      } elseif { [metobs is $data] } {
         set id   ""
         set lbl  [DataBar::IdMetObs $data]
         set typ  ""
      } elseif { [trajectory is $data] } {
         set id   ""
         set lbl  [DataBar::IdTraj $data]
         set typ  trajectory
      }

      set y [expr $y+$h]
      if { $y>$Y1 } {
         break
      }

      #----- Get active status
      set col black
      set act 1
      catch {
         eval set col \[$typ configure $data -color\]
         eval set act \[$typ configure $data -active\]
      }
      if { ![set Data(OnOff$n) $act] } {
         set col gray75
      }

      $Frame.page.canvas create text $x $y -text $lbl -tags "PAGE DB$VP" -anchor sw -font $Param(Font) -fill $col
      $Frame.page.canvas create text [expr $X0+10] $y -text $id -tags "PAGE DB$VP" -anchor s -font $Param(Font)
      $Frame.page.canvas create line $X0 [expr $y-1] $X1 [expr $y-1] -tags "PAGE DB$VP" -fill #CCCCCC

      if { ![winfo exists $Frame.page.canvas.up$n] } {
         checkbutton $Frame.page.canvas.on$n -bg $GDefs(ColorFrame) -variable DataBar::Data(OnOff$n) -indicatoron False -cursor hand1 -bd 1 -activeforeground yellow -relief raised -image OPT_CHECK -selectimage "" -onvalue 1 -offvalue 0
         button $Frame.page.canvas.up$n -bg $GDefs(ColorFrame) -bitmap @$GDefs(Dir)/share/bitmap/up.xbm -cursor hand1 -bd 1 -relief raised -height 9 -width 9
         button $Frame.page.canvas.dn$n -bg $GDefs(ColorFrame) -bitmap @$GDefs(Dir)/share/bitmap/down.xbm -cursor hand1 -bd 1 -relief raised -height 9 -width 9
      }

      $Frame.page.canvas.up$n configure -command "set Viewport::Data(Data$VP) \[linsert \[lreplace \$Viewport::Data(Data$VP) $n $n\] [expr $n-1] $data\] ;Viewport::UpdateData $Frame $VP; Page::UpdateCommand $Frame"
      $Frame.page.canvas.dn$n configure -command "set Viewport::Data(Data$VP) \[linsert \[lreplace \$Viewport::Data(Data$VP) $n $n\] [expr $n+1] $data\] ;Viewport::UpdateData $Frame $VP; Page::UpdateCommand $Frame"
      $Frame.page.canvas.on$n configure -command "catch { $typ configure \[lindex \$Viewport::Data(Data$VP) $n\] -active \$DataBar::Data(OnOff$n) }; Animator::EmptyPlayList; Viewport::UpdateData $Frame $VP; Page::UpdateCommand $Frame"
      $Frame.page.canvas create window $X1 $y           -window $Frame.page.canvas.up$n -anchor se -tags "PAGE DB$VP UDDB$VP NOPRINT"
      $Frame.page.canvas create window [expr $X1-13] $y -window $Frame.page.canvas.dn$n -anchor se -tags "PAGE DB$VP UDDB$VP NOPRINT"
      $Frame.page.canvas create window [expr $X1-26] $y -window $Frame.page.canvas.on$n -anchor se -tags "PAGE DB$VP UDDB$VP NOPRINT"
      
      incr n
   }

   #----- Memo quelle est la formule ???
   if { [string trim $Viewport::Data(Operand$VP)]!="" } {
      set y [expr $y+$h]
      if { $y<=$Y1 } {
         $Frame.page.canvas create text $x $y -text "$Viewport::Data(Operand$VP)" -tags "PAGE DB$VP" -anchor sw -font $Param(Font)
         $Frame.page.canvas create text [expr $X0+10] $y -text "=" -tags "PAGE DB$VP" -anchor s -font $Param(Font)
      }
   }
}

#------------------------------------------------------------------------------
# Nom      : <DataBar::IdField>
# Creation : Octobre 20012- J.P. Gauthier - CMC/CMOE -
#
# But     : Creer le texte de la legend pour le type de donnee "fstdfield"
#
# Parametres :
#   <Field>  : Identificateur de la donnee
#
# Retour     :
#
# Remarques  :
#
#-------------------------------------------------------------------------------

proc DataBar::IdField { Field } {
   global GDefs
   variable Param
   variable Lbl

   if { $Param(NOMVAR) } { append lbl "[fstdfield define $Field -NOMVAR] - "  }
   if { $Param(Desc) }   { append lbl "[fstdfield configure $Field -desc] " }
   if { $Param(Unit) }   { append lbl "([fstdfield configure $Field -unit]) " }
   if { $Param(Level) }  { append lbl "[lrange [fstdgrid convip [fstdfield define $Field -IP1]] 0 1] " }
   if { $Param(ETIKET) } { append lbl "ETIKET=[fstdfield define $Field -ETIKET] " }
   if { $Param(IP1) }    { append lbl "IP1=[fstdfield define $Field -IP1] " }
   if { $Param(IP2) }    { append lbl "IP2=[fstdfield define $Field -IP2] " }
   if { $Param(IP3) }    { append lbl "IP3=[fstdfield define $Field -IP3] " }
   if { $Param(DATEV) }  { append lbl "[lindex $Lbl(Valid) $GDefs(Lang)] [clock format [fstdstamp toseconds [fstdfield define $Field -DATEV]] -format "%H:%M %Y%m%d" -timezone :UTC] " }
   if { $Param(DATEO) }  { append lbl "[lindex $Lbl(Run) $GDefs(Lang)] [clock format [fstdstamp toseconds [fstdfield define $Field -DATEO]] -format "%H:%M %Y%m%d" -timezone :UTC] " }
   if { $Param(Range) && [set ip3 [fstdfield define $Field -IP3]]>32768 }  { 
      
      set ip1 [lrange [fstdgrid convip [fstdfield define $Field -IP1]] 0 1]
      set ip2 [lrange [fstdgrid convip [fstdfield define $Field -IP2]] 0 1]
      set ip3 [lrange [fstdgrid convip $ip3] 0 1]
      
      switch [lindex $ip3 1] {
         m  -
         sg -
         mb -
         -  -
         m  -
         hy -
         th { append lbl "[lindex $Lbl(Between) $GDefs(Lang)] $ip1 [lindex $Lbl(And) $GDefs(Lang)] $ip3" }
         nb {}
         hr { append lbl "[lindex $Lbl(Between) $GDefs(Lang)] $ip3 [lindex $Lbl(And) $GDefs(Lang)] $ip2" }
         i  { append lbl "[lindex $Lbl(On) $GDefs(Lang)] $ip2 ([lindex $ip3 0] [lindex $Lbl(Elements) $GDefs(Lang)])" }
         x  {}
         mp {}
      }
   }

   return $lbl
}

#------------------------------------------------------------------------------
# Nom      : <DataBar::IdGrib>
# Creation : Juin 2013- J.P. Gauthier - CMC/CMOE -
#
# But     : Creer le texte de la legend pour le type de donnee "gribfield"
#
# Parametres :
#   <Field>  : Identificateur de la donnee
#
# Retour     :
#
# Remarques  :
#
#-------------------------------------------------------------------------------

proc DataBar::IdGrib { Field } {
   global GDefs

   if { $Param(NOMVAR) } { append lbl "[fstdfield define $Field -NOMVAR] - "  }
   if { $Param(Desc) }   { append lbl "[fstdfield configure $Field -desc] " }
   if { $Param(Unit) }   { append lbl "([fstdfield configure $Field -unit]) " }
   if { $Param(Level) }  { append lbl "[lrange [fstdgrid convip [fstdfield define $Field -IP1]] 0 1] " }
   if { $Param(DATEV) }  { append lbl "[lindex { "à" "at" } $GDefs(Lang)] [clock format [fstdstamp toseconds [fstdfield define $Field -DATEV]] -format "%H:%M %Y%m%d" -timezone :UTC] " }
   if { $Param(DATEO) }  { append lbl "[lindex { "passe de" "run of" } $GDefs(Lang)] [clock format [fstdstamp toseconds [fstdfield define $Field -DATEO]] -format "%H:%M %Y%m%d" -timezone :UTC]" }

   return $lbl
}
#------------------------------------------------------------------------------
# Nom      : <DataBar::IdObs>
# Creation : Octobre 20012- J.P. Gauthier - CMC/CMOE -
#
# But     : Creer le texte de la legend pour le type de donnee "observation"
#
# Parametres :
#   <Obs>    : Identificateur de la donnee
#
# Retour     :
#
# Remarques  :
#
#-------------------------------------------------------------------------------

proc DataBar::IdObs { Obs } {
   global GDefs

   set lbl    "Obs [observation configure $Obs -desc]([observation define $Obs -NB])"
   append lbl " [lindex { "a" "at" } $GDefs(Lang)] [clock format [observation define $Obs -DATE] -format "%H:%M %Y%m%d" -timezone :UTC]"

   return $lbl
}

#------------------------------------------------------------------------------
# Nom      : <DataBar::IdMetObs>
# Creation : Octobre 20012- J.P. Gauthier - CMC/CMOE -
#
# But     : Creer le texte de la legend pour le type de donnee "metobs"
#
# Parametres :
#   <MetObs> : Identificateur de la donnee
#
# Retour     :
#
# Remarques  :
#
#-------------------------------------------------------------------------------

proc DataBar::IdMetObs { MetObs } {
   global GDefs

   set lbl    "$MetObs"
   append lbl " [lindex { "a" "at" } $GDefs(Lang)] [clock format [metobs define $MetObs -VALID] -format "%H:%M %Y%m%d" -timezone :UTC]"

   return $lbl
}

#------------------------------------------------------------------------------
# Nom      : <DataBar::IdTraj>
# Creation : Octobre 20012- J.P. Gauthier - CMC/CMOE -
#
# But     : Creer le texte de la legend pour le type de donnee "trajectory"
#
# Parametres :
#   <Traj>    : Identificateur de la donnee
#
# Retour     :
#
# Remarques  :
#
#-------------------------------------------------------------------------------

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

   append lbl " [lindex $info $GDefs(Lang)] [clock format [lindex $pr 0] -format "%H:%M %Y%m%d" -timezone :UTC]]"

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
         Shape::UnBind $Frame.page.canvas DB$VP
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
         Shape::UnBind $Frame.page.canvas $db
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

   set Data($VP) [list $xv $yc [expr $xv+$wv] [expr $yc+$hc] $Data(Title$VP) $Data(Full$VP)]
   DataBar::Draw $Frame $VP $xv $yc [expr $xv+$wv] [expr $yc+$hc]

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
   set Data($VP) [concat [$Frame.page.canvas coords FR$Tag] $Data(Title$VP) $Data(Full$VP)]
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

      set Data(Full$VP) False
      $Frame.page.canvas scale $Tag $x0 $y0 $dx $dy
      set Data($VP) [concat [$Frame.page.canvas coords FR$Tag] $Data(Title$VP) $Data(Full$VP)]
      eval DataBar::Draw $Frame $VP [$Frame.page.canvas coords FR$Tag]
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
   variable Param

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
            set t  [lindex $Data($vp) 4]
            set f  [lindex $Data($vp) 5]
         } else {
            set x [$Frame.page.canvas itemcget $vp -x]
            set y [$Frame.page.canvas itemcget $vp -y]
            set w [$Frame.page.canvas itemcget $vp -width]
            set h 50
            set t $Param(Title)
            set f $Param(Full)
         }
         lappend lst [DataBar::Create $Frame $vp $x $y $w $h $t $f]
      }
   }

   if { [info exist Data(List$Frame)] } {
      foreach id $Data(List$Frame) {
         if { [lsearch -exact $lst $id]==-1 } {
            Shape::UnBind $Frame.page.canvas $id
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
         DataBar::Create $Frame $VP $x $y $w $h
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
   variable Param

   puts $File "   #-----  Positionnement des DataBars"
   puts $File ""
   puts $File "   set DataBar::Data(Active\$Frame) 1"
   puts $File "   set DataBar::Param(NOMVAR) $Param(NOMVAR)"
   puts $File "   set DataBar::Param(DATEV)  $Param(DATEV)"
   puts $File "   set DataBar::Param(DATEO)  $Param(DATEO)"
   puts $File "   set DataBar::Param(ETIKET) $Param(ETIKET)"
   puts $File "   set DataBar::Param(IP1)    $Param(IP1)"
   puts $File "   set DataBar::Param(IP2)    $Param(IP2)"
   puts $File "   set DataBar::Param(IP3)    $Param(IP3)"
   puts $File "   set DataBar::Param(Desc)   $Param(Desc)"
   puts $File "   set DataBar::Param(Unit)   $Param(Unit)"
   puts $File "   set DataBar::Param(Level)  $Param(Level)"
   puts $File "   set DataBar::Param(Range)  $Param(Range)"

   foreach vp [Page::Registered $Frame Viewport] {
      if { [info exists DataBar::Data($vp)] } {
         puts $File "   set DataBar::Data(\$$Viewport::Data(Alias$vp)) \[list [lindex $Data($vp) 0] [lindex $Data($vp) 1] [lindex $Data($vp) 2] [lindex $Data($vp) 3] \"[lindex $Data($vp) 4]\" [lindex $Data($vp) 5]\]"
      }
   }
   puts $File ""
}
