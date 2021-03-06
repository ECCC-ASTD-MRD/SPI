#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet    : Librairie de definitions pour les domaines des projections
# Fichier   : ProjCam.tcl
# Creation  : Septembre 2000 - J.P. Gauthier - CMC/CMOE
#
# Description: Definitions des structures et fonctions relatives aux
#              cameras et a leurs manipulations
#
# Fonctions:
#
#    ProjCam::CloseUp        { Frame VP Lat0 Lon0 Lat1 Lon1 Off }
#    ProjCam::Create         { Cam }
#    ProjCam::Delete         { Combo }
#    ProjCam::Do             { Cam Frame VP args }
#    ProjCam::Rotate         { Cam Frame X { Y 0.0 } { Z 0.0 } }
#    ProjCam::Mem            { Cam Name }
#    ProjCam::ParamFrame     { }
#    ProjCam::Read           { }
#    ProjCam::Reset          { Cam }
#    ProjCam::Save           { Combo }
#    ProjCam::Select         { Cam Frame Name }
#    ProjCam::Set            { Name To From Up Lens CFX CFY CFZ CTX CTY CTZ Lat Lon }
#    ProjCam::XYInit         { Cam Frame VP X Y }
#    ProjCam::XYDo           { Cam Frame VP X Y }
#    ProjCam::XYDone         { Cam Frame }
#    ProjCam::ZInit          { Cam Frame VP X Y }
#    ProjCam::ZDo            { Cam Frame VP X Y }
#    ProjCam::ZDone          { Cam Frame }
#    ProjCam::Zoom           { Cam Frame Lens { Store False } }
#    ProjCam::ZoomBox        { Frame X0 Y0 X Y }
#    ProjCam::ZoomInit       { Frame X Y VP }
#    ProjCam::ZoomIn         { Cam Frame VP { Factor 0 } }
#    ProjCam::ZoomOut        { Cam Frame VP Reset { Pos False } }
#    ProjCam::ZoomScroll     { Cam Frame VP X Y Lens }
#    ProjCam::Write          { Frame File }
#
# Remarques :
#   Aucune
#
#===============================================================================

package provide ProjCam 2.2

catch { SPI::Splash "Loading Canvas Package ProjCam 2.2" }

package require Bubble
package require Dialog

#----- Definitions des constantes

namespace eval ProjCam {
   variable Param
   variable Data
   variable Lbl
   variable Msg
   variable Bubble

   #----- Definitions des parametres de la camera

   set Param(To)       { 0.0 0.0 1.0 }
   set Param(From)     { 0.0 0.0 2.0 }
   set Param(Up)       { 0.0 1.0 0.0 }
   set Param(Lens)     1.0                   ;#Valeur de zoom
   set Param(Speed)    0.0                   ;#Valeur de vitesse
   set Param(Function) EXPONENTIAL           ;#Goto movement function (LINEAR, QUADRATIC,EXPONENTIAL)
   set Param(Proc)     ""                    ;#Print movement trace

   set Param(CFX) 0.0
   set Param(CFY) 0.0
   set Param(CFZ) 1.0
   set Param(CTX) 0.0
   set Param(CTY) 0.0
   set Param(CTZ) 1.0

   set Data(TmpX)   0
   set Data(TmpY)   0
   set Data(Name)   ""                            ;#Nome de la camera courante
   set Data(Names)  {}                            ;#Liste des noms de vues
   set Data(Combo)  ""

   #----- Definitions des labels

   set Lbl(Yes)       { "Oui" "Yes" }
   set Lbl(No)        { "Non" "No" }

   #----- Definitions des textes

   set Msg(Del)   { "Voulez-vous vraiment supprimer cette d??finition de cam??ra ?"
                    "Do you really want to delete this camera definition ? " }

   set Msg(Saved) { "D??finition de cam??ra sauvegard??e."
                    "Camera definition saved." }

   set Msg(Exist) { "Ce nom de cam??ra existe d??ja, voulez-vous l'??craser ?"
                    "This camera name exists do you wish to overwrite it ?" }
}

#----------------------------------------------------------------------------
# Nom      : <ProjCam::CloseUp>
# Creation : Fevrier 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Deplacement et Zoom maximum sur une region.
#
# Parametres :
#  <Cam>     : Identificateur de Camera
#  <Frame>   : Identificateur de la Page
#  <VP>      : Identificateur du Viewport
#  <Lat0>    : Latitude du coin inferieur gauche
#  <Lon0>    : Longitude du coin inferieur gauche
#  <Lat1>    : Latitude du coin superieur droit
#  <Lon1>    : Longitude du coin superieur droit
#  <Off>     : Recul en pourcentage
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc ProjCam::CloseUp { Cam Frame VP Lat0 Lon0 Lat1 Lon1 { Off 0.0 } } {
   variable Data

   upvar #0 ProjCam::Data${Cam}::Cam cam

   set Viewport::Map(Grabbed) [clock click -milliseconds]

   #----- Calculer les deltas centraux
   set dlat [expr $Lat1-$Lat0]

   if { $Lon1<$Lon0 } {
      set dlon [expr abs(-180-$Lon1)+abs(180-$Lon0)]
   } else {
      set dlon [expr $Lon1-$Lon0]
   }

   #----- Centrer la projection
   set lat [expr $Lat0+$dlat/2.0]
   set lon [Viewport::CheckCoord [expr $Lon0+$dlon/2.0]]

   if { $Viewport::Map(Type$Frame)=="cylindric" || $Viewport::Map(Type$Frame)=="mercator" } {
      set lens 0.5
   } else {
      set lens 1.0
   }

   #----- In software mode, stop the geo loading while checking location
   if { $OpenGL::Param(Res)!=1 } {
      set res [projection configure $Frame -mapres]
      projection configure $Frame -mapres -1
   }
   projcam configure $Cam -lens $lens
   projection configure $Frame -location $lat $lon
   $Frame.page.canvas itemconf $VP -frame 0 -update True
   update idletasks

   if { $OpenGL::Param(Res)!=1 } {
      projection configure $Frame -mapres $res
   }

   #----- Initialiser aux limites
   set w [lindex [$Frame.page.canvas itemconfigure $VP -width] 4]
   set h [lindex [$Frame.page.canvas itemconfigure $VP -height] 4]

   set x0 [expr $w+[lindex [$Frame.page.canvas itemconfigure $VP -x] 4]]
   set y0 [expr $h+[lindex [$Frame.page.canvas itemconfigure $VP -y] 4]]
   set x1 0
   set y1 0

   #----- Determiner les minmax
   if { [llength [set c [$VP -project $Lat0 $Lon0 0 True]]] } {
      set x [lindex $c 0]
      set y [lindex $c 1]
      set x0 [expr $x<$x0?$x:$x0]
      set x1 [expr $x>$x1?$x:$x1]
      set y0 [expr $y<$y0?$y:$y0]
      set y1 [expr $y>$y1?$y:$y1]
   }
   if { [llength [set c [$VP -project $Lat0 $Lon1 0 True]]] } {
      set x [lindex $c 0]
      set y [lindex $c 1]
      set x0 [expr $x<$x0?$x:$x0]
      set x1 [expr $x>$x1?$x:$x1]
      set y0 [expr $y<$y0?$y:$y0]
      set y1 [expr $y>$y1?$y:$y1]
   }

   if { [llength [set c [$VP -project $Lat1 $Lon1 0 True]]] } {
      set x [lindex $c 0]
      set y [lindex $c 1]
      set x0 [expr $x<$x0?$x:$x0]
      set x1 [expr $x>$x1?$x:$x1]
      set y0 [expr $y<$y0?$y:$y0]
      set y1 [expr $y>$y1?$y:$y1]
   }

   if { [llength [set c [$VP -project $Lat1 $Lon0 0 True]]] } {
      set x [lindex $c 0]
      set y [lindex $c 1]
      set x0 [expr $x<$x0?$x:$x0]
      set x1 [expr $x>$x1?$x:$x1]
      set y0 [expr $y<$y0?$y:$y0]
      set y1 [expr $y>$y1?$y:$y1]
   }

   #----- Check pour inversion
   if { $x0>$x1 } {
      set x $x0
      set x0 $x1
      set x1 $x
   }

   if { $y0>$y1 } {
      set y $y0
      set y0 $y1
      set y1 $y
   }

   set dx [expr $x1-$x0]
   set dy [expr $y1-$y0]

   if { $dx!=0 && $dy!=0 } {
      if { [expr $dx/$w] > [expr $dy/$h] } {
         #----- La longitude est prioritaire
         set lens [expr double($w/$dx)*$lens]
     } else {
         #----- La latitude est prioritaire
         set lens [expr double($h/$dy)*$lens]
      }
      lappend cam(LLens) [list $cam(Lens) $Viewport::Map(Lat) $Viewport::Map(Lon)]
      set Data(Name) ""

      #----- Appliquer le recul
      if { $Off!=0.0 } {
         set lens [expr $lens-$lens*$Off]
      }
      set Viewport::Map(Lat) [expr $Viewport::Map(Lat)+0.001]
      set Viewport::Map(Lon) [expr $Viewport::Map(Lon)+0.001]

      Viewport::GoTo $Frame $lat $lon $lens
   }
}

#----------------------------------------------------------------------------
# Nom      : <ProjCam::Create>
# Creation : Octobre 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer une nouvelle camera et initialiser ses parametres
#
# Parametres :
#   <Cam>    : Identificateur de Camera
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc ProjCam::Create { Cam } {
   variable Param


   if { [projcam is $Cam] } {
      return
   }

   namespace eval Data$Cam {
      variable Cam

      set Cam(To)     $ProjCam::Param(To)
      set Cam(From)   $ProjCam::Param(From)
      set Cam(Up)     $ProjCam::Param(Up)
      set Cam(CTX)    $ProjCam::Param(CTX)
      set Cam(CTY)    $ProjCam::Param(CTY)
      set Cam(CTZ)    $ProjCam::Param(CTZ)
      set Cam(CFX)    $ProjCam::Param(CFX)
      set Cam(CFY)    $ProjCam::Param(CFY)
      set Cam(CFZ)    $ProjCam::Param(CFZ)
      set Cam(Lens)   $ProjCam::Param(Lens)   ;#Valeur de zoom
      set Cam(LLens)  {}               ;#Liste des zoom repetitifs
   }

   projcam create $Cam
   projcam configure $Cam -lens $Param(Lens) -from $Param(From) -to $Param(To) -up $Param(Up)
   projcam define $Cam -circleto $Param(CTX)  $Param(CTY) $Param(CFZ)
   projcam define $Cam -circlefrom $Param(CFX)  $Param(CFY) $Param(CFZ)
}

#----------------------------------------------------------------------------
# Nom      : <ProjCam::Delete>
# Creation : Septembre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Supprime une camera de la liste
#
# Parametres :
#   <Combo>  : Identificateur du combobox
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc ProjCam::Delete { Combo } {
   global env
   variable Data
   variable Lbl
   variable Msg

   if { [ComboBox::Index $Combo exact $Data(Name)]!=-1 && $Data(Name)!="" } {

      if { [Dialog::Default . 200 WARNING $Msg(Del) "\n\n\t$Data(Name)" 0 $Lbl(No) $Lbl(Yes)] } {

         file copy -force $env(HOME)/.spi/ProjCam $env(HOME)/.spi/ProjCam.old
         exec grep -v "$Data(Name).*" $env(HOME)/.spi/ProjCam.old > $env(HOME)/.spi/ProjCam

         #----- Supprimer la vue a la liste des vues

         ComboBox::Del $Combo $Data(Name)
         set Data(Name) ""
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <ProjCam::Do>
# Creation : Novembre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectue la rotation de la camera.
#
# Parametres :
#  <Cam>     : Identificateur de Camera
#  <Frame>   : Identificateur de Page
#  <VP>      : Identificateur du Viewport
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc ProjCam::Do { Cam Frame VP args } {
   variable Data

   upvar #0 ProjCam::Data${Cam}::Cam  cam

   projcam define $Cam -circlefrom $cam(CFX) $cam(CFY) $cam(CFZ)
   set cam(From) [projcam configure $Cam -from]
   set cam(Up)   [projcam configure $Cam -up]

   set Data(Name) ""

   Page::Update $Frame
}

#----------------------------------------------------------------------------
# Nom      : <ProjCam::Rotate>
# Creation : Novembre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectue la rotation de la camera selon un increment.
#
# Parametres :
#  <Cam>     : Identificateur de Camera
#  <Frame>   : Identificateur de Page
#  <X>       : Rotation en X
#  <Y>       : Rotation en Y
#  <Z>       : Rotation en Z
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc ProjCam::Rotate { Cam Frame X { Y 0.0 } { Z 0.0 } } {
   variable Data

   upvar #0 ProjCam::Data${Cam}::Cam  cam

   set cam(CFX) [expr $cam(CFX)+$X]
   set cam(CFY) [expr $cam(CFY)+$Y]
   set cam(CFZ) [expr $cam(CFZ)+$Z]

   ProjCam::Do $Cam $Frame -
}

#----------------------------------------------------------------------------
# Nom      : <ProjCam::Fly>
# Creation : Avril 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Boucler sur les delacement de camera
#
# Parametres :
#   <Cam>    : Identificateur de Camera
#   <Frame>  : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc ProjCam::Fly { Cam Frame } {
   variable Param
   variable Data

   upvar #0 ProjCam::Data${Cam}::Cam cam

   while (1) {

      set cam(CFZ) [expr $cam(CFZ)-$Param(Speed)]
      Log::Print DEBUG "$Param(Speed) $cam(CTX) $cam(CTY) $cam(CFZ)"
      set cam(To) [projcam define $Cam -circleto $cam(CTX) $cam(CTY) $cam(CFZ)]

      Page::Update $Frame
      update
   }
}

proc ProjCam::FlyPath { Cam Type { List {}} } {
   variable Data
   variable Fly

   upvar #0 ProjCam::Data${Cam}::Cam  cam

   set flypath {}
   
   switch $Type {
      "CIRCLE" {
         projcam configure $Cam -from $cam(From) -up $cam(Up)
         lappend flypath [list $cam(From) $cam(To) $cam(Up) $cam(Lens)]

         projcam define $Cam -circlefrom [expr $cam(CFX)+45] $cam(CFY) $cam(CFZ)
         lappend flypath [list [projcam configure $Cam -from] [projcam configure $Cam -to] [projcam configure $Cam -up] $cam(Lens)]

         projcam define $Cam -circlefrom [expr $cam(CFX)+90] $cam(CFY) $cam(CFZ)
         lappend flypath [list [projcam configure $Cam -from] [projcam configure $Cam -to] [projcam configure $Cam -up] $cam(Lens)]

         projcam define $Cam -circlefrom [expr $cam(CFX)+135] $cam(CFY) $cam(CFZ)
         lappend flypath [list [projcam configure $Cam -from] [projcam configure $Cam -to] [projcam configure $Cam -up] $cam(Lens)]

         projcam define $Cam -circlefrom [expr $cam(CFX)+180] $cam(CFY) $cam(CFZ)
         lappend flypath [list [projcam configure $Cam -from] [projcam configure $Cam -to] [projcam configure $Cam -up] $cam(Lens)]

         projcam define $Cam -circlefrom [expr $cam(CFX)+225] $cam(CFY) $cam(CFZ)
         lappend flypath [list [projcam configure $Cam -from] [projcam configure $Cam -to] [projcam configure $Cam -up] $cam(Lens)]

         projcam define $Cam -circlefrom [expr $cam(CFX)+270] $cam(CFY) $cam(CFZ)
         lappend flypath [list [projcam configure $Cam -from] [projcam configure $Cam -to] [projcam configure $Cam -up] $cam(Lens)]

         projcam define $Cam -circlefrom [expr $cam(CFX)+315] $cam(CFY) $cam(CFZ)
         lappend flypath [list [projcam configure $Cam -from] [projcam configure $Cam -to] [projcam configure $Cam -up] $cam(Lens)]

         lappend flypath [list $cam(From) $cam(To) $cam(Up) $cam(Lens)]
      }
      "AROUND" {
         set ll [projection configure $Cam -location]
         foreach lon { 0 10 20 30 40 50 60 70 80 90 100 110 120 130 140 150 160 170 180 -170 -160 -150 -140 -130 -120 -110 -100 -90 -80 -70 -60 -50 -40 -30 -20 -10 } {
            lappend flypath [list $cam(From) $cam(To) $cam(Up) $cam(Lens) [lindex $ll 0] [expr [lindex $ll 1]+$lon]]
          }
      }
      "TO" {
         projcam configure $Cam -from $cam(From) -up $cam(Up)
         lappend flypath [list $cam(From) $cam(To) $cam(Up) $cam(Lens)]

         projcam define $Cam -circlefrom $cam(CFX) [expr (-89.0+$cam(CFY))/2.0] [expr $cam(CFZ)/3.0]
         lappend flypath [list [projcam configure $Cam -from] [projcam configure $Cam -to] [projcam configure $Cam -up] $cam(Lens)]

         projcam define $Cam -circlefrom $cam(CFX) -89.0 1e-20
         lappend flypath [list [projcam configure $Cam -from] [projcam configure $Cam -to] [projcam configure $Cam -up] $cam(Lens)]
      }
      "THROUGH" {
         projcam configure $Cam -from $cam(From) -up $cam(Up)
         lappend flypath [list $cam(From) $cam(To) $cam(Up) $cam(Lens)]

         projcam define $Cam -circlefrom [expr $cam(CFX)+90.0] -89.0 0.0
         lappend flypath [list [projcam configure $Cam -from] [projcam configure $Cam -to] $cam(Up) $cam(Lens)]

         projcam define $Cam -circlefrom [expr $cam(CFX)+180.0] $cam(CFY)  $cam(CFZ)
         lappend flypath [list [projcam configure $Cam -from] [projcam configure $Cam -to] [projcam configure $Cam -up] $cam(Lens)]
      }
      "DEFAULT" {
         foreach idx $List {
            lappend flypath [list [lindex $ProjCam::Data(Params$idx) 1] [lindex $ProjCam::Data(Params$idx) 0] [lindex $ProjCam::Data(Params$idx) 2] [lindex $ProjCam::Data(Params$idx) 3] [lindex $ProjCam::Data(Params$idx) end-1] [lindex $ProjCam::Data(Params$idx) end]]
         }
      }
   }

   projcam define $Cam -path $flypath
#   $Page::Data(Canvas) itemconf $Page::Data(VP) -camera $Page::Data(Frame)

   return [llength $flypath]
}

#TODO: Finalize, need monotone interpolation to avoid overshoots

# proc ProjCam::FlyTo { Cam { Path {}} } {
# 
#    set t0 [set Viewport::Map(Grabbed) [clock click -milliseconds]]
# 
#    if { [llength $Path] } {
#       projcam define $Cam -path $Path
#    }
#    set Function QUADRATIC
#    
#    set Viewport::Map(Speed) [expr 20.0/$Viewport::Map(Delay)]
#    set n  [expr ([llength $Path]-1)/$Viewport::Map(Speed)]
#    set n2 [expr $n*0.5]
#    set idx 0.0
#    while { $idx<[expr [llength $Path]-1] } {
#       set i [expr $idx]
#        puts stderr $i...$n....$n2
#  
#       switch $Function {
#          "EXPONENTIAL" { set spd [expr 1.0-(pow($i,10)*($n/pow($n,10)))/$n] }
#          "QUADRATIC"   { set spd [expr 1.0-(($i-$n2)*($i-$n2)+($i-$n2))*($n/(($n2*$n2)+$n2))/$n] }
#          "LINEAR"      { set spd 1.0 }
#       }
#       puts stderr $idx...$spd
#             set idx [expr $idx+$spd*$Viewport::Map(Speed)]
#       set coords [projcam define $Cam -fly $idx]
#       set lat [lindex $coords 0]
#       set lon [lindex $coords 1]
# 
#       if { $lat!=-999 } {
#          projection configure $Page::Data(Frame) -location $lat $lon
#       }
#       Page::Update $Page::Data(Frame)
# update
#       if { ![projection is $Page::Data(Frame)] || $Viewport::Map(Grabbed)>$t0 } {
#          break;
#       }
#    }
#    set coords [projcam define $Cam -fly [expr [llength $Path]-1]]
#    set lat [lindex $coords 0]
#    set lon [lindex $coords 1]
# 
#    if { $lat!=-999 } {
#       projection configure $Page::Data(Frame) -location $lat $lon
#    }
# }

proc ProjCam::ToDo { Cam Frame VP X Y } {
   variable Data

   upvar #0 ProjCam::Data${Cam}::Cam  cam

   set cam(CTX) [expr $cam(CTX)+double($Data(TmpX)-$X)*0.1]
   set cam(CTY) [expr $cam(CTY)+double($Data(TmpY)-$Y)*0.1]
   set Data(TmpX) $X
   set Data(TmpY) $Y

#   projcam define $Cam -circleto $cam(CTX) $cam(CTY) $cam(CFZ)

#   set cam(To) [projcam configure $Cam -to]
#   set cam(Up) [projcam configure $Cam -up]

   set Data(Name) ""
}

#----------------------------------------------------------------------------
# Nom      : <ProjCam::Mem>
# Creation : Novembre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Memoriser les parametres de la camera
#
# Parametres :
#   <Cam>    : Identificateur de Camera
#   <Name>   : Nom de la camera
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc ProjCam::Mem { Cam Name } {
   variable Data

   upvar #0 ProjCam::Data${Cam}::Cam  cam

   #----- Camera, projection and page (Frame) all have the same name so use the cam name for the projection
   set ll [projection configure $Cam -location]
   set Data(Params$Name) "{$cam(To)} {$cam(From)} {$cam(Up)} $cam(Lens) $cam(CFX) $cam(CFY) $cam(CFZ) $cam(CTX) $cam(CTY) $cam(CTZ) [lindex $ll 0] [lindex $ll 1]"

   return $Data(Params$Name)
}

#----------------------------------------------------------------------------
# Nom      : <ProjCam::Read>
# Creation : Septembre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Lit le fichier de definitions de cameras
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc ProjCam::Read { { Paths {} } } {
   global env
   variable Data

   if { ![llength $Paths] } {
      set Paths $env(HOME)/.spi
      if { [info exists env(SPI_TOOL)] } {
         set Paths [concat [split $env(SPI_TOOL) :] $Paths]
      }
   }
   
   foreach path $Paths {
      if { [file exists $path/ProjCam] } {

         set file [open $path/ProjCam r]

         while { ![eof $file] } {

            gets $file line
            if { [string index $line 0] != "#" && [string length $line] > 0 } {
               eval ProjCam::Set $line
            }
         }
         close $file
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <ProjCam::Set>
# Creation : Janvier 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Definir de nouveau parametre de camera
#
# Parametres :
#   <Name>   : Nom de la camera
#   <To>     : Vecteur To
#   <From>   : Vecteur From
#   <Up>     : Vecteur Haut
#   <Lens>   : Lentille (Zoom)
#   <CFX>    :
#   <CFY>    :
#   <CFZ>    :
#   <CTX>    :
#   <CTY>    :
#   <CTZ>    :
#   <Lat>    : Latitude du point focal
#   <Lon>    : Longitude du point focal
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc ProjCam::Set { Name To From Up Lens CFX CFY CFZ CTX CTY CTZ Lat Lon { No -1 } } {
   variable Data

   set Data(Params$Name) [list $To $From $Up $Lens $CFX $CFY $CFZ $CTX $CTY $CTZ $Lat $Lon]
   if { [lsearch -exact $Data(Names) $Name]==-1 } {
      lappend Data(Names) $Name
      if { [winfo exists .bar.cam.sel] } {
         ComboBox::Add .bar.cam.sel $Name
      }
   }
   if { $No!=-1 } {
      bind . <Control-KeyPress-$No> "ProjCam::Select \$Page::Data(Frame) \$Page::Data(Frame) $Name"
   }
}

#----------------------------------------------------------------------------
# Nom      : <ProjCam::Reset>
# Creation : Octobre 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Reinitialiser une camera
#
# Parametres :
#   <Cam>    : Identificateur de Camera
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc ProjCam::Reset { Cam { All True } } {
   variable Data

   upvar #0 ProjCam::Data${Cam}::Cam  cam

   if { $All } {
      set cam(LLens) {}
      set cam(Lens)   1.0
   }

   Viewport::GoTo $Page::Data(Frame) [expr $Viewport::Map(Lat)+0.00001] [expr $Viewport::Map(Lon)+0.00001] $cam(Lens) { 0.0 0.0 2.0 } { 0.0 0.0 1.0 } { 0.0 1.0 0.0 }

   set cam(To)     { 0.0 0.0 1.0 }
   set cam(From)   { 0.0 0.0 2.0 }
   set cam(Up)     { 0.0 1.0 0.0 }
   set cam(CTX)    0
   set cam(CTY)    0
   set cam(CTZ)    0
   set cam(CFX)    0
   set cam(CFY)    0
   set cam(CFZ)    1

   set Data(Name)   ""
}

#----------------------------------------------------------------------------
# Nom      : <ProjCam::Save>
# Creation : Septembre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Sauvegarde la definition de camera courante
#
# Parametres :
#   <Combo>  : Identificateur du combobox
#   <Name>   : Camera name
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc ProjCam::Save { Combo Name } {
   global   env
   variable Data
   variable Msg
   variable Lbl

   #----- If this is a valid name
   if { $Name=="" } {
      return
   }
   regsub -all " " $Name "_" Name

   set nok 0
   if { [ComboBox::Index $Combo exact $Name]!=-1 } {
      set nok [Dialog::Default . 200 WARNING $Msg(Exist) "" 1 $Lbl(Yes) $Lbl(No)]
   }

   if { !$nok } {

      #----- Save the camera parameters
      set Data(Name) $Name
      set line "$Data(Name) [ProjCam::Mem $Page::Data(Frame) $Data(Name)]"

      if { [file exists $env(HOME)/.spi/ProjCam] } {
         file rename -force $env(HOME)/.spi/ProjCam $env(HOME)/.spi/ProjCam.old
      }
      catch { exec grep -v "^$Data(Name) " $env(HOME)/.spi/ProjCam.old > $env(HOME)/.spi/ProjCam }
      exec echo $line >> $env(HOME)/.spi/ProjCam

      #----- Add to list of camera
      ComboBox::Add $Combo $Data(Name)

      Dialog::Info . $Msg(Saved)
   } else {
      Dialog::Error . $Msg(Name)
   }
   ComboBox::Close $Combo
}

#----------------------------------------------------------------------------
# Nom      : <ProjCam::Select>
# Creation : Septembre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Appliquer les parametres de la camera selectionnee.
#
# Parametres :
#   <Cam>    : Identificateur de Camera
#   <Frame>  : Identificateur de Page
#   <Name>   : Nom de la Camera
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc ProjCam::Select { Cam Frame Name { Now False } } {
   variable Data
   variable Param

   upvar #0 ProjCam::Data${Cam}::Cam  cam

   set Viewport::Map(Grabbed) [clock click -milliseconds]

   #----- Obtenir les parametres de la vue selectionnee

   if { [array get Data Params$Name]!="" } {
      set params $Data(Params$Name)
      set Data(Name) $Name
   } else {
      return
   }

   #----- Repositionner la camera
   switch $Now { 
      "True"  { Viewport::Rotate $Frame [lindex $params 10] [lindex $params 11] [lindex $params 3] [lindex $params 1] [lindex $params 0] [lindex $params 2] }
      "False" { Viewport::GoTo $Frame [lindex $params 10] [lindex $params 11] [lindex $params 3] [lindex $params 1] [lindex $params 0] [lindex $params 2] }
      "Full"  { Viewport::GoToBatch $Frame [lindex $params 10] [lindex $params 11] [lindex $params 3] [lindex $params 1] [lindex $params 0] [lindex $params 2] }
   }

   set cam(CFX)  [lindex $params 4]
   set cam(CFY)  [lindex $params 5]
   set cam(CFZ)  [lindex $params 6]
   set cam(CTX)  [lindex $params 7]
   set cam(CTY)  [lindex $params 8]
   set cam(CTZ)  [lindex $params 9]
   set Data(Name) $Name
}

#----------------------------------------------------------------------------
# Nom      : <ProjCam::XYInit>
# Creation : Aout 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Initie le mouvement en XY.
#
# Parametres :
#  <Cam>     : Identificateur de Camera
#  <Frame>   : Identificateur de Page
#  <VP>      : Identificateur du Viewport
#  <X>       : Coordonnne x du pointeur de la souris
#  <Y>       : Coordonnne y du pointeur de la souris
#
# Retour:
#
# Remarques :
#
# Modifications :
#----------------------------------------------------------------------------

proc ProjCam::XYInit { Cam Frame VP X Y } {
   variable Data

   set Viewport::Map(Grabbed) [clock click -milliseconds]
   
   #----- Stop Flybys
   set Animator::Fly(Length) 0
   set Animator::Fly(Path)   ""

   set Data(TmpX) $X
   set Data(TmpY) $Y

   Viewport::Resolution $Frame [expr $OpenGL::Param(Res)==1?2:$OpenGL::Param(Res)]
}

#----------------------------------------------------------------------------
# Nom      : <ProjCam::XYDo>
# Creation : Aout 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectue le mouvement en XY.
#
# Parametres :
#  <Cam>     : Identificateur de Camera
#  <Frame>   : Identificateur de Page
#  <VP>      : Identificateur du Viewport
#  <X>       : Coordonnne x du pointeur de la souris
#  <Y>       : Coordonnne y du pointeur de la souris
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc ProjCam::XYDo { Cam Frame VP X Y } {
   variable Data

   upvar #0 ProjCam::Data${Cam}::Cam  cam

   set Data(CFX0) $cam(CFX)
   set cam(CFX)   [Viewport::CheckCoord [expr $cam(CFX)+double($Data(TmpX) - $X)/5]]
   set cam(CFY)   [expr $cam(CFY)+double($Data(TmpY) - $Y)/5]

   set Data(TmpX) $X
   set Data(TmpY) $Y

   set Viewport::Map(Grabbed) [clock click -milliseconds]
   ProjCam::Do $Cam $Frame $VP
}

#----------------------------------------------------------------------------
# Nom      : <ProjCam::XYDone>
# Creation : Aout 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Termine le mouvement en XY.
#
# Parametres :
#  <Cam>     : Identificateur de Camera
#  <Frame>   : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc ProjCam::XYDone { Cam Frame  { Sling False } } {
   variable Data

   upvar #0 ProjCam::Data${Cam}::Cam  cam

   set res 1

   if { $Sling } {
      set dt [expr [clock click -milliseconds]-$Viewport::Map(Grabbed)]
      set dg [expr $cam(CFX)-$Data(CFX0)]

      #----- Calculate displacement in meters
      set dx [expr (3.141592653589793115997963468544*$cam(CFZ)*6378140/180.0)*$dg*0.017453292519943295474371680598]

      if { [expr abs($dg)]>1 && $dt>0  && $dt<250 } {
         #----- speed = distance / temps
         set spd [expr int($dx/$dt)*0.1]
         set res [Viewport::GoAround $Frame $spd $Viewport::Map(Lat) $Viewport::Map(Lon)]
      }
   }

   if { $res } {
      Viewport::Resolution $Frame 1
   }
}

#----------------------------------------------------------------------------
# Nom      : <ProjCam::ZInit>
# Creation : Aout 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Initie le mouvement en Z.
#
# Parametres :
#  <Cam>     : Identificateur de Camera
#  <Frame>   : Identificateur de Page
#  <VP>      : Identificateur du Viewport
#  <X>       : Coordonnne x du pointeur de la souris
#  <Y>       : Coordonnne y du pointeur de la souris
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc ProjCam::ZInit { Cam Frame VP X Y } {
   variable Data

   set Viewport::Map(Grabbed) [clock click -milliseconds]
   set Data(TmpX) $X
   set Data(TmpY) $Y

   Viewport::Resolution $Frame [expr $OpenGL::Param(Res)==1?2:$OpenGL::Param(Res)]
}

#----------------------------------------------------------------------------
# Nom      : <ProjCam::ZDo>
# Creation : Aout 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectue le mouvement en Z.
#
# Parametres :
#  <Cam>     : Identificateur de Camera
#  <Frame>   : Identificateur de Page
#  <VP>      : Identificateur du Viewport
#  <X>       : Coordonnne x du pointeur de la souris
#  <Y>       : Coordonnne y du pointeur de la souris
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc ProjCam::ZDo { Cam Frame VP X Y } {
   variable Data

   upvar #0 ProjCam::Data${Cam}::Cam  cam

   set cam(CFZ) [expr $cam(CFZ)-double($Data(TmpY) - $Y)/(200.0/($cam(CFZ)))]

   set Data(TmpX) $X
   set Data(TmpY) $Y

   ProjCam::Do $Cam $Frame $VP
}

#----------------------------------------------------------------------------
# Nom      : <ProjCam::ZDone>
# Creation : Aout 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Termine le mouvement en Z.
#
# Parametres :
#  <Cam>     : Identificateur de Camera
#  <Frame>   : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc ProjCam::ZDone { Cam Frame } {

   Viewport::Resolution $Frame 1
}

#----------------------------------------------------------------------------
# Nom      : <ProjCam::ZoomBox>
# Creation : Septembre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Ajuster la boite en fonction du format de la fenetre.
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#  <X0>      : Coordonnee x du premier point
#  <Y0>      : Coordonnee y du premier point
#  <X1>      : Coordonnee x du deuxieme point
#  <Y1>      : Coordonnee y du deuxieme point
#  <Width>   : Largeur du Viewport
#  <Height>  : Hauteur du Viewport
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc ProjCam::ZoomBox { Canvas X0 Y0 X1 Y1 Width Height } {

   #----- Definir la grandeur de la boite

   set dh [expr double($Height)/$Width]
   set dw [expr double($Width)/$Height]

   set w0 [expr abs($X1 - $X0)]
   set h0 [expr abs($Y1 - $Y0)]
   set w1 [expr $h0 * $dw]
   set h1 [expr $w0 * $dh]

   if {$w1 < $w0 } {
     if { $X1 > $X0 } {
        set X1 [expr round($h0*$dw+$X0)]
     } else {
        set X1 [expr round($X0 - $h0*$dw)]
     }
   } elseif {$h1 < $h0 } {
       if { $Y1 > $Y0 } {
         set Y1 [expr round($w0*$dh+$Y0)]
       } else {
         set Y1 [expr round($Y0 - $w0*$dh)]
       }
   }

   #----- Mettre a jour la boite

   $Canvas coord RECTZOOM $X0 $Y0 $X1 $Y1

   #----- Replacer le cible du centre

   set X1 [expr $X0 + ($X1 - $X0)/2]
   set Y1 [expr $Y0 + ($Y1 - $Y0)/2]
   $Canvas coord TARGETX $X1 [expr $Y1-5] $X1 [expr $Y1+5]
   $Canvas coord TARGETY [expr $X1-5] $Y1 [expr $X1+5] $Y1
}

#----------------------------------------------------------------------------
# Nom      : <ProjCam::ZoomInit>
# Creation : Avril 1998 - J.P. Gauthier - CMC/CMOE
#
# But      : Debute l'acquisition du deplacement de la souris.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <VP>      : Identificateur du Viewport
#  <X>       : Coordonnee x du point
#  <Y>       : Coordonnee y du point
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc ProjCam::ZoomInit { Frame VP X Y } {

   set Viewport::Map(Grabbed) [clock click -milliseconds]
   
   #----- Stop Flybys
   set Animator::Fly(Length) 0
   set Animator::Fly(Path)   ""
   
   set c $Frame.page.canvas

   $c create rectangle $X $Y $X $Y -tags RECTZOOM -outline red -width 2
   $c create line $X [expr $Y-5] $X [expr $Y+5] -tags "TARGETX" -fill red -width 2
   $c create line [expr $X-5] $Y [expr $X+5] $Y -tags "TARGETY" -fill red -width 2

   $c bind PAGE$VP <B2-Motion> "ProjCam::ZoomBox $c $X $Y \[$c canvasx %x\] \[$c canvasy %y\] $Viewport::Data(Width$VP) $Viewport::Data(Height$VP)"
}

#----------------------------------------------------------------------------
# Nom      : <ProjCam::Zoom>
# Creation : Fevrier 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectuer le zoom.
#
# Parametres :
#  <Cam>     : Identificateur de Camera
#  <Frame>   : Identificateur de Page
#  <Store>   : Ajouter le zoom a la liste
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc ProjCam::Zoom { Cam Frame Lens { Store False } } {

   upvar #0 ProjCam::Data${Cam}::Cam  cam

   set t0 [set Viewport::Map(Grabbed) [clock click -milliseconds]]

   if { $Store } {
      lappend cam(LLens) [list $cam(Lens) $Viewport::Map(Lat) $Viewport::Map(Lon)]
   }
   set cam(Lens) $Lens
   set Data(Name) ""

   projcam configure $Frame -lens $Lens

   update
   if { $Viewport::Map(Grabbed)<=$t0 } {
      Page::Update $Frame
   }
}

#----------------------------------------------------------------------------
# Nom      : <ProjCam::ZoomIn>
# Creation : Avril 1998 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectuer le zoom.
#
# Parametres :
#  <Cam>     : Identificateur de Camera
#  <Frame>   : Identificateur de Page
#  <VP>      : Identificateur du Viewport
#  <Factor>  : Facteur a appliquer (Optionnel)
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc ProjCam::ZoomIn { Cam Frame VP { Factor 0 } } {
   variable Data

   upvar #0 ProjCam::Data${Cam}::Cam  cam

   if { $Factor!=0 } {
      set lens $Factor
      set lat  $Viewport::Map(Lat)
      set lon  $Viewport::Map(Lon)
   } else {
      #----- Extraire les coordonnees du rectangle de zoom du viewport actif

      if { [llength [set cr [$Frame.page.canvas coord RECTZOOM]]]!=4 } {
         return
      }

      $Frame.page.canvas delete RECTZOOM TARGETX TARGETY
      set x  [lindex $cr 0]
      set y  [lindex $cr 1]
      set x1 [lindex $cr 2]
      set y1 [lindex $cr 3]
      if { [expr abs($x-$x1)] <5 || [expr abs($y-$y1)] <5 } {
         return
      }

      #----- Effectuer le zoom

      set midx    [expr $x+($x1-$x)/2]
      set midy    [expr $y+($y1-$y)/2]
      set latlon  [$VP -unproject $midx $midy]

      if { [lindex $latlon 0]==999.0 || [lindex $latlon 1]== 999.0 } {
         return
      }

      set lens [expr double([lindex [$Frame.page.canvas itemconfigure $VP -width] 4])/($x1-$x)*$cam(Lens)]
      set lat  [lindex $latlon 0]
      set lon  [lindex $latlon 1]
   }

   set Viewport::Map(Grabbed) [clock click -milliseconds]
   $Frame.page.canvas config -cursor watch
   update idletasks

   set Data(Name) ""

   projcam configure $Cam -to { 0 0 1 }
   Viewport::GoTo $Frame $lat $lon $lens

   #----- Dans le cas de projection grille

   set ij [projection configure $Frame -gridpoint]
   set Viewport::Map(GridI) [lindex $ij 0]
   set Viewport::Map(GridJ) [lindex $ij 1]

   $Frame.page.canvas config -cursor left_ptr
}

#----------------------------------------------------------------------------
# Nom      : <ProjCam::ZoomOut>
# Creation : Avril 1998 - J.P. Gauthier - CMC/CMOE
#
# But      : Retour au zoom precedent.
#
# Parametres :
#  <Cam>     : Identificateur de Camera
#  <Frame>   : Identificateur de Page
#  <VP>      : Identificateur du viewport
#  <Reset>   : Mode de retour
#  <Pos>     : Retour a la position precedente
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc ProjCam::ZoomOut { Cam Frame VP Reset { Pos False } } {
   variable Data

   upvar #0 ProjCam::Data${Cam}::Cam cam

   #----- Verifier l'historique de zoom

   if { ![llength $cam(LLens)] } {
      return
   }

   set Viewport::Map(Grabbed) [clock click -milliseconds]
   $Frame.page.canvas config -cursor watch
   update idletasks

   #----- Effectuer le recul de l'historique de zoom

   if { $Reset } {
      set plens [lindex $cam(LLens) 0]
      set lens  1
      set cam(LLens) { }
   } else {
      set plens [lindex $cam(LLens) end]
      set lens  [lindex $plens 0]
      set cam(LLens) [lreplace $cam(LLens) end end]
   }
   if { $Pos } {
      set lat [lindex $plens 1]
      set lon [lindex $plens 2]
   } else {
      set lat $Viewport::Map(Lat)
      set lon $Viewport::Map(Lon)
   }

   set Data(Name) ""

   if { $Viewport::Map(Type$Frame)=="grid" && $lens<=1 } {
      set ninj [projection configure $Frame -gridsize]
      set ext  [projection configure $Frame -gridextent]
      set Viewport::Map(GridI) [expr ([lindex $ninj 0]-1.0)*0.5+[lindex $ext 0]]
      set Viewport::Map(GridJ) [expr ([lindex $ninj 1]-1.0)*0.5+[lindex $ext 1]]
      set ll [projection function $Frame -coordgrid $Viewport::Map(GridI) $Viewport::Map(GridJ)]
      set lat [lindex $ll 0]
      set lon [lindex $ll 1]
   }
   Viewport::GoTo $Frame $lat $lon $lens

   $Frame.page.canvas config -cursor left_ptr
}

#----------------------------------------------------------------------------
# Nom      : <ProjCam::ZoomScroll>
# Creation : Novembre 2011 - J.P. Gauthier - CMC/CMOE
#
# But      : Zoom avec un fixe sur une localisation.
#
# Parametres :
#  <Cam>     : Identificateur de Camera
#  <Frame>   : Identificateur de Page
#  <VP>      : Identificateur du viewport
#  <X>       : Pixel en X du curseur
#  <Y>       : Pixel en Y du curseur
#  <Lens>    : Facteur de zoom
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc ProjCam::ZoomScroll { Cam Frame VP X Y Lens { Centered True } } {

   set t0 [set Viewport::Map(Grabbed) [clock click -milliseconds]]

   Viewport::Resolution $Frame 2

   if { $Centered } {
      set lens [projcam configure $Frame -lens]

      set ll0 [$VP -unproject $X $Y]
      projcam configure $Frame -lens $Lens
      $Frame.page.canvas itemconf $VP -projection $Frame -frame 0
      set ll1 [$VP -unproject $X $Y]

      if { [projection configure $Frame -geographic] } {
         #----- Calculate displacement needed to focus on cursor (course-distance method)
         set d  [projection function $Frame -dist    [list [lindex $ll0 0] [lindex $ll0 1]  [lindex $ll1 0] [lindex $ll1 1]]]
         set c  [projection function $Frame -bearing [lindex $ll1 0] [lindex $ll1 1]  [lindex $ll0 0] [lindex $ll0 1]]
         set ll [projection function $Frame -circle  $Viewport::Map(Lat) $Viewport::Map(Lon) $d $c]

         set lat [lindex $ll 0]
         set lon [lindex $ll 1]
      } else {
         #-----  Calculate displacement needed to focus on cursor (delta coord method)
         set da [expr [lindex $ll1 0]-[lindex $ll0 0]]
         set do [expr [lindex $ll1 1]-[lindex $ll0 1]]

         set lat [expr $Viewport::Map(Lat)-$da]
         set lon [expr $Viewport::Map(Lon)-$do]

         set lat [expr ($lat>90.0  || $lat<-90.0) ?$Viewport::Map(Lat):$lat]
         set lon [expr $lon>180?$lon-360:$lon<-180?$lon+360:$lon]
      }
      Viewport::Rotate $Frame $lat $lon $Lens
   } else {
      ProjCam::Zoom $Cam $Frame $Lens
   }
   
   #----- Put back labels if no more scrolling
   after 1000 "Viewport::Resolution $Frame 1 $t0"
}

#------------------------------------------------------------------------------
# Nom      : <ProjCam::Write>
# Creation : Decembre 2003 - J.P. Gauthier - CMC/CMOE -
#
# But     : Engeristrer les parametres de la camera dans un fichier Layout
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <File>   : Identificateur de Fichier
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc ProjCam::Write { Frame File } {
   variable Data

   if { $Viewport::Map(Type$Frame)=="grid" } {
      set ll [projection configure $Frame -location]
      set Viewport::Map(Lat) [lindex $ll 0]
      set Viewport::Map(Lon) [lindex $ll 1]
   }

   puts $File "   #----- Definition de la camera"
   puts $File ""
   puts $File "   set ProjCam::Data(Params\[namespace current\]) \"[ProjCam::Mem $Frame _____]\""
   puts $File "   ProjCam::Select \$Frame \$Frame \[namespace current\]"
   puts $File ""
}
