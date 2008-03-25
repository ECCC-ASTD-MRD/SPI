#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet    : Librairie de definitions pour les trajectoires
# Fichier   : Data_Trajectory.tk
# Creation  : Janvier 1999 - J.P. Gauthier - CMC/CMOE
#
# Description: Definitions d'une structure de donnees et de procedures
#              relatives aux trajectoires
#
# Fonctions:
#
#   Trajectory::Export             { Id Path }
#   Trajectory::GetColor           { Id No }
#   Trajectory::GetRes             { }
#   Trajectory::GetShape           { Id No }
#   Trajectory::Locate             { Id Offset }
#   Trajectory::ParamApply         { }
#   Trajectory::ParamSet           { }
#   Trajectory::ParamFrame         { }
#   Trajectory::ParamType          { }
#   Trajectory::ParamTypeSet       { String Type }
#   Trajectory::ParticleInfoEnable { Canvas Id }
#   Trajectory::ParticleId         { Canvas Id Particle X Y }
#   Trajectory::ParticleUnId       { Canvas Id Particle }
#   Trajectory::Project            { Frame Id }
#   Trajectory::Graph              { Frame BG FG X0 Y0 X1 Y1 TrajId }
#   Trajectory::GraphPlot          { Frame FG TrajId }
#   Trajectory::GraphScale         { Frame BG FG TrajId X Y }
#   Trajectory::Height             { Frame BG FG X0 Y0 X1 Y1 TrajId }
#   Trajectory::HeightScale        { Frame BG FG TrajId X Y }
#   Trajectory::Legend             { Frame BG FG X0 Y0 X1 Y1 TrajId }
#   Trajectory::LegendScale        { Frame BG FG TrajId X Y }
#   Trajectory::Update             { }
#   Trajectory::UpdateItems        { Frame }
#
# Remarques :
#   Aucune
#
#===============================================================================

package provide Trajectory 4.0

proc IdTrajectory { Show } {

   if { $Show } {
      puts "(INFO) Loading Standard CMC/CMOE Data package Trajectory Version 4.0"
   }

   package require DateStuff ; IdDateStuff   False
}

#----- Definitions des constantes

namespace eval Trajectory {
   variable Param
   variable Params
   variable Data
   variable Lbl
   variable Resources

   #----- Definitions des labels

   set Lbl(Fill)       { "Remplissage" "Fill" }
   set Lbl(Interval)   { "Intervalle" "Interval" }
   set Lbl(Particle)   { "Particule" "Particle" }
   set Lbl(Size)       { "Dimension" "Size" }
   set Lbl(Speed)      { "Vitesse" "Speed" }
   set Lbl(Traj)       { "Trajectoire" "Trajectory" }
   set Lbl(Width)      { "Largeur" "Width" }
   set Lbl(Color)      { "Couleur" "Color" }
   set Lbl(Ico)        { "Icone" "Icon" }
   set Lbl(Level)      { "Niveaux" "Level" }
   set Lbl(Single)     { "Unique" "Single" }
   set Lbl(Display)    { "Affichage" "Display" }
   set Lbl(Options)    { "Options" "Options" }
   set Lbl(Height)     "Hauteur/Temps - Height/Time"

   #----- Constantes relatives a l'affichage des trajectoires

   set Param(Color_Sel) orange                                                      ;#Couleur lorsque selectionee
   set Param(Color)     #ff0000                                                     ;#Couleur courante
   set Param(Shape)     Triangle                                                    ;#Icone courante
   set Param(Shapes)    { Triangle Square Circle HBar VBar Losange Sable Star X + } ;#Liste des icones des niveaux
   set Param(Colors)    { #ff0000 #0000ff #006400 #4C7A5C #FFCC00 #FF00CC #00FFFF #785D0C #ACF003 } ;#Liste des couleurs des niveaux
   set Param(Style)     1                                                           ;#Type d'affichage
   set Param(Size)      3                                                           ;#Grandeur des icones
   set Param(Width)     1                                                           ;#Grandeur des icones
   set Param(Fill)      2                                                           ;#Remplir les icones
   set Param(Interval)  3                                                           ;#Intervale de selection des donnees
   set Param(Speed)     0                                                           ;#Afficher les vitesse lorsque disponible
   set Param(Level)     0                                                           ;#Level a configurer
   set Param(TypeS)     [lindex $Lbl(Level) $GDefs(Lang)]                           ;#Type de selection des parametres
   set Param(Type)      LEVEL                                                       ;#Type de selection des parametres
   set Param(No)        0                                                           ;#Numero du type a selectionner

   catch {
      set Resources(LogoCMC)   @$GDefs(Dir)/Resources/Bitmap/SMC_hor_small.xbm
      set Resources(LogoARL)   @$GDefs(Dir)/Resources/Bitmap/ARL_hor_small.xbm
   }

   set Data(Levels)         { 500.00 1500.00 3000.00 500.00 700.00 250.00 } ;#Liste des niveaux de trajectoire en memoire
   set Data(List)           ""    ;#Liste des Id de trajectoire en memoire
   set Data(ParticleDate)   ""    ;#Altitude de la particule
   set Data(ParticleCoords) ""    ;#Localisation de la particule
   set Data(ParticleSpeed)  ""    ;#Vitesse de la particule
   set Data(Frame)          ""    ;#Path du frame de configuration

   #----- Definir les parametres par defaut standard

   set Param(ColorIdx) -1
   set Param(ShapeIdx) -1
   foreach level { 500.00 1500.00 3000.00 } {
      set Param(Shape$level) [lindex $Param(Shapes) [expr [incr Param(ShapeIdx)]%[llength $Param(Shapes)]]]
      set Param(Color$level) [lindex $Param(Colors) [expr [incr Param(ColorIdx)]%[llength $Param(Colors)]]]
   }

   set Param(ColorIdx) -1
   set Param(ShapeIdx) -1
   foreach level { 500.00 700.00 250.00 } {
      set Param(Shape$level) [lindex $Param(Shapes) [expr [incr Param(ShapeIdx)]%[llength $Param(Shapes)]]]
      set Param(Color$level) [lindex $Param(Colors) [expr [incr Param(ColorIdx)]%[llength $Param(Colors)]]]
   }
}

#----------------------------------------------------------------------------
# Nom      : <Trajectory::Export>
# Creation : Juin 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Exporter des fichiers de trajectoires en format MID/MIFF de MapInfo
#
# Parametres :
#  <Id>      : Identificateur de la trajectoire
#  <Path>    : Nom complet du ficheir d'exportation
#
# Retour:
#
# Remarques :
#    -On produit deux suites de primitives
#        des polyline pour le chemin des particules
#        des points pour les positions des particules
#
#----------------------------------------------------------------------------

proc Trajectory::Export { Id Path } {
   variable Data

   #----- Ouvrir les fichiers

   set mid [open $Path.mid w]
   set mif [open $Path.mif w]

   #----- Inscrire l'entete

   puts $mif "VERSION 300
DELIMITER \",\"
COLUMNS 6
    ENTITE char(40)
    DESCRIPTION char(40)
    HAUTEUR float
    DATE date
    HOUR smallint
    MINUTE smallint
DATA
"
   #----- Creer les segments

   foreach traj $Data(List) {

      set parcels [trajectory define $traj -PARCELS]
      set parcel  [lindex $parcels 0]

      set line [lindex $Data(${Id}_$i) 0]
      set elev [lindex $parcel 5]

      set date [string range [lindex $parcel 0] 0 7]
      set hour [string range [lindex $parcel 0] 8 9]
      set min  00

      #----- Inscrire les paths

      puts $mid "TRAJECTORY,Path,$elev,$date,$hour,$min"
      puts $mif "PLINE\n[llength $parcels]"

      foreach parcel $parcels {
         puts $mif "[lindex $parcel 2] [lindex $parcel 1]"
      }

      #----- Inscrire les positions

      foreach parcel $parcels {

         set lat  [lindex $parcel 1]
         set lon  [lindex $parcel 2]
         set elev [lindex $parcel 5]
         set hour [string range [lindex $parcel 0] 8 9]
         set min  00
         set date [string range [lindex $parcel 0] 0 7]

         puts $mid "TRAJECTORY,Position,$elev,$date,$hour,$min"
         puts $mif "POINT $lon $lat"
      }
   }

   close $mid
   close $mif
}

#----------------------------------------------------------------------------
# Nom      : <Trajectory::GetColor>
# Creation : Fevrier 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Recuperer la couleur selon le type de parametrage
#
# Parametres :
#   <Id>     : Identificateur de la trajectoire
#   <No>     : Numero de sequence
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Trajectory::GetColor { Id No } {
   variable Data
   variable Param

   switch $Param(Type) {
      "PARCEL" {  set idx $No }
      "LEVEL"  {  if { [trajectory is $Id] } {
                     if { [trajectory define $Id -LEVELTYPE]=="P" } {
                        set idx [format "%.2f" [lindex [trajectory define $Id -PARCEL 0] 4]]
                     } else {
                        set idx [format "%.2f" [lindex [trajectory define $Id -PARCEL 0] 5]]
                     }
                  } else {
                     set idx $No
                  }
              }
      "SAME"  { set idx 0 }

   }
   if { ![info exists Param(Color$idx)] } {
      set Param(Color$idx) [lindex $Param(Colors) [expr [incr Param(ColorIdx)]%[llength $Param(Colors)]]]
   }
   return $Param(Color$idx)
}

#----------------------------------------------------------------------------
# Nom      : <Trajectory::GetRes>
# Creation : Fevrier 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Recuperer le numero de parametres
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Trajectory::GetRes { } {
   variable Data
   variable Param

   if { $Param(Type)=="LEVEL" } {
      return $Param(Level)
   } else {
      return $Param(No)
   }
}

#----------------------------------------------------------------------------
# Nom      : <Trajectory::GetShape>
# Creation : Fevrier 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Recuperer l'icone selon le type de parametrage
#
# Parametres :
#   <Id>     : Identificateur de la trajectoire
#   <No>     : Numero de sequence
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Trajectory::GetShape { Id No } {
   variable Data
   variable Param

   switch $Param(Type) {
      "PARCEL" { set idx $No }
      "LEVEL" { if { [trajectory is $Id] } {
               if { [trajectory define $Id -LEVELTYPE]=="P" } {
                  set idx [format "%.2f" [lindex [trajectory define $Id -PARCEL 0] 4]]
               } else {
                  set idx [format "%.2f" [lindex [trajectory define $Id -PARCEL 0] 5]]
               }
             } else {
                set idx $No
             }
          }
      "SAME" { set idx 0 }
   }

   if { ![info exists Param(Shape$idx)] } {
      set Param(Shape$idx) [lindex $Param(Shapes) [expr [incr Param(ShapeIdx)]%[llength $Param(Shapes)]]]
   }
   return $Param(Shape$idx)
}

#----------------------------------------------------------------------------
# Nom      : <Trajectory::Locate>
# Creation : Mars 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Centrer et "zoomer" sur la trajectoire.
#
# Parametres  :
#    <Id>     : Identificateur de la trajectoire
#    <Offset> : Pourcentage de recul
# Retour:
#
# Remarques :
#    - On cherche les coins de la boites qui englobe la trajectoire
#      et on effecture un zoom sur les coins de cette boite moins
#      un pourcentage afin d'encardrer largement
#
#----------------------------------------------------------------------------

proc Trajectory::Locate { Id Offset } {

   set lat0   90.0
   set lat1  -90.0
   set lon0   180.0
   set lon1  -180.0
   set blon0 -180.0
   set blon1  180.0
   set flip   0

   #----- Parcourir toutes les parcelles pour determiner les min max de la boite

   set plon   0

   foreach traj $Id {
      foreach parcels [trajectory define $traj -PARCELS] {

         #----- Recuperer les coordonnees limites

         set lat  [lindex $parcels 1]
         set lat0 [expr $lat<$lat0?$lat:$lat0]
         set lat1 [expr $lat>$lat1?$lat:$lat1]

         set lon  [lindex $parcels 2]
         set lon0 [expr $lon<$lon0?$lon:$lon0]
         set lon1 [expr $lon>$lon1?$lon:$lon1]

         #----- Recuperer les coordonnees limites (Inverses)

         if { $lon<0 } {
            set blon0 [expr $lon>$blon0?$lon:$blon0]
         } else {
            set blon1 [expr $lon<$blon1?$lon:$blon1]
         }

         #----- Verifier si on traverse ls 180/-180

         if { $lon<180 && $lon>90 && $plon>-180 && $plon<-90 } {
            set flip 1
         }
         if { $lon>-180 && $lon<-90 && $plon<180 && $plon>90 } {
            set flip 1
         }
         set plon $lon
      }
   }

   #----- Si on traverse ls 180/-180, utiliser les coordonnees inverses

   if { $flip } {
      set lon0 $blon1
      set lon1 $blon0
   }

   ProjCam::CloseUp $Page::Data(Frame) $Page::Data(Frame) $Viewport::Data(VP) $lat0 $lon0 $lat1 $lon1 $Offset
}

#----------------------------------------------------------------------------
# Nom      : <Trajectory::Param>
# Creation : Fevrier 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Assigner les parametres aux trajectoires
#
# Parametres :
#   <List>   : Liste des trajectoires a parametrer
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Trajectory::ParamApply { { List { } } } {
   variable Data
   variable Param

   if { [llength $List] } {
      set list $List
   } else {
      set list $Data(List)
   }

   set i   0
   foreach traj $list {
      trajectory configure $traj -width $Param(Width) -color [Trajectory::GetColor $traj $i] \
         -mark $Param(Style) -size $Param(Size)
      incr i
   }
}

#----------------------------------------------------------------------------
# Nom      : <Trajectory::ParamSet>
# Creation : Fevrier 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialiser les parametres cournats
#
# Parametres :
#   <Apply>  : Apply button
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Trajectory::ParamSet {  { Apply "" } } {
   variable Param

   set no [Trajectory::GetRes]
   set Param(Shape$no) $Param(Shape)
   set Param(Color$no) $Param(Color)

   if { $Apply!="" } {
      $Apply configure -state normal
   }
}

#----------------------------------------------------------------------------
# Nom      : <Trajectory::ParamFrame>
# Creation : Decembre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Afficher un frame de selection des parametres des trajectoires.
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

proc Trajectory::ParamFrame { Frame Apply } {
   global GDefs
   variable Lbl
   variable Data

   set Data(Frame) [TabFrame::Add $Frame 2 [lindex $Lbl(Traj) $GDefs(Lang)] False ""]

   frame $Data(Frame).left

      labelframe $Data(Frame).left.traj -text "[lindex $Lbl(Traj) $GDefs(Lang)]"
         menubutton $Data(Frame).left.traj.type -textvariable Trajectory::Param(TypeS) \
            -bd 0 -menu $Data(Frame).left.traj.type.lst -width 10 -anchor w
         ComboBox::Create $Data(Frame).left.traj.lvl Trajectory::Param(Level) noedit unsorted nodouble -1 \
                "" 8 3 "Trajectory::ParamType"
         spinbox $Data(Frame).left.traj.par -textvariable Trajectory::Param(No) -width 8 -from 0 -to 999 -command "Trajectory::ParamType" -bd 1
         label $Data(Frame).left.traj.uni -width 10
         pack $Data(Frame).left.traj.type $Data(Frame).left.traj.lvl -side left -fill x
      pack $Data(Frame).left.traj -side top -pady 5 -fill x

      labelframe $Data(Frame).left.show -text [lindex $Lbl(Options) $GDefs(Lang)]
         frame $Data(Frame).left.show.size
            IcoMenu::Create $Data(Frame).left.show.size.sel $GDefs(Dir)/Resources/Bitmap \
               "zeroth.xbm size1.xbm size2.xbm size3.xbm size4.xbm size5.xbm sizeRene.xbm" "0 1 2 3 4 5 10" \
                Trajectory::Param(Size) "$Apply configure -state normal" \
                $Trajectory::Param(Size) -relief groove -bd 2
            label $Data(Frame).left.show.size.lbl -text " [lindex $Lbl(Size) $GDefs(Lang)]" -anchor w
         pack  $Data(Frame).left.show.size.sel $Data(Frame).left.show.size.lbl -side left -fill x

         frame $Data(Frame).left.show.traj
            IcoMenu::Create $Data(Frame).left.show.traj.sel $GDefs(Dir)/Resources/Bitmap \
               "tstyle0.xbm tstyle1.xbm tstyle2.xbm tstyle3.xbm tstyle4.xbm" "0 1 2 3 4" \
                Trajectory::Param(Style) "$Apply configure -state normal" \
                $Trajectory::Param(Style) -relief groove -bd 2
            label $Data(Frame).left.show.traj.lbl -text " [lindex $Lbl(Traj) $GDefs(Lang)]" -anchor w
         pack  $Data(Frame).left.show.traj.sel $Data(Frame).left.show.traj.lbl -side left -fill x

         frame $Data(Frame).left.show.fill
            IcoMenu::Create $Data(Frame).left.show.fill.sel $GDefs(Dir)/Resources/Bitmap \
               "nofill.xbm fill.xbm fill00.xbm" "0 1 2" \
                Trajectory::Param(Fill) "$Apply configure -state normal" \
                $Trajectory::Param(Fill) -relief groove -bd 2
            label $Data(Frame).left.show.fill.lbl -text " [lindex $Lbl(Fill) $GDefs(Lang)]" -anchor w
         pack  $Data(Frame).left.show.fill.sel $Data(Frame).left.show.fill.lbl -side left -fill x

         frame $Data(Frame).left.show.width
            IcoMenu::Create $Data(Frame).left.show.width.sel $GDefs(Dir)/Resources/Bitmap \
               "zeroth.xbm width1.xbm width2.xbm width3.xbm width4.xbm width5.xbm" "0 1 2 3 4 5" \
                Trajectory::Param(Width) "$Apply configure -state normal" $Trajectory::Param(Width) -relief groove -bd 2
            label $Data(Frame).left.show.width.lbl -text " [lindex $Lbl(Width) $GDefs(Lang)]" -anchor w
         pack  $Data(Frame).left.show.width.sel $Data(Frame).left.show.width.lbl -side left -fill x

         frame $Data(Frame).left.show.int
            IcoMenu::Create $Data(Frame).left.show.int.sel $GDefs(Dir)/Resources/Bitmap \
               "digit1.xbm digit3.xbm digit6.xbm digit12.xbm digit24.xbm" "1 3 6 12 24" \
                Trajectory::Param(Interval) "$Apply configure -state normal" $Trajectory::Param(Interval) -relief groove -bd 2
            label $Data(Frame).left.show.int.lbl -text " [lindex $Lbl(Interval) $GDefs(Lang)]" -anchor w
            pack  $Data(Frame).left.show.int.sel $Data(Frame).left.show.int.lbl -side left -fill x

         frame $Data(Frame).left.show.spd
            label $Data(Frame).left.show.spd.lbl -text " [lindex $Lbl(Speed) $GDefs(Lang)]"
            checkbutton $Data(Frame).left.show.spd.sel -variable Trajectory::Param(Speed) -relief raised -bd 1 \
               -bitmap @$GDefs(Dir)/Resources/Bitmap/zeroth.xbm -indicatoron false \
               -command "$Apply configure -state normal" -selectcolor "" -relief groove -bd 1
            pack $Data(Frame).left.show.spd.sel -side left -ipadx 1
            pack $Data(Frame).left.show.spd.lbl -side left -fill y
         pack $Data(Frame).left.show.spd -side top -padx 2

         pack $Data(Frame).left.show.size $Data(Frame).left.show.traj $Data(Frame).left.show.fill $Data(Frame).left.show.width \
            $Data(Frame).left.show.int $Data(Frame).left.show.spd -side top -padx 2 -fill x
      pack $Data(Frame).left.traj $Data(Frame).left.show -side top -pady 2 -fill x -anchor n

   frame  $Data(Frame).right

      labelframe $Data(Frame).right.part -text "[lindex $Lbl(Display) $GDefs(Lang)]"
         frame $Data(Frame).right.part.ico
            label $Data(Frame).right.part.ico.lbl -text " [lindex $Lbl(Ico) $GDefs(Lang)]" -width 12 -anchor w
            IcoMenu::Create $Data(Frame).right.part.ico.sel $GDefs(Dir)/Resources/Bitmap \
              "stri.xbm ssquare.xbm scircle.xbm shbar.xbm svbar.xbm slos.xbm ssable.xbm sstar.xbm sx.xbm s+.xbm" $Trajectory::Param(Shapes) \
               Trajectory::Param(Shape) "Trajectory::ParamSet $Apply" 0 -relief groove -bd 2
            pack $Data(Frame).right.part.ico.sel $Data(Frame).right.part.ico.lbl -side left

         frame $Data(Frame).right.part.col
            label $Data(Frame).right.part.col.lbl -text " [lindex $Lbl(Color) $GDefs(Lang)]" -width 12 -anchor w
            ColorBox::CreateSel $Data(Frame).right.part.col.sel Trajectory::Param(Color) Trajectory::ParamSet $Apply
            pack $Data(Frame).right.part.col.sel $Data(Frame).right.part.col.lbl -side left

         pack $Data(Frame).right.part.ico $Data(Frame).right.part.col -side top -padx 2
      pack $Data(Frame).right.part -side left -pady 2
   pack $Data(Frame).left $Data(Frame).right -side left -padx 5 -pady 5 -anchor n -fill x

   menu $Data(Frame).left.traj.type.lst
      $Data(Frame).left.traj.type.lst add command -label [lindex $Lbl(Particle) $GDefs(Lang)] \
         -command "Trajectory::ParamTypeSet [lindex $Lbl(Particle) $GDefs(Lang)] PARCEL $Apply"
      $Data(Frame).left.traj.type.lst add command -label [lindex $Lbl(Level)    $GDefs(Lang)] \
         -command "Trajectory::ParamTypeSet [lindex $Lbl(Level)    $GDefs(Lang)] LEVEL $Apply"
      $Data(Frame).left.traj.type.lst add command -label [lindex $Lbl(Single)   $GDefs(Lang)] \
         -command "Trajectory::ParamTypeSet [lindex $Lbl(Single)   $GDefs(Lang)] SAME $Apply"

   IcoMenu::Set $Data(Frame).left.show.int.sel $Trajectory::Param(Interval)
}

#----------------------------------------------------------------------------
# Nom      : <Trajectory::ParamType>
# Creation : Fevrier 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Configurer les widgets selon les parametres courants
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Trajectory::ParamType { } {
   variable Data
   variable Param

   set no [Trajectory::GetRes]
   set Param(Shape) [Trajectory::GetShape {} $no]
   set Param(Color) [Trajectory::GetColor {} $no]

   $Data(Frame).right.part.ico.sel.menu invoke [lsearch -exact $Param(Shapes) $Param(Shape)]
   ColorBox::ConfigNoColor $Data(Frame).right.part.col.sel $Param(Color)
}

#----------------------------------------------------------------------------
# Nom      : <Trajectory::ParamTypeSet>
# Creation : Fevrier 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Selectionner la methode de parametrage
#
# Parametres :
#  <Apply>   : Commande d'update de l'etat
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Trajectory::ParamTypeSet { String Type Apply } {
   variable Data
   variable Param

   set Param(Type)  $Type
   set Param(TypeS) $String

   pack forget $Data(Frame).left.traj.par $Data(Frame).left.traj.lvl $Data(Frame).left.traj.uni

   switch $Type {
      "PARCEL" { pack $Data(Frame).left.traj.par -side left -fill x }
      "LEVEL" { pack $Data(Frame).left.traj.lvl -side left -fill x }
      "SAME" { pack $Data(Frame).left.traj.uni -side left -fill x }
   }
   set Param(No) 0

   Trajectory::ParamSet $Apply
}

#----------------------------------------------------------------------------
# Nom      : <Trajectory::ParticleInfoEnable>
# Creation : Aout 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialiser les fonction d'extraction d'information sur les particules.
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#  <Id>      : Nom de la trajectoire (Identificateur)
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Trajectory::ParticleInfoEnable { Canvas Id } {
   variable Data

   for { set part 0 } { $part < [trajectory define $Id -PARCELNB] } { incr part } {
      $Canvas bind "P.$Id.$part" <Enter> "Trajectory::ParticleId $Canvas $Id $part \[$Canvas canvasx %x\] \[$Canvas canvasy %y\]"
      $Canvas bind "P.$Id.$part" <Leave> "Trajectory::ParticleUnId $Canvas $Id $part"
   }
}

#----------------------------------------------------------------------------
# Nom      : <Trajectory::ParticleId>
# Creation : Juin 1998 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectue les evenements de selectoin de particule
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#  <Id>      : Identificateur de la trajectoire (Identificateur)
#  <Particle>: Index de la particule
#  <Traj>    : Index de la trajectoire
#  <X>       : Coordonnee X du curseur
#  <Y>       : Coordonnee Y du curseur
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Trajectory::ParticleId { Canvas Id Particle X Y } {
   global GDefs
   variable Data
   variable Param

   $Canvas itemconfigure P.$Id.$Particle -fill $Trajectory::Param(Color_Sel)

   #----- Extraire l'information sur la particule

   set parcel [trajectory define $Id -PARCEL $Particle]

   set Data(ParticleDate)   [DateStuff::StringDateFromSeconds [lindex $parcel 0] $GDefs(Lang)]
   set Data(ParticleCoords) [Convert::FormatCoord [lindex $parcel 1] [lindex $parcel 2] DEG]
   set Data(ParticleElev)  "[format %5.1f [lindex $parcel 5]] m"

   set speed [lindex $parcel 8]
   if { $speed!="" } {
      set Data(ParticleSpeed) "\n[lindex $parcel 8] m/s"
   } else {
      set Data(ParticleSpeed) ""
   }

   $Canvas create text [expr $X+5] [expr $Y+5] -text "$Data(ParticleElev)$Data(ParticleSpeed)" \
      -anchor sw -fill black -font XFont10 -tags TRAJID

   #----- Afficher la coordonnee de la particule

   set Page::Data(Coords) $Data(ParticleCoords)
   set Page::Data(Value) $Data(ParticleDate)
}

#----------------------------------------------------------------------------
# Nom      : <Trajectory::ParticleUnId>
# Creation : Juin 1998 - J.P. Gauthier - CMC/CMOE
#
# But      : Supprime les evenements de selectoin de particule
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#  <Id>      : Nom de la trajectoire (Identificateur)
#  <Particle>: Numero de la particule
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Trajectory::ParticleUnId { Canvas Id Particle } {
   global GDefs
   variable Data

   $Canvas itemconfigure P.$Id.$Particle -fill white
   $Canvas itemconfigure P.$Id.${Particle}.TEXT -fill black

   set Data(ParticleDate)    ""
   set Data(ParticleCoords)  ""
   set Data(ParticleSpeed)   ""
   set Data(ParticleElev)    ""

   $Canvas delete TRAJID
}

#----------------------------------------------------------------------------
# Nom      : <Trajectory::Project>
# Creation : Octobre 1998 - J.P. Gauthier - CMC/CMOE
#
# But      : Dessine toutes les trajectoires.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <Id>      : Nom de la trajectoire (Identificateur)
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Trajectory::Project { Frame Id { Force 0 } } {
   global   GDefs
   variable Param
   variable Data

   $Frame.page.canvas delete PARTICLE

   if { $Param(Size) <= 0 } {
      return
   }

   #----- Parcourir les trajectoires

   set i 0

   foreach t $Id {

      if { $Force } {
         set fr $Page::Data(Frame)
         set vp $Viewport::Data(VP)
      } else {
         set tag    [trajectory stats $t -tag]
         set fr     [lindex $tag 0]
         set vp     [lindex $tag 1]

         if { $fr!=$Frame } {
            continue
         }
      }

      if { [Page::Registered $Frame Viewport $vp]==-1 } {
         continue
      }

      set shape  [Trajectory::GetShape $t $i]
      set col    [trajectory configure $t -color]
      set prc    0
      set prn    [trajectory define $t -PARCELNB]
      set time   [projection configure $fr -date]

      foreach parcel [trajectory define $t -PARCELS] {

         set sec [lindex $parcel 0]

         if { $time>0 && $sec>$time  } { continue }

         set lat  [lindex $parcel 1]
         set lon  [lindex $parcel 2]
         set elev [lindex $parcel 5]
         set hour [clock format $sec -format "%H" -gmt true]
         if { $hour=="00" } {
            set hour 0
         } else {
            set hour [string trimleft $hour 0]
         }

         #----- Pour l'interval choisie, incluant la date de depart et d'arrivee

         if { $prc == 0 || $prc == [expr $prn-1] || [expr $hour%$Param(Interval)]==0 } {

            #----- Calculer la cordonnee XY

            set pix [$vp -project $lat $lon $elev]
            if { $pix!="" && [lindex $pix 2]>0 } {

               if { $Trajectory::Param(Fill)==1 || ($hour == "00" && $Trajectory::Param(Fill)==2) } {
                  set fill 1
               } else {
                  set fill 0
               }

               Shape::Draw$shape $fr.page.canvas $pix "P.$t.$prc PARTICLE" $col $Param(Size) $fill
            }
         }
         incr prc
      }
      incr i
   }
   $Frame.page.canvas raise PARTICLE
}

#----------------------------------------------------------------------------
# Nom      : <Trajectory::Graph>
# Creation : Decembre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche le graphique des elevations
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <BG>      : Couleur du Fond
#  <FG>      : Couleur du graph
#  <X0>      : X Coin superieur gauche
#  <Y0>      : Y Coin superieur gauche
#  <X1>      : X Coin inferieur droit
#  <Y1>      : Y Coin inferieur droit
#  <TrajID>  : Identificateur des trajectoires
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Trajectory::Graph { Frame BG FG X0 Y0 X1 Y1 TrajId } {
   global   GDefs
   variable Graph
   variable Lbl

   set canvas $Frame.page.canvas

   #----- Si aucune coordonnee specifie, utiliser la position courante

   if { $X0==-999 || $X1==-999  } {
      set coo [$canvas coords TRAJGRAPHFRAME]
   }
   if { $X0==-999  } {
      set X0 [lindex $coo 0]
      set Y0 [lindex $coo 1]
   }
   if { $X1==-999  } {
      set X1 [lindex $coo 2]
      set Y1 [lindex $coo 3]
   }

   $canvas delete TRAJGRAPH

   #----- Definir les parametres et coordonnees d'affichage

   set Graph(X0) [expr $X0 + 50]
   set Graph(Y0) [expr $Y1 - 30]
   set Graph(X1) [expr $X1 - 20]
   set Graph(Y1) [expr $Y0 + 20]

   #----- Creer le graph

   $canvas create rectangle $X0 $Y0 $X1 $Y1 -fill $BG -width 1 -outline $FG -tags "TRAJGRAPH TRAJGRAPHFRAME"
   $canvas create line $Graph(X0) $Graph(Y0) $Graph(X0) $Graph(Y1) -fill $FG -width 1 -tags TRAJGRAPH
   $canvas create line $Graph(X0) $Graph(Y0) $Graph(X1) $Graph(Y0) -fill $FG -width 1 -tags TRAJGRAPH
   $canvas create text [expr ($X1 - $X0)/2 + $X0] [expr $Y0 + 5]\
      -text $Lbl(Height) -fill $FG -font XFont10 -tags TRAJGRAPH -anchor n

   if { [llength $TrajId] == 0 } {
      return
   }

   #----- Determiner les limites temporelles et verticales

   set Graph(H0) 1e301
   set Graph(H1) -1e301
   set Graph(T0) 1e301
   set Graph(T1) -1e301

   foreach traj $TrajId {
      set t0 [lindex [trajectory define $traj -PARCEL 0]   0]
      set t1 [lindex [trajectory define $traj -PARCEL end] 0]

      set Graph(H0)  [expr $Graph(H0)<[trajectory define $traj -MIN]?$Graph(H0):[trajectory define $traj -MIN]]
      set Graph(H1)  [expr $Graph(H1)>[trajectory define $traj -MAX]?$Graph(H1):[trajectory define $traj -MAX]]

      if { [trajectory define $traj -BACKWARD] } {
         set Graph(T0) [expr $Graph(T0)>$t1?$t1:$Graph(T0)]
         set Graph(T1) [expr $Graph(T1)<$t0?$t0:$Graph(T1)]
      } else {
         set Graph(T0) [expr $Graph(T0)>$t0?$t0:$Graph(T0)]
         set Graph(T1) [expr $Graph(T1)<$t1?$t1:$Graph(T1)]
      }
   }

   #----- Calculer la valeur de l'increment dans l'axe y du graph

   set Graph(H0) [expr int($Graph(H0)>0?0:$Graph(H0))]
   set Graph(H)  [expr $Graph(H1)-$Graph(H0)]

   set incr [expr int($Graph(H)/6)]

   if { $incr > 1000 } {
      set incr [expr $incr - $incr%1000]
   } elseif { $incr > 500 } {
      set incr [expr $incr - $incr%500]
   } elseif { $incr > 100 } {
      set incr [expr $incr - $incr%100]
   } elseif { $incr > 50 } {
     set incr [expr $incr - $incr%50]
   } elseif { $incr > 10 } {
     set incr [expr $incr - $incr%10]
   } else {
     set incr 5
   }

   set Graph(DeltaH)    [expr ($Graph(Y0) - $Graph(Y1))/$Graph(H)]
   set Graph(DeltaT)    [expr double($Graph(X1) - $Graph(X0))/($Graph(T1)-$Graph(T0))]

   #----- Afficher l'echelle des hauteurs

   for { set i $Graph(H0) } { $i <= $Graph(H1) } { incr i $incr } {

      set ii [expr $i-$Graph(H0)]
      set y [expr $Graph(Y0)-($ii*$Graph(DeltaH))]
      $canvas create line [expr $Graph(X0)-3] $y $Graph(X0) $y -fill $FG -tags TRAJGRAPH
      $canvas create text [expr $Graph(X0)-40] $y -fill $FG -font XFont10 -tags TRAJGRAPH -text [format %5i $i] -anchor w

      if { $ii> 0 && [expr $ii % 2] == 0 } {
         $canvas create line $Graph(X0) $y $Graph(X1) $y -fill #AAAAAA -width 0 -tags TRAJGRAPH  -dash .
      }
   }

   Trajectory::GraphPlot $Frame $FG $TrajId
   $canvas raise GRAPH
}

#----------------------------------------------------------------------------
# Nom      : <Trajectory::GraphPlot>
# Creation : Decembre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche les elevations dans le graphique des elevations
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <FG>      : Couleur du graph
#  <TrajId>  : Identificateurs des trajectoires
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Trajectory::GraphPlot { Frame FG TrajId } {
   global   GDefs
   variable Graph
   variable Data
   variable Param

   set canvas $Frame.page.canvas

   #----- Definir les parametres et coordonnees d'affichage

   set d0    1e301
   set d1    0
   set dap   0
   set dlst  ""
   set i      0

   #----- Creer le graph

   foreach t $TrajId {

      set retro [trajectory define $t -BACKWARD]

      set date0 [lindex [trajectory define $t -PARCEL 0] 0]
      set date1 [lindex [trajectory define $t -PARCEL end] 0]
      set datea [trajectory define $t -DATEAP]
      set d0    [expr ($date0-$Graph(T0))*$Graph(DeltaT)]

      set ppixel ""
      set no     0
      set col    [trajectory configure $t -color]
      set shape  [Trajectory::GetShape $t $i]

      foreach parcel [trajectory define $t -PARCELS] {

         set date  [lindex $parcel 0]
         set elev  [lindex $parcel 5]
         set spd   [lindex $parcel 8]
         set hour [clock format $date -format "%H" -gmt true]
         if { $hour=="00" } {
            set hour 0
         } else {
            set hour [string trimleft $hour 0]
         }

         set dx    [expr ($date-$date0)*$Graph(DeltaT)]
         set xloc  [expr $d0+$Graph(X0)+$dx]

         #----- Pour l'interval choisie, incluant la date de depart et d'arrivee

         if { $date==$date0 || $date==$date1 || [expr $hour%$Param(Interval)]==0 } {

            #----- Faire la ligne du anal-prog si elle existe

            if { $dap==0 && $datea!=0 } {

               if { $datea==$date && !$retro } {

                  #----- En mode Trajectoire si l'echelle inclus la date d'anal-prog faire la ligne
                  #      A la date exacte
                  $canvas create line $xloc $Graph(Y0) $xloc $Graph(Y1) -fill $FG -tags TRAJGRAPH
                  set dap 1
               } elseif { $datea<$date && !$retro } {

                  #----- En mode Trajectoire si l'echelle n'inclus pas la date d'anal-prog faire la ligne
                  #      A la date precedente
                  if { [expr $xloc - $dx] >= $Graph(X0) } {
                     $canvas create line [expr $xloc-$dx] $Graph(Y0) [expr $xloc-$dx] $Graph(Y1) -fill $FG -tags TRAJGRAPH
                  }
                  set dap 1
               } elseif { $datea>=$date && $retro } {

                  #----- En mode Retro-Trajectoire si l'echelle inclus la date d'anal-prog faire la ligne
                  #      A la date exacte
                  $canvas create line $xloc $Graph(Y0) $xloc $Graph(Y1) -fill $FG -tags TRAJGRAPH
                  set dap 1
               }
            }

            #----- Si la date est deja inscrite, ne pas la reinscrire

            if { [lsearch -exact $dlst $date] == -1 } {

               $canvas create text $xloc [expr $Graph(Y0)+15] -text $hour -fill $FG -font XFont10 -tags TRAJGRAPH
               $canvas create line $xloc $Graph(Y0) $xloc [expr $Graph(Y0)+3] -fill $FG -tags TRAJGRAPH

               #----- Plotter les dates a 00

               if { $hour == "00" } {
                  $canvas create text $xloc [expr $Graph(Y0)+25] -text "[clock format $date -format %m/%d -gmt true]" \
                     -fill $FG -font XFont10 -tags TRAJGRAPH
               }
               lappend dlst $date
            }

            #----- Plotter les icones de localisation des particules

            set YLoc [expr $Graph(Y0)-(($elev-$Graph(H0))*$Graph(DeltaH))]
            set pixel " $xloc $YLoc"
            append ppixel $pixel

            if { $Param(Size) > 0 } {
               if { $Param(Fill)==1 || ($hour == "00" && $Param(Fill)==2) } {
                   set fill 1
               } else {
                  set fill 0
               }
               Shape::Draw$shape $canvas $pixel "P.$t.$no TRAJGRAPH SHAPE" $col $Param(Size) $fill
            }

            if { $Param(Speed) && $spd!="" } {
               $canvas create text $xloc [expr $YLoc-$Param(Size)] -text "$spd" \
                  -anchor sw -tags TRAJGRAPH -font XFont10 -fill $col
            }
         }
         incr no
      }
      if { $Param(Width) > 0 } {
         eval $canvas create line $ppixel -fill $col -width $Param(Width) -tag "TRAJGRAPH"
      }
      incr i
   }
   $canvas raise SHAPE
}

proc Trajectory::GraphScale { Frame BG FG TrajId X Y } {

   set coo [$Frame.page.canvas coords TRAJGRAPHFRAME]
   set x0 [lindex $coo 0]
   set y0 [lindex $coo 1]

   if { $X>[expr $x0+10] && $Y>[expr $y0+10] } {
      Trajectory::Graph $Frame $BG $FG -999 -999 $X $Y $TrajId
      return True
   } else {
      return False
   }
}

#----------------------------------------------------------------------------
# Nom      : <Trajectory::Height>
# Creation : Fevrier 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche la liste des elevations
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <BG>      : Couleur du Fond
#  <FG>      : Couleur du graph
#  <X0>      : X Coin superieur gauche
#  <Y0>      : Y Coin superieur gauche
#  <X1>      : X Coin inferieur droit
#  <Y1>      : Y Coin inferieur droit
#  <TrajID>  : Identificateur des trajectoires
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Trajectory::Height { Frame BG FG X0 Y0 X1 Y1 TrajId } {
   global   GDefs
   variable Graph
   variable Data
   variable Param

   set canvas $Frame.page.canvas

   #----- Si aucune coordonnee specifie, utiliser la position courante

   if { $X0==-999 || $X1==-999  } {
      set coo [$canvas coords TRAJHEIGHTFRAME]
   }
   if { $X0==-999  } {
      set X0 [lindex $coo 0]
      set Y0 [lindex $coo 1]
   }
   if { $X1==-999  } {
      set X1 [lindex $coo 2]
      set Y1 [lindex $coo 3]
   }

   $canvas delete TRAJHEIGHT

   #----- Afficher le rectangle delimitateur

   $canvas create rectangle $X0 $Y0 $X1 $Y1 -fill $BG -width 1 -outline $FG -tags "TRAJHEIGHT TRAJHEIGHTFRAME"

   #----- Inscrire les donnees

   set y [expr int($Y0+10)]
   set x [expr $X1-17]
   set i 0

   foreach t $TrajId {
      set parcels [trajectory define $t -PARCELS]
      set date0   [lindex [lindex $parcels 0]   0]
      set date1   [lindex [lindex $parcels end] 0]
      set col     [trajectory configure $t -color]
      set shape   [Trajectory::GetShape $t $i]

      if { $y < $Y1 } {
         if { [trajectory define $t -LEVELTYPE]=="P" } {
            set Title "[format %5.0f [lindex [lindex $parcels 0] 4]] HPA"
         } else {
            set Title "[format %5.0f [lindex [lindex $parcels 0] 5]] AGL"
         }
         $canvas create text $x $y -text "$Title" -fill $col -tags TRAJHEIGHT -anchor e -font XFont10
      } else {
         break
      }
      set no 0

      foreach parcel $parcels {

         set date [lindex $parcel 0]
         set elev [format %5.1f [lindex $parcel 5]]
         set hour [clock format $date -format "%H" -gmt true]
         if { $hour=="00" } {
            set hour 0
         } else {
            set hour [string trimleft $hour 0]
         }

         #----- Pour l'interval choisie , incluant la date de depart et d'arrivee

         if { $date==$date0 || $date==$date1 || [expr $hour%$Param(Interval)]==0 } {

            if { [incr y 10] < $Y1 } {
               $canvas create text $x $y -text "$elev" -fill $FG -font XFont10 -anchor e \
                   -tags "P.$t.$no P.$t.$no.TEXT TRAJHEIGHT"

               if { $Param(Size) > 0 } {
                  if { $hour == "00" } {
                     if { $Param(Fill)!=0 } {
                        set fill 1
                     } else {
                        set fill 0
                     }

                     set Pixel "[expr $X1-10] $y"
                     Shape::Draw$shape $canvas $Pixel "TRAJHEIGHT" $col $Param(Size) $fill
                  }
               }
            } else {
               break
            }
         }
         incr no
      }
      incr y 20
      incr i
   }
}

proc Trajectory::HeightScale { Frame BG FG TrajId X Y } {

   set coo [$Frame.page.canvas coords TRAJHEIGHTFRAME]
   set x0 [lindex $coo 0]
   set y0 [lindex $coo 1]

   if { $X>[expr $x0+10] && $Y>[expr $y0+10] } {
      Trajectory::Height $Frame $BG $FG -999 -999 $X $Y $TrajId
      return True
   } else {
      return False
   }
}

#----------------------------------------------------------------------------
# Nom      : <Trajectory::Legend>
# Creation : Fevrier 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche la legende
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <BG>      : Couleur du Fond
#  <FG>      : Couleur du graph
#  <X0>      : X Coin superieur gauche
#  <Y0>      : Y Coin superieur gauche
#  <X1>      : X Coin inferieur droit
#  <Y1>      : Y Coin inferieur droit
#  <TrajID>  : Identificateur des trajectoires
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Trajectory::Legend { Frame BG FG X0 Y0 X1 Y1 TrajId } {
   global   GDefs
   variable Lbl
   variable Graph
   variable Data
   variable Param
   variable Resources

   set canvas $Frame.page.canvas

   #----- Si aucune coordonnee specifie, utiliser la position courante

   if { $X0==-999 || $X1==-999  } {
      set coo [$canvas coords TRAJLEGENDFRAME]
   }
   if { $X0==-999  } {
      set X0 [lindex $coo 0]
      set Y0 [lindex $coo 1]
   }
   if { $X1==-999  } {
      set X1 [lindex $coo 2]
      set Y1 [lindex $coo 3]
   }

   $canvas delete TRAJLEGEND
   $canvas create rectangle $X0 $Y0 $X1 $Y1 -fill $BG -width 1 -outline $FG -tags "TRAJLEGEND TRAJLEGENDFRAME"
   $canvas create rectangle $X0 $Y0 [expr $X0+241] [expr $Y0+28] -fill $BG -width 1 -outline $FG -tags "TRAJLEGEND"


   #----- Pour la premiere Trajectoire selectionnee

   if { [set t [lindex $TrajId end]] == "" } {
      return
   }

   #----- Determiner les parametres de la legende

   if { [trajectory define $t -BACKWARD] } {
      set start "Arrivee    / Arrival "
      set where "A          / At      "

      switch [trajectory define $t -MODE] {

         "0" {
            set traj_f "Prevision de retrotrajectoires"
            set traj_a "Back trajectory forecasts"
         }
         "1" {
            set traj_f "Prevision a posteriori de retrotrajectoires"
            set traj_a "Back trajectory hindcasts"
         }
         "2" {
            set traj_f "Retro-trajectoires mixtes"
            set traj_a "Mixed mode back trajectories"
         }
         "3" {
            set traj_f "Retro-trajectoires"
            set traj_a "Back trajectories"
         }
      }
   } else {
      set start "Depart     / Start   "
      set where "De         / From    "

      switch [trajectory define $t -MODE] {

         "0" {
            set traj_f "Prevision de trajectoires"
            set traj_a "Trajectory forecasts"
         }
         "1" {
            set traj_f "Prevision a posteriori de trajectoires"
            set traj_a "Trajectory hindcasts"
         }
         "2" {
            set traj_f "Trajectoires mixtes"
            set traj_a "Mixed mode trajectories"
         }
         "3" {
            set traj_f "Trajectoires"
            set traj_a "Trajectories"
         }
      }
   }

   set inter "Intervalle / Interval"
   set parcel [trajectory define $t -PARCEL 0]
   set name   [trajectory define $t -ID]
   set date   [clock format [trajectory define $t -DATE] -format "%Y%m%d %H%M" -gmt True]
   set type   [trajectory define $t -LEVELTYPE]
   set coordm [Convert::FormatCoord [lindex $parcel 1] [lindex $parcel 2] MIN 0]
   set coordd [Convert::FormatCoord [lindex $parcel 1] [lindex $parcel 2] DEG]

   #----- Plotter l'identification

   $canvas create bitmap [expr $X0+2] [expr $Y0+2] -bitmap $Resources(LogoCMC) -tags TRAJLEGEND -anchor nw -foreground red

   $canvas create text [expr $X0+255] [expr $Y0+2]  -text $traj_f -font XFont10 -tags TRAJLEGEND -fill black -anchor nw
   $canvas create text [expr $X0+255] [expr $Y0+11] -text $traj_a -font XFont10 -tags TRAJLEGEND -fill black -anchor nw
   $canvas create text [expr $X0+255] [expr $Y0+24] -text $name -font XFont20 -tags TRAJLEGEND -anchor nw
   $canvas create text [expr $X0+255] [expr $Y0+47] -text "$start : $date UTC" -font XFont10 -tags TRAJLEGEND -fill black -anchor w
   $canvas create text [expr $X0+255] [expr $Y0+56] -text "$where : $coordm" -font XFont10 -tags TRAJLEGEND -anchor w
   $canvas create text [expr $X0+255] [expr $Y0+65] -text "                      : $coordd" -font XFont10 -tags TRAJLEGEND -anchor w


   if { $type=="P" } {
      set txt_elev   "HPA"
      set txt_elev_a "All heights (M) above sea level"
      set txt_elev_f "Hauteurs (M) au-dessus du niveau de la mer"
   } else {
      set txt_elev   "AGL"
      set txt_elev_a "All heights (M) above surface"
      set txt_elev_f "Hauteurs (M) au-dessus de la surface"
   }

   $canvas create text [expr $X0+2] [expr $Y0+30] -text ${txt_elev_f} -font XFont8 -tags TRAJLEGEND -anchor nw
   $canvas create text [expr $X0+2] [expr $Y0+40] -text ${txt_elev_a} -font XFont8 -tags TRAJLEGEND -anchor nw

   #----- Plotter la legende des signes

   set str [expr int($X0+5)]
   set end [expr int($X0+35)]
   set y   [expr int($Y0+55)]
   set i  0

   foreach t $TrajId {

      set parcel [trajectory define $t -PARCEL 0]
      set col    [trajectory configure $t -color]
      set shape  [Trajectory::GetShape $t $i]
      set Pixel1 "$str $y"
      set Pixel2 "$end $y"

      if { $type=="P" } {
         set lbl [format "%.2f" [lindex $parcel 4]]
      } else {
         set lbl [format "%.2f" [lindex $parcel 5]]
      }

      $canvas create line $str $y $end $y -fill $col -width $Param(Width) -tag "TRAJLEGEND"

      if { $Param(Size) > 0 } {
         Shape::Draw$shape $canvas $Pixel1 "TRAJLEGEND" $col $Param(Size) 0
         Shape::Draw$shape $canvas $Pixel2 "TRAJLEGEND" $col $Param(Size) 0
      }

      $canvas create text [expr $end+8] [expr $y+1] -text "$lbl $txt_elev" -font XFont10 -tags TRAJLEGEND \
        -fill $col -anchor w

      if { $i == 2 } {
         incr str 120
         incr end 120
         set  y   [expr int($Y0+55)]
      } else {
         incr y 9
      }
      incr i
   }
   $canvas raise TRAJLEGEND
}

proc Trajectory::LegendScale { Frame BG FG TrajId X Y } {

   set coo [$Frame.page.canvas coords TRAJLEGENDFRAME]
   set x0 [lindex $coo 0]
   set y0 [lindex $coo 1]

   if { $X>[expr $x0+10] && $Y>[expr $y0+10] } {
      Trajectory::Legend $Frame $BG $FG -999 -999 $X $Y $TrajId
      return True
   } else {
      return False
   }
}

#----------------------------------------------------------------------------
# Nom      : <Trajectory::GetUpdate>
# Creation : Fevrier 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Mise a jour des parametres lors de l'assignation de trajectoires
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Trajectory::Update { } {
   variable Data

   if { [llength $Data(List)] } {
      set parcel [trajectory define [lindex $Data(List) 0] -PARCEL 0]
      set Viewport::Map(LatReset) [lindex $parcel 1]
      set Viewport::Map(LonReset) [lindex $parcel 2]
   }

   foreach traj $Data(List) {
      if { [trajectory define $traj -LEVELTYPE]=="P" } {
         set level [format "%.2f" [lindex [trajectory define $traj -PARCEL 0] 4]]
      } else {
         set level [format "%.2f" [lindex [trajectory define $traj -PARCEL 0] 5]]
      }
      if { [lsearch $Data(Levels) $level]==-1 } {
         lappend Data(Levels) $level
      }
   }

   ComboBox::DelAll $Data(Frame).left.traj.lvl
   ComboBox::AddList $Data(Frame).left.traj.lvl $Data(Levels)
}

#----------------------------------------------------------------------------
# Nom      : <Trajectory::UpdateItems>
# Creation : Juin 1998 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectue le refresh des trajectroires affichees sur la projection
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour:
#
# Remarques :
#   Cette procedure est appele par la librairie de projection Page et Viewport
#
#----------------------------------------------------------------------------

proc Trajectory::UpdateItems { Frame } {
   variable Data

   Trajectory::Project $Frame $Data(List)
}
