#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet    : Librairie de definitions pour les trajectoires
# Fichier   : Data_Trajectory.tcl
# Creation  : Janvier 1999 - J.P. Gauthier - CMC/CMOE
#
# Description: Definitions d'une structure de donnees et de procedures
#              relatives aux trajectoires
#
# Fonctions:
#
#    Trajectory::Export      { Id Path }
#    Trajectory::Locate      { Id Offset }
#    Trajectory::ParamFrame  { Frame Apply }
#    Trajectory::ParamGet    { { Spec "" } }
#    Trajectory::ParamSet    { { Spec "" } }
#    Trajectory::ParamPut    { }
#    Trajectory::ParamInit   { Traj { Spec "" } }
#    Trajectory::ParamUpdate { { Trajs { } } }
#    Trajectory::Register    { Traj { Update True } }
#    Trajectory::UnRegister  { Traj { Update True } }
#    Trajectory::VarMode     { Mode }
#    Trajectory::Graph       { Frame X0 Y0 X1 Y1 TrajId }
#    Trajectory::GraphPlot   { Frame TrajId }
#    Trajectory::GraphScale  { Frame TrajId X Y }
#    Trajectory::GraphFollow { Frame X Y }
#    Trajectory::Height      { Frame X0 Y0 X1 Y1 TrajId }
#    Trajectory::HeightScale { Frame TrajId X Y }
#    Trajectory::Legend      { Frame X0 Y0 X1 Y1 TrajId }
#    Trajectory::LegendScale { Frame TrajId X Y }
#
# Remarques :
#   Aucune
#
#===============================================================================

package provide Trajectory 4.1

catch { SPI::Splash "Loading Data Package Trajectory 4.1" }

package require DateStuff

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
   set Lbl(Traj)       { "Trajectoire" "Trajectory" }
   set Lbl(Width)      { "Largeur" "Width" }
   set Lbl(Color)      { "Couleur" "Color" }
   set Lbl(Icon)       { "Icone" "Icon" }
   set Lbl(Single)     { "Unique" "Single" }
   set Lbl(Style)      { "Style 3D" "3D Style" }
   set Lbl(Options)    { "Options" "Options" }
   set Lbl(Height)     "Hauteur/Temps - Height/Time"

   #----- Constantes relatives a l'affichage des trajectoires

   set Param(Icons)     { TRIANGLE SQUARE CIRCLE LOZENGE HBAR VBAR PENTAGON HEXAGON LIGHTNING X + }
   set Param(Colors)    { #ff0000 #0000ff #006400 #4C7A5C #FFCC00 #FF00CC #00FFFF #785D0C #ACF003 } ;#Liste des couleurs des niveaux
   set Param(SizeVars)  { "" ZPRES ZMODEL ZMSL DIST SPEED }
   set Param(Icon)      TRIANGLE
   set Param(Color)     #ff0000                                                     ;#Couleur courante
   set Param(Style)     1                                                           ;#Type d'affichage
   set Param(Size)      3                                                           ;#Grandeur des icones
   set Param(SizeVar)   ""                                                          ;#Variable de redimenssionement
   set Param(Width)     1                                                           ;#Grandeur des icones
   set Param(Mark)      24                                                          ;#Remplir les icones
   set Param(Interval)  3                                                           ;#Intervale de selection des donnees
   set Param(Idx)       2                                                           ;#Index de config

   set Param(Modes)     { LEVEL PARCEL ALL }                                        ;#Mode de selection des parametres
   set Param(Mode)      LEVEL                                                       ;#Mode de selection des parametres
   set Param(Spec)      ""                                                          ;#Variable a parametrer

   catch {
      set Resources(LogoCMC)   @$GDefs(Dir)/share/bitmap/SMC_hor_small.xbm
      set Resources(LogoARL)   @$GDefs(Dir)/share/bitmap/ARL_hor_small.xbm
   }

   set Data(List)           ""    ;#Liste des Id de trajectoire en memoire
   set Data(Frame)          ""    ;#Path du frame de configuration

   #----- Definir les parametres par defaut standard (VAAC/RSMC)

   dataspec create 500.00 -width $Param(Width) -fill white -color #FF0000 -icon TRIANGLE \
      -style $Param(Style) -size $Param(Size) -intervals [expr $Param(Interval)*3600] -mark [expr $Param(Mark)*3600]

   dataspec create 1500.00 -width $Param(Width)  -fill white  -color #0000FF -icon SQUARE \
      -style $Param(Style) -size $Param(Size) -intervals [expr $Param(Interval)*3600] -mark [expr $Param(Mark)*3600]

   dataspec create 3000.00 -width $Param(Width)  -fill white  -color #006400 -icon CIRCLE \
      -style $Param(Style) -size $Param(Size) -intervals [expr $Param(Interval)*3600] -mark [expr $Param(Mark)*3600]

   dataspec create 700.00  -width $Param(Width)  -fill white  -color #0000FF -icon SQUARE \
      -style $Param(Style) -size $Param(Size) -intervals [expr $Param(Interval)*3600] -mark [expr $Param(Mark)*3600]

   dataspec create 250.00 -width $Param(Width)  -fill white  -color #006400 -icon CIRCLE \
      -style $Param(Style) -size $Param(Size) -intervals [expr $Param(Interval)*3600] -mark [expr $Param(Mark)*3600]
}

proc Trajectory::ParcelInfo { Object Tag } {
   global GDefs

   set parcel [trajectory define $Object -PARCEL $Tag]
   set info  "[trajectory define $Object -ID]\n[format %.2f [lindex $parcel 5]] m\n[format %.2f [lindex $parcel 8]] m/s"

   return $info
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

   ProjCam::CloseUp $Page::Data(Frame) $Page::Data(Frame) $Page::Data(VP) $lat0 $lon0 $lat1 $lon1 $Offset
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
   variable Param

   set Data(Frame) [TabFrame::Add $Frame 2 [lindex $Lbl(Traj) $GDefs(Lang)] False ""]
   set Data(ApplyButton) $Apply

   labelframe $Data(Frame).id -text "[lindex $Lbl(Traj) $GDefs(Lang)]"
      menubutton $Data(Frame).id.type -textvariable Trajectory::Param(Mode) -relief groove -bd 2 -menu $Data(Frame).id.type.lst -width 10 -anchor w
      ComboBox::Create $Data(Frame).id.lvl Trajectory::Param(Spec) noedit unsorted nodouble -1 "" 8 3 { Trajectory::ParamGet; Trajectory::ParamPut }
      label $Data(Frame).id.uni -width 10
      pack $Data(Frame).id.type -side left -fill x -padx 2 -pady 1
      pack $Data(Frame).id.lvl -side left -fill both -expand True -padx 2 -pady 2

   menu $Data(Frame).id.type.lst
      foreach mode $Param(Modes) {
         $Data(Frame).id.type.lst add command -label $mode \
            -command "Trajectory::VarMode $mode"
      }

   labelframe $Data(Frame).part -text "[lindex $Lbl(Particle) $GDefs(Lang)]"
      frame $Data(Frame).part.icon
         label $Data(Frame).part.icon.lbl -text [lindex $Lbl(Icon) $GDefs(Lang)] -anchor w -width 11
         IcoMenu::Create $Data(Frame).part.icon.sel $GDefs(Dir)/share/bitmap \
            { zeroth.xbm stri.xbm ssquare.xbm scircle.xbm slos.xbm shbar.xbm svbar.xbm spenta.xbm shexa.xbm slight.xbm sx.xbm s+.xbm } [concat NONE $Trajectory::Param(Icons)] \
            Trajectory::Param(Icon) "Trajectory::ParamSet" 0 -relief groove -bd 2
         pack $Data(Frame).part.icon.lbl $Data(Frame).part.icon.sel -side left

      frame $Data(Frame).part.col
         label $Data(Frame).part.col.lbl -text [lindex $Lbl(Color) $GDefs(Lang)] -anchor w -width 11
         ColorBox::CreateSel $Data(Frame).part.col.sel Trajectory::Param(Color) Trajectory::ParamSet
         pack $Data(Frame).part.col.lbl $Data(Frame).part.col.sel -side left

      frame $Data(Frame).part.size
         scale $Data(Frame).part.size.sel -bd 1 -relief flat -width 15 -sliderlength 10 -from 1 -to 25 -variable Trajectory::Param(Size) \
            -orient horizontal -showvalue False -command "Trajectory::ParamSet; catch"
         label $Data(Frame).part.size.lbl -text [lindex $Lbl(Size) $GDefs(Lang)] -anchor w -width 11
         ComboBox::Create $Data(Frame).part.size.var Trajectory::Param(SizeVar) noedit sorted nodouble -1 $Param(SizeVars) 7 8 Trajectory::ParamSet
         pack $Data(Frame).part.size.lbl $Data(Frame).part.size.sel $Data(Frame).part.size.var -side left -fill x

      frame $Data(Frame).part.int
         IcoMenu::Create $Data(Frame).part.int.sel $GDefs(Dir)/share/bitmap \
            "zeroth.xbm digit1.xbm digit3.xbm digit6.xbm digit12.xbm digit24.xbm" "0 1 3 6 12 24" \
               Trajectory::Param(Interval) "Trajectory::ParamSet" $Trajectory::Param(Interval) -relief groove -bd 2
         label $Data(Frame).part.int.lbl -text [lindex $Lbl(Interval) $GDefs(Lang)] -anchor w -width 11
         pack $Data(Frame).part.int.lbl $Data(Frame).part.int.sel -side left -fill x

      frame $Data(Frame).part.mark
         IcoMenu::Create $Data(Frame).part.mark.sel $GDefs(Dir)/share/bitmap \
            "zeroth.xbm digit1.xbm digit3.xbm digit6.xbm digit12.xbm digit24.xbm" "0 1 3 6 12 24" \
               Trajectory::Param(Mark) "Trajectory::ParamSet" $Trajectory::Param(Mark) -relief groove -bd 2
         label $Data(Frame).part.mark.lbl -text [lindex $Lbl(Fill) $GDefs(Lang)] -anchor w -width 11
      pack $Data(Frame).part.mark.lbl $Data(Frame).part.mark.sel -side left -fill x

      pack $Data(Frame).part.icon $Data(Frame).part.col $Data(Frame).part.size $Data(Frame).part.int $Data(Frame).part.mark -side top -padx 2 -fill x

   labelframe $Data(Frame).traj -text [lindex $Lbl(Traj) $GDefs(Lang)]

      frame $Data(Frame).traj.style
         IcoMenu::Create $Data(Frame).traj.style.sel $GDefs(Dir)/share/bitmap \
            "zeroth.xbm tstyle0.xbm tstyle1.xbm tstyle2.xbm tstyle3.xbm tstyle4.xbm" "0 1 2 3 4 5" \
               Trajectory::Param(Style) "Trajectory::ParamSet" $Trajectory::Param(Style) -relief groove -bd 2
         label $Data(Frame).traj.style.lbl -text [lindex $Lbl(Style) $GDefs(Lang)] -anchor w -width 11
      pack $Data(Frame).traj.style.lbl $Data(Frame).traj.style.sel -side left -fill x

      frame $Data(Frame).traj.width
         IcoMenu::Create $Data(Frame).traj.width.sel $GDefs(Dir)/share/bitmap \
            "zeroth.xbm width1.xbm width2.xbm width3.xbm width4.xbm width5.xbm" "0 1 2 3 4 5" \
               Trajectory::Param(Width) "Trajectory::ParamSet" $Trajectory::Param(Width) -relief groove -bd 2
         label $Data(Frame).traj.width.lbl -text [lindex $Lbl(Width) $GDefs(Lang)] -anchor w -width 11
         pack $Data(Frame).traj.width.lbl $Data(Frame).traj.width.sel -side left -fill x

      pack $Data(Frame).traj.width $Data(Frame).traj.style  -side top -padx 2 -fill x

   pack $Data(Frame).id $Data(Frame).part $Data(Frame).traj -side top -padx 5 -pady 5 -fill x -anchor n

   IcoMenu::Set $Data(Frame).part.int.sel $Trajectory::Param(Interval)
}

#-------------------------------------------------------------------------------
# Nom      : <Trajcetory::ParamGet>
# Creation : Mai 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Recuperer les parametres d'une trajectoire.
#
# Parametres :
#   <Spec>   : Specification to configure
#
# Retour     :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Trajectory::ParamGet { { Spec "" } } {
   variable Data
   variable Param
   variable Resources

   if { $Spec=="" } {
      set Spec $Param(Spec)
   }

   if { ![dataspec is $Spec] } {
      return
   }

   set Param(Icon)      [dataspec configure $Spec -icon]
   set Param(Size)      [dataspec configure $Spec -size]
   set Param(SizeVar)   [dataspec configure $Spec -sizevar]
   set Param(Color)     [dataspec configure $Spec -color]

   if { [llength [dataspec configure $Spec -intervals]] } {
      set Param(Interval)  [expr int([dataspec configure $Spec -intervals]/3600)]
   }
   set Param(Mark)      [expr int([dataspec configure $Spec -mark]/3600)]

   set Param(Style)     [dataspec configure $Spec -style]
   set Param(Width)     [dataspec configure $Spec -width]
}

#----------------------------------------------------------------------------
# Nom      : <Trajectory::ParamSet>
# Creation : Main 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialise les options de trajectoires
#
# Parametres :
#   <Spec>   : Specification a configurer
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Trajectory::ParamSet { { Spec "" } } {
   variable Param
   variable Data

   if { $Spec=="" } {
      set Spec $Param(Spec)
   }

   if { [dataspec is $Spec] } {
      dataspec configure $Spec -color $Param(Color) -icon $Param(Icon)
   }

   foreach t [trajectory all] {
      trajectory configure $t -width $Param(Width) -style $Param(Style) -fill #FFFFFF \
            -size $Param(Size) -sizevar $Param(SizeVar) -intervals [expr $Param(Interval)*3600] -mark [expr $Param(Mark)*3600]
   }
   catch { $Data(ApplyButton) configure -state normal }
}

#-------------------------------------------------------------------------------
# Nom      : <Trajectory::ParamPut>
# Creation : Mai 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Inserer les parametres dans l'interface.
#
# Parametres :
#
# Retour     :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Trajectory::ParamPut { } {
   variable Data
   variable Param

   IcoMenu::Set $Data(Frame).part.icon.sel $Param(Icon)
   IcoMenu::Set $Data(Frame).part.mark.sel $Param(Mark)
   IcoMenu::Set $Data(Frame).part.int.sel $Param(Interval)
   IcoMenu::Set $Data(Frame).traj.style.sel $Param(Style)
   IcoMenu::Set $Data(Frame).traj.width.sel $Param(Width)

   ColorBox::ConfigNoColor $Data(Frame).part.col.sel $Param(Color)
}

#-------------------------------------------------------------------------------
# Nom      : <Trajectory::ParamInit>
# Creation : Mai 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialiser les nouvelles configurations.
#
# Parametres :
#   <Traj>   : Trajectoir a configurer
#   <Spec>   : Specification a configurer
#
# Retour     :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Trajectory::ParamInit { Traj { Spec "" } } {
   variable Param

   if { [dataspec is $Spec] } {
      set set [dataspec configure $Spec -set]

      if { !$set } {
         incr Param(Idx)
         dataspec configure $Spec -color [lindex $Param(Colors) [expr $Param(Idx)%[llength $Param(Colors)]]] \
            -icon [lindex $Param(Icons) [expr $Param(Idx)%[llength $Param(Icons)]]] -width $Param(Width) \
            -fill white -style $Param(Style) -size $Param(Size) -intervals [expr $Param(Interval)*3600] \
            -mark [expr $Param(Mark)*3600]
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Trajectory::Register>
# Creation : Mai 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Enregistrer la trajectoire dans la liste des trajectoires connues et configurables
#
# Parametres  :
#   <Traj>    : Identificateur de trajectoire
#   <Update>  : Mise a jour des parametres
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Trajectory::Register { Traj { Update True } } {
   variable Data
   variable Param

   if { [set idx [lsearch -exact $Data(List) $Traj]]==-1 } {
      lappend Data(List) $Traj
   }
   if { $Update } {
      Trajectory::ParamUpdate
   }
   return $idx
}

proc Trajectory::UnRegister { Traj { Update True } } {
   variable Data

   if { [set idx [lsearch -exact $Data(List) $Traj]]!=-1 } {
      set Data(List) [lreplace $Data(List) $idx $idx]
   }
   if { $Update } {
      Trajectory::ParamUpdate
   }
   return $idx
}

#----------------------------------------------------------------------------
# Nom      : <Trajectory::ParamUpdate>
# Creation : Mai 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Mettre a jour la liste des trajectoires selectionnees
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Trajectory::ParamUpdate { { Trajs { } } } {
   variable Param
   variable Data

   if { ![llength $Trajs] } {
      set Trajs $Data(List)
   }
   set current $Param(Spec)
   set exist 0
   set var   ""

   #----- Inserer les items dans la liste de configuration

   ComboBox::DelAll $Data(Frame).id.lvl

   foreach traj $Trajs {
      if { [trajectory is $traj] && [trajectory define $traj -PARCELNB] } {
          switch $Param(Mode) {
            "LEVEL"    { set var [format "%.2f" [trajectory define $traj -LEVEL]] }
            "PARCEL"   { set var $traj }
            "ALL"      { set var TRAJ }
         }

         if { ![dataspec is $var] || [trajectory configure $traj -set] } {
            set spec [trajectory configure $traj -dataspec]
            if { $spec!=$var } {
               dataspec copy $var $spec
               dataspec free $spec
            }

            Trajectory::ParamInit $traj $var
         }

         if { "$var"=="$current" } {
             set Param(Spec) $current
             set exist 1
         }
         trajectory configure $traj -dataspec $var
#         trajectory configure $traj -width $Param(Width) -style $Param(Style) -size $Param(Size) -intervals [expr $Param(Interval)*3600] -mark [expr $Param(Mark)*3600]
         ComboBox::Add $Data(Frame).id.lvl $var
      }
   }

   if { !$exist && $var!="" } {
      set Param(Spec) $var
      Trajectory::ParamGet
      Trajectory::ParamPut
   }
}

#----------------------------------------------------------------------------
# Nom      : <Trajectory::VarMode>
# Creation : Mai 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Modifier le mode de selection des parametres des trajectoires
#
# Parametres :
#    <Mode>  : Mode de selection des parametres
#
# Retour:
#
# Remarques :
#     - Il y a differente methodes:
#
#         LEVEL:  : Parametres par niveaux
#         PARCEL  : Parametres par particule
#         ALL     : Parametres uniques
#
#----------------------------------------------------------------------------

proc Trajectory::VarMode { Mode } {
   variable Param
   variable Data

   set Param(Mode) $Mode
   Trajectory::ParamUpdate

   catch { $Data(ApplyButton) configure -state normal }
}

#----------------------------------------------------------------------------
# Nom      : <Trajectory::Graph>
# Creation : Decembre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche le graphique des elevations
#
# Parametres :
#  <Frame>   : Identificateur de Page
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

proc Trajectory::Graph { Frame X0 Y0 X1 Y1 TrajId } {
   global   GDefs
   variable Graph
   variable Lbl

   set canvas $Frame.page.canvas

   #----- Si aucune coordonnee specifie, utiliser la position courante

   if { $X0==-999 || $X1==-999  } {
      set coo [$canvas coords TRAJGRAPH]
   }
   if { $X0==-999  } {
      set X0 [lindex $coo 0]
      set Y0 [lindex $coo 1]
   }
   if { $X1==-999  } {
      set X1 [expr [lindex $coo 0]+[lindex [$canvas itemconfigure TRAJGRAPH -width] end]]
      set Y1 [expr [lindex $coo 1]+[lindex [$canvas itemconfigure TRAJGRAPH -height] end]]
   }

   $canvas delete TRAJGRAPH

   #----- Creer le graph

   $canvas create graph -x $X0 -y $Y0 -width [expr $X1-$X0] -height [expr $Y1-$Y0] -anchor nw -command "trajgraph" \
       -fg black -bg white -fill white -tags "TRAJGRAPH" -font XFont10 -title $Lbl(Height) -legend False
   $canvas bind TRAJGRAPH <Motion>  "Trajectory::GraphFollow $Frame \[$canvas canvasx %x\] \[$canvas canvasy %y\]"

   if { [llength $TrajId] } {
      if { ![graphaxis is TRAJGRAPHAXISX] } {
         graphaxis create TRAJGRAPHAXISX
         graphaxis create TRAJGRAPHAXISY

         graphaxis configure TRAJGRAPHAXISX -font XFont10 -color black -position LL -width 1 -highoffset 0.01 -format 00HH/MMDD \
            -gridcolor black -gridwidth 1 -highlightcolor gray75 -highlightwidth 2 -highoffset 10
         graphaxis configure TRAJGRAPHAXISY -font XFont10 -color black -gridcolor gray50 -gridwidth 1 -dash . -position LL -width 1 -highoffset 10
      }
      Trajectory::GraphPlot $Frame $TrajId
   }
}

#----------------------------------------------------------------------------
# Nom      : <Trajectory::GraphFollow>
# Creation : Mai 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Extrait l'information d'une particule a partir de la coordonnee X-Y
#            dans le graph
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <X>       : X dans le canvas
#  <Y>       : Y dans le canvas
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Trajectory::GraphFollow { Frame X Y } {
   global GDefs

   set info ""

   if { [llength [set pick [trajgraph -pick $X $Y]]] } {
      set item [lindex $pick 0]
      set no   [lindex $pick 1]
      set obj  [graphitem configure $item -tag]
      set time [expr int([vector get [graphitem configure $item -xdata] $no])]

      set info  [Trajectory::ParcelInfo $obj $time]
      set parcel [trajectory define $obj -PARCEL $time]
      set Page::Data(Value)   "[trajectory define $obj -ID]:[DateStuff::StringDateFromSeconds [lindex $parcel 0] $GDefs(Lang)]"
      set Page::Data(Coord)    [Convert::FormatCoord [lindex $parcel 1] [lindex $parcel 2] $Page::Data(CoordUnit) $Page::Data(CoordPrec)]
      set Page::Data(Altitude) [lindex $parcel 5]
   } else {
      set Page::Data(Value)    ""
      set Page::Data(Coord)    ""
      set Page::Data(Altitude) ""
   }
   Page::CursorInfo $Frame $X $Y $info
}

#----------------------------------------------------------------------------
# Nom      : <Trajectory::GraphPlot>
# Creation : Decembre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche les elevations dans le graphique des elevations
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <TrajId>  : Identificateurs des trajectoires
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Trajectory::GraphPlot { Frame TrajId } {
   global   GDefs
   variable Graph
   variable Data
   variable Param

   #----- Creer le graph

   set items {}
   set h0 1e32
   set h1 1e-32
   set secs {}

   foreach t $TrajId {

      if { ![graphitem is TRAJGRAPH$t] } {
         graphitem create TRAJGRAPH$t
      }
      lappend items TRAJGRAPH$t

      set date0 [lindex [trajectory define $t -PARCEL 0] 0]
      set date1 [lindex [trajectory define $t -PARCEL end] 0]
      set datea [trajectory define $t -DATEAP]
      set inter [trajectory configure $t -intervals]
      set mark  [trajectory configure $t -mark]

      if { ![vector is TRAJGRAPH$t] } {
         vector create TRAJGRAPH$t
         vector dim    TRAJGRAPH$t { X Y }
      }
      vector set TRAJGRAPH$t.X {}
      vector set TRAJGRAPH$t.Y {}

      foreach parcel [trajectory define $t -PARCELS] {

         set date  [lindex $parcel 0]
         set elev  [lindex $parcel 5]
         set spd   [lindex $parcel 8]

         #----- Keep only > hourly date/time for axis
         if { [expr $date%3600]==0 || $date==$date0 || $date==$date1 } {
            lappend secs $date
         }
         set hour [clock format $date -format "%H" -gmt true]
         set min  [clock format $date -format "%M" -gmt true]
         if { $hour=="00" } {
            set hour 0
         } else {
            set hour [string trimleft $hour 0]
         }

         #----- Pour l'interval choisie, incluant la date de depart et d'arrivee

         vector append TRAJGRAPH$t.X $date
         vector append TRAJGRAPH$t.Y $elev

         set h  [vector stats TRAJGRAPH$t.Y -min]
         set h0 [expr $h<$h0?$h:$h0]
         set h  [vector stats TRAJGRAPH$t.Y -max]
         set h1 [expr $h>$h1?$h:$h1]
      }

      #----- Calculer la valeur de l'increment dans l'axe y du graph

      set h0   [expr int($h0>0?0:$h0)]
      set incr [expr int(($h1-$h0)/6)]

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

      set secs [lsort -unique $secs]
      graphaxis configure TRAJGRAPHAXISX -intervals $secs -min [lindex $secs 0] -max [lindex $secs end] -grid $datea -spacing 1
      graphaxis configure TRAJGRAPHAXISY -incr $incr -min $h0 -max $h1

      graphitem configure TRAJGRAPH$t -xaxis TRAJGRAPHAXISX -yaxis TRAJGRAPHAXISY -type LINE \
         -icon [trajectory configure $t -icon] -outline [trajectory configure $t -color] -iconoutline [trajectory configure $t -color] \
         -size [trajectory configure $t -size] -width [trajectory configure $t -width] -iconfill [trajectory configure $t -color] \
         -iconxshowvalue $inter -iconxfillvalue $mark -xdata TRAJGRAPH$t.X -ydata TRAJGRAPH$t.Y -tag $t

   }
   $Frame.page.canvas itemconfigure TRAJGRAPH -item $items
}

proc Trajectory::GraphScale { Frame TrajId X Y } {

   set x [$Frame.page.canvas itemcget TRAJGRAPH -x]
   set y [$Frame.page.canvas itemcget TRAJGRAPH -y]
   set w [expr $X-$x]
   set h [expr $Y-$y]

   $Frame.page.canvas itemconfigure TRAJGRAPH -width $w -height $h
   return 1
}

#----------------------------------------------------------------------------
# Nom      : <Trajectory::Height>
# Creation : Fevrier 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche la liste des elevations
#
# Parametres :
#  <Frame>   : Identificateur de Page
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

proc Trajectory::Height { Frame X0 Y0 X1 Y1 TrajId } {
   global   GDefs
   variable Graph
   variable Data

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

   $canvas create rectangle $X0 $Y0 $X1 $Y1 -fill white -width 1 -outline black -tags "TRAJHEIGHT TRAJHEIGHTFRAME"

   #----- Inscrire les donnees

   set y [expr int($Y0+10)]
   set x [expr $X1-17]
   set i 0

   foreach t $TrajId {
      set parcels [trajectory define $t -PARCELS]
      set date0   [lindex [lindex $parcels 0]   0]
      set date1   [lindex [lindex $parcels end] 0]
      set color   [trajectory configure $t -color]
      set size    [trajectory configure $t -size]
      set icon    [trajectory configure $t -icon]
      set mark    [expr int([trajectory configure $t -mark]/3600)]
      set inter   [expr int([trajectory configure $t -intervals]/3600)]

      if { $y < $Y1 } {
         if { [trajectory define $t -LEVELTYPE]=="P" } {
            set Title "[format %5.0f [trajectory define $t -LEVEL]] HPA"
         } else {
            set Title "[format %5.0f [trajectory define $t -LEVEL]] AGL"
         }
         $canvas create text $x $y -text "$Title" -fill $color -tags TRAJHEIGHT -anchor e -font XFont10
      } else {
         break
      }
      set no 0

      foreach parcel $parcels {

         set date [lindex $parcel 0]
         set elev [format %5.1f [lindex $parcel 5]]
         set hour [clock format $date -format "%H" -gmt true]
         set min  [clock format $date -format "%M" -gmt true]
         if { $hour=="00" } {
            set hour 0
         } else {
            set hour [string trimleft $hour 0]
         }

         #----- Pour l'interval choisie , incluant la date de depart et d'arrivee

         if { $inter==0 || $date==$date0 || $date==$date1 || ($min=="00" && [expr $hour%$inter]==0) } {

            if { [incr y 10] < $Y1 } {
               $canvas create text $x $y -text "$elev" -fill black -font XFont10 -anchor e -tags "P.$t.$no P.$t.$no.TEXT TRAJHEIGHT"

               if { $size>0 && $icon!="NONE"} {
                  if { $mark && [expr (($mark==24 && $hour==0) || $hour%$mark==0)] } {
                     Shape::Draw$icon $canvas "[expr $X1-10] $y" "TRAJHEIGHT" $color 4 1
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

proc Trajectory::HeightScale { Frame TrajId X Y } {

   set coo [$Frame.page.canvas coords TRAJHEIGHTFRAME]
   set x0 [lindex $coo 0]
   set y0 [lindex $coo 1]

   if { $X>[expr $x0+10] && $Y>[expr $y0+10] } {
      Trajectory::Height $Frame -999 -999 $X $Y $TrajId
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

proc Trajectory::Legend { Frame X0 Y0 X1 Y1 TrajId } {
   global   GDefs
   variable Lbl
   variable Graph
   variable Data
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
   $canvas create rectangle $X0 $Y0 $X1 $Y1 -fill white -width 1 -outline black -tags "TRAJLEGEND TRAJLEGENDFRAME"
   $canvas create rectangle $X0 $Y0 [expr $X0+241] [expr $Y0+28] -fill white -width 1 -outline black -tags "TRAJLEGEND"


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

   set parcel [trajectory define $t -PARCEL 0]
   set name   [trajectory define $t -ID]
   set date   [clock format [trajectory define $t -DATE] -format "%Y%m%d %H%M" -gmt True]
   set type   [trajectory define $t -LEVELTYPE]
   set coordm [Convert::FormatCoord [lindex $parcel 1] [lindex $parcel 2] MIN 1]
   set coordd [Convert::FormatCoord [lindex $parcel 1] [lindex $parcel 2] DEG 6]

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
      set color  [trajectory configure $t -color]
      set shape  [trajectory configure $t -icon]
      set size   [trajectory configure $t -size]
      set Pixel1 "$str $y"
      set Pixel2 "$end $y"

      if { $type=="P" } {
         set lbl [format "%.2f" [lindex $parcel 4]]
      } else {
         set lbl [format "%.2f" [lindex $parcel 5]]
      }

      $canvas create line $str $y $end $y -fill $color -width 1 -tag "TRAJLEGEND"

      if { $size>0 } {
         Shape::Draw$shape $canvas $Pixel1 "TRAJLEGEND" $color 4 0
         Shape::Draw$shape $canvas $Pixel2 "TRAJLEGEND" $color 4 0
      }

      $canvas create text [expr $end+8] [expr $y+1] -text "$lbl $txt_elev" -font XFont10 -tags TRAJLEGEND -fill $color -anchor w

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

proc Trajectory::LegendScale { Frame TrajId X Y } {

   set coo [$Frame.page.canvas coords TRAJLEGENDFRAME]
   set x0 [lindex $coo 0]
   set y0 [lindex $coo 1]

   if { $X>[expr $x0+10] && $Y>[expr $y0+10] } {
      Trajectory::Legend $Frame -999 -999 $X $Y $TrajId
      return True
   } else {
      return False
   }
}
