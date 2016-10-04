#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie de fonctions Tcl relatives aux enregistrements info
# Fichier  : Grid.tcl
# Creation : Avril 2015 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Definitions de diverses fonctionnalites relatives a la creatin de grille.
#
# Fonctions:
#    Grid::Window    { Frame }
#    Grid::Init      { }
#    Grid::Center    { Lat Lon }
#    Grid::BBoxOrder { }
#    Grid::Create    { { GridInfo {} } }
#    Grid::Decode    { Scale { Lat 0.0 } { Lon 0.0 } }
#    Grid::CreatePS  { Lat Lon Res NI NJ { ID MODELGRID } }
#    Grid::CreateL   { Lat0 Lon0 Lat1 Lon1 Res { ID MODELGRID } }
#    Grid::CreateLZ  { Lat0 Lon0 Lat1 Lon1 Res { ID MODELGRID } }
#    Grid::CreateUTM { Lat0 Lon0 Lat1 Lon1 Res { ID MODELGRID } }
#    Grid::Write     { FILE ID { IP1 0 } { IP2 0 } { IP3 0 } { GRID True } }
#    Grid::MoveInit  { Canvas VP }
#    Grid::Move      { Frame VP }
#    Grid::MoveDone  { Canvas VP }
#    Grid::DrawInit  { Canvas VP }
#    Grid::Draw      { Canvas VP }
#    Grid::DrawDone  { Canvas VP }
#
# Remarques :
#
#===============================================================================

package provide Grid 1.0

catch { SPI::Splash "Loading Package Grid 1.0" }

package require Dialog

namespace eval Grid {
   variable Lbl
   variable Msg
   variable Param
   variable Bubble

   set Param(Id)       "User"                                                    ;# Grid identification (name)
   set Param(Data)     Float32                                                   ;# Data format of GRID field
   set Param(Type)     "LZ"                                                      ;# Current grid type
   set Param(Types)    { "PS" "PS_N" "PS_S" "LL" "LZ" "UTM" }                    ;# List of grid types
   set Param(ResM)     10000                                                     ;# Grid resolution in meters
   set Param(ResMs)    { 1 5 10 100 1000 2000 5000 10000 25000 50000 150000 }    ;# List of predefined resolution in meters
   set Param(ResLL)    0.1                                                       ;# Grid resolution in degree
   set Param(ResLLs)   { 0.01 0.1 0.25 0.5 1 2 }                                 ;# List of predefined resolution in degrees
   set Param(NI)       0                                                         ;# Number of gridpoint in I
   set Param(NJ)       0                                                         ;# Number of gridpoint in J
   set Param(Lat0)     0                                                         ;# Latitude of first bbox corner
   set Param(Lat1)     0                                                         ;# Latitude of second bbox corner
   set Param(Lon0)     0                                                         ;# Longitude of first bbox corner
   set Param(Lon1)     0                                                         ;# Longitude of second bbox corner
   set Param(LatM)     0                                                         ;# Delta on latitute translating grid
   set Param(LonM)     0                                                         ;# Delta on longitude whehn translating grid
   set Param(PGSM)     ""                                                        ;# Grid description for PGSM
   set Param(GridInfo) ""                                                        ;# General grid description
   set Param(NIJWarn)  4000000                                                   ;# Warning grid size 2000x2000
   set Param(SizeWarn) [expr [info exists ::tk_version]?True:False]              ;# Warn for large grid (Only in interactive mode)
   set Param(LL2M)     [expr 1852.0*60]                                          ;# Conversion factor from degrees to meters

   set Lbl(Grid)       { "Type de grille    " "Grid type         " }
   set Lbl(ResM)       { "Résolution (m)    " "Resolution (m)    " }
   set Lbl(ResLL)      { "Résolution (deg)  " "Resolution (deg)  " }
   set Lbl(Size)       { "Dimension         " "Dimension         " }
   set Lbl(BBox)       { "Couverture" "Bounding box" }
   set Lbl(Yes)        { "Oui" "Yes" }
   set Lbl(No)         { "Non" "No" }

   set Msg(Size)         { "Ces paramètres vont générer une grille très grande, voulez vous continuer ?" "These parameters will generate a very large grid, do you wish to continue ?" }
   
   set Bubble(Types)     { "Sélection du type de grille:\n\tPS  : Polaire stéréographique\n\tPS_N: Polaire stéréographique centrée au pôle nord\n\tPS_S: Polaire stéréographique centrée au pôle sud\n\tLL  : LatLon traditionelle (GRTYP=L)\n\tLZ  : LatLon utilisant des ^^ << (GRTYP:Z)\n\tUTM : Universelle mercator transveralle (GRTYP=Z)"
                           "Grid type selection:\n\tPS  : Polar stereographic\n\tPS_N: Polar stereographic centered on north pole\n\tPS_S: Polar stereographic centered on south pole\n\tLL  : LatLon traditionnal (GRTYP=L)\n\tLZ  : LatLon using ^^ << (GRTYP:Z)\n\tUTM : Universal transverse mercator (GRTYP=Z)" }
   set Bubble(ResM)      { "Résolution en mètres (selon l'axe des latitudes pour les grilles latlon)" "Resolution in meters (on the latutide axis for latlon grids)" }
   set Bubble(ResLL)     { "Résolution en degrées" "Resolution in degrees" }
   set Bubble(Dimension) { "Nombre de points de grilles" "Number of gridpoints" }
   set Bubble(Coverage)  { "Couverture de la grille en latlon, spécifié par les coins opposés" "Grid coverage specified by the opposite corners in latlon" }
}

#----------------------------------------------------------------------------
# Nom      : <Grid::Reset>
# Creation : Avril 2015 - J.P. Gauthier - CMC/CMOE
#
# But      : Reset internal parameters.
#
# Parametres :
#
# Retour:
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Grid::Reset { } {
   variable Param

   set Param(NI)       0                                                         ;# Number of gridpoint in I
   set Param(NJ)       0                                                         ;# Number of gridpoint in J
   set Param(Lat0)     0                                                         ;# Latitude of first bbox corner
   set Param(Lat1)     0                                                         ;# Latitude of second bbox corner
   set Param(Lon0)     0                                                         ;# Longitude of first bbox corner
   set Param(Lon1)     0                                                         ;# Longitude of second bbox corner
   set Param(LatM)     0                                                         ;# Delta on latitute translating grid
   set Param(LonM)     0                                                         ;# Delta on longitude whehn translating grid
   set Param(PGSM)     ""                                                        ;# Grid description for PGSM
   set Param(GridInfo) ""                                                        ;# General grid description

   fstdfield free MODELGRID
}

#----------------------------------------------------------------------------
# Nom      : <Grid::Window>
# Creation : Avril 2015 - J.P. Gauthier - CMC/CMOE
#
# But      : Interface de creation de grille.
#
# Parametres :
#
# Retour:
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Grid::Window { Frame } {
   global GDefs
   variable Lbl
   variable Param
   variable Bubble

   #----- If wigdet already created
   if { [winfo exists $Frame.type] } {
      return
   }

   Option::Create $Frame.type  [lindex $Lbl(Grid) $GDefs(Lang)] Grid::Param(Type) 0 -1 $Grid::Param(Types) "Grid::Init; Grid::WindowSet $Frame"
   Option::Create $Frame.reskm [lindex $Lbl(ResM) $GDefs(Lang)] Grid::Param(ResM) 1 7 $Grid::Param(ResMs) "set Grid::Param(ResLL) \[expr \$Grid::Param(ResM)/$Param(LL2M)\]; Grid::Create"
   Option::Create $Frame.resll [lindex $Lbl(ResLL) $GDefs(Lang)] Grid::Param(ResLL) 1 7 $Grid::Param(ResLLs) "set Grid::Param(ResM) \[expr \$Grid::Param(ResLL)*$Param(LL2M)\]; Grid::Create"
   frame  $Frame.dim
      label $Frame.dim.lbl -text [lindex $Lbl(Size) $GDefs(Lang)]
      entry $Frame.dim.ni -relief sunken -bd 1 -bg $GDefs(ColorLight) -textvariable Grid::Param(NI) -width 5
      label $Frame.dim.x -text x
      entry $Frame.dim.nj -relief sunken -bd 1 -bg $GDefs(ColorLight) -textvariable Grid::Param(NJ) -width 5
      pack $Frame.dim.lbl $Frame.dim.ni $Frame.dim.x $Frame.dim.nj -side left
   label $Frame.bboxl -text [lindex $Lbl(BBox) $GDefs(Lang)] -anchor w
   pack $Frame.type $Frame.reskm $Frame.resll $Frame.dim $Frame.bboxl -side top -fill x -expand True

   frame $Frame.bbox
#      checkbutton $Frame.bbox.mode -variable Page::Data(ToolMode) -onvalue Grid -offvalue SPI \
#         -image ARROW -indicatoron 0 -relief sunken -bd 1 -overrelief raised -offrelief flat -selectcolor $GDefs(ColorLight) \
#         -command "SPI::ToolMode \$Page::Data(ToolMode) Data True"

      label $Frame.bbox.llat0 -text lat0 -relief groove -bd 2
      label $Frame.bbox.llon0 -text lon0 -relief groove -bd 2
      label $Frame.bbox.llat1 -text lat1 -relief groove -bd 2
      label $Frame.bbox.llon1 -text lon1 -relief groove -bd 2
      entry $Frame.bbox.elat0 -relief sunken -bd 1 -bg $GDefs(ColorLight) -textvariable Grid::Param(Lat0) -width 10
      entry $Frame.bbox.elon0 -relief sunken -bd 1 -bg $GDefs(ColorLight) -textvariable Grid::Param(Lon0) -width 10
      entry $Frame.bbox.elat1 -relief sunken -bd 1 -bg $GDefs(ColorLight) -textvariable Grid::Param(Lat1) -width 10
      entry $Frame.bbox.elon1 -relief sunken -bd 1 -bg $GDefs(ColorLight) -textvariable Grid::Param(Lon1) -width 10
#      grid  $Frame.bbox.mode -rowspan 2 -row 0 -column 0 -sticky nsew
      grid  $Frame.bbox.llat0 -sticky ew -row 0 -column 1
      grid  $Frame.bbox.llon0 -sticky ew -row 0 -column 2
      grid  $Frame.bbox.llat1 -sticky ew -row 0 -column 3
      grid  $Frame.bbox.llon1 -sticky ew -row 0 -column 4
      grid  $Frame.bbox.elat0 -sticky ew -row 1 -column 1
      grid  $Frame.bbox.elon0 -sticky ew -row 1 -column 2
      grid  $Frame.bbox.elat1 -sticky ew -row 1 -column 3
      grid  $Frame.bbox.elon1 -sticky ew -row 1 -column 4

   pack $Frame.bbox -padx 2 -pady 5 -ipadx 2 -ipady 2

   bind $Frame.reskm.e    <Return> "catch { set Grid::Param(ResLL) \[expr \$Grid::Param(ResM)/$Param(LL2M)\]; Grid::Create }"
   bind $Frame.resll.e    <Return> "catch { set Grid::Param(ResM) \[expr \$Grid::Param(ResLL)*$Param(LL2M)\]; Grid::Create }"
   bind $Frame.dim.ni     <Return> "catch { Grid::Create }"
   bind $Frame.dim.nj     <Return> "catch { Grid::Create }"
   bind $Frame.bbox.elat0 <Return> "catch { Grid::Create }"
   bind $Frame.bbox.elon0 <Return> "catch { Grid::Create }"
   bind $Frame.bbox.elat1 <Return> "catch { Grid::Create }"
   bind $Frame.bbox.elon1 <Return> "catch { Grid::Create }"

   set Param(SizeWarn) True
   set Param(ResLL)    [expr $Param(ResM)/$Param(LL2M)]
   
   Bubble::Create $Frame.type  $Bubble(Types)
   Bubble::Create $Frame.reskm $Bubble(ResM)
   Bubble::Create $Frame.resll $Bubble(ResLL)
   Bubble::Create $Frame.dim   $Bubble(Dimension)
   Bubble::Create $Frame.bbox  $Bubble(Coverage)
   
   Grid::WindowSet $Frame
}

#----------------------------------------------------------------------------
# Nom      : <Grid::Set>
# Creation : Juin 2015 - J.P. Gauthier - CMC/CMOE
#
# But      : Ajuster l'interface selon le type de grille.
#
# Parametres :
#  <Frame>   : Widget parent
#
# Retour:
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Grid::WindowSet { Frame } {
   variable Param
   
   $Frame.bbox.elat0 configure -state normal 
   $Frame.bbox.elon0 configure -state normal 
   $Frame.bbox.elat1 configure -state normal 
   $Frame.bbox.elon1 configure -state normal 
   $Frame.dim.ni     configure -state normal 
   $Frame.dim.nj     configure -state normal 
   
   switch $Param(Type) {
      "PS"   - 
      "PS_N" -
      "PS_S" { $Frame.bbox.elat1 configure -state disabled 
               $Frame.bbox.elon1 configure -state disabled 
             }     
      "LL"   -
      "LZ"   -
      "UTM"  { $Frame.dim.ni configure -state disabled 
               $Frame.dim.nj configure -state disabled 
             }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Grid::Init>
# Creation : Avril 2015 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialiser des parametres selon le type de grille
#
# Parametres :
#
# Retour:
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Grid::Init { } {
   variable Param

   switch $Param(Type) {
      "PS"   { set Param(NI) 229; set Param(NJ) 229; }
      "PS_N" { set Param(NI) 229; set Param(NJ) 229; set Param(ResM)  150000; set Param(ResLL) [expr $Param(ResM)/$Param(LL2M)]; set Param(Lon0) 0.0; set Param(Lat0)  90.0 }
      "PS_S" { set Param(NI) 229; set Param(NJ) 229; set Param(ResM)  150000; set Param(ResLL) [expr $Param(ResM)/$Param(LL2M)]; set Param(Lon0) 0.0; set Param(Lat0) -90.0 }
      "LL"   { }
      "LZ"   { }
      "UTM"  { }
   }

   Grid::Create
}

#----------------------------------------------------------------------------
# Nom      : <Grid::Center>
# Creation : Avril 2015 - J.P. Gauthier - CMC/CMOE
#
# But      : Centrer la grille sur une localisation
#
# Parametres :
#    <Lat>   : Latitude centrale
#    <Lon>   : Longitude centrale
#
# Retour:
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Grid::Center { Lat Lon } {
   variable Param

   switch $Param(Type) {
      "PS"   { set Param(Lon0) $Lon; set Param(Lat0) $Lat ; set Param(Lon1) 0.0; set Param(Lat1) 0.0 }
      "PS_N" { set Param(Lon0) 0.0;  set Param(Lat0)  90.0; set Param(Lon1) 0.0; set Param(Lat1) 0.0 }
      "PS_S" { set Param(Lon0) 0.0;  set Param(Lat0) -90.0; set Param(Lon1) 0.0; set Param(Lat1) 0.0 }
      "LL"   -
      "LZ"   -
      "UTM"  { set dlat [expr ($Param(Lat1)-$Param(Lat0))*0.5]
               set dlon [expr ($Param(Lon1)-$Param(Lon0))*0.5]
               set Param(Lat0) [expr $Lat-$dlat]
               set Param(Lat1) [expr $Lat+$dlat]
               set Param(Lon0) [expr $Lon-$dlon]
               set Param(Lon1) [expr $Lon+$dlon]
             }
   }

   Grid::Create
}

#----------------------------------------------------------------------------
# Nom      : <Grid::BBoxOrder>
# Creation : Avril 2015 - J.P. Gauthier - CMC/CMOE
#
# But      : Ordonnre les coordonnees de la bounding box
#
# Parametres :
#
# Retour:
#
# Remarques :
#    Cette fonction est utilise avec "uplevel" afin de simuler une macro
#
#----------------------------------------------------------------------------

proc Grid::BBoxOrder { } {
   variable Param

   uplevel {
      set Lat0 [expr $Lat0+$Param(LatM)]
      set Lon0 [expr $Lon0+$Param(LonM)]
      set Lat1 [expr $Lat1+$Param(LatM)]
      set Lon1 [expr $Lon1+$Param(LonM)]

      if { $Lat1<$Lat0 } {
         set tmp $Lat0
         set Lat0 $Lat1
         set Lat1 $tmp
      }

      if { $Lon1<$Lon0 } {
         set tmp $Lon0
         set Lon0 $Lon1
         set Lon1 $tmp
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Grid::Create>
# Creation : Avril 2015 - J.P. Gauthier - CMC/CMOE
#
# But      : Fonction de creation de grille appelee par l'interface
#
# Parametres :
#    <GridInfo> : Grid description string
#    <ID>       : Identificateur du champs qui sera cree
#
# Retour:
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Grid::Create { { GridInfo {} } { ID MODELGRID } } {
   variable Param
   variable Data

   if { [llength $GridInfo] } {
      scan $GridInfo "%s %i %i %f %f %f %f %f %f" Param(Type) Param(NI) Param(NJ) Param(Lat0) Param(Lon0) Param(Lat1) Param(Lon1) Param(ResM) Param(ResLL)]
   }

   if { [string match "PS*" [lindex $Param(Type) 0]] || ($Param(Lat0)!=$Param(Lat1) && $Param(Lon0)!=$Param(Lon1)) } {

      switch $Param(Type) {
         "PS"    { Grid::CreatePS  $Param(Lat0) $Param(Lon0) $Param(ResM) $Param(NI) $Param(NJ) $ID }
         "PS_S"  -
         "PS_N"  { Grid::CreatePS  $Param(Lat0) $Param(Lon0) $Param(ResM) $Param(NI) $Param(NJ) $ID }
         "LL"    { Grid::CreateL   $Param(Lat0) $Param(Lon0) $Param(Lat1) $Param(Lon1) $Param(ResLL) $ID }
         "LZ"    { Grid::CreateLZ  $Param(Lat0) $Param(Lon0) $Param(Lat1) $Param(Lon1) $Param(ResLL) $ID }
         "UTM"   { Grid::CreateUTM $Param(Lat0) $Param(Lon0) $Param(Lat1) $Param(Lon1) $Param(ResLL) $ID }
      }
      set Param(GridInfo) [format "$Param(Type) $Param(NI) $Param(NJ) %.7f %.7f %.7f %.7f %.2f %.7f" $Param(Lat0) $Param(Lon0) $Param(Lat1) $Param(Lon1) $Param(ResM) $Param(ResLL)]

      if { [info exists ::tk_version] } {
         fstdfield configure $ID -rendergrid 1 -colormap FLDMAPDEFAULT -color black -font XFont10

         set Data(Frame) $Page::Data(Frame)
         set Data(VP)    $Viewport::Data(VP)

         Viewport::Assign $Data(Frame) $Data(VP) $ID
         Viewport::UpdateData $Data(Frame)
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Grid::Decode>
# Creation : Avril 2015 - J.P. Gauthier - CMC/CMOE
#
# But      : Fonction de creation de grille a partir d'une définition d'expérience
#
# Parametres :
#    <Scale> : Grid description string
#    <Lat>   : Latitude centrale
#    <Lon>   : Longitude centrale
#
# Retour:
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Grid::Decode { Scale { Lat 0.0 } { Lon 0.0 } } {
   variable Param

   set center      False

   set Param(Id)   [lindex $Scale 0]                          ;#----- Grid scale name
   set Param(Type) [string trimleft  [lindex $Scale 1] "("]   ;#----- Grid type
   set res         [lindex $Scale 2]                          ;#----- Grid scale resolution
   set dim         [string trimright [lindex $Scale 4] ")"]   ;#----- Grid size NIxNJ

   #----- Get resolutions in meters and degrees
   switch [lindex $Scale 3] {
     "deg," { set Param(ResLL) $res;             set Param(ResM)  [expr $Param(ResLL)*$Param(LL2M)] }
     "m,"   { set Param(ResM)  $res;             set Param(ResLL) [expr $Param(ResM)/$Param(LL2M)] }
     "km,"  { set Param(ResM)  [expr $res*1000]; set Param(ResLL) [expr $Param(ResM)/$Param(LL2M)] }
   }

   #----- Get grid dimensions
   if { [set idx [string first "x" $dim]] != -1 } {
      set Param(NI) [string range $dim 0 [expr $idx - 1]]
      set Param(NJ) [string range $dim [expr $idx + 1] end]
   }

   #----- If hemispheric, check for north or south
   if { [string match "*HEMI*" $Param(Id)] } {
      if { $Lat>0 } {
         set Param(Type) "PS_N"
         set Param(Lat0) 90.0
      } else {
         set Param(Type) "PS_S"
         set Param(Lat0) -90.0
      }
      set Param(Lon0) 0.0
   }

   #----- For latlon grid, check for global case
   if { $Param(Type)=="LZ" || $Param(Type)=="LL" } {
      if { [expr ($Param(NI)*$Param(ResLL))>=(360-$Param(ResLL))] } {
          set Param(Lon0) -180
          set Param(Lon1) 180
          set Param(Lat0) -90
          set Param(Lat1) 90
      } else {
          set center True
          set Param(Lon0) 0
          set Param(Lon1) [expr $Param(ResLL)*$Param(NI)]
          set Param(Lat0) 0
          set Param(Lat1) [expr $Param(ResLL)*$Param(NJ)]
      }
   }

   if { $center || $Lon!=0.0 } {
      Grid::Center $Lat $Lon
   } else {
      Grid::Create
   }
}

#----------------------------------------------------------------------------
# Nom      : <Grid::CreatePS>
# Creation : Avril 2015 - J.P. Gauthier - CMC/CMOE
#
# But      : Creation d'une grille Polaire Stereographique (PS)
#
# Parametres :
#   <Lat>    : Latitude centrale
#   <Lon>    : Longitude centrale
#   <Res>    : Resolution en metres
#   <NI>     : Nombre de point de grille en I
#   <NJ>     : Nombre de point de grille en J
#   <ID>     : Identificateur du champs qui sera cree
#
# Retour:
#   <ID>     : Identificateur du champs cree
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Grid::CreatePS { Lat Lon Res NI NJ { ID MODELGRID } } {
   variable Param

   set xg3 $Res
   set lat [expr $Lat+$Param(LatM)]
   set lon [expr $Lon+$Param(LonM)]

   #----- choix de l'hemisphere SUD on NORD
   if { $lat<=0 } {
      set grtyp SUD
      set nhem  2
      set xg4  [expr 90.0+$Lon]
      set xg4  [expr floor(fmod($xg4+360.0,360.0))]
   } else {
      set grtyp NORD
      set nhem  1
      set xg4   [expr (270.0-$Lon+360.0)/360.0]
      set xg4   [expr ($xg4-floor($xg4))*360.0]
   }

   set dd60 1.0
   set xy [fstdgrid xyfll $lat $lon $dd60 $xg4 $nhem]

   set xg1 [expr ((($NI-1.0)/2.0) * $xg3 - [lindex $xy 0]) / $xg3 + 1.0]
   set xg2 [expr ((($NJ-1.0)/2.0) * $xg3 - [lindex $xy 1]) / $xg3 + 1.0]

   fstdfield free $ID
   fstdfield create $ID $NI $NJ 1
   fstdfield define $ID -NOMVAR "GRID" -ETIKET "GRID" -TYPVAR X -GRTYP [string index $grtyp 0] $xg1 $xg2 $xg3 $xg4

   set Param(PGSM) [format "PS $NI $NJ %.7f %.7f %.7f %.7f $grtyp" $xg1 $xg2 $xg3 $xg4]

   return ${ID}
}

#----------------------------------------------------------------------------
# Nom      : <Grid::CreateL>
# Creation : Avril 2015 - J.P. Gauthier - CMC/CMOE
#
# But      : Creation d'une grille LatLon (L)
#
# Parametres :
#   <Lat0>   : Latitude du premier coin
#   <Lon0>   : Longitude du premier coin
#   <Lat1>   : Latitude du deuxieme coin
#   <Lon1>   : Longitude du deuxieme coin
#   <Res>    : Resolution en degree
#   <ID>     : Identificateur du champs qui sera cree
#
# Retour:
#   <ID>     : Identificateur du champs cree
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Grid::CreateL { Lat0 Lon0 Lat1 Lon1 Res { ID MODELGRID } } {
   variable Param
   variable Msg
   variable Lbl

   Grid::BBoxOrder

   set ni [expr int(ceil(($Lon1-$Lon0)/$Res))+1]
   set nj [expr int(ceil(($Lat1-$Lat0)/$Res))+1]

   if { $Param(SizeWarn) && [expr $ni*$nj]>$Param(NIJWarn) } {
      if { ![Dialog::Default . 400 WARNING $Msg(Size) "\n\n\t$ni x $nj" 0 $Lbl(No) $Lbl(Yes)] } {
         return ""
      }
      set Param(SizeWarn) False
   }

   set Param(NI) $ni
   set Param(NJ) $nj

   fstdfield create ${ID} $Param(NI) $Param(NJ) 1 $Param(Data)
   fstdfield define ${ID} -NOMVAR "GRID" -ETIKET "GRID" -TYPVAR X -GRTYP L $Lat0 $Lon0 $Res $Res

   set Param(PGSM) [format "LATLON $Param(NI) $Param(NJ) %.7f %.7f %.7f %.7f" $Lat0 $Lon0 $Res $Res]

   return ${ID}
}

#----------------------------------------------------------------------------
# Nom      : <Grid::CreateLZ>
# Creation : Avril 2015 - J.P. Gauthier - CMC/CMOE
#
# But      : Creation d'une grille Z sur reference de grille latlon (L)
#
# Parametres :
#   <Lat0>   : Latitude du premier coin
#   <Lon0>   : Longitude du premier coin
#   <Lat1>   : Latitude du deuxieme coin
#   <Lon1>   : Longitude du deuxieme coin
#   <Res>    : Resolution en degree
#   <ID>     : Identificateur du champs qui sera cree
#
# Retour:
#   <ID>     : Identificateur du champs cree
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Grid::CreateLZ { Lat0 Lon0 Lat1 Lon1 Res { ID MODELGRID } } {
   variable Param
   variable Msg
   variable Lbl

   Grid::BBoxOrder

   set ni [expr int(ceil(($Lon1-$Lon0)/$Res))+1]
   set nj [expr int(ceil(($Lat1-$Lat0)/$Res))+1]

   if { $Param(SizeWarn) && [expr $ni*$nj]>$Param(NIJWarn) } {
      if { ![Dialog::Default . 400 WARNING $Msg(Size) "\n\n\t$ni x $nj" 0 $Lbl(No) $Lbl(Yes)] } {
         return ""
      }
      set Param(SizeWarn) False
   }

   set Param(NI) $ni
   set Param(NJ) $nj

   fstdfield free ${ID} ${ID}TIC ${ID}TAC
   fstdfield create ${ID}TIC $Param(NI) 1 1
   fstdfield create ${ID}TAC 1 $Param(NJ) 1

   fstdfield define ${ID}TIC -NOMVAR ">>" -ETIKET "GRID" -TYPVAR X -GRTYP L 0 0 1.0 1.0
   fstdfield define ${ID}TAC -NOMVAR "^^" -ETIKET "GRID" -TYPVAR X -GRTYP L 0 0 1.0 1.0

   #----- Compute tic grid coordinates.
   set lon $Lon0
   for { set i 0 } { $i < $Param(NI) } { incr i } {
      fstdfield stats ${ID}TIC -gridvalue $i 0 $lon
      set lon [expr $lon+$Res]
   }

   #----- Compute tac grid coordinates.
   set lat $Lat0
   for { set j 0 } { $j < $Param(NJ) } { incr j } {
      fstdfield stats ${ID}TAC -gridvalue 0 $j $lat
      set lat [expr $lat+$Res]
   }
   
   #----- Create the grid ans assign the tic/tac
   fstdfield create ${ID} $Param(NI) $Param(NJ) 1 $Param(Data)
   fstdfield define ${ID} -NOMVAR "GRID" -ETIKET "GRID" -TYPVAR X -GRTYP Z
   fstdfield define ${ID} -positional ${ID}TIC ${ID}TAC

   set Param(PGSM) ""

   return ${ID}
}

#----------------------------------------------------------------------------
# Nom      : <Grid::CreateUTM>
# Creation : Avril 2015 - J.P. Gauthier - CMC/CMOE
#
# But      : Creation d'une grille UTM avec la methode WKT
#
# Parametres :
#   <Lat0>   : Latitude du premier coin
#   <Lon0>   : Longitude du premier coin
#   <Lat1>   : Latitude du deuxieme coin
#   <Lon1>   : Longitude du deuxieme coin
#   <Res>    : Resolution en degree
#   <ID>     : Identificateur du champs qui sera cree
#
# Retour:
#   <ID>     : Identificateur du champs cree
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Grid::CreateUTM { Lat0 Lon0 Lat1 Lon1 Res { ID MODELGRID } } {
   variable Param
   variable Msg
   variable Lbl

   Grid::BBoxOrder

   set zone     [expr int(ceil((180+(($Lon1+$Lon0)/2))/6))]
   set meridian [expr -((180-($zone*6))+3)]
   set wkt      "\{PROJCS\[\"WGS_1984_UTM_Zone_${zone}N\",\
         GEOGCS\[\"GCS_WGS_1984\",\
            DATUM\[\"D_WGS_1984\",\
               SPHEROID\[\"WGS_1984\",6378137.0,298.257223563\]\],\
            PRIMEM\[\"Greenwich\",0.0\],\
            UNIT\[\"Degree\",0.0174532925199433\]\],\
         PROJECTION\[\"Transverse_Mercator\"\],\
         PARAMETER\[\"False_Easting\",500000.0\],\
         PARAMETER\[\"False_Northing\",0.0\],\
         PARAMETER\[\"Central_Meridian\",$meridian\],\
         PARAMETER\[\"Scale_Factor\",0.9996\],\
         PARAMETER\[\"Latitude_Of_Origin\",0.0\],\
         UNIT\[\"Meter\",1.0\]\]\}"

   georef free $ID
   fstdfield free ${ID} ${ID}PROJ ${ID}MTRX

   georef create $ID $wkt

   set xy0 [georef unproject $ID $Lat0 $Lon0]
   set xy1 [georef unproject $ID $Lat1 $Lon1]

   set ni [expr int(ceil(([lindex $xy1 0] - [lindex $xy0 0])/$Res))+1]
   set nj [expr int(ceil(([lindex $xy1 1] - [lindex $xy0 1])/$Res))+1]

   if { $Param(SizeWarn) && [expr $ni*$nj]>$Param(NIJWarn) } {
      if { ![Dialog::Default . 400 WARNING $Msg(Size) "\n\n\t$ni x $nj" 0 $Lbl(No) $Lbl(Yes)] } {
         return ""
      }
      set Param(SizeWarn) False
   }

   set Param(NI) $ni
   set Param(NJ) $nj

   set scalex    [expr abs($Res)]
   set scaley    [expr -1.0 * abs($Res)]
   set uly       [lindex $xy1 1]
   set ulx       [lindex $xy0 0]

   set  transform [list $ulx $scalex 0.000000000000000 $uly 0.000000000000000 $scaley]

   georef define $ID -transform $transform

   #----- Create projection and transform field
   fstdfield create ${ID}PROJ [string length $wkt] 1 1 UByte
   fstdfield define ${ID}PROJ -NOMVAR "PROJ" -ETIKET "GRID" -TYPVAR X -GRTYP X
   fstdfield define ${ID}PROJ -DATA [binary format A* $wkt]

   fstdfield create ${ID}MTRX 6 1 1 Float32
   fstdfield define ${ID}MTRX -NOMVAR "MTRX" -ETIKET "GRID" -TYPVAR X -GRTYP X
   fstdfield define ${ID}MTRX -DATA [binary format f* $transform]

   #----- Create the grid ans assign the tic/tac
   fstdfield create ${ID} $Param(NI) $Param(NJ) 1 $Param(Data)
   fstdfield define ${ID} -georef $ID -NOMVAR "GRID" -ETIKET "GRID" -TYPVAR X -GRTYP W

   set Param(PGSM) ""

   return ${ID}
}

#----------------------------------------------------------------------------
# Nom      : <Grid::Write>
# Creation : Avril 2015 - J.P. Gauthier - CMC/CMOE
#
# But      : Sauvegarde d'une grille dans un fichier
#
# Parametres :
#   <FILE>   : Identificateur de fichier RPN
#   <ID>     : Identificateur du champs grille
#   <IP1>    : IP1 des descripteurs
#   <IP2>    : IP2 des descripteurs
#   <IP3>    : IP3 des descripteurs
#   <Grid>   : Write field on grid
#
# Retour:
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Grid::Write { FILE ID { IP1 0 } { IP2 0 } { IP3 0 } { Grid True }} {

   set dateo [fstdstamp fromseconds [clock seconds]]

   #----- If no IP specified, use "unique" values take from current datestamp
   if { !$IP1 && !$IP2 && !$IP3 } {
      set IP1 [string range $dateo 0 2]
      set IP2 [string range $dateo 3 5]
      set IP3 [string range $dateo 6 9]
   }
   
   switch [fstdfield define $ID -GRTYP] {
      "Z" {
         fstdfield define ${ID}TIC -DATEO $dateo -IP1 $IP1 -IP2 $IP2 -IP3 $IP3
         fstdfield define ${ID}TAC -DATEO $dateo -IP1 $IP1 -IP2 $IP2 -IP3 $IP3
         fstdfield define ${ID}    -DATEO $dateo -TYPVAR X -IG1 $IP1 -IG2 $IP2 -IG3 $IP3 -IG4 0

         fstdfield write ${ID}TIC $FILE -32 True
         fstdfield write ${ID}TAC $FILE -32 True

        }
      "W" {
         fstdfield define ${ID}PROJ -DATEO $dateo -IP1 $IP1 -IP2 $IP2 -IP3 $IP3
         fstdfield define ${ID}MTRX -DATEO $dateo -IP1 $IP1 -IP2 $IP2 -IP3 $IP3
         fstdfield define ${ID}     -DATEO $dateo -TYPVAR X -IG1 $IP1 -IG2 $IP2 -IG3 $IP3 -IG4 0

         fstdfield write ${ID}PROJ $FILE 0 True
         fstdfield write ${ID}MTRX $FILE -32 True
      }
      default {
         fstdfield define ${ID}     -DATEO $dateo 
      }
   }

   if { $Grid } {
      fstdfield write ${ID}    $FILE -8 True
   }
}

#----------------------------------------------------------------------------
# Nom      : <Grid::...>
# Creation : Avril 2015 - J.P. Gauthier - CMC/CMOE
#
# But      : Fonctions de creation et deplacement interactifs
#
# Parametres :
#   <Canvas> : Identificateur du canvas
#   <VP>     : Identificateur de la vue
#
# Retour:
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Grid::MoveInit { Canvas VP } { }

proc Grid::Move { Frame VP } {
   variable Sim
   variable Param

   set Param(LatM) $Viewport::Map(LatD)
   set Param(LonM) $Viewport::Map(LonD)

   Grid::Create
}

proc Grid::MoveDone { Canvas VP } {
  variable Param

   set Param(Lat0) [expr $Param(Lat0)+$Param(LatM)]
   set Param(Lon0) [expr $Param(Lon0)+$Param(LonM)]
   set Param(Lat1) [expr $Param(Lat1)+$Param(LatM)]
   set Param(Lon1) [expr $Param(Lon1)+$Param(LonM)]
   set Param(LatM) 0
   set Param(LonM) 0

}

proc Grid::DrawInit { Canvas VP } {
  variable Param

   set Param(Lat0) $Viewport::Map(LatCursor)
   set Param(Lon0) $Viewport::Map(LonCursor)
   }

proc Grid::Draw     { Canvas VP } {
  variable Param

   set Param(Lat1) $Viewport::Map(LatCursor)
   set Param(Lon1) $Viewport::Map(LonCursor)

   if { [lindex $Param(Type) 0]=="PS" } {
      set Param(Lat0) $Viewport::Map(LatCursor)
      set Param(Lon0) $Viewport::Map(LonCursor)
   }
   Grid::Create
}

proc Grid::DrawDone { Canvas VP } { }
