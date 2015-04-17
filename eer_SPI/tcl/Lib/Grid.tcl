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
#
#
# Remarques :
#
#===============================================================================

package provide Grid 1.0

catch { SPI::Splash "Loading Package Grid 1.0" }

namespace eval Grid {
   variable Lbl
   variable Msg
   variable Param

   set Param(Data)     Float32
   set Param(Type)     "LatLon Z"
   set Param(Types)    { "PS" "PS N" "PS S" "LatLon L" "LatLon Z" "UTM" }
   set Param(ResM)     10000
   set Param(ResMs)    { 1 5 10 100 1000 2000 5000 10000 25000 50000 150000 }
   set Param(ResLL)    0.1
   set Param(ResLLs)   { 0.01 0.1 0.5 1 2 }
   set Param(NI)       0
   set Param(NJ)       0
   set Param(Lat0)     0
   set Param(Lat1)     0
   set Param(Lon0)     0
   set Param(Lon1)     0
   set Param(LatM)     0                                               ;# Delta on latitute translating grid
   set Param(LonM)     0                                               ;# Delta on longitude whehn translating grid
   set Param(PGSM)     ""                                              ;# Grid description for PGSM
   set Param(NIJWarn)  4000000                                         ;# Warning grid size 2000x2000
   set Param(SizeWarn) True                                            ;# Warn for large grid
   set Param(LL2M)     [expr 1852.0*60]

   set Lbl(Params)     { "Paramètres" "Parameters" }
   set Lbl(Grid)       { "Type de grille   " "Grid type        " }
   set Lbl(ResM)      { "Résolution (m)   " "Resolution (m)   " }
   set Lbl(ResLL)      { "Résolution (deg) " "Resolution (deg) " }
   set Lbl(Size)       { "Dimension        " "Dimension        " }
   set Lbl(BBox)       { "Couverture" "Bounding box" }
   set Lbl(Yes)        { "Oui" "Yes" }
   set Lbl(No)         { "Non" "No" }

   set Msg(Size)       { "Ces paramètres vont générer une grille très grande, voulez vous continuer ?" "These parameters will generate a very large grid, do you wish to continue ?" }
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

proc Grid::Window { } {
   global GDefs
   variable Lbl
   variable Param

   if { ![winfo exists .grid] } {

      toplevel .grid

      wm title         .grid [lindex $Lbl(Grid) $GDefs(Lang)]
      wm protocol      .grid WM_DELETE_WINDOW { Grid::Close }

      if { $SPI::Param(Window) } { wm transient .grid . }

      labelframe .grid.param -text [lindex $Lbl(Params) $GDefs(Lang)]
         Option::Create .grid.param.type [lindex $Lbl(Grid) $GDefs(Lang)] Grid::Param(Type) 0 -1 $Grid::Param(Types) "Grid::Init"
         Option::Create .grid.param.reskm  [lindex $Lbl(ResM) $GDefs(Lang)] Grid::Param(ResM) 1 7 $Grid::Param(ResMs) "set Grid::Param(ResLL) \[expr \$Grid::Param(ResM)/$Param(LL2M)\]; Grid::Create"
         Option::Create .grid.param.resll  [lindex $Lbl(ResLL) $GDefs(Lang)] Grid::Param(ResLL) 1 7 $Grid::Param(ResLLs) "set Grid::Param(ResM) \[expr \$Grid::Param(ResLL)*$Param(LL2M)\]; Grid::Create"
         frame  .grid.param.dim
            label .grid.param.dim.lbl -text [lindex $Lbl(Size) $GDefs(Lang)]
            entry .grid.param.dim.ni -relief sunken -bd 1 -bg $GDefs(ColorLight) -textvariable Grid::Param(NI) -width 5
            label .grid.param.dim.x -text x
            entry .grid.param.dim.nj -relief sunken -bd 1 -bg $GDefs(ColorLight) -textvariable Grid::Param(NJ) -width 5
            pack .grid.param.dim.lbl .grid.param.dim.ni .grid.param.dim.x .grid.param.dim.nj -side left
         pack .grid.param.type .grid.param.reskm .grid.param.resll .grid.param.dim -side top -fill x -expand True

      labelframe .grid.bbox -text [lindex $Lbl(BBox) $GDefs(Lang)]
         checkbutton .grid.bbox.mode -variable Page::Data(ToolMode) -onvalue Grid -offvalue SPI \
            -image ARROW -indicatoron 0 -relief sunken -bd 1 -overrelief raised -offrelief flat -selectcolor $GDefs(ColorLight) \
            -command "SPI::ToolMode \$Page::Data(ToolMode) Data True"

         label .grid.bbox.llat0 -text lat0 -relief groove -bd 2
         label .grid.bbox.llon0 -text lon0 -relief groove -bd 2
         label .grid.bbox.llat1 -text lat1 -relief groove -bd 2
         label .grid.bbox.llon1 -text lon1 -relief groove -bd 2
         entry .grid.bbox.elat0 -relief sunken -bd 1 -bg $GDefs(ColorLight) -textvariable Grid::Param(Lat0) -width 10
         entry .grid.bbox.elon0 -relief sunken -bd 1 -bg $GDefs(ColorLight) -textvariable Grid::Param(Lon0) -width 10
         entry .grid.bbox.elat1 -relief sunken -bd 1 -bg $GDefs(ColorLight) -textvariable Grid::Param(Lat1) -width 10
         entry .grid.bbox.elon1 -relief sunken -bd 1 -bg $GDefs(ColorLight) -textvariable Grid::Param(Lon1) -width 10
         grid  .grid.bbox.mode -rowspan 2 -row 0 -column 0 -sticky nsew
         grid  .grid.bbox.llat0 -sticky ew -row 0 -column 1
         grid  .grid.bbox.llon0 -sticky ew -row 0 -column 2
         grid  .grid.bbox.llat1 -sticky ew -row 0 -column 3
         grid  .grid.bbox.llon1 -sticky ew -row 0 -column 4
         grid  .grid.bbox.elat0 -row 1 -column 1
         grid  .grid.bbox.elon0 -row 1 -column 2
         grid  .grid.bbox.elat1 -row 1 -column 3
         grid  .grid.bbox.elon1 -row 1 -column 4
      pack .grid.param .grid.bbox -side top -fill x -expand True

      bind .grid.param.reskm.e <Return> "catch { set Grid::Param(ResLL) \[expr \$Grid::Param(ResM)/$Param(LL2M)\]; Grid::Create }"
      bind .grid.param.resll.e <Return> "catch { set Grid::Param(ResM) \[expr \$Grid::Param(ResLL)*$Param(LL2M)\]; Grid::Create }"
      bind .grid.bbox.elat0 <Return> "catch { Grid::Create }"
      bind .grid.bbox.elon0 <Return> "catch { Grid::Create }"
      bind .grid.bbox.elat1 <Return> "catch { Grid::Create }"
      bind .grid.bbox.elon1 <Return> "catch { Grid::Create }"
   }

   set Param(SizeWarn) True
   set Param(ResLL)    [expr $Param(ResM)/$Param(LL2M)]
}

#----------------------------------------------------------------------------
# Nom      : <Grid::Close>
# Creation : Avril 2015 - J.P. Gauthier - CMC/CMOE
#
# But      : Fermer l'interface de creation de grille et enlever de la vue
#
# Parametres :
#
# Retour:
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Grid::Close { } {

   destroy .grid
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
      "PS"       { set Param(NI) 229; set Param(NJ) 229; }
      "PS N"     { set Param(NI) 229; set Param(NJ) 229; set Param(ResM)  150000; set Param(ResLL) [expr $Param(ResM)/$Param(LL2M)]; set Param(Lon0) 0.0; set Param(Lat0)  90.0 }
      "PS S"     { set Param(NI) 229; set Param(NJ) 229; set Param(ResM)  150000; set Param(ResLL) [expr $Param(ResM)/$Param(LL2M)]; set Param(Lon0) 0.0; set Param(Lat0) -90.0 }
      "LatLon L" { }
      "LatLon Z" { }
      "UTM"      { }
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
#
# Retour:
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Grid::Create { } {
   variable Param
   variable Data

   if { [lindex $Param(Type) 0]=="PS" || ($Param(Lat0)!=$Param(Lat1) && $Param(Lon0)!=$Param(Lon1)) } {

      switch $Param(Type) {
         "PS"       { Grid::CreatePS  $Param(Lat0) $Param(Lon0) $Param(ResM) $Param(NI) $Param(NJ)   }
         "PS S"     -
         "PS N"     { Grid::CreatePS  $Param(Lat0) $Param(Lon0) $Param(ResM) $Param(NI) $Param(NJ)   }
         "LatLon L" { Grid::CreateL   $Param(Lat0) $Param(Lon0) $Param(Lat1) $Param(Lon1) $Param(ResLL) }
         "LatLon Z" { Grid::CreateZL  $Param(Lat0) $Param(Lon0) $Param(Lat1) $Param(Lon1) $Param(ResLL) }
         "UTM"      { Grid::CreateUTM $Param(Lat0) $Param(Lon0) $Param(Lat1) $Param(Lon1) $Param(ResLL) }
      }

      if { $Draw } {
         fstdfield configure MODELGRID -rendergrid 1 -colormap FLDMAPDefault -color black -font XFont10

         set Data(Frame) $Page::Data(Frame)
         set Data(VP)    $Viewport::Data(VP)

         Viewport::Assign $Data(Frame) $Data(VP) MODELGRID
         Viewport::UpdateData $Data(Frame)
      }
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
   fstdfield define $ID -NOMVAR GRID
   fstdfield define $ID -GRTYP [string index $grtyp 0] $xg1 $xg2 $xg3 $xg4

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

   set ni [expr int(ceil(($Lon1-$Lon0)/$Res))]
   set nj [expr int(ceil(($Lat1-$Lat0)/$Res))]

   if { $Param(SizeWarn) && [expr $ni*$nj]>$Param(NIJWarn) } {
      if { ![Dialog::Default .grid 400 WARNING $Msg(Size) "\n\n\t$ni x $nj" 0 $Lbl(No) $Lbl(Yes)] } {
         set Param(SizeWarn) False
         return ""
      }
   }

   set Param(NI) $ni
   set Param(NJ) $nj

   fstdfield create ${ID} $Param(NI) $Param(NJ) 1 $Param(Data)
   fstdfield define ${ID} -NOMVAR "GRID" -GRTYP L $Lat0 $Lon0 $Res $Res

   set Param(PGSM) [format "LATLON $Param(NI) $Param(NJ) %.7f %.7f %.7f %.7f L" $lat0 $lon0 $Res $Res]

   return ${ID}
}

#----------------------------------------------------------------------------
# Nom      : <Grid::CreateZL>
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

proc Grid::CreateZL { Lat0 Lon0 Lat1 Lon1 Res { ID MODELGRID } } {
   variable Param
   variable Msg
   variable Lbl

   Grid::BBoxOrder

   set ni [expr int(ceil(($Lon1-$Lon0)/$Res))]
   set nj [expr int(ceil(($Lat1-$Lat0)/$Res))]

   if { $Param(SizeWarn) && [expr $ni*$nj]>$Param(NIJWarn) } {
      if { ![Dialog::Default .grid 400 WARNING $Msg(Size) "\n\n\t$ni x $nj" 0 $Lbl(No) $Lbl(Yes)] } {
         set Param(SizeWarn) False
         return ""
      }
   }

   set Param(NI) $ni
   set Param(NJ) $nj

   fstdfield free ${ID} ${ID}TIC ${ID}TAC
   fstdfield create ${ID}TIC $Param(NI) 1 1
   fstdfield create ${ID}TAC 1 $Param(NJ) 1

   fstdfield define ${ID}TIC -GRTYP L 0 0 1.0 1.0
   fstdfield define ${ID}TAC -GRTYP L 0 0 1.0 1.0

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
   fstdfield define ${ID} -NOMVAR "GRID" -GRTYP Z
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

   set Param(NI) [expr int(ceil(([lindex $xy1 0] - [lindex $xy0 0])/$Res))]
   set Param(NJ) [expr int(ceil(([lindex $xy1 1] - [lindex $xy0 1])/$Res))]

   set scalex    [expr abs($Res)]
   set scaley    [expr -1.0 * abs($Res)]
   set uly       [lindex $xy1 1]
   set ulx       [lindex $xy0 0]

   set  transform [list $ulx $scalex 0.000000000000000 $uly 0.000000000000000 $scaley]

   georef define $ID -transform $transform

   #----- Create projection and transform field
   fstdfield create ${ID}PROJ [string length $wkt] 1 1 UByte
   fstdfield define ${ID}PROJ -GRTYP X
   fstdfield define ${ID}PROJ -DATA [binary format A* $wkt]

   fstdfield create ${ID}MTRX 6 1 1 Float32
   fstdfield define ${ID}MTRX -GRTYP X
   fstdfield define ${ID}MTRX -DATA [binary format f* $transform]

   #----- Create the grid ans assign the tic/tac
   fstdfield create ${ID} $Param(NI) $Param(NJ) 1 $Param(Data)
   fstdfield define ${ID} -georef $ID -NOMVAR "GRID" -GRTYP W

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
#   <ETIKET> : Etiquette a utiliser
#   <IP1>    : IP1 des descripteurs
#   <IP2s>   : IP2 des descripteurs
#   <IP3>    : IP3 des descripteurs
#
# Retour:
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Grid::Write { FILE ID ETIKET { IP1 0 } { IP2 0 } { IP3 0 } } {

   set dateo [fstdstamp fromseconds [clock seconds]]

   switch [fstdfield define $ID -GRTYP] {
      "Z" {
         fstdfield define ${ID}TIC -DATEO $dateo -DEET 0 -NPAS 0 -IP1 $IP1 -IP2 $IP3 -IP3 $IP3 -ETIKET $ETIKET -NOMVAR ">>" -TYPVAR X
         fstdfield define ${ID}TAC -DATEO $dateo -DEET 0 -NPAS 0 -IP1 $IP1 -IP2 $IP3 -IP3 $IP3 -ETIKET $ETIKET -NOMVAR "^^" -TYPVAR X
         fstdfield define ${ID}    -DATEO $dateo -DEET 0 -NPAS 0 -IP1 1200 -IP2 0    -IP3 0    -ETIKET $ETIKET -TYPVAR X -IG1 $IP1 -IG2 $IP2 -IG3 $IP3 -IG4 0

         fstdfield write ${ID}TIC FILE -32 True
         fstdfield write ${ID}TAC FILE -32 True
         fstdfield write ${ID}    FILE -8 True
        }
      "W" {
         fstdfield define ${ID}PROJ -DATEO $dateo -DEET 0 -NPAS 0 -IP1 $IP1 -IP2 $IP3 -IP3 $IP3 -ETIKET $ETIKET -NOMVAR "PROJ" -TYPVAR X
         fstdfield define ${ID}MTRX -DATEO $dateo -DEET 0 -NPAS 0 -IP1 $IP1 -IP2 $IP3 -IP3 $IP3 -ETIKET $ETIKET -NOMVAR "MTRX" -TYPVAR X
         fstdfield define ${ID}     -DATEO $dateo -DEET 0 -NPAS 0 -IP1 1200 -IP2 0    -IP3 0    -ETIKET $ETIKET -TYPVAR X -IG1 $IP1 -IG2 $IP2 -IG3 $IP3 -IG4 0

         fstdfield write ${ID}PROJ FILE 0 True
         fstdfield write ${ID}MTRX FILE -32 True
         fstdfield write ${ID}     FILE -8 True
      }
      default {
         fstdfield define ${ID}    -DATEO $dateo -DEET 0 -NPAS 0 -IP1 1200 -IP2 0    -IP3 0    -ETIKET $ETIKET -TYPVAR X

         fstdfield write ${ID}     FILE -8 True
      }
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
