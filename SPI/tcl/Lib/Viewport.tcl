#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie d'objects visuel interactifs
# Fichier  : Viewport.tcl
# Creation : Mai 2000 - J.P. Gauthier - CMC/CMOE
#
# Description: Ce package s'occupe de l'affichage et d ela manipulation
#              de viewport geographiques
#
# Fonctions:
#
#    Viewport::Activate       { Frame { VP "" } }
#    Viewport::Assign         { Frame VP Ids { Force 0 } }
#    Viewport::Assigned       { Frame VP { Type fstdfield } }
#    Viewport::AssignedTo     { Data { Page "" } { VP "" } { Box "" } }
#    Viewport::UnAssign       { Frame VP Ids }
#    Viewport::ConfigGet      { Frame VP }
#    Viewport::ConfigPut      { Frame VP }
#    Viewport::ConfigSet      { Frame }
#    Viewport::Follow         { Frame VP X Y }
#    Viewport::Follower       { Page Canvas VP Lat Lon X Y }
#    Viewport::FollowerAdd    { Follower }
#    Viewport::FollowerRemove { Follower }
#    Viewport::FollowerInfo   { Frame VP }
#    Viewport::FollowerSet    { Idx Id Value CoordX CoordY RefX RefY }
#    Viewport::CheckCoord     { Lon }
#    Viewport::CheckInside    { Lat0 Lon0 Lat1 Lon1 Lat Lon }
#    Viewport::Create         { Frame X0 Y0 Width Height Active Full }
#    Viewport::Destroy        { Frame { VP {} } }
#    Viewport::Do             { Frame }
#    Viewport::DrawArea       { Frame VP Coords Tags SingleTag Color Outline Stipple Smooth BD }
#    Viewport::DrawLine       { Frame VP Coords Tags Color BD }
#    Viewport::DrawRange      { Frame VP Lat0 Lon0 Lat1 Lon1 Tag Color { Text "" } }
#    Viewport::ForceGrid      { Frame } { Clean False }
#    Viewport::GoAlong        { Frame Speed Bearing Lat Lon { Damping True } }
#    Viewport::GoARound       { Frame Speed Lat Lon { Damping True } }
#    Viewport::GoTo           { Frame Lat Lon { Zoom 0 } { From {} } { To {} } { Up {} } { Function "" } }
#    Viewport::CloseUp        { Frame Lat0 Lon0 Lat1 Lon1 { Off 0.0 } } {
#    Viewport::LinkDo         { VP }
#    Viewport::LinkSet        { }
#    Viewport::Link           { }
#    Viewport::UnLink         { }
#    Viewport::ParamFrame     { Frame Apply }
#    Viewport::ParamSet       { }
#    Viewport::Reset          { Frame { Fast False } }
#    Viewport::Resolution     { Frame Res { Grab 0 } }
#    Viewport::Rotate         { Frame Lat Lon { Zoom 0 } { From {} } { To {} } { Up {} } }
#    Viewport::RotateDo       { Frame VP X Y }
#    Viewport::RotateDone     { Frame VP }
#    Viewport::RotateInit     { Frame VP X Y }
#    Viewport::Resize         { Frame VP X0 Y0 X1 Y1 Limit }
#    Viewport::ResizeDepend   { Frame VP DX DY }
#    Viewport::MoveDepend     { Frame VP DX DY }
#    Viewport::Setup          { Frame }
#    Viewport::UnSetup        { Frame }
#    Viewport::UpdateData     { Frame { VP { } } }
#    Viewport::Write          { Frame File }
#
#===============================================================================

package provide Viewport 5.1

catch { SPI::Splash "Loading Canvas Package Viewport 5.1" }

namespace eval Viewport {
   variable Data
   variable Map
   variable Resources
   variable Lbl

   set Data(VPNb)       0            ;#Compteur de viewport
   set Data(VP)         ""           ;#Viewport courant
   set Data(Seconds)    0            ;#Temps en seconde de la projection
   set Data(Followers)  { Viewport } ;#Liste des packages de suivit des coordonnees
   set Data(FollowerNb) 0            ;#Nombre de suivit courant
   set Data(Link)       {}           ;#Lien source
   set Data(Data)       {}           ;#Liste des donnees
   set Data(Picked)     {}           ;#Objet ous le curseur

   set Map(MinSize)     5            ;#Dimension minimale
   set Map(Draw)        1            ;#Affichage de la geographie
   set Map(Sun)         0            ;#Affichage du Soleil
   set Map(Res)         0            ;#Resolution geographique (0=Auto,2,4,8,16,32,64,128)
   set Map(Mask)        0            ;#Masque (Tout=0,Terre=1,Mer=2)
   set Map(Coast)       1            ;#Cotes
   set Map(Lake)        1            ;#Lacs
   set Map(River)       0            ;#Rivieres
   set Map(Polit)       1            ;#Bordures politiques
   set Map(Place)       0            ;#Endroits
   set Map(Admin)       0            ;#Bordures politiques internes
   set Map(City)        0            ;#Villes
   set Map(Road)        0            ;#Routes
   set Map(Rail)        0            ;#Chemin de fer
   set Map(Topo)        0            ;#Topographie
   set Map(Bath)        0            ;#Bathymetrie
   set Map(Text)        0            ;#Texture
   set Map(Coord)       1            ;#Largeur des segments latlons
   set Map(CoordLoc)    1            ;#Positionnement des latlon (-1=Ocean,1=Partout)
   set Map(CoordDef)    10.0         ;#Intervale entre les latlon en degres
   set Map(CoordNum)    2            ;#Numerotation des latlon
   set Map(Crowd)       20           ;#Buffer de controle de supperposition (Crow control)
   set Map(TimeOut)     60           ;#Timeout when waiting for thread data load
   set Map(Elev)        1.0          ;#Facteur d'expansion des elevations
   set Map(GeoRef)      ""           ;#Geo-reference courante (Mode Grid)
   set Map(Grabbed)     0            ;#Etat de la vue
   set Map(Delay)       500.0         ;#Temps de deplacement en millisecondes
   set Map(Speed)       0.0          ;#Vitesse de deplacement en metres/millisecondes
   set Map(Damping)     1.07         ;#Facteur de l'effet de ralentissement
   set Map(Perspective) False        ;#Affichage en perspective
   set Map(Type)        orthographic ;#Type de projection
   set Map(ClickFactor) -4           ;#Double click zoom factor
   set Map(ZAxis)       0
   set Map(ZAxisZ)      0
   set Map(ZAxisCoord)  { 0.0 0.0 }

   set Map(Types)       { "azimuthal equidistant" "azimuthal equal-area" "orthographic" "cylindric" "mercator" "grid" } ;#Type de projection geographique

   set MapDef(PS_North)        { { PROJCS["North_PolarStereographic",GEOGCS["WGS 84",DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563]],PRIMEM["Greenwich",0],UNIT["degree",0.01745329251994328]],UNIT["metre",1],PROJECTION["Polar_Stereographic"],PARAMETER["latitude_of_origin",60],PARAMETER["central_meridian",-100],PARAMETER["scale_factor",1],PARAMETER["false_easting",0],PARAMETER["false_northing",0],AXIS["Easting",UNKNOWN],AXIS["Northing",UNKNOWN]] } {WRAP} -13000000 13000000 -13000000 13000000 } 
   set MapDef(PS_South)        { { PROJCS["South_PolarStereographic",GEOGCS["WGS 84",DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563]],PRIMEM["Greenwich",0],UNIT["degree",0.01745329251994328]],UNIT["metre",1],PROJECTION["Polar_Stereographic"],PARAMETER["latitude_of_origin",-60],PARAMETER["central_meridian",-100],PARAMETER["scale_factor",1],PARAMETER["false_easting",0],PARAMETER["false_northing",0],AXIS["Easting",UNKNOWN],AXIS["Northing",UNKNOWN]] } {WRAP} -13000000 13000000 -13000000 13000000 } 
   set MapDef(Robinson)        { { PROJCS["World_Robinson",GEOGCS["GCS_WGS_1984",DATUM["WGS_1984",SPHEROID["WGS_1984",6378137,298.257223563]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Robinson"],PARAMETER["False_Easting",0],PARAMETER["False_Northing",0],PARAMETER["Central_Meridian",0],UNIT["Meter",1],AUTHORITY["EPSG","54030"]] } { WRAP PSEUDO } -18000000 18000000 -9000000 9000000 }
   set MapDef(Mollweide)       { { PROJCS["World_Mollweide",GEOGCS["GCS_WGS_1984",DATUM["WGS_1984",SPHEROID["WGS_1984",6378137,298.257223563]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Mollweide"],PARAMETER["False_Easting",0],PARAMETER["False_Northing",0],PARAMETER["Central_Meridian",0],UNIT["Meter",1],AUTHORITY["EPSG","54009"]] } { WRAP PSEUDO } -18000000 18000000 -9000000 9000000 }
   set MapDef(Eckert_I)        { { PROJCS["World_Eckert_I",GEOGCS["GCS_WGS_1984",DATUM["WGS_1984",SPHEROID["WGS_1984",6378137,298.257223563]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Eckert_I"],PARAMETER["False_Easting",0],PARAMETER["False_Northing",0],PARAMETER["Central_Meridian",0],UNIT["Meter",1],AUTHORITY["EPSG","54015"]] } { WRAP PSEUDO } -18000000 18000000 -9000000 9000000 }
   set MapDef(Eckert_III)      { { PROJCS["World_Eckert_III",GEOGCS["GCS_WGS_1984",DATUM["WGS_1984",SPHEROID["WGS_1984",6378137,298.257223563]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Eckert_III"],PARAMETER["False_Easting",0],PARAMETER["False_Northing",0],PARAMETER["Central_Meridian",0],UNIT["Meter",1],AUTHORITY["EPSG","54013"]] } { WRAP PSEUDO } -18000000 18000000 -9000000 9000000 }
   set MapDef(Eckert_V)        { { PROJCS["Sphere_Eckert_V",GEOGCS["GCS_Sphere",DATUM["Not_specified_based_on_Authalic_Sphere",SPHEROID["Sphere",6371000,0]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Eckert_V"],PARAMETER["False_Easting",0],PARAMETER["False_Northing",0],PARAMETER["Central_Meridian",0],UNIT["Meter",1],AUTHORITY["EPSG","53011"]] } { WRAP PSEUDO } -18000000 18000000 -9000000 9000000 }
   set MapDef(HomolonsineTest) { { PROJCS["unnamed",GEOGCS["WGS 84",DATUM["unknown",SPHEROID["WGS84",6378137,298.257223563]],PRIMEM["Greenwich",0],UNIT["degree",0.0174532925199433]],PROJECTION["Interrupted_Goode_Homolosine"]] } -22000000 22000000 -9000000 9000000 }
   set MapDef(Canada_Atlas_Lambert) { { PROJCS["NAD83 / Canada Atlas Lambert",GEOGCS["NAD83",DATUM["North_American_Datum_1983",SPHEROID["GRS 1980",6378137,298.257222101,AUTHORITY["EPSG","7019"]],TOWGS84[0,0,0,0,0,0,0],AUTHORITY["EPSG","6269"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4269"]],PROJECTION["Lambert_Conformal_Conic_2SP"],PARAMETER["standard_parallel_1",49],PARAMETER["standard_parallel_2",77],PARAMETER["latitude_of_origin",49],PARAMETER["central_meridian",-95],PARAMETER["false_easting",0],PARAMETER["false_northing",0],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH],AUTHORITY["EPSG","3978"]] } {WRAP} -4926566.525911789 5347437.183552671 -5396820.57509386 4623183.287829272 }

   set Map(Mode)        Zoom        ;#Mode de la souris (Zoom,Selection,Draw)
   set Map(X)           0           ;#Pixel en X
   set Map(Y)           0           ;#Pixel en Y
   set Map(Lat)         41.0        ;#Latitude centrale de l'affichage
   set Map(Lon)         -103.0      ;#Longitude centrale de l'affichage
   set Map(AltCursor)   0           ;#Altitude du curseur de la souris
   set Map(LatCursor)   0.0         ;#Latitude du curseur de la souris
   set Map(LonCursor)   0.0         ;#Longitude du curseur de la souris
   set Map(LatReset)    41.0        ;#Latitude initiale de remise a zero
   set Map(LonReset)    -103.0      ;#Longitude initiale de remise a zero
   set Map(LatRot)      0.0         ;#Coordonnees de rotation en latitude
   set Map(LonRot)      0.0         ;#Coordonnees de rotation en longitude
   set Map(Lat0)        0.0         ;#Coordonnees initiales de rotation/deplacement en latitude
   set Map(Lon0)        0.0         ;#Coordonnees initialse de rotation/deplacement en longitude
   set Map(LatD)        0.0         ;#Delta de deplacement en latitude
   set Map(LonD)        0.0         ;#Delta de deplacement en longitude

   set Map(GridI)       0.0         ;#Point de grille central en x de l'affichage
   set Map(GridJ)       0.0         ;#Point de grille central en y de l'affichage
   set Map(GridICursor) 0.0         ;#Coordonnees I du curseur de la souris
   set Map(GridJCursor) 0.0         ;#Coordonnees J du curseur de la souris
   set Map(GridIRot)    0.0         ;#Coordonnees de rotation en I
   set Map(GridJRot)    0.0         ;#Coordonnees de rotation en J

   #----- Descriptions des resources utilisees par le package

   set Resources(Bkg)       white        ;#Couleur du font (background)
   set Resources(BD)        1            ;#Largeur du pourtour (background)
   set Resources(FillCoast) ""           ;#Cotes (Polygones)
   set Resources(FillLake)  ""           ;#Lacs (Polygones)
   set Resources(Coast)     #000000      ;#Cotes
   set Resources(Lake)      #0000ff      ;#Lacs
   set Resources(River)     #0000ff      ;#Rivieres
   set Resources(Polit)     #ff0000      ;#Bordures politiques
   set Resources(Place)     #000000      ;#Endroits
   set Resources(Admin)     #ff0000      ;#Bordures politiques internes
   set Resources(City)      #ffa500      ;#Villes
   set Resources(Road)      #404040      ;#Routes
   set Resources(Rail)      #ff1493      ;#Chemin de fer
   set Resources(Coord)     #000000      ;#Latlon
   set Resources(DashCoord) ""           ;#LatLon dashing
   set Resources(Font)      ""           ;#Police

   #----- Definitions des labels relatives a la projection

   set Lbl(Background)     { "Fond"     "Background" }
   set Lbl(Coast)          { "Côtes"    "Coast" }
   set Lbl(Lake)           { "Lac"      "Lake" }
   set Lbl(River)          { "Rivière"  "River" }
   set Lbl(Polit)          { "Pays"     "State" }
   set Lbl(Place)          { "Endroit" "Place" }
   set Lbl(Admin)          { "Province" "Province" }
   set Lbl(City)           { "Ville"    "City" }
   set Lbl(Road)           { "Route"    "Road" }
   set Lbl(Rail)           { "Rail"     "Rail" }
   set Lbl(Coord)          { "LatLon"   "LatLon" }
   set Lbl(Sun)            { "Soleil"   "Sun" }
   set Lbl(Crowd)          { "Peuplement"   "Clutter" }

   set Lbl(None)           { "Auncun" "None" }
   set Lbl(Name)           { "Nom"    "Name" }
   set Lbl(Color)          { "Couleurs" "Colors" }
   set Lbl(Vector)         { "Vectoriel" "Vectorial" }
   set Lbl(Degrees)        { "Degrés" "Degrees" }
   set Lbl(HighRes)        { "Haute résolution" "High resolution" }
   set Lbl(Raster)         { "Matriciel" "Raster" }
   set Lbl(Mask)           { "Masque"   "Mask" }
   set Lbl(Topo)           { "Topographie" "Topography" }
   set Lbl(Bath)           { "Bathymétrie" "Bathymetry" }
   set Lbl(Land)           { "Terre/Mer" "Land/Sea" }
   set Lbl(Numbered)       { "Numérotation" "Numbered" }
   set Lbl(Geo)            { "Géographie" "Geographic" }
   set Lbl(Proj)           { "Projection" "Projection" }
   set Lbl(Factor)         { "Facteurs d'expansion" "Expension factors" }
   set Lbl(Sea)            { "Mer" "Sea" }
   set Lbl(Show)           { "Afficher" "Show" }
   set Lbl(Text)           { "Texture" "Texture" }
   set Lbl(MinSize)        { "Détails" "Details" }

   set Lbl(Apply)          { "Appliquer" "Apply" }
   set Lbl(Close)          { "Fermer" "Close" }
   set Lbl(Extent)         { "Étendue" "Extent" }
   set Lbl(Min)            { "Minimum" "Minimum" }
   set Lbl(Max)            { "Maximum" "Maximum" }
   set Lbl(Transform)      { "Transformation" "Transform" }
   set Lbl(Translation)    { "Translation" "Translation" }
   set Lbl(Rotation)       { "Rotation" "Rotation" }
   set Lbl(Scaling)        { "Échelle" "Scaling" }
   set Lbl(Ident)          { "Identification" "Identification" }
   set Lbl(CoordLoc)       { "Local" "Local" }
   set Lbl(CoordRef)       { "Référence" "Reference" }
   set Lbl(Value)          { "Valeur" "Value" }
   set Lbl(Viewport)       { "Vue" "Viewport" }

   #----- Definitions des bulles d'aides

   set Bubble(Lat)     { "Sélecteur/Indicateur de latitude/j" "Latitude/j Selector/Indicator" }
   set Bubble(Lon)     { "Sélecteur/Indicateur de longitude/i" "Longitude/i Selector/Indicator" }
   set Bubble(Reset)   { "Recentrage de la projection" "Recenter the projection" }
   set Bubble(Proj)    { "Sélection du type de projection" "Select thr projection type" }
   set Bubble(Topo)    { "Topographie" "Global Topography" }
   set Bubble(Bath)    { "Bathymétrie" "Global Bathymetry" }
   set Bubble(Texture) { "Texture apliquée a la topographie" "Textured applied to the topography" }
   set Bubble(Elev)    { "Facteur multiplicatif appliqué aux élévations" "Mutlipicative factor applied to the elevations" }
   set Bubble(Vector)  { "Paramètres d'affichage des données\nvectorielles (couleur et largeur)" "Vectorial data parameters (Color and line width)" }
   set Bubble(Sun)     { "Illumination selon la position du soleil" "Light based on sun position" }
}

#----------------------------------------------------------------------------
# Nom      : <Viewport::Activate>
# Creation : Janvier 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Mettre une viewport actif et desactiver le precedent
#
# Parametres :
#   <Frame>  : Indentificateur de Page
#   <VP>     : Indentificateur du Viewport
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Viewport::Activate { Frame { VP "" } } {
   global GDefs
   variable Data

   #----- Si le viewport actif est le courant, out
   if { $Frame=="" || [string range $VP 0 3]=="MINI" || $Data(VP)==$VP } {
      return
   }

   #----- Desactiver le viewport precedent
   Page::ActiveUnWrap $Frame $Data(VP)

   #----- Si pas de viewport, selectionner celui defaut du frame
   if { $VP!="" } {
      set Data(VP$Frame) $VP
   }

   #----- Definir les objects courants
   set Data(VP)                 $Data(VP$Frame)
   set Page::Data(VP)           $Data(VP$Frame)
   set Data(Data)               $Data(Data$Frame)

   #----- Activer le viewport courant
   Page::ActiveWrap $Frame $Data(VP)

   if { $Data(VP)!="" && [llength [$Frame.page.canvas find withtag $Data(VP)]] } {

      #----- Recuperer et instaurer ses parametres
      Viewport::ConfigGet $Frame $Data(VP)
      Viewport::ConfigPut $Frame $Data(VP)

      #----- Operande de la calculatrice
      set FieldCalc::Data(Operand) $Data(Operand$Data(VP))
      set FieldCalc::Data(Formula) $Data(Formula$Data(VP))
   }
}

#----------------------------------------------------------------------------
# Nom      : <Viewport::Assign>
# Creation : Fevrier 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Assigner une donnee a un viewport
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <VP>      : Identificateur du Viewport
#  <Ids>     : Identificateurs des donnees
#  <Force>   : Forcer le reaffichage
#  <Index>   : Index d'insertion pour l'ordre d'affichage
#
# Retour:
#  <ok>      : Ajout effectue
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Viewport::Assign { Frame VP Ids { Force 0 } { Index end } } {
   variable Data

   set idx 0

   if { [info exists Viewport::Data(Active$VP)] } {
      foreach id $Ids {
         if { [set idx [lsearch -exact $Data(Data$VP) $id]]==-1 } {
            set Data(Data$VP) [linsert $Data(Data$VP) $Index $id]
         }

         #----- Definir les tags aux emplacements
         if { [fstdfield is $id True] } {
            fstdfield stats $id -tag "$Frame $VP -1"
            FSTD::Register $id
         } elseif { [observation is $id] } {
            observation stats $id -tag "$Frame $VP -1"
            Obs::Register $id
         } elseif { [metobs is $id] } {
            metobs stats $id -tag "$Frame $VP -1"
            Obs::Register $id
         } elseif { [trajectory is $id] } {
            trajectory stats $id -tag "$Frame $VP -1"
            Trajectory::Register $id
         }
         set Force [expr $Force==-1?0:1]
      }
      if { $Force } {
         Viewport::UpdateData $Frame $VP
      }
   }
   if { $idx==-1 } {
      return 1
   } else {
      return 0
   }
}

#----------------------------------------------------------------------------
# Nom      : <Viewport::AssignedTo>
# Creation : Fevrier 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Determiner a qui est assigner la donnee
#
# Parametres :
#  <Data>    : Identificateur de la donnee
#  <Page>    : Retour de la page assignee
#  <VP>      : Retour du Viewport assignee
#  <Box>     : Retour de la boite de selection assignee
#
# Retour:
#  <Assigned : Assignation active
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Viewport::AssignedTo { Id { Page "" } { VP "" } { Box "" } } {
   variable Data

   set yes 1

   if { [fstdfield is $Id True] } {
      set tag [fstdfield stats $Id -tag]
   } elseif { [observation is $Id] } {
      set tag [observation stats $Id -tag]
   } elseif { [metobs is $Id] } {
      set tag [metobs stats $Id -tag]
   } elseif { [trajectory is $Id] } {
      set tag [trajectory stats $Id -tag]
   } else {
      set tag {}
      set yes 0
   }

   if { $Page!="" } {
      upvar $Page page
      set page [lindex $tag 0]
   }
   if { $VP!="" } {
      upvar $VP vp
      set vp [lindex $tag 1]
   }
   if { $Box!="" } {
      upvar $Box box
      set box [lindex $tag 2]
   }
   return $yes
}

#----------------------------------------------------------------------------
# Nom      : <Viewport::Assigned>
# Creation : Fevrier 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Recuperer la liste du type de donnees assignees a un viewport
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <VP>      : Identificateur du Viewport
#  <Types>   : Typesde donnees
#
# Retour:
#  <List>    : Liste des identificateurs des donnees ("" = aucun)
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Viewport::Assigned { Frame VP { Types { } } } {

   set list ""

   if { [info exists Viewport::Data(Active$VP)] } {
      set datas [lindex [$Frame.page.canvas itemconfigure $VP -data] 4]
      if { [llength $Types] } {
         foreach data [lindex [$Frame.page.canvas itemconfigure $VP -data] 4] {
            foreach type $Types {
               eval set is \[$type is \$data\]
               if { $is } {
                  lappend list $data
                  break
               }
            }
         }
      } else {
         set list $datas
      }
   }
   return $list
}

#----------------------------------------------------------------------------
# Nom      : <Viewport::UnAssign>
# Creation : Fevrier 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Desassigner une donnee a un viewport
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <VP>      : Identificateur du Viewport
#  <Ids>     : Identificateurs des donnees
#
# Retour:
#  <ok>      : Suppression effectue
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Viewport::UnAssign { Frame VP { Ids "" } { Force 0 } } {
   variable Data

   set ok 0

   if { [info exists Viewport::Data(Active$VP)] } {

      if { $Ids=="" } {
         foreach id $Data(Data$VP) {
            if { [fstdfield is $id True] } {
               FSTD::UnRegister $id
            } elseif { [observation is $id] } {
               Obs::UnRegister $id
            } elseif { [metobs is $id] } {
               Obs::UnRegister $id
            } elseif { [trajectory is $id] } {
               Trajectory::UnRegister $id
            }
         }
         set Data(Data$VP) ""
         set ok 1
      } else {
         foreach id $Ids {
            if { [set idx [lsearch -exact $Data(Data$VP) $id]]!=-1 } {
               set  Data(Data$VP) [lreplace $Data(Data$VP) $idx $idx]
               if { [fstdfield is $id True] } {
                  FSTD::UnRegister $id
               } elseif { [observation is $id] } {
                  Obs::UnRegister $id
               } elseif { [metobs is $id] } {
                  Obs::UnRegister $id
               } elseif { [trajectory is $id] } {
                  Trajectory::UnRegister $id
               }
               set ok 1
            }
         }
      }
   }

   if { ($ok && $Force!=-1) || $Force==1 } {
      Viewport::UpdateData $Frame $VP
   }

   return $ok
}

#----------------------------------------------------------------------------
# Nom      : <Viewport::ConfigGet>
# Creation : Octobre 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Recuperer les parametres de configurations d'un viewport/projection
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

proc Viewport::ConfigGet { Frame VP } {
   variable Map
   variable Data
   variable Resources

   if { ![projection is $Frame] } {
      return
   }

   set sec                 [projection configure $Frame -date]
   set Data(Seconds$Frame) [expr $sec-$Data(Seconds)]

   set Map(GeoRef$Frame)  [projection configure $Frame -georef]
   set Map(Type$Frame)    [projection configure $Frame -type]
   set Map(Draw$Frame)    [projection configure $Frame -draw]

   if { $Map(GeoRef$Frame)!="" } {
      set Map(Type)      $Map(Type$Frame):$Map(GeoRef$Frame)
   } else {
      set Map(Type)      $Map(Type$Frame)
   }

   set Map(Data)        [projection configure $Frame -data]
   set Map(Elev)        [projection configure $Frame -scale]
   set loc              [projection configure $Frame -location]
   set Map(Lat)         [lindex $loc 0]
   set Map(Lon)         [lindex $loc 1]

   set coo              [projection configure $Frame -mapcoord]
   set Map(Coord)       [expr abs([lindex $coo 0])]
   set Map(CoordLoc)    [expr [lindex $coo 0]<0?-1:1]
   set Map(CoordDef)    [lindex $coo 1]
   set Map(CoordNum)    [lindex $coo 2]

   set Map(MinSize)     [projection configure $Frame -minsize]
   set Map(Sun)         [projection configure $Frame -sun]
   set Map(Res)         [projection configure $Frame -mapres]
   set Map(Mask)        [projection configure $Frame -mask]
   set Map(Coast)       [projection configure $Frame -mapcoast]
   set Map(Lake)        [projection configure $Frame -maplake]
   set Map(River)       [projection configure $Frame -mapriver]
   set Map(Admin)       [projection configure $Frame -mapadmin]
   set Map(Road)        [projection configure $Frame -maproad]
   set Map(Rail)        [projection configure $Frame -maprail]
   set Map(Topo)        [projection configure $Frame -maptopo]
   set Map(Bath)        [projection configure $Frame -mapbath]
   set Map(Text)        [projection configure $Frame -maptext]

   set Map(Crowd)       [lindex [$Frame.page.canvas itemconf $VP -crowd] 4]
   set Resources(Font)      [lindex [$Frame.page.canvas itemconf $VP -font] 4]
   set Resources(Bkg)       [lindex [$Frame.page.canvas itemconf $VP -bg] 4]
   set Resources(BD)        [lindex [$Frame.page.canvas itemconf $VP -bd] 4]

   set Resources(FillCoast) [lindex [$Frame.page.canvas itemconf $VP -colorfillcoast] 4]
   set Resources(FillLake)  [lindex [$Frame.page.canvas itemconf $VP -colorfilllake] 4]
   set Resources(Coast)     [lindex [$Frame.page.canvas itemconf $VP -colorcoast] 4]
   set Resources(Lake)      [lindex [$Frame.page.canvas itemconf $VP -colorlake] 4]
   set Resources(River)     [lindex [$Frame.page.canvas itemconf $VP -colorriver] 4]
   set Resources(Polit)     [lindex [$Frame.page.canvas itemconf $VP -colorpolit] 4]
   set Resources(Place)     [lindex [$Frame.page.canvas itemconf $VP -colorplace] 4]
   set Resources(Admin)     [lindex [$Frame.page.canvas itemconf $VP -coloradmin] 4]
   set Resources(City)      [lindex [$Frame.page.canvas itemconf $VP -colorcity] 4]
   set Resources(Road)      [lindex [$Frame.page.canvas itemconf $VP -colorroad] 4]
   set Resources(Rail)      [lindex [$Frame.page.canvas itemconf $VP -colorrail] 4]
   set Resources(Coord)     [lindex [$Frame.page.canvas itemconf $VP -colorcoord] 4]
   set Resources(DashCoord) [lindex [$Frame.page.canvas itemconf $VP -dashcoord] 4]
}

#----------------------------------------------------------------------------
# Nom      : <Viewport::ConfigPut>
# Creation : Octobre 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Instaurer les paramtres dans l'interface de configuration
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

proc Viewport::ConfigPut { Frame VP } {
   global   GDefs
   variable Data
   variable Map
   variable Resources

   $Data(Frame).layer.vp.col configure -fg $Resources(Bkg)

   IcoMenu::Set $Data(Frame).left.ras.mask.sel $Map(Mask)

   set lst "zeroth.xbm width1.xbm width2.xbm width3.xbm width4.xbm width5.xbm width6.xbm"
   $Data(Frame).layer.vp.sz  configure -bitmap @$GDefs(Dir)/share/bitmap/[lindex $lst $Resources(BD)]
   $Data(Frame).layer.coast.sz  configure -bitmap @$GDefs(Dir)/share/bitmap/[lindex $lst $Map(Coast)]
   $Data(Frame).layer.lake.sz  configure -bitmap @$GDefs(Dir)/share/bitmap/[lindex $lst $Map(Lake)]
   $Data(Frame).layer.river.sz  configure -bitmap @$GDefs(Dir)/share/bitmap/[lindex $lst $Map(River)]
   $Data(Frame).layer.poli.sz  configure -bitmap @$GDefs(Dir)/share/bitmap/[lindex $lst $Map(Polit)]
   $Data(Frame).layer.admin.sz  configure -bitmap @$GDefs(Dir)/share/bitmap/[lindex $lst $Map(Admin)]
   $Data(Frame).layer.road.sz  configure -bitmap @$GDefs(Dir)/share/bitmap/[lindex $lst $Map(Road)]
   $Data(Frame).layer.rail.sz  configure -bitmap @$GDefs(Dir)/share/bitmap/[lindex $lst $Map(Rail)]
   $Data(Frame).layer.ll.sz  configure -bitmap @$GDefs(Dir)/share/bitmap/[lindex $lst $Map(Coord)]

   ColorBox::ConfigNoColor $Data(Frame).layer.coast.col $Resources(Coast)
   ColorBox::ConfigNoColor $Data(Frame).layer.lake.col $Resources(Lake)
   ColorBox::ConfigNoColor $Data(Frame).layer.coast.fcol $Resources(FillCoast)
   ColorBox::ConfigNoColor $Data(Frame).layer.lake.fcol $Resources(FillLake)
   ColorBox::ConfigNoColor $Data(Frame).layer.river.col $Resources(River)
   ColorBox::ConfigNoColor $Data(Frame).layer.poli.col $Resources(Polit)
   ColorBox::ConfigNoColor $Data(Frame).layer.place.col $Resources(Place)
   ColorBox::ConfigNoColor $Data(Frame).layer.admin.col $Resources(Admin)
   ColorBox::ConfigNoColor $Data(Frame).layer.city.col $Resources(City)
   ColorBox::ConfigNoColor $Data(Frame).layer.road.col $Resources(Road)
   ColorBox::ConfigNoColor $Data(Frame).layer.rail.col $Resources(Rail)
   ColorBox::ConfigNoColor $Data(Frame).layer.ll.col $Resources(Coord)

   IcoMenu::Set $Data(Frame).layer.ll.ds      $Resources(DashCoord)
}

#----------------------------------------------------------------------------
# Nom      : <Viewport::ConfigSet>
# Creation : Octobre 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Modifier les parametres de configurations d'un viewport/projection
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Viewport::ConfigSet { Frame } {
   variable Data
   variable Map
   variable MapDef
   variable Resources

   if { ![projection is $Frame] } {
      return
   }

   set Map(Type$Frame)   [lindex [split $Map(Type) :] 0]
   set Map(GeoRef$Frame) [lindex [split $Map(Type) :] 1]
   set Map(Draw$Frame)   $Map(Draw)

   #----- Check for grid type definition
   set clean False
   if { [set def [lindex [split $Map(Type) :] 1]]!="" } {
      if { [info exists ::Viewport::MapDef($def)] } {
          catch { georef create $def }
          georef define $def -type [lindex $MapDef($def) 1]
          if { $Map(GeoRef)!=$def } {
             set clean True
          }
          Viewport::ParamProjSet $def $def
      }
   }

   Viewport::ForceGrid $Frame $clean

   projection configure $Frame -type $Map(Type$Frame) -scale $Map(Elev) -mapres $Map(Res) -mask $Map(Mask) \
      -draw $Map(Draw$Frame) -mapcoast $Map(Coast) -maplake $Map(Lake) -mapriver $Map(River) -mappolit $Map(Polit) -mapadmin $Map(Admin) \
      -mapcity $Map(City) -maproad  $Map(Road) -mapplace $Map(Place) -maprail $Map(Rail) -maptopo $Map(Topo) \
      -mapbath $Map(Bath) -maptext $Map(Text) -mapcoord [expr $Map(Coord)*$Map(CoordLoc)] $Map(CoordDef) $Map(CoordNum) \
      -sun $Map(Sun) -date [expr $Data(Seconds$Frame)+$Data(Seconds)] -minsize $Map(MinSize) -perspective $Map(Perspective) \
      -axis $Map(ZAxis) -axiscoord [lindex $Map(ZAxisCoord) 0] [lindex $Map(ZAxisCoord) 1] [lindex $Map(ZAxisZ) 0]

   set ll       [projection configure $Frame -location]
   set Map(Lat) [lindex $ll 0]
   set Map(Lon) [lindex $ll 1]

   foreach vp [Page::Registered $Frame Viewport] {
      $Frame.page.canvas itemconfigure $vp -crowd $Map(Crowd) -font $Resources(Font) -bg $Resources(Bkg) -bd $Resources(BD)\
         -colorcoast $Resources(Coast) -colorlake $Resources(Lake) -colorfillcoast $Resources(FillCoast) -colorfilllake $Resources(FillLake) \
         -colorriver $Resources(River) -colorpolit $Resources(Polit) -coloradmin $Resources(Admin) -colorcity $Resources(City) \
         -colorroad $Resources(Road) -colorrail $Resources(Rail) -colorplace $Resources(Place) -colorcoord $Resources(Coord) -dashcoord $Resources(DashCoord)
   }

   foreach mini $Miniport::Data(Mini$Frame) {
      Miniport::Projection $Frame $mini

      projection configure $mini -draw $Map(Draw$Frame) -mapcoast $Map(Coast) -maplake $Map(Lake) -mapriver $Map(River) -mappolit $Map(Polit) \
         -mapadmin $Map(Admin) -maproad $Map(Road) -maprail $Map(Rail) \
         -maptopo $Map(Topo) -mapbath $Map(Bath) -maptext $Map(Text) -mapcoord [expr $Map(Coord)*$Map(CoordLoc)] $Map(CoordDef) $Map(CoordNum) \
         -sun $Map(Sun) -minsize $Map(MinSize) -perspective $Map(Perspective) -date [expr $Data(Seconds$Frame)+$Data(Seconds)]

     $Frame.page.canvas itemconfigure $mini -font $Resources(Font) -bg $Resources(Bkg) \
         -colorcoast $Resources(Coast) -colorlake $Resources(Lake)  -colorfillcoast $Resources(FillCoast) -colorfilllake $Resources(FillLake) \
         -colorriver $Resources(River) -colorpolit $Resources(Polit) -coloradmin $Resources(Admin) -colorcity $Resources(City) \
         -colorroad $Resources(Road) -colorrail $Resources(Rail) -colorplace $Resources(Place) -colorcoord $Resources(Coord) -dashcoord $Resources(DashCoord)
   }
}

#----------------------------------------------------------------------------
# Nom      : <Viewport::Follow>
# Creation : Avril 1998 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche les coordonneees lat-lon de la position du curseur.
#
# Parametres :
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

proc Viewport::Follow { Frame VP X Y } {
   global   GDefs
   variable Map
   variable Data

   set Page::Data(Value)    ""
   set Page::Data(Coord)    ""
   set Page::Data(Altitude) ""

   #----- Obtenir les coordonnees du curseur

   set latlon [$VP -unproject $X $Y]

   set Map(X) $X
   set Map(Y) $Y
   set Map(LatCursor)  [lindex $latlon 0]
   set Map(LonCursor)  [lindex $latlon 1]
   set Map(AltCursor)  [lindex $latlon 2]

   if { $Map(Type$Frame)=="grid" } {
      set ij [$VP -ungrid $X $Y]

      set Map(GridICursor)   [lindex $ij 0]
      set Map(GridJCursor)   [lindex $ij 1]
   }

   if { $Page::Data(CoordUnit)=="REF" || ![projection configure $Frame -geographic] } {
      catch { set Page::Data(Coord) [format "%.$Page::Data(CoordPrec)f %.$Page::Data(CoordPrec)f" $Map(GridICursor) $Map(GridJCursor)] }
   } else {
      catch { set Page::Data(Coord) [Convert::FormatCoord $Map(LatCursor) $Map(LonCursor) $Page::Data(CoordUnit) $Page::Data(CoordPrec)] }
   }
   set Page::Data(Altitude) $Map(AltCursor)

   $Frame.page.canvas delete COORDLINK SQUARECURSOR

   #----- Activation du curseur carre

   if { $Page::Param(Square) } {
      $Frame.page.canvas create line [expr $X-$Page::Param(Square)] [expr $Y-$Page::Param(Square)] [expr $X-$Page::Param(Square)] [expr $Y+$Page::Param(Square)] \
          [expr $X+$Page::Param(Square)] [expr $Y+$Page::Param(Square)] [expr $X+$Page::Param(Square)] [expr $Y-$Page::Param(Square)] [expr $X-$Page::Param(Square)] [expr $Y-$Page::Param(Square)] \
          -width 2 -fill red -tags "PAGE$VP SQUARECURSOR"
   }

   #----- Activation du pointeur commun

   if { $Page::Data(CoordLink) } {
      foreach frame $Page::Data(Frames) {
         $frame.page.canvas delete COORDLINK
         foreach vp [Page::Registered $frame Viewport] {
            if { [set xy [$vp -project $Map(LatCursor) $Map(LonCursor) 0.0]]!= "" && [lindex $xy 2]>0 } {
               set x [lindex $xy 0]
               set y [lindex $xy 1]
               $frame.page.canvas create line $Data(X$vp) $y [expr $x-5] $y -width 2 -fill red -tags "PAGE$vp COORDLINK"
               $frame.page.canvas create line [expr $Data(X$vp)+$Data(Width$vp)] $y [expr $x+5] $y -width 2 -fill red -tags "PAGE$vp COORDLINK"
               $frame.page.canvas create line $x $Data(Y$vp) $x [expr $y-5] -width 2 -fill red -tags "PAGE$vp COORDLINK"
               $frame.page.canvas create line $x [expr $Data(Y$vp)+$Data(Height$vp)] $x [expr $y+5] -width 2 -fill red -tags "PAGE$vp COORDLINK"
            }
         }
         foreach mini $Miniport::Data(Mini$frame) {
            if { [set xy [$mini -project $Map(LatCursor) $Map(LonCursor) 0.0]]!= "" && [lindex $xy 2]>0 } {
               set x [lindex $xy 0]
               set y [lindex $xy 1]
               $frame.page.canvas create line $Data(X$mini) $y [expr $x-5] $y -width 2 -fill red -tags "PAGE$vp COORDLINK"
               $frame.page.canvas create line [expr $Data(X$mini)+$Data(Width$mini)] $y [expr $x+5] $y -width 2 -fill red -tags "PAGE$vp COORDLINK"
               $frame.page.canvas create line $x $Data(Y$mini) $x [expr $y-5] -width 2 -fill red -tags "PAGE$vp COORDLINK"
               $frame.page.canvas create line $x [expr $Data(Y$mini)+$Data(Height$mini)] $x [expr $y+5] -width 2 -fill red -tags "PAGE$vp COORDLINK"
            }
         }
      }
   }

   #----- Activer les follower externes

   if { [winfo exists .position] } {
      Viewport::FollowerInfo $Frame $VP
   } else {
      foreach follower $Data(Followers) {
         eval ${follower}::Follower $Frame $Frame.page.canvas $VP $Map(LatCursor) $Map(LonCursor) $X $Y
      }
   }

   #----- Verifier la validitee de la coordonnee

   if { $Map(LatCursor)==-999.0 || $Map(LonCursor)==-999.0 } {
      return 0
   } else {
      if { [string range $VP 0 3]!="MINI" } {
         #----- Auto refresh du MiniPort
         foreach mini $Miniport::Data(Mini$Frame) {
            Miniport::Lens $Frame $mini
            Miniport::UpdateData $Frame $mini $VP
            Miniport::Coverage $Frame $mini $VP
         }
      }
      return 1
   }
}

#----------------------------------------------------------------------------
# Nom      : <Viewport::Follower>
# Creation : Novembre 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Plugin au bindings de suivit de coordonnees du Viewport
#
# Parametres :
#   <Frame>  : Page courante
#   <Canvas> : Canvas courant
#   <VP>     : Viewport courant
#   <Lat>    : Lattitude
#   <Lon>    : Longitude
#   <X>      : Pixel en X
#   <Y>      : Pixel en Y
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Viewport::Follower { Page Canvas VP Lat Lon X Y } {
   global GDefs
   variable Data
   variable Map

   set info  ""
   set graph ""
   set list  ""
   set sec   ""

   if { $Map(Speed)==0.0 } {
      set Data(Picked) [$VP -pick $X $Y { trajectory observation metobs } False]

      if { [llength $Data(Picked)] && [set tag [lindex $Data(Picked) 2]]!="" && [set obj [lindex $Data(Picked) 1]]!="" } {
         set id ""
         switch [lindex $Data(Picked) 0] {
            "trajectory"  { set info   [Trajectory::ParcelInfo $obj $tag]
                            set parcel [trajectory define $obj -PARCEL $tag]
                            set coord  [list [lindex $parcel 1] [lindex $parcel 2] [lindex $parcel 5]]
                            set id     [trajectory define $obj -ID]
                            set sec    [lindex $parcel 0]
                            set vals   [DateStuff::StringDateFromSeconds [lindex $parcel 0] $GDefs(Lang)]
                            if { [llength $vals] } {
                               append Page::Data(Value) "$id:$vals "
                            }
                          }
            "observation" {
                            set info  [observation define $obj -ID $tag]
                            set coord [observation define $obj -COORD $tag]
                            set id    $obj
                            set vals  [observation define $obj -DATA $tag]
                            if { [llength $vals] } {
                               append Page::Data(Value) "$id:$vals "
                            }
                          }
            "metobs"      { set info  [metobs define $obj -ID $tag]
                            set coord [metobs define $obj -COORD $tag]
                            set item  [lindex [metmodel define [metobs define $obj -MODEL] -items] [lindex $Data(Picked) 3]]
                            set id    [lindex [metobs table -desc [lindex $item 2]] 0]
                            set vals  [lindex [metobs define $obj -ELEMENT $tag [lindex $item 2] [metobs define $obj -VALID]] [lindex $Data(Picked) 4]]

                            if { [llength $vals] } {
                               append Page::Data(Value) "$id:[format "%g" [lindex $vals 0]] "
                               set info  $info\n$id:[format "%g" [lindex $vals 0]]
                            }
                            set graph [Obs::InfoGraph $obj $tag [lindex $item 2]]
                          }
         }
         if { $id!="" } {
            lappend list [list $id {} {} $vals]
            catch { set Page::Data(Coord) [Convert::FormatCoord [lindex $coord 0] [lindex $coord 1] $Page::Data(CoordUnit) $Page::Data(CoordPrec)] }
            catch { set Page::Data(Altitude) [format "%.2f" [lindex $coord 2]] }
         }
      }
   }
   catch { graphaxis configure TRAJGRAPHAXISX -highlight $sec }

   Page::CursorInfo $Page $X $Y $info $graph
   return $list
}

#----------------------------------------------------------------------------
# Nom      : <Viewport::FollowerAdd>
# Creation : Novembre 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Ajouter un package a la liste des follower
#
# Parametres  :
#  <Follower> : Package
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Viewport::FollowerAdd { Follower } {
   variable Data

   lappend Data(Followers) $Follower
   set Data(Followers) [lsort -unique -dictionary $Data(Followers)]
}

#----------------------------------------------------------------------------
# Nom      : <Viewport::FollowerRemove>
# Creation : Novembre 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Supprimer un package de la liste des follower
#
# Parametres  :
#  <Follower> : Package
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Viewport::FollowerRemove { Follower } {
   variable Data

   if { [set idx [lsearch -exact $Data(Followers) $Follower]]!=-1 } {
      set Data(Followers) [lreplace $Data(Followers) $idx $idx]
   }
}

#----------------------------------------------------------------------------
# Nom      : <Viewport::FollowerInfo>
# Creation : Janvier 2005 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche une fenetres d'information detaillee sur la position du curseur.
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

proc Viewport::FollowerInfo { Frame VP } {
   global GDefs
   variable Data
   variable Map
   variable Lbl

   if { ![winfo exists .position] } {
      toplevel .position

      wm transient .position .

      frame .position.head
         label .position.head.ids  -text [lindex $Lbl(Ident) $GDefs(Lang)]        -width 15 -relief raised -bd 1 -justify left -anchor w
         label .position.head.coox -text "[lindex $Lbl(CoordLoc) $GDefs(Lang)] X" -width 15 -relief raised -bd 1 -justify left -anchor w
         label .position.head.cooy -text "[lindex $Lbl(CoordLoc) $GDefs(Lang)] Y" -width 15 -relief raised -bd 1 -justify left -anchor w
         label .position.head.prjx -text "[lindex $Lbl(CoordRef) $GDefs(Lang)] X" -width 15 -relief raised -bd 1 -justify left -anchor w
         label .position.head.prjy -text "[lindex $Lbl(CoordRef) $GDefs(Lang)] Y" -width 15 -relief raised -bd 1 -justify left -anchor w
         label .position.head.val  -text [lindex $Lbl(Value) $GDefs(Lang)]        -width 15 -relief raised -bd 1 -justify left -anchor w
         pack .position.head.ids .position.head.val .position.head.coox .position.head.cooy \
            .position.head.prjx .position.head.prjy -side left -ipadx 2 -ipady 2 -fill x -expand true
      pack .position.head -side top -fill x

      frame .position.coord -relief raised -bd 1
         label .position.coord.name -text "Projection"
         label .position.coord.ll -textvariable Page::Data(Coord) -relief sunken -width 36 -bd 1 -bg $GDefs(ColorLight)
         label .position.coord.height -relief sunken -width 5 -bd 1 -bg $GDefs(ColorLight)
         pack .position.coord.name .position.coord.ll -side left
         pack .position.coord.height -side left -fill x -expand true
      pack .position.coord -side bottom -fill x
   }

   set i -1
   foreach follower $Data(Followers) {
      if { $Page::Data(CoordLink) } {
         set fs {}
         foreach frame $Page::Data(Frames) {
            foreach vp [Page::Registered $frame Viewport] {
               foreach f [${follower}::Follower $frame $frame.page.canvas $vp $Map(LatCursor) $Map(LonCursor) $Map(X) $Map(Y)] {
                   Viewport::FollowerSet [incr i] [lindex $f 0] [lindex $f 3] [lindex [lindex $f 1] 0] [lindex [lindex $f 1] 1] [lindex [lindex $f 2] 0] [lindex [lindex $f 2] 1]
               }
            }
         }
      } else {
         foreach f [${follower}::Follower $Frame $Frame.page.canvas $VP $Map(LatCursor) $Map(LonCursor) $Map(X) $Map(Y)] {
            Viewport::FollowerSet [incr i] [lindex $f 0] [lindex $f 3] [lindex [lindex $f 1] 0] [lindex [lindex $f 1] 1] [lindex [lindex $f 2] 0] [lindex [lindex $f 2] 1]
         }
      }
   }

   for { set n $Data(FollowerNb) } { $n>$i } { incr n -1 } {
      destroy .position.info$n
   }

   set Data(FollowerNb) $i
}

#----------------------------------------------------------------------------
# Nom      : <Viewport::FollowerSet>
# Creation : Janvier 2005 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche une fenetres d'information detaillee sur la position du curseur.
#
# Parametres :
#  <Idx>     : Index du suivit
#  <Id>      : Identificateur du suivit
#  <Value>   : Valeur
#  <CoordX>  : Coordonnee en X
#  <CoordY>  : Coordonnee en Y
#  <RefX>    : Coordonnee en X dans le referentiel
#  <RefY>    : Coordonnee en Y dans le referentiel
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Viewport::FollowerSet { Idx Id Value CoordX CoordY RefX RefY } {
   global GDefs

   if { ![winfo exists .position.info$Idx] } {
      frame .position.info$Idx
         label .position.info$Idx.ids  -width 15 -relief sunken -bd 1 -bg $GDefs(ColorLight) -anchor w
         label .position.info$Idx.val  -width 15 -relief sunken -bd 1 -bg $GDefs(ColorLight) -anchor w
         label .position.info$Idx.coox -width 15 -relief sunken -bd 1 -bg $GDefs(ColorLight) -anchor w
         label .position.info$Idx.cooy -width 15 -relief sunken -bd 1 -bg $GDefs(ColorLight) -anchor w
         label .position.info$Idx.prjx -width 15 -relief sunken -bd 1 -bg $GDefs(ColorLight) -anchor w
         label .position.info$Idx.prjy -width 15 -relief sunken -bd 1 -bg $GDefs(ColorLight) -anchor w
         pack .position.info$Idx.ids .position.info$Idx.val .position.info$Idx.coox .position.info$Idx.cooy \
            .position.info$Idx.prjx .position.info$Idx.prjy -side left -ipadx 2 -ipady 2 -fill x -expand true
      pack .position.info$Idx -side bottom -after .position.coord -fill x
   }

   .position.info$Idx.ids  configure -text $Id
   .position.info$Idx.val  configure -text $Value
   .position.info$Idx.coox configure -text $CoordX
   .position.info$Idx.cooy configure -text $CoordY
   .position.info$Idx.prjx configure -text $RefX
   .position.info$Idx.prjy configure -text $RefY
}

#----------------------------------------------------------------------------
# Nom      : <Viewport::CheckCoord>
# Creation : Avril 1998 - J.P. Gauthier - CMC/CMOE
#
# But      : Verifie les limites des longitudes (180 et -180) et renvoie la bonne valeur.
#
# Parametres :
#  <Lon>     : Longitude a verifier
#
# Retour:
#  <Lon>     : Longitude ajustee
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Viewport::CheckCoord { Lon } {

   return [expr $Lon<-180.0?$Lon+360.0:($Lon>180.0?$Lon-360.0:$Lon)]
}

#----------------------------------------------------------------------------
# Nom      : <Viewport::CheckInside>
# Creation : Octobre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Determiner l'inclusion d'une coordonnees a l'interieur d'une
#            region definie comme telle:
#
#                 #--------------#(Lat1,Lon1)
#                 |              |
#                 |              |
#                 |              |
#                 |              |
#      (Lat0,Lon0)#--------------#
#
#       avec Lon0 toujours plus petit que Lon1
#
#      -90<Lat<90 -180<Lon<180
#
# Parametres :
#   <Lat0>   : Latitude minimale
#   <Lon0>   : Longitude minimale
#   <Lat1>   : Latitude maximale
#   <Lon1>   : Longitude maximale
#   <Lat>    : Latitude
#   <Lon>    : Longitude
#
# Retour     :
#    <in>    : Boolean
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Viewport::CheckInside { Lat0 Lon0 Lat1 Lon1 Lat Lon } {

   if { $Lat0==$Lat1 || $Lon0==$Lon1 } {
      return 1
   }

   if { [expr $Lon0*$Lon1]<0 } {
      set delta [expr $Lon1-$Lon0]
   } else {
      set delta 0
   }

   if { $Lat>=$Lat0 && $Lat<=$Lat1 } {
      if { $delta<=180 } {
         if { $Lon>=$Lon0 && $Lon<=$Lon1 } {
            return 1
         }
      } else {
         if { ($Lon<=$Lon0 && $Lon>-180) || ($Lon>=$Lon1 && $Lon<180) } {
            return 1
         }
      }
   }
   return 0
}

#----------------------------------------------------------------------------
# Nom      : <Viewport::Create>
# Creation : Decembre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer un viewport ainsi que tout ses widgets
#
# Parametres :
#   <Frame>  : Indentificateur de Page
#   <X0>     : Coordonee X du coin superieur gauche
#   <Y0>     : Coordonee Y du coin superieur gauche
#   <Width>  : Largeur du Viewport
#   <Height> : Hauteur du Viewport
#   <Active> : Fonction active (Deplacement,Agrandisement)
#   <Full>   : Mode Full screen
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Viewport::Create { Frame X0 Y0 Width Height Active Full { VP "" } } {
   global   GDefs
   variable Data
   variable Map
   variable Resources

   $Frame.page.canvas configure -cursor watch
   update idletasks

   if { $VP=="" } {
      set vp VP[incr Data(VPNb)]
   } else {
      set vp $VP
   }

   if { [info exists Viewport::Data(Active$vp)] } {
      return
   }

   set Page::Data(Frame)   $Frame
   set Page::Data(Canvas)  $Frame.page.canvas

   if { $Full } {
      set X0       0
      set Y0       0
      set Width   [winfo width  $Page::Data(Canvas)]
      set Height  [winfo height $Page::Data(Canvas)]
   }
   
   #----- Initialiser les variables du viewport

   set Data(Full$vp)      $Full      ;#Mode FullCanvas
   set Data(Active$vp)    $Active    ;#Mode Active (Manipulation in place)
   set Data(X$vp)         $X0        ;#Offset en x
   set Data(Y$vp)         $Y0        ;#Offset en y
   set Data(Width$vp)     $Width     ;#Largeur de la projection
   set Data(Height$vp)    $Height    ;#Hauteur de la projection
   set Data(Data$vp)      ""         ;#Donnees associees
   set Data(Frame$vp)     $Frame     ;#Frame
   set Data(Operand$vp)   ""         ;#Operande de calcul
   set Data(Formula$vp)   ""         ;#Nom de la formule de calcul
   set Data(Link$vp)      ""         ;#Lien a un autre Viewport
   set Data(Linked$vp)    ""         ;#Viewport liees

   #----- Definir comme lien si non-existant

   if { ![llength $Data(Link)] } {
      set Data(Link) [list $Frame $vp]
   }

   set tag PAGE$vp
   set x1 [expr $Width+$X0]
   set y1 [expr $Height+$Y0]

   Viewport::ConfigSet $Frame

   $Frame.page.canvas create viewport -x $X0 -y $Y0 -width $Width -height $Height -bd $Resources(BD) -timeout $Map(TimeOut) -crowd $Map(Crowd) -fg black -font $Resources(Font) -bg $Resources(Bkg) \
      -colorcoast $Resources(Coast) -colorlake $Resources(Lake)  -colorfillcoast $Resources(FillCoast) -colorfilllake $Resources(FillLake) \
      -colorriver $Resources(River) -colorpolit $Resources(Polit) -coloradmin $Resources(Admin) -colorcity $Resources(City) \
      -colorroad $Resources(Road) -colorrail $Resources(Rail) -colorplace $Resources(Place) -colorcoord $Resources(Coord) -dashcoord $Resources(DashCoord) \
      -tags "$vp $tag" -projection $Frame -camera $Frame -command $vp -maskitem [$Frame.page.canvas find withtag VPINTRUDE] -maskwidth $Page::Param(Intrusion)

   if { $Active } {
      scale $Frame.sc$vp -bg white -relief raised -bd 1 -width 8 -sliderlength 15  -orient horizontal -showvalue False -resolution 0.01 \
         -from [expr log10(0.5)/log10(2)] -to [expr log10(131072)/log10(2)] \
         -variable Page::Data(L$Frame) -command "ProjCam::Zoom $Frame $Frame \[expr pow(2,\$Page::Data(L$Frame))\]; catch"
      bind $Frame.sc$vp <ButtonPress-1> "Viewport::Resolution $Frame [expr $OpenGL::Param(Res)==1?2:$OpenGL::Param(Res)]; ProjCam::Zoom $Frame $Frame \[expr pow(2,\$Page::Data(L$Frame))\] True"
      bind $Frame.sc$vp <ButtonRelease-1> "Viewport::Resolution $Frame 1"

      $Frame.page.canvas create window [expr $x1-150-35] $y1 -window $Frame.sc$vp -anchor sw -tags "SCPAGE$vp NOPRINT" -width 151

      Page::ActiveWrapper Viewport $Frame $vp $X0 $Y0 $x1 $y1
   }

   Viewport::Activate $Frame $vp
   $Frame.page.canvas bind $tag <Button-3> "Viewport::Activate $Frame $vp ; tk_popup .mapmenu %X %Y 0"

   Page::ActiveFull Viewport $Frame $vp $Full
   Page::Register $Frame Viewport $vp

   update idletasks
   $Frame.page.canvas configure -cursor left_ptr

   return $vp
}

#----------------------------------------------------------------------------
# Nom      : <Viewport::Destroy>
# Creation : Janvier 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Supprimer un viewport ainsi que tout ses widgets
#
# Parametres :
#   <Frame>  : Indentificateur de Page
#   <VP>     : Indentificateur du Viewport
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Viewport::Destroy { Frame { VP {} } } {
   variable Data

   $Frame.page.canvas configure -cursor watch
   update idletasks

   if { ![llength $VP] } {
      set VP [Page::Registered $Frame Viewport]
   }

   foreach vp $VP {

      if { [Page::Registered $Frame Viewport $vp]!=-1 } {

         #----- Cleanup le data associe
         Viewport::UnAssign $Frame $vp

         #----- Supprimer les widgets de controle
         Page::ActiveUnWrapper Viewport $Frame $vp

         #----- Supprimer le viewport et ses items
         set tag PAGE$vp
         $Frame.page.canvas delete $vp $tag

         #----- Supprimer les variables du viewport
         unset Data(Full$vp)
         unset Data(Active$vp)
         unset Data(X$vp)
         unset Data(Y$vp)
         unset Data(Width$vp)
         unset Data(Height$vp)
         unset Data(Data$vp)
         unset Data(Frame$vp)
         unset Data(Operand$vp)
         unset Data(Formula$vp)

         #----- Clear le lien source si c'est le vp courant
         if { [lindex $Data(Link) 1]=="$vp" } {
            set Data(Link) {}
         }

         set Data(VP$Frame) [Page::UnRegister $Frame Viewport $vp]

         #----- Supprimer les items associees
         ColorBar::DestroyAll $Frame $vp
         DataBar::DestroyAll $Frame $vp
      }
   }

   if { $Viewport::Data(VP$Frame)!="" && [info exist Viewport::Data(VP$Frame)] } {
      Viewport::Activate $Frame $Data(VP$Frame)
   }

   update idletasks
   $Frame.page.canvas configure -cursor left_ptr
}

#----------------------------------------------------------------------------
# Nom      : <Viewport::Do>
# Creation : Novembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Alias pour ajuster tout les parametres pour les versions xbatch.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Viewport::Do { Frame } {
   variable Map
   variable Data
   variable Resources

   set Map(Type$Frame)   [lindex [split $Map(Type) :] 0]
   set Map(GeoRef$Frame) $Map(GeoRef)

   projection configure $Frame -type $Map(Type$Frame) -georef $Map(GeoRef) -scale $Map(Elev) -mask $Map(Mask) \
      -mapres $Map(Res) -draw $Map(Draw$Frame)  -mapcoast $Map(Coast) -maplake $Map(Lake) -mapriver $Map(River) -mappolit $Map(Polit) \
      -mapadmin $Map(Admin) -mapcity $Map(City) -maproad  $Map(Road) -maprail $Map(Rail) -maptopo $Map(Topo) \
      -mapplace $Map(Place) -maptext $Map(Text) -mapcoord [expr $Map(Coord)*$Map(CoordLoc)] $Map(CoordDef) $Map(CoordNum) -sun $Map(Sun) \
      -data $Data(Data$Frame) -minsize $Map(MinSize) -perspective $Map(Perspective)

   projcam configure $Frame -lens $ProjCam::Param(Lens) -from $ProjCam::Param(From) -to $ProjCam::Param(To)

   foreach vp [Page::Registered $Frame Viewport] {
      $Frame.page.canvas itemconfigure $vp -crowd $Map(Crowd) -bg $Resources(Bkg) -bd $Resources(BD) \
         -colorcoast $Resources(Coast) -colorlake $Resources(Lake)  -colorfillcoast $Resources(FillCoast) -colorfilllake $Resources(FillLake) \
         -colorriver $Resources(River) -colorpolit $Resources(Polit) -coloradmin $Resources(Admin) -colorcity $Resources(City) -colorplace $Resources(Place) \
         -colorroad $Resources(Road) -colorrail $Resources(Rail) -colorcoord $Resources(Coord) -dashcoord $Resources(DashCoord)
   }

   Page::Update  $Frame
}

#----------------------------------------------------------------------------
# Nom      : <Viewport::DrawArea>
# Creation : Juillet 1998 - J.P. Gauthier - CMC/CMOE
#
# But      : Dessine une region sur la projection.
#
# Parametres   :
#  <Frame>     : Identificateur de Page
#  <VP>        : Identificateur du Viewport
#  <Coords>    : Liste des coordonnnes
#  <Tags>      : Identificateurs de la ligne
#  <SingleTag> : Identificateur unique de la ligne
#  <Color>     : Couleur
#  <Outline>   : Couleur du pourtour
#  <Stipple>   : Pattern de remplissage de la region
#  <Smooth>    : Segments droit ou courbe
#  <BD>        : Epaisseur de la ligne en pixel.
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Viewport::DrawArea { Frame VP Coords Tags SingleTag Color Outline Stipple Smooth BD } {
   variable Data
   variable Map

   set a  [$VP -projectline TRUE $Coords]
   set a0 [lindex $a 0]
   set a1 [lindex $a 1]

   $Frame.page.canvas delete $SingleTag

   #----- Affiche la partie cachee ou gauche

   if { $Map(Type$Frame)!="orthographic" && [llength $a1]>4 } {
      eval $Frame.page.canvas create polygon $a1 -stipple \$Stipple -tags \$Tags \
         -fill \$Color -outline \$Outline -smooth $Smooth -width \$BD
   } elseif { [llength $a1]==4 } {
      eval $Frame.page.canvas create line $a1 -tags \$Tags -fill $Outline -width \$BD
   }

   #----- Affiche la partie avant ou droite

   if { [llength $a0]>4 } {
      eval $Frame.page.canvas create polygon $a0 -stipple \$Stipple -tags \$Tags \
        -fill \$Color -outline \$Outline -smooth $Smooth -width $BD
   } elseif { [llength $a0]==4 } {
      eval $Frame.page.canvas create line $a0 -tags \$Tags -fill \$Outline -width \$BD
   }
}

#----------------------------------------------------------------------------
# Nom      : <Viewport::DrawLine>
# Creation : Juillet 1998 - J.P. Gauthier - CMC/CMOE
#
# But      : Dessine une ligne sur la projection.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <VP>      : Identificateur du Viewport
#  <Coords>  : Coordonnne (lat lon elev)
#  <Tags>    : Identificateur de la ligne
#  <Color>   : Couleur.
#  <BD>      : Epaisseur de la ligne en pixel.
#  <Mode>    : Mode de projection (ONE,COORD,TRUE)
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Viewport::DrawLine { Frame VP Coords Tags Color BD { Mode COORD }} {
   variable Data
   variable Map

   #----- Verifier le type de projection pour les parametres et les couleurs

   set l  [$VP -projectline $Mode $Coords]
   set l0 [lindex $l 0]
   set l1 [lindex $l 1]

   if { [llength $l0] > 2 } {
      eval $Frame.page.canvas create line $l0 -fill $Color -width $BD -tags \$Tags
   }

   if { [llength $l1] > 2 && $Map(Type$Frame)!="orthographic" } {
      eval $Frame.page.canvas create line  $l1 -fill $Color -width $BD -tags \$Tags
   }
}

#----------------------------------------------------------------------------
# Nom      : <Viewport::DrawRange>
# Creation : Juin 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Dessine une reginon de selection sur la projection.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <VP>      : Identificateur du Viewport
#  <Lat0>    : Coordonnne en latitude 0
#  <Lon0>    : Coordonnne en longitude 0
#  <Lat1>    : Coordonnne en latitude 0
#  <Lon1>    : Coordonnne en longitude 0
#  <Tag>     : Identificateur de la ligne
#  <Color>   : Couleur.
#  <Text>    : Texte d'identification
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Viewport::DrawRange { Frame VP Lat0 Lon0 Lat1 Lon1 Tag Color { Text "" } { Font XFont10 } } {

   if { $Lat0!=-999.00 && $Lat1!=-999.00 } {
      Viewport::DrawLine $Frame $VP "$Lat0 $Lon0 0 $Lat1 $Lon0 0 $Lat1 $Lon1 0 $Lat0 $Lon1 0 $Lat0 $Lon0 0" $Tag $Color 2

      if { $Text!="" } {
         if { [set vr [$VP -project $Lat0 $Lon1 0]]!= "" && [lindex $vr 2]>0 } {
            $Frame.page.canvas create text [expr [lindex $vr 0]-2] [expr [lindex $vr 1]-2] -text $Text -anchor se -font $Font -fill $Color -tag $Tag
         }
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Viewport::ForceGrid>
# Creation : Septembre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Forcer la prise en charge des parametres de projections grilles.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <Clean>   : Force cleanup/reset
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Viewport::ForceGrid { Frame { Clean False } } {
   variable Map

   if { $Map(Type$Frame)=="grid" } {
      if { $Map(GeoRef)=="" } {
         if { [set vp [lindex [Page::Registered $Frame Viewport] 0]]!="" } {
            if { [set fld [lindex [Viewport::Assigned $Frame $vp { fstdfield gribfield } ] 0]]!="" } {
               if { [set georef [fstdfield define $fld -georef]]!="" } {
                  set Map(GeoRef) $georef
                  set pref  [projection configure $Frame -georef]
                  projection configure $Frame -type grid -georef $georef

                  #----- If georef is different, reset projection
                  if { ![georef isequal $georef $pref] } {
                     set Clean True
                  }
               }
            }
         }
      } else {
         projection configure $Frame -georef $Map(GeoRef)
      }

      if { $Clean } {
         georef define $Map(GeoRef) -grid $FSTD::Param(GridNo)

         #----- Force redraw to set internal gridid (U grids)
         foreach vp [Page::Registered $Frame Viewport] {
            $Frame.page.canvas itemconf $vp -projection $Frame -frame 0
         }
         update idletasks

         projection clean $Frame

         if { $ProjCam::Data(Name)=="" } {
            Viewport::Reset $Frame True
         }
      }

      set ij [projection configure $Frame -gridpoint]

      set Map(GridI) [lindex $ij 0]
      set Map(GridJ) [lindex $ij 1]
   } else {
      projection configure $Frame -georef ""
   }
}

#----------------------------------------------------------------------------
# Nom      : <Viewport::ParamProjGet>
# Creation : Mars 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Recuperer les parametres de projection.
#
# Parametres :
#  <Ref>     : Identificateur de georeference
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Viewport::ParamProjGet { Ref } {
   variable Map

   if { [georef is $Ref] } {
      set Map(GridProj) [georef define $Ref -projection]
      if { [llength [set extent [georef define $Ref -extent]]] } {
         set Map(GridMinX) [lindex $extent 0]
         set Map(GridMinY) [lindex $extent 1]
         set Map(GridMaxX) [lindex $extent 2]
         set Map(GridMaxY) [lindex $extent 3]
      } else {
         set Map(GridMinX) ""
         set Map(GridMinY) ""
         set Map(GridMaxX) ""
         set Map(GridMaxY) ""
      }

      if { [llength [set transform [georef define $Ref -transform]]] } {
         set Map(GridTrX) [lindex $extent 0]
         set Map(GridRtX) [lindex $extent 2]
         set Map(GridScX) [lindex $extent 1]
         set Map(GridTrY) [lindex $extent 3]
         set Map(GridRtY) [lindex $extent 4]
         set Map(GridScY) [lindex $extent 5]
      } else {
         set Map(GridTrX) ""
         set Map(GridRtX) ""
         set Map(GridScX) ""
         set Map(GridTrY) ""
         set Map(GridRtY) ""
         set Map(GridScY) ""
      }
   } else {
      set Map(GridProj) ""
      set Map(GridMinX) ""
      set Map(GridMinY) ""
      set Map(GridMaxX) ""
      set Map(GridMaxY) ""
      set Map(GridTrX) ""
      set Map(GridRtX) ""
      set Map(GridScX) ""
      set Map(GridTrY) ""
      set Map(GridRtY) ""
      set Map(GridScY) ""
    }
}

#----------------------------------------------------------------------------
# Nom      : <Viewport::ParamProjSet>
# Creation : Mars 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Definir les parametres de projection.
#
# Parametres :
#  <Ref>     : Identificateur de georeference
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Viewport::ParamProjSet { { Ref "" } { Def "" } } {
   variable Map
   variable MapDef

   if { ![georef is $Ref] } {
      set Ref $Page::Data(Frame)
      catch { georef create $Ref }
   }
   set Map(GeoRef) $Ref

   if { [info exists ::Viewport::MapDef($Def)] } {
      set Map(GridProj) [lindex $MapDef($Def) 0]
      set Map(GridMinX) [lindex $MapDef($Def) 2]
      set Map(GridMaxX) [lindex $MapDef($Def) 3]
      set Map(GridMinY) [lindex $MapDef($Def) 4]
      set Map(GridMaxY) [lindex $MapDef($Def) 5]
      set Map(GridTrX) ""
      set Map(GridScX) ""
      set Map(GridRtX) ""
      set Map(GridTrY) ""
      set Map(GridScY) ""
      set Map(GridRtY) ""
   }
   eval set tr \[list $Map(GridTrX) $Map(GridScX) $Map(GridRtX) $Map(GridTrY) $Map(GridRtY) $Map(GridScY)\]
   georef define $Map(GeoRef) -projection $Map(GridProj)
   georef define $Map(GeoRef) -transform $tr
   georef define $Map(GeoRef) -extent [list $Map(GridMinX) $Map(GridMinY) $Map(GridMaxX) $Map(GridMaxY)]

   projection clean $Page::Data(Frame)
   projection configure $Page::Data(Frame) -type grid -georef $Map(GeoRef)

#TODO:   Viewport::ConfigGet $Page::Data(Frame) $Viewport::Data(VP)
#   Viewport::Reset $Page::Data(Frame)
}

#----------------------------------------------------------------------------
# Nom      : <Viewport::ParamProj>
# Creation : Mars 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Definition des parametres de projection.
#
# Parametres :
#  <Ref>     : Identificateur de georeference
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Viewport::ParamProj { Ref } {
   global GDefs
   variable Lbl
   variable Map

   if { [winfo exists .viewportproj] } {
      raise .viewportproj
      return
   }

   toplevel         .viewportproj -class Dialog
   wm title         .viewportproj "[lindex $Lbl(Proj) $GDefs(Lang)]"
   wm resizable     .viewportproj 1 1
   wm protocol      .viewportproj WM_DELETE_WINDOW { }
   wm geometry      .viewportproj =300x150+[expr [winfo rootx .]+10]+[expr [winfo rooty .]+10]

   Viewport::ParamProjGet $Ref

   TabFrame::Create .viewportproj.tab 1 ""
   pack .viewportproj.tab -side top -fill both -expand true -padx 2 -pady 2

   set tab [TabFrame::Add .viewportproj.tab 1 [lindex $Lbl(Proj) $GDefs(Lang)] True]

   frame $tab.proj
      frame $tab.proj.head
         button $tab.proj.head.file -image OPEN -relief flat -bd 0 -overrelief raised \
            -command "Mapper::ProjFile $tab.proj.val \[FileBox::Create . \"\" Load \[list \$FileBox::Type(PROJ) \$FileBox::Type(TXT)\]\]"
         pack $tab.proj.head.file -side left -padx 2
      pack $tab.proj.head -side top -fill x

      text $tab.proj.val -bd 1 -bg $GDefs(ColorLight) -height 5 -width 25
      pack $tab.proj.val -side right -fill both -expand true
   pack $tab.proj -side top -fill both -expand true

   set text  $tab.proj.val
   $tab.proj.val insert 0.0 $Map(GridProj)

   set tab [TabFrame::Add .viewportproj.tab 1 [lindex $Lbl(Extent) $GDefs(Lang)] True]
   frame $tab.col
      label  $tab.col.lbl0
      label  $tab.col.min -text [lindex $Lbl(Min) $GDefs(Lang)] -width 11 -anchor w
      label  $tab.col.max -text [lindex $Lbl(Max) $GDefs(Lang)] -width 11 -anchor w
      pack   $tab.col.lbl0 $tab.col.min $tab.col.max -side top -fill x

   frame $tab.colx
      label  $tab.colx.x -text X
      entry  $tab.colx.minx -bg $GDefs(ColorLight) -width 12 -bd 1 -textvariable Viewport::Map(GridMinX)
      entry  $tab.colx.maxx -bg $GDefs(ColorLight) -width 12 -bd 1 -textvariable Viewport::Map(GridMaxX)
      pack   $tab.colx.x $tab.colx.minx $tab.colx.maxx -side top -fill x -expand true

   frame $tab.coly
      label  $tab.coly.y -text Y
      entry  $tab.coly.miny -bg $GDefs(ColorLight) -width 12 -bd 1 -textvariable Viewport::Map(GridMinY)
      entry  $tab.coly.maxy -bg $GDefs(ColorLight) -width 12 -bd 1 -textvariable Viewport::Map(GridMaxY)
      pack   $tab.coly.y $tab.coly.miny $tab.coly.maxy -side top -fill x
   pack $tab.col -side left -fill x -padx 2 -pady 10 -anchor n
   pack $tab.colx $tab.coly -side left -fill x -padx 2 -pady 10 -expand true  -anchor n

   set tab [TabFrame::Add .viewportproj.tab 1 [lindex $Lbl(Transform) $GDefs(Lang)] True]
   frame $tab.col
      label  $tab.col.lbl0
      label  $tab.col.tr -text [lindex $Lbl(Translation) $GDefs(Lang)] -width 11 -anchor w
      label  $tab.col.sc -text [lindex $Lbl(Scaling) $GDefs(Lang)]     -width 11 -anchor w
      label  $tab.col.rt -text [lindex $Lbl(Rotation) $GDefs(Lang)]    -width 11 -anchor w
      pack   $tab.col.lbl0 $tab.col.tr $tab.col.sc $tab.col.rt -side top -fill x

   frame $tab.colx
      label  $tab.colx.x -text X
      entry  $tab.colx.trx -bg $GDefs(ColorLight) -width 12 -bd 1 -textvariable Viewport::Map(GridTrX)
      entry  $tab.colx.scx -bg $GDefs(ColorLight) -width 12 -bd 1 -textvariable Viewport::Map(GridScX)
      entry  $tab.colx.rtx -bg $GDefs(ColorLight) -width 12 -bd 1 -textvariable Viewport::Map(GridRtX)
      pack   $tab.colx.x $tab.colx.trx $tab.colx.scx $tab.colx.rtx -side top -fill x -expand true

   frame $tab.coly
      label  $tab.coly.y -text Y
      entry  $tab.coly.try -bg $GDefs(ColorLight) -width 12 -bd 1 -textvariable Viewport::Map(GridTrY)
      entry  $tab.coly.scy -bg $GDefs(ColorLight) -width 12 -bd 1 -textvariable Viewport::Map(GridScY)
      entry  $tab.coly.rty -bg $GDefs(ColorLight) -width 12 -bd 1 -textvariable Viewport::Map(GridRtY)
      pack   $tab.coly.y $tab.coly.try $tab.coly.scy $tab.coly.rty -side top -fill x
   pack $tab.col -side left -fill x -padx 2 -pady 10 -anchor n
   pack $tab.colx $tab.coly -side left -fill x -padx 2 -pady 10 -expand true -anchor n

   frame .viewportproj.cmd
      button .viewportproj.cmd.close -text [lindex $Lbl(Close) $GDefs(Lang)] -bd 1 -relief raised -command "destroy .viewportproj"
      button .viewportproj.cmd.apply -text [lindex $Lbl(Apply) $GDefs(Lang)] -bd 1 -relief raised -command "set Viewport::Map(GridProj) \[$text get 0.0 end\]; Viewport::ParamProjSet $Ref"
      pack .viewportproj.cmd.apply .viewportproj.cmd.close -side left -fill x -expand true
   pack .viewportproj.cmd -side bottom -fill x  -padx 2 -pady 2
}

#----------------------------------------------------------------------------
# Nom      : <Viewport::ParamFrame>
# Creation : Mars 2000 - J.P. Gauthier - CMC/CMOE
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

proc Viewport::ParamFrame { Frame Apply } {
   global GDefs
   variable Data
   variable Bubble
   variable Lbl
   variable Map
   variable Resources

   set Data(Frame) [TabFrame::Add $Frame 1 [lindex $Lbl(Geo) $GDefs(Lang)] False ""]

   labelframe $Data(Frame).proj -text [lindex $Lbl(Proj) $GDefs(Lang)]
      ComboBox::Create $Data(Frame).proj.type Viewport::Map(Type) noedit unsorted nodouble -1 $Map(Types) 12 4 "set Viewport::Map(GeoRef) \"\"; Viewport::ParamSet ; $Apply configure -state normal"
      button $Data(Frame).proj.def -image WORLD -relief flat -bd 0 -overrelief raised -command { Viewport::ParamProj $Viewport::Map(GeoRef) }
      pack $Data(Frame).proj.def -side left -padx 2
      pack $Data(Frame).proj.type -side left -fill x -expand true

      frame $Data(Frame).proj.sunk -relief sunken -bd 1
         checkbutton $Data(Frame).proj.sunk.persp  -text "Perspective"  -variable Viewport::Map(Perspective) \
            -indicatoron false -command "$Apply configure -state normal" -onvalue True -offvalue False -bd 1
         pack $Data(Frame).proj.sunk.persp -side left -ipadx 2
      pack $Data(Frame).proj.sunk -side left -fill x -padx 2

   pack $Data(Frame).proj -side top -fill x -padx 5 -pady 5

   #----- Add user defined projections
   foreach def [lsort [array names Viewport::MapDef]] {
      ComboBox::Add $Data(Frame).proj.type "grid:$def"
   }

   frame $Data(Frame).left

   labelframe $Data(Frame).left.ras -text [lindex $Lbl(Raster) $GDefs(Lang)]
      frame $Data(Frame).left.ras.topo
         label $Data(Frame).left.ras.topo.lbl -text " [lindex $Lbl(Topo) $GDefs(Lang)]"
         checkbutton $Data(Frame).left.ras.topo.sel -variable Viewport::Map(Topo) -relief raised -bd 1 -onvalue 1 -offvalue 0  -selectcolor "" -relief groove -bd 1\
            -bitmap @$GDefs(Dir)/share/bitmap/zeroth.xbm -indicatoron false -command "$Apply configure -state normal"
         pack $Data(Frame).left.ras.topo.sel -side left -ipadx 1
         pack $Data(Frame).left.ras.topo.lbl -side left -fill y
      frame $Data(Frame).left.ras.bath
         label $Data(Frame).left.ras.bath.lbl -text " [lindex $Lbl(Bath) $GDefs(Lang)]"
         checkbutton $Data(Frame).left.ras.bath.sel -variable Viewport::Map(Bath) -relief raised -bd 1 -onvalue 1 -offvalue 0 -selectcolor "" -relief groove -bd 1 \
            -bitmap @$GDefs(Dir)/share/bitmap/zeroth.xbm -indicatoron false -command "$Apply configure -state normal"
         pack $Data(Frame).left.ras.bath.sel -side left -ipadx 1
         pack $Data(Frame).left.ras.bath.lbl -side left -fill y
      frame $Data(Frame).left.ras.text
         label $Data(Frame).left.ras.text.lbl -text " [lindex $Lbl(Text) $GDefs(Lang)]"
         checkbutton $Data(Frame).left.ras.text.sel -variable Viewport::Map(Text) -relief raised -bd 1 -onvalue 1 -offvalue 0 -selectcolor "" -relief groove -bd 1 \
            -bitmap @$GDefs(Dir)/share/bitmap/zeroth.xbm -indicatoron false -command "$Apply configure -state normal"
         pack $Data(Frame).left.ras.text.sel -side left -ipadx 1
         pack $Data(Frame).left.ras.text.lbl -side left -fill y
      frame $Data(Frame).left.ras.mask
         IcoMenu::CreateDef $Data(Frame).left.ras.mask.sel $GDefs(Dir)/share/bitmap \
             { zeroth.xbm mask1.xbm mask2.xbm } { NONE LAND SEA } \
             Viewport::Map(Mask) "$Apply configure -state normal" $Viewport::Map(Mask) -relief groove -bd 2

#         IcoMenu::Create $Data(Frame).left.ras.mask.sel $GDefs(Dir)/share/bitmap \
#            "zeroth.xbm mask1.xbm mask2.xbm" "0 1 2" Viewport::Map(Mask) "$Apply configure -state normal" $Viewport::Map(Mask) -relief groove -bd 2
         label $Data(Frame).left.ras.mask.lbl -text " [lindex $Lbl(Mask) $GDefs(Lang)]"
         pack $Data(Frame).left.ras.mask.sel -side left -ipadx 1
         pack $Data(Frame).left.ras.mask.lbl -side left -fill y
      pack $Data(Frame).left.ras.topo $Data(Frame).left.ras.bath $Data(Frame).left.ras.text $Data(Frame).left.ras.mask -side top -fill x -padx 2

   labelframe $Data(Frame).left.sun -text [lindex $Lbl(Sun) $GDefs(Lang)]
      checkbutton $Data(Frame).left.sun.on -variable Viewport::Map(Sun) -relief raised -bd 1 -onvalue 1 -offvalue 0  -selectcolor "" -relief groove -bd 1\
         -bitmap @$GDefs(Dir)/share/bitmap/zeroth.xbm -indicatoron false -command "$Apply configure -state normal"
      scale $Data(Frame).left.sun.time -orient horizontal -from 0 -to 86400 \
         -showvalue false -variable Viewport::Data(Seconds) -relief flat \
         -command "$Apply configure -state normal; Viewport::ConfigSet \$Page::Data(Frame);  catch " -width 14 -sliderlength 8  -bd 1 -resolution 60
      pack  $Data(Frame).left.sun.on -side left -padx 2 -pady 2
      pack $Data(Frame).left.sun.time -side left -fill x -expand true -padx 2 -pady 2

   labelframe $Data(Frame).left.axis -text "Axis"
      IcoMenu::Create $Data(Frame).left.axis.on $GDefs(Dir)/share/bitmap \
         "zeroth.xbm zaxis0.xbm zaxis1.xbm" "0 1 2" \
         Viewport::Map(ZAxis) "$Apply configure -state normal" $Viewport::Map(ZAxis) -relief groove -bd 2
      checkbutton $Data(Frame).left.axis.pick -variable Page::Data(ToolMode) -relief sunken -bd 1 -overrelief raised -offrelief flat \
         -onvalue Viewport -offvalue SPI -selectcolor "" -image ARROW -indicatoron false -command { SPI::ToolMode $Page::Data(ToolMode) Data True }
      entry  $Data(Frame).left.axis.z -textvariable Viewport::Map(ZAxisZ) -relief sunken -bd 1 -bg $GDefs(ColorLight) -width 5
      pack $Data(Frame).left.axis.on -side left -padx 2 -pady 2
      pack $Data(Frame).left.axis.z -side left -fill x -expand true -padx 2 -pady 2
      pack $Data(Frame).left.axis.pick -side left -padx 2 -pady 2
      bind $Data(Frame).left.axis.z <Any-KeyRelease> "$Apply configure -state normal"

   labelframe $Data(Frame).left.scale -text "Elevation"
      scale $Data(Frame).left.scale.height -orient horizontal -from 1 -to 200 \
         -showvalue true -variable Viewport::Map(Elev) -relief flat \
         -command "if { \[projection is  \$Page::Data(Frame)\] && \[projection configure \$Page::Data(Frame) -scale\]!=\$Viewport::Map(Elev) } { Viewport::ParamSet }; $Apply configure -state normal; catch " -width 14 -sliderlength 8 -bd 1 -resolution 0.1
      pack $Data(Frame).left.scale.height -side left -fill x -expand true -padx 2 -pady 2

   pack $Data(Frame).left.ras -side top -fill x
   pack $Data(Frame).left.sun -side top -fill x -pady 5
   pack $Data(Frame).left.axis -side top -fill x
   pack $Data(Frame).left.scale -side top -fill x -pady 5

   labelframe $Data(Frame).layer -text [lindex $Lbl(Vector) $GDefs(Lang)]

      frame $Data(Frame).layer.vp
         IcoMenu::Create $Data(Frame).layer.vp.sz $GDefs(Dir)/share/bitmap \
            "zeroth.xbm width1.xbm width2.xbm width3.xbm width4.xbm width5.xbm" "0 1 2 3 4 5" \
            Viewport::Resources(BD) "$Apply configure -state normal" $Viewport::Resources(BD) -relief groove -bd 2
         button $Data(Frame).layer.vp.font -relief groove -bd 2 -bitmap @$GDefs(Dir)/share/bitmap/font.ico\
            -command "FontBox::Create $Data(Frame).layer.vp.font \"$Apply configure -state normal; $Apply invoke\"  \$Viewport::Resources(Font)"
         ColorBox::CreateSel $Data(Frame).layer.vp.col Viewport::Resources(Bkg) $Apply configure -state normal
         label $Data(Frame).layer.vp.lbl -text [format "%-10s" [lindex $Lbl(Viewport) $GDefs(Lang)]]
         pack $Data(Frame).layer.vp.col $Data(Frame).layer.vp.sz $Data(Frame).layer.vp.font -side left
         pack $Data(Frame).layer.vp.lbl -side right

      frame $Data(Frame).layer.coast
         IcoMenu::Create $Data(Frame).layer.coast.sz $GDefs(Dir)/share/bitmap \
            "zeroth.xbm width1.xbm width2.xbm width3.xbm width4.xbm width5.xbm" "0 1 2 3 4 5" \
            Viewport::Map(Coast) "$Apply configure -state normal" $Viewport::Map(Coast) -relief groove -bd 2
         ColorBox::CreateSel $Data(Frame).layer.coast.col Viewport::Resources(Coast) $Apply configure -state normal
         ColorBox::CreateSel $Data(Frame).layer.coast.fcol Viewport::Resources(FillCoast) $Apply configure -state normal
         label $Data(Frame).layer.coast.lbl -text [format "%-10s" [lindex $Lbl(Coast) $GDefs(Lang)]]
         pack $Data(Frame).layer.coast.col $Data(Frame).layer.coast.sz $Data(Frame).layer.coast.fcol -side left
         pack $Data(Frame).layer.coast.lbl -side right

      frame $Data(Frame).layer.lake
         IcoMenu::Create $Data(Frame).layer.lake.sz $GDefs(Dir)/share/bitmap \
            "zeroth.xbm width1.xbm width2.xbm width3.xbm width4.xbm width5.xbm" "0 1 2 3 4 5" \
            Viewport::Map(Lake) "$Apply configure -state normal" $Viewport::Map(Lake) -relief groove -bd 2
         ColorBox::CreateSel $Data(Frame).layer.lake.col Viewport::Resources(Lake) $Apply configure -state normal
         ColorBox::CreateSel $Data(Frame).layer.lake.fcol Viewport::Resources(FillLake) $Apply configure -state normal
         label $Data(Frame).layer.lake.lbl -text [format "%-10s" [lindex $Lbl(Lake) $GDefs(Lang)]]
         pack $Data(Frame).layer.lake.col $Data(Frame).layer.lake.sz $Data(Frame).layer.lake.fcol -side left
         pack $Data(Frame).layer.lake.lbl -side right

      frame $Data(Frame).layer.river
         IcoMenu::Create $Data(Frame).layer.river.sz $GDefs(Dir)/share/bitmap \
            "zeroth.xbm width1.xbm width2.xbm width3.xbm width4.xbm width5.xbm" "0 1 2 3 4 5" \
            Viewport::Map(River) "$Apply configure -state normal" $Viewport::Map(River) -relief groove -bd 2
         ColorBox::CreateSel $Data(Frame).layer.river.col Viewport::Resources(River) $Apply configure -state normal
         label $Data(Frame).layer.river.lbl -text [format "%-10s" [lindex $Lbl(River) $GDefs(Lang)]]
         pack $Data(Frame).layer.river.col $Data(Frame).layer.river.sz -side left
         pack $Data(Frame).layer.river.lbl -side right

      frame $Data(Frame).layer.poli
         IcoMenu::Create $Data(Frame).layer.poli.sz $GDefs(Dir)/share/bitmap \
            "zeroth.xbm width1.xbm width2.xbm width3.xbm width4.xbm width5.xbm" "0 1 2 3 4 5" \
            Viewport::Map(Polit) "$Apply configure -state normal" $Viewport::Map(Polit) -relief groove -bd 2
         $Data(Frame).layer.poli.sz.menu add separator
         ColorBox::CreateSel $Data(Frame).layer.poli.col Viewport::Resources(Polit) $Apply configure -state normal
         label $Data(Frame).layer.poli.lbl -text [format "%-10s" [lindex $Lbl(Polit) $GDefs(Lang)]]
         pack $Data(Frame).layer.poli.col $Data(Frame).layer.poli.sz -side left
         pack $Data(Frame).layer.poli.lbl -side right

      frame $Data(Frame).layer.admin
         IcoMenu::Create $Data(Frame).layer.admin.sz $GDefs(Dir)/share/bitmap \
            "zeroth.xbm width1.xbm width2.xbm width3.xbm width4.xbm width5.xbm" "0 1 2 3 4 5" \
            Viewport::Map(Admin) "$Apply configure -state normal" $Viewport::Map(Admin) -relief groove -bd 2
         ColorBox::CreateSel $Data(Frame).layer.admin.col Viewport::Resources(Admin) $Apply configure -state normal
         label $Data(Frame).layer.admin.lbl -text [format "%-10s" [lindex $Lbl(Admin) $GDefs(Lang)]]
         pack $Data(Frame).layer.admin.col $Data(Frame).layer.admin.sz -side left
         pack $Data(Frame).layer.admin.lbl -side right

      frame $Data(Frame).layer.city
         checkbutton $Data(Frame).layer.city.sz -variable Viewport::Map(City) -relief raised -bd 1 -onvalue 1 -offvalue 0 -selectcolor "" -relief groove -bd 1 \
            -bitmap @$GDefs(Dir)/share/bitmap/zeroth.xbm -indicatoron false -command "$Apply configure -state normal"
         ColorBox::CreateSel $Data(Frame).layer.city.col Viewport::Resources(City) $Apply configure -state normal
         label $Data(Frame).layer.city.lbl -text [format "%-10s" [lindex $Lbl(City) $GDefs(Lang)]]
         pack $Data(Frame).layer.city.col -side left
         pack $Data(Frame).layer.city.sz -side left -padx 1 -ipadx 1
         pack $Data(Frame).layer.city.lbl -side right

      frame $Data(Frame).layer.place
         checkbutton $Data(Frame).layer.place.sz -variable Viewport::Map(Place) -relief raised -bd 1 -onvalue 1 -offvalue 0 -selectcolor "" -relief groove -bd 1 \
            -bitmap @$GDefs(Dir)/share/bitmap/zeroth.xbm -indicatoron false -command "$Apply configure -state normal"
         ColorBox::CreateSel $Data(Frame).layer.place.col Viewport::Resources(Place) $Apply configure -state normal
         label $Data(Frame).layer.place.lbl -text [format "%-10s" [lindex $Lbl(Place) $GDefs(Lang)]]
         pack $Data(Frame).layer.place.col -side left
         pack $Data(Frame).layer.place.sz -side left -padx 1 -ipadx 1
         pack $Data(Frame).layer.place.lbl -side right

      frame $Data(Frame).layer.road
         IcoMenu::Create $Data(Frame).layer.road.sz $GDefs(Dir)/share/bitmap \
            "zeroth.xbm width1.xbm width2.xbm width3.xbm width4.xbm width5.xbm" "0 1 2 3 4 5" \
            Viewport::Map(Road) "$Apply configure -state normal" $Viewport::Map(Road) -relief groove -bd 2
         ColorBox::CreateSel $Data(Frame).layer.road.col Viewport::Resources(Road) $Apply configure -state normal
         label $Data(Frame).layer.road.lbl -text [format "%-10s" [lindex $Lbl(Road) $GDefs(Lang)]]
         pack $Data(Frame).layer.road.col $Data(Frame).layer.road.sz -side left
         pack $Data(Frame).layer.road.lbl -side right

      frame $Data(Frame).layer.rail
         IcoMenu::Create $Data(Frame).layer.rail.sz $GDefs(Dir)/share/bitmap \
            "zeroth.xbm width1.xbm width2.xbm width3.xbm width4.xbm width5.xbm" "0 1 2 3 4 5" \
            Viewport::Map(Rail) "$Apply configure -state normal" $Viewport::Map(Rail) -relief groove -bd 2
         ColorBox::CreateSel $Data(Frame).layer.rail.col Viewport::Resources(Rail) $Apply configure -state normal
         label $Data(Frame).layer.rail.lbl -text [format "%-10s" [lindex $Lbl(Rail) $GDefs(Lang)]]
         pack $Data(Frame).layer.rail.col $Data(Frame).layer.rail.sz -side left
         pack $Data(Frame).layer.rail.lbl -side right

      frame $Data(Frame).layer.ll
         IcoMenu::Create $Data(Frame).layer.ll.sz $GDefs(Dir)/share/bitmap \
            "zeroth.xbm width1.xbm width2.xbm width3.xbm width4.xbm width5.xbm" "0 1 2 3 4 5" \
            Viewport::Map(Coord) "$Apply configure -state normal" $Viewport::Map(Coord) -relief groove -bd 2
         IcoMenu::CreateDef $Data(Frame).layer.ll.ds $GDefs(Dir)/share/bitmap \
            { dash0.xbm dash1.xbm dash2.xbm dash3.xbm dash4.xbm dash5.xbm } { "" . - .- .-- .-. } \
            Viewport::Resources(DashCoord) "$Apply configure -state normal" 0 -relief groove -bd 2
         ColorBox::CreateSel $Data(Frame).layer.ll.col Viewport::Resources(Coord) $Apply configure -state normal
         label $Data(Frame).layer.ll.lbl -text [format "%-10s" [lindex $Lbl(Coord) $GDefs(Lang)]]
         menubutton $Data(Frame).layer.ll.opt -bd 2 -relief groove -bitmap @$GDefs(Dir)/share/bitmap/more.xbm \
            -menu $Data(Frame).layer.ll.opt.menu
         pack $Data(Frame).layer.ll.col $Data(Frame).layer.ll.sz $Data(Frame).layer.ll.ds $Data(Frame).layer.ll.opt -side left
         pack $Data(Frame).layer.ll.lbl -side right

      frame $Data(Frame).layer.min
         scale $Data(Frame).layer.min.sc -orient horizontal -from 0 -to 100 \
            -showvalue false -variable Viewport::Map(MinSize) -relief flat \
            -command "$Apply configure -state normal; Viewport::ConfigSet \$Page::Data(Frame);  catch " -width 14 -length 107 -sliderlength 8  -bd 1 -resolution 1
         label $Data(Frame).layer.min.lbl -text [format " %-10s" [lindex $Lbl(MinSize) $GDefs(Lang)]]
         pack $Data(Frame).layer.min.sc $Data(Frame).layer.min.lbl -side left

      frame $Data(Frame).layer.crowd
         scale $Data(Frame).layer.crowd.sc -orient horizontal -from 0 -to 100 \
            -showvalue false -variable Viewport::Map(Crowd) -relief flat \
            -command "$Apply configure -state normal; Viewport::ConfigSet \$Page::Data(Frame);  catch " -width 14 -length 107 -sliderlength 8  -bd 1 -resolution 1
         label $Data(Frame).layer.crowd.lbl -text [format " %-10s" [lindex $Lbl(Crowd) $GDefs(Lang)]]
         pack $Data(Frame).layer.crowd.sc $Data(Frame).layer.crowd.lbl -side left

         pack $Data(Frame).layer.vp $Data(Frame).layer.coast $Data(Frame).layer.lake $Data(Frame).layer.river $Data(Frame).layer.poli \
         $Data(Frame).layer.admin $Data(Frame).layer.city $Data(Frame).layer.place $Data(Frame).layer.road $Data(Frame).layer.rail \
         $Data(Frame).layer.ll $Data(Frame).layer.min $Data(Frame).layer.crowd -side top -anchor sw -padx 2 -fill x

   #----- LatLon

   menu $Data(Frame).layer.ll.opt.menu -title [lindex $Lbl(Coord) $GDefs(Lang)]
      $Data(Frame).layer.ll.opt.menu add radiobutton -label [lindex $Lbl(Sea) $GDefs(Lang)] -variable Viewport::Map(CoordLoc) -value -1 -underline 0 -command "$Apply configure -state normal"
      $Data(Frame).layer.ll.opt.menu add radiobutton -label [lindex $Lbl(Land) $GDefs(Lang)] -variable Viewport::Map(CoordLoc) -value 1 -underline 0 -command "$Apply configure -state normal"
      $Data(Frame).layer.ll.opt.menu add separator
      $Data(Frame).layer.ll.opt.menu add radiobutton -label "  20 [lindex $Lbl(Degrees) $GDefs(Lang)]" -variable Viewport::Map(CoordDef) -value 20.0 -command "$Apply configure -state normal"
      $Data(Frame).layer.ll.opt.menu add radiobutton -label "  10 [lindex $Lbl(Degrees) $GDefs(Lang)]" -variable Viewport::Map(CoordDef) -value 10.0 -command "$Apply configure -state normal"
      $Data(Frame).layer.ll.opt.menu add radiobutton -label "   5 [lindex $Lbl(Degrees) $GDefs(Lang)]" -variable Viewport::Map(CoordDef) -value  5.0 -command "$Apply configure -state normal"
      $Data(Frame).layer.ll.opt.menu add radiobutton -label "   2 [lindex $Lbl(Degrees) $GDefs(Lang)]" -variable Viewport::Map(CoordDef) -value  2.0 -command "$Apply configure -state normal"
      $Data(Frame).layer.ll.opt.menu add radiobutton -label "   1 [lindex $Lbl(Degrees) $GDefs(Lang)]" -variable Viewport::Map(CoordDef) -value  1.0 -command "$Apply configure -state normal"
      $Data(Frame).layer.ll.opt.menu add radiobutton -label "0.50 [lindex $Lbl(Degrees) $GDefs(Lang)]" -variable Viewport::Map(CoordDef) -value 0.50 -command "$Apply configure -state normal"
      $Data(Frame).layer.ll.opt.menu add radiobutton -label "0.25 [lindex $Lbl(Degrees) $GDefs(Lang)]" -variable Viewport::Map(CoordDef) -value 0.25 -command "$Apply configure -state normal"
      $Data(Frame).layer.ll.opt.menu add radiobutton -label "0.15 [lindex $Lbl(Degrees) $GDefs(Lang)]" -variable Viewport::Map(CoordDef) -value 0.15 -command "$Apply configure -state normal"
      $Data(Frame).layer.ll.opt.menu add radiobutton -label "0.10 [lindex $Lbl(Degrees) $GDefs(Lang)]" -variable Viewport::Map(CoordDef) -value 0.10 -command "$Apply configure -state normal"
      $Data(Frame).layer.ll.opt.menu add radiobutton -label "0.05 [lindex $Lbl(Degrees) $GDefs(Lang)]" -variable Viewport::Map(CoordDef) -value 0.05 -command "$Apply configure -state normal"
      $Data(Frame).layer.ll.opt.menu add radiobutton -label "0.01 [lindex $Lbl(Degrees) $GDefs(Lang)]" -variable Viewport::Map(CoordDef) -value 0.01 -command "$Apply configure -state normal"
      $Data(Frame).layer.ll.opt.menu add separator
      $Data(Frame).layer.ll.opt.menu add radiobutton -label "[lindex $Lbl(Numbered) $GDefs(Lang)] 0/1" -variable Viewport::Map(CoordNum) -value 0 -command "$Apply configure -state normal"
      $Data(Frame).layer.ll.opt.menu add radiobutton -label "[lindex $Lbl(Numbered) $GDefs(Lang)] 1/1" -variable Viewport::Map(CoordNum) -value 1 -command "$Apply configure -state normal"
      $Data(Frame).layer.ll.opt.menu add radiobutton -label "[lindex $Lbl(Numbered) $GDefs(Lang)] 1/2" -variable Viewport::Map(CoordNum) -value 2 -command "$Apply configure -state normal"
      $Data(Frame).layer.ll.opt.menu add radiobutton -label "[lindex $Lbl(Numbered) $GDefs(Lang)] 1/3" -variable Viewport::Map(CoordNum) -value 3 -command "$Apply configure -state normal"

      pack $Data(Frame).left $Data(Frame).layer -side left -fill x -padx 5 -pady 5 -anchor n

   Bubble::Create $Data(Frame).proj.type.list.select $Bubble(Proj)
   Bubble::Create $Data(Frame).left.ras.topo         $Bubble(Topo)
   Bubble::Create $Data(Frame).left.ras.sun          $Bubble(Sun)
   Bubble::Create $Data(Frame).left.ras.bath         $Bubble(Bath)
   Bubble::Create $Data(Frame).left.ras.text         $Bubble(Texture)
   Bubble::Create $Data(Frame).left.scale            $Bubble(Elev)
   Bubble::Create $Data(Frame).layer                 $Bubble(Vector)
   Bubble::Create $Data(Frame).left.sun              $Bubble(Sun)
}

#-------------------------------------------------------------------------------
# Nom      : <Viewport::Draw...>
# Creation : Fevrier 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Fonctions de manipulations de la selection sur la projection.
#
# Parametres :
#   <Frame   : Identificateur de Page
#   <VP>     : Identificateur du Viewport
#
# Remarques :
#    - Ces fonctions sont appele par le package Page au besoin.
#
#-------------------------------------------------------------------------------

proc Viewport::Draw     { Frame VP } {
   variable Map

   set Map(ZAxisCoord) [list $Map(LatCursor) $Map(LonCursor)]
   Viewport::ConfigSet $Frame
   Page::Update $Frame
}

proc Viewport::DrawDone { Frame VP } {
}

proc Viewport::DrawInit { Frame VP } {
}

proc Viewport::Move { Frame VP } {
   Viewport::Draw $Frame $VP
}

proc Viewport::MoveDone { Frame VP } {
}

proc Viewport::MoveInit { Frame VP } {
}

#----------------------------------------------------------------------------
# Nom      : <Viewport::ParamSet>
# Creation : Octobre 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Change les parameters de la projection.
#
# Parametres :
#  <Clean>   : Reinitialisation des donneese
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Viewport::ParamSet { } {
   variable Map

   foreach field $FSTD::Data(List) {
      fstdfield clean $field
   }
   foreach field $FSTD::Data(ListTool) {
      fstdfield clean $field
   }
}

#----------------------------------------------------------------------------
# Nom      : <Viewport::Reset>
# Creation : Avril 1998 - J.P. Gauthier - CMC/CMOE
#
# But      : Reinitialise les parametres de la projection avec les valeurs initiales.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <Fast>    : Immediat ou avec mouvement
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Viewport::Reset { Frame { Fast False } } {
   variable Map
   variable Data

   #----- Verifier le type de projection pour les parametres

   set Map(Grabbed) [clock click -milliseconds]

   if { $Map(Type$Frame)=="grid" } {
      set ninj [projection configure $Frame -gridsize]
      set ext  [projection configure $Frame -gridextent]
      set Map(GridI) [expr ([lindex $ninj 0])*0.5+[lindex $ext 0]]
      set Map(GridJ) [expr ([lindex $ninj 1])*0.5+[lindex $ext 1]]
      set ll [projection function $Frame -coordgrid $Map(GridI) $Map(GridJ)]
      set Map(LatReset) [lindex $ll 0]
      set Map(LonReset) [lindex $ll 1]
   }

   if { $Fast } {
      Viewport::Rotate $Page::Data(Frame) $Map(LatReset) $Map(LonReset) 1.0 { 0.0 0.0 2.0 } { 0.0 0.0 1.0 } { 0.0 1.0 0.0 }
   } else {
      Viewport::GoTo $Page::Data(Frame) $Map(LatReset) $Map(LonReset) 1.0 { 0.0 0.0 2.0 } { 0.0 0.0 1.0 } { 0.0 1.0 0.0 }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Viewport::Resolution>
# Creation : Janvier 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectue la configurations en basse qualite d'image.
#
# Parametres   :
#  <Frame>     : Identificateur de Page
#  <Res>       : Resolution
#  <Grab>      : Grabbed time
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Viewport::Resolution { Frame Res { Grab 0 } } {
   variable Map
   variable Data

   if { (!$Grab || $Map(Grabbed)<=$Grab) && $Res!=[glrender -resolution] } {         

      glrender -resolution $Res -delay 1000

      if { $Res==1 } {
         glrender -delay $OpenGL::Param(Delay)

         foreach vp [Page::Registered $Frame Viewport] {
            if { [winfo exists $Frame.page.canvas] } {
               $Frame.page.canvas itemconf $vp -update True
            }
         }

         foreach mini $Miniport::Data(Mini$Frame) {
            $Frame.page.canvas itemconf $mini -update True
         }
         update idletasks
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Viewport::Rotate>
# Creation : Avril 1998 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectue la rotation.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <Lat>     : Latitude
#  <Lon>     : Longitude
#  <Zoom>    : Zoom
#  <From>    : Vecteur position de la camera
#  <To>      : Vecteur point focal de la camera
#  <Up>      : Vecteur direction haut de la camera
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Viewport::Rotate { Frame { Lat -999 } { Lon -999 } { Zoom 0 } { From {} } { To {} } { Up {} } { Update True } } {
   variable Map
   variable Data
  
   #----- Stop Flybys
   set Animator::Fly(Length) 0
   set Animator::Fly(Path)   ""
   
   if { [winfo exists $Frame] } {

      upvar #0 ProjCam::Data${Frame}::Cam cam

      if { $Lat!=-999 && $Lon!=-999 } {
         set Map(Lat) $Lat
         set Map(Lon) $Lon

         if { $Map(Type$Frame)=="grid" } {
            set ij [projection function $Frame -gridcoord $Lat $Lon]
            set Map(GridI) [lindex $ij 0]
            set Map(GridJ) [lindex $ij 1]
         }
      }

      if { $Map(Type$Frame)=="grid" } {
         projection configure $Frame -gridpoint $Map(GridI) $Map(GridJ)
      } else {
         projection configure $Frame -location $Map(Lat) $Map(Lon)
      }

      if { $Zoom } {
         projcam configure $Frame -lens [set cam(Lens) $Zoom]
      }
      if { [llength $From] } {
         projcam configure $Frame -from [set cam(From) $From]
      }
      if { [llength $To] } {
         projcam configure $Frame -to [set cam(To) $To]
      }
      if { [llength $Up] } {
         projcam configure $Frame -up [set cam(up) $Up]
      }
      if { $Update } {
         Page::Update $Frame
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Viewport::RotateDo>
# Creation : Avril 1998 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectue la rotation relative a un point.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <VP>      : Identificateur du Viewport
#  <X>       : Coordonne x du point actuel
#  <Y>       : Coordonne y du point actuel

# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Viewport::RotateDo { Frame VP X Y } {
   variable Map
   variable Data

   upvar #0 ProjCam::Data${Frame}::Cam cam

   #----- check for grid projection
   if { $Map(Type$Frame)=="grid" } {
      set ij [$VP -ungrid $X $Y]
      set i [lindex $ij 0]
      set j [lindex $ij 1]

      #----- if we're off grid
      if { $i==-1 || $j==-1 } {
         return
      }
      set Map(GridI)  [expr $Map(GridI) + ($Map(GridIRot)-$i)]
      set Map(GridJ)  [expr $Map(GridJ) + ($Map(GridJRot)-$j)]

      projection configure $Frame -gridpoint $Map(GridI) $Map(GridJ)

      #----- Get back the position since it could've been clamped
      set ij [projection configure $Frame -gridpoint]
      set Map(GridI) [lindex $ij 0]
      set Map(GridJ) [lindex $ij 1]
   } else {
      set latlon [$VP -unproject $X $Y]
      set lat [lindex $latlon 0]
      set lon [lindex $latlon 1]

      #----- Si le pointeur n'est pas sur le globe
      if { $lat==-999.00 || $lon==-999.00 || $Map(LonRot)==-999.0 || $Map(LatRot)==-999.0 } {
         return
      }
      set Map(Lat0) $Map(Lat)
      set Map(Lon0) $Map(Lon)

      if { $Map(Type$Frame)=="orthographic" } {

         #----- Verifier le cote en longitude du curseur pour calculer le sens de l'increment en latitude
         set lonside [expr abs([CheckCoord [expr $Map(Lon) - $Map(LonRot)]])]

         #----- Si on est dans le deadzone ( 70 a 100 par rapport a la longitude courante) on fait rien
         if { $lonside > 110.0 } {
            set Map(Lat) [expr $Map(Lat) - ($Map(LatRot) - $lat)]
         } elseif { $lonside < 70.0 } {
            set Map(Lat) [expr $Map(Lat) + ($Map(LatRot) - $lat)]
         }
      } else {
         set Map(Lat) [expr $Map(Lat) + ($Map(LatRot) - $lat)]
      }

      #----- Calculer l'increment en longitude
      set Map(Lon) [CheckCoord [expr $Map(Lon) + ($Map(LonRot) - $lon)]]
      if { [string range $VP 0 3]=="MINI" } {
         projection configure $VP -location $Map(Lat) $Map(Lon)

         #----- If the miniport zoom is less, center master projection
         if { [projcam configure $VP -lens]<[projcam configure $Frame -lens] } {
            projection configure $Frame -location $Map(Lat) $Map(Lon)
         }
      } else {
         projection configure $Frame -location $Map(Lat) $Map(Lon)
      }
   }

   set Map(Grabbed) [clock click -milliseconds]
   set ProjCam::Data(Name)  ""

   Page::Update $Frame
}

#----------------------------------------------------------------------------
# Nom      : <Viewport::RotateDone>
# Creation : Avril 1998 - J.P. Gauthier - CMC/CMOE
#
# But      : Termine la rotation relative.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <VP>      : Identificateur du Viewport
#  <Sling>   : Effet slingshot
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Viewport::RotateDone { Frame VP { Sling False } } {
   variable Map

   set res 1

   if { $Sling && $Map(Type$Frame)!="grid" && [string range $VP 0 3]!="MINI" } {

      #----- Force an update to get the right distance
      update idletasks
      set dt [expr [clock click -milliseconds]-$Map(Grabbed)]
      set dx [projection function $Frame -dist [list $Map(Lat0) $Map(Lon0) $Map(Lat) $Map(Lon)] 0.0]

      #----- If the distance is long enought, slingshot
      if { $dx>[expr 2*[$VP -distpix]] && $dt>0 && $dt<250 } {
         set dir [expr [projection function $Frame -bearing $Map(Lat0) $Map(Lon0) $Map(Lat) $Map(Lon)]]
         set spd [expr int($dx/$dt)]
         set res [Viewport::GoAlong $Frame $spd $dir $Map(Lat) $Map(Lon)]
      }
   }

   if { $res } {
      Viewport::Resolution $Frame 1
   }
   set Map(Grabbed) [clock click -milliseconds]
}

#----------------------------------------------------------------------------
# Nom      : <Viewport::RotateInit>
# Creation : Avril 1998 - J.P. Gauthier - CMC/CMOE
#
# But      : Initie la rotation relative par le point selectionne.
#
# Parametres :
#  <Frame >  : Identificateur de Page
#  <VP>      : Identificateur du Viewport
#  <X>       : Coordonnne x du pointeur de la souris
#  <Y>       : Coordonnne y du pointeur de la souris
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Viewport::RotateInit { Frame VP X Y } {
   variable Map

   set Map(Grabbed) [clock click -milliseconds]
   
   #----- Stop Flybys
   set Animator::Fly(Length) 0
   set Animator::Fly(Path)   ""

   if { [string range $VP 0 3]=="MINI" } {
      set ll [projection configure $VP -location]
   } else {
      set ll [projection configure $Frame -location]
   }
   set Map(Lat0) [set Map(Lat) [lindex $ll 0]]
   set Map(Lon0) [set Map(Lon) [lindex $ll 1]]

   if { $Map(Type$Frame)=="grid" } {
      set Map(GridIRot) $Map(GridICursor)
      set Map(GridJRot) $Map(GridJCursor)
   } else {
      set Map(LonRot) $Map(LonCursor)
      set Map(LatRot) $Map(LatCursor)
   }

   Viewport::Resolution $Frame [expr $OpenGL::Param(Res)==1?2:$OpenGL::Param(Res)]
}

#----------------------------------------------------------------------------
# Nom      : <Viewport::GoAlong>
# Creation : Juillet 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Anime le deplacement le long d'une direction.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <Speed>   : Vitesse initiale
#  <Bearing> : Direction
#  <Lat>     : Latitude d'origine
#  <Lon>     : Longitude d'origine
#  <Damping> : Effet de ralentissement
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Viewport::GoAlong { Frame Speed Bearing Lat Lon { Damping True } } {
   variable Map

   set ProjCam::Data(Name)  ""

   #----- Do only if we have the power to

   if { $OpenGL::Param(Res)==1 && $Map(Type$Frame)!="grid" } {
      set dx  0
      set t2 [set t0  [clock click -milliseconds]]
      set min [expr $Speed/10000.0]
      set Map(Speed) $Speed

      #----- While the speed is fast enough

      while { $Map(Speed)>$min } {

         #----- Damp the speed and calculate the displacement along the bearing

         if { $Damping } {
            set Map(Speed) [expr $Map(Speed)/$Map(Damping)]
         }
         set t1 $t2
         set t2 [clock click -milliseconds]
         set dx [expr $dx+$Map(Speed)*($t2-$t1)]
         set ll [projection function $Frame -circle $Lat $Lon $dx $Bearing]
         set Map(Lat) [lindex $ll 0]
         set Map(Lon) [lindex $ll 1]
         projection configure $Frame -location $Map(Lat) $Map(Lon)
         Page::Update $Frame

         #----- check if we should continue

         update
         if { ![projection is $Frame] || $Map(Grabbed)>$t0 } {
            set Map(Speed) 0.0
            return 0
         }
      }
      set Map(Speed) 0.0
      Page::Update $Frame
   }
   return 1
}

#----------------------------------------------------------------------------
# Nom      : <Viewport::GoAround>
# Creation : Juillet 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Anime le deplacement autour d'un point.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <Speed>   : Vitesse initiale
#  <Bearing> : Direction
#  <Lat>     : Latitude d'origine
#  <Lon>     : Longitude d'origine
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Viewport::GoAround { Frame Speed Lat Lon { Damping True } } {
   variable Map

   upvar #0 ProjCam::Data${Frame}::Cam  cam

   set ProjCam::Data(Name)  ""

   #----- Do only if we have the power to

   if { $OpenGL::Param(Res)==1 } {
      set fg  $cam(CFX)
      set dx  0
      set t2 [set t0  [clock click -milliseconds]]
      set min [expr abs($Speed/1000.0)]
      set Map(Speed) $Speed

      #----- Center on the localtion

      set Map(Lat) $Lat
      set Map(Lon) $Lon
      projection configure $Frame -location $Map(Lat) $Map(Lon)

      #----- While the speed is fast enough
      while { [expr abs($Map(Speed))]>$min } {

         #----- Damp the speed and calculate the displacement along the bearing

         if { $Damping } {
            set Map(Speed) [expr $Map(Speed)/$Map(Damping)]
         }
         set t1 $t2
         set t2 [clock click -milliseconds]

         #----- Get distance increment over time
         set dx [expr $dx+$Map(Speed)*($t2-$t1)]

         #----- Calculate angle of dispolacement
         set dg [expr ($dx/0.017453292519943295474371680598)/(3.141592653589793115997963468544*$cam(CFZ)*6378140/180.0)]

         set cam(CFX) [Viewport::CheckCoord [expr $fg+($dg)]]
         ProjCam::Do $Frame $Frame $Frame

         #----- check if we should continue

         update
         if {  ![projection is $Frame] || $Map(Grabbed)>$t0 } {
            set Map(Speed) 0.0
            return 0;
         }
      }
      set Map(Speed) 0.0
      Page::Update $Frame
   }
   return 1
}

#----------------------------------------------------------------------------
# Nom      : <Viewport::GoTo>
# Creation : Juillet 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Anime le deplacement vers un point.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <Lat>     : Latitude cible
#  <Lon>     : Longitude cible
#  <Zoom>    : Zoom cible
#  <From>    : Vecteur position de la camera
#  <To>      : Vecteur point focal de la camera
#  <Up>      : Vecteur direction haut de la camera
#  <Function>: Zoom mode (LINEAR,QUADRATIC,EXPONENTIAL ou "" pour defaut)
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Viewport::GoTo { Frame Lat Lon { Zoom 0 } { From {} } { To {} } { Up {} } { Function "" } } {
   variable Map

   upvar #0 ProjCam::Data${Frame}::Cam  cam

   set ProjCam::Data(Name)   ""
   
   #----- Stop Flybys
   set Animator::Fly(Length) 0
   set Animator::Fly(Path)   ""

   if { $Function=="" } {
     set Function $ProjCam::Param(Function)
   }

   #----- if we zoom, insert the previous zoom in the zoom back list
   if { $Zoom } {
      if { $Zoom<0 } {
         set Zoom [expr $cam(Lens)*abs($Zoom)]
      }
      if { $Zoom>$cam(Lens) } {
         lappend cam(LLens) [list $cam(Lens) $Viewport::Map(Lat) $Viewport::Map(Lon)]
      }
   }

   if { ![set F [llength $From]] } {
      set From $cam(From)
   }

   if { ![set T [llength $To]] } {
      set To $cam(To)
   }

   if { ![set U [llength $Up]] } {
      set Up $cam(Up)
   }

   #----- If we are far enough
   set isgeo [catch { set dp [projection function $Frame -dist [list $Map(Lat) $Map(Lon) $Lat $Lon] 0.0] }]

   #----- Do only if we have the power to
   if { $OpenGL::Param(Res)==1 && !$isgeo } {

      if { $dp>10 || $Zoom || $F || $T || $U } {

         Viewport::Resolution $Frame 2
         
         set l 1
         set dz 1
         set path [list [list [projcam configure $Frame -from] [projcam configure $Frame -to] [projcam configure $Frame -up] [projcam configure $Frame -lens]] [list $From $To $Up $Zoom]]

         #----- If location is out of the current view and the zoom is high enough
         if { $cam(Lens)>10 && $Zoom>10 && [expr $Zoom/$cam(Lens)]<10.0 && [$Page::Data(VP) -project $Lat $Lon 0.0]=="" } {
            set z [expr (6*6371000.0)/$dp]
            if { $z<[expr min($cam(Lens),$Zoom)] } {
               set l 2
               set dz [expr log($dp)]
               set path [linsert $path 1 [list $From $To $Up $z]]
            }
         }
         
         projcam define $Frame -path $path
         set dprr  1e32
         set dir   [expr [projection function $Frame -bearing $Map(Lat) $Map(Lon) $Lat $Lon]]
         set lat   $Map(Lat)
         set lon   $Map(Lon)

         set t0 [set Map(Grabbed) [clock click -milliseconds]]
         set Map(Speed) [expr 20.0/$Map(Delay)]
        
         #----- While we are not there yet
         set n  [expr 1.0/$Map(Speed)/$dz]
         set n2 [expr $n*0.5]
         set idx 0
         while { $idx<1.0 } {
            set i [expr $idx*$n]
            switch $Function {
               "EXPONENTIAL" { set spd [expr 1.0-(pow($i,10)*($n/pow($n,10)))/$n] }
               "QUADRATIC"   { set spd [expr 1.0-(($i-$n2)*($i-$n2)+($i-$n2))*($n/(($n2*$n2)+$n2))/$n] }
               "LINEAR"      { set spd 1.0 }
            }
            set idx [expr $idx+$spd*$Map(Speed)]

            #----- Calculate new position
            set ll [projection function $Frame -circle $lat $lon [expr $dp*$idx] $dir]
            set Map(Lat)  [lindex $ll 0]
            set Map(Lon)  [lindex $ll 1]

            #----- Check of overshoot
            if { [set dpr [projection function $Frame -dist [list $Map(Lat) $Map(Lon) $Lat $Lon] 0.0]]>$dprr } {
               break
            }
            set dprr $dpr
            
            #----- Move to new incremental position
            projcam define $Frame -fly [expr $idx*$l]
            projection configure $Frame -location $Map(Lat) $Map(Lon)
            Page::Update $Frame

            #----- Apply in movement proc
            if { $ProjCam::Param(Proc)!="" } {
               eval uplevel 2 \{ $ProjCam::Param(Proc) \}
            }
            
            #----- check if we should continue
            update
            if { ![projection is $Frame] || $Map(Grabbed)>$t0 } {
               break;
            }
         }

         #----- Got to final destination
         if { $Map(Grabbed)<=$t0 } {
            if { $F || $T || $U } {
               set cam(CFX)    0
               set cam(CFY)    0
               set cam(CFZ)    1
               projcam define  $Frame -circlefrom $cam(CFX) $cam(CFY) $cam(CFZ)
            }
            Viewport::Rotate $Frame $Lat $Lon $Zoom $From $To $Up False
         }
         set Map(Speed) 0.0
         Viewport::Resolution $Frame 1 $t0
         Page::Update $Frame
      }
   } else {
      Viewport::Rotate $Frame $Lat $Lon $Zoom $From $To $Up
      Viewport::Resolution $Frame 1
   }
}

proc Viewport::CloseUp { Frame Lat0 Lon0 Lat1 Lon1 { Off 0.0 } } {
   variable Data

   if { $Data(VP$Frame)!="" } {
      ProjCam::CloseUp $Frame $Frame $Data(VP$Frame) $Lat0 $Lon0 $Lat1 $Lon1 $Off
   }
}

#----------------------------------------------------------------------------
# Nom      : <Viewport::Resize>
# Creation : Avril 1998 - J.P. Gauthier - CMC/CMOE
#
# But      : Redimensionner la projection selon la grandeur de la fenetre.
#
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <VP>      : Identificateur du Viewport
#  <Width>   : Nouvelle largeur
#  <Height>  : Nouvelle hauteur
#  <Limit>   : Seulement le frame
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Viewport::Resize { Frame VP X0 Y0 X1 Y1 Limit } {
   variable Data

   set cv $Frame.page.canvas

   if { $X0==-999 } {
      set coo [$cv coords $VP]
      set X0  [lindex $coo 0]
      set Y0  [lindex $coo 1]
   }

   if { [set dx [expr $X1-$X0]]>180 } {
      set px [expr $Data(Width$VP)-$dx]
      set Data(Width$VP) $dx
   }  else {
      set px $Data(Width$VP)
      set X1 [expr $X0+$Data(Width$VP)]
   }

   if { [set dy [expr $Y1-$Y0]]>180 } {
      set py [expr $Data(Height$VP)-$dy]
      set Data(Height$VP) $dy
   }  else {
      set py $Data(Height$VP)
      set Y1 [expr $Y0+$Data(Height$VP)]
   }

   set Data(X$VP) $X0
   set Data(Y$VP) $Y0

   $cv itemconfigure $VP -x $X0 -y $Y0 -width $Data(Width$VP) -height $Data(Height$VP)

   if { $Data(Active$VP) } {
      $cv coords BSPAGE$VP $X1 $Y1
      $cv coords BMPAGE$VP [expr $X1-11] $Y1
      $cv coords BFPAGE$VP [expr $X1-22] $Y1
      $cv coords BDPAGE$VP $X1 $Y0
      $cv coords SCPAGE$VP [expr $X1-150-35] $Y1
   }
   Viewport::ResizeDepend $Frame $VP $px $py

   if { !$Limit } {
      $cv config -cursor watch
      update idletasks

      Page::Update $Frame

      $cv config -cursor left_ptr
   }
}

#----------------------------------------------------------------------------
# Nom      : <Viewport::ResizeDepend>
# Creation : Avril 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Redimensionner les items dependant de la projection.
#
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <VP>      : Identificateur du Viewport
#  <DX>      : Translation en X
#  <DY>      : Translation en Y
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Viewport::ResizeDepend { Frame VP DX DY } {

   #----- Check for databar full viewport toggle

   if { [llength [$Frame.page.canvas find withtag DB$VP]] } {
      if { $DataBar::Data(Full$VP) } {
         Shape::Full $Frame.page.canvas DB$VP DataBar::Full $Frame DB$VP $VP $DY
      }
   }
   #----- Check for colorbars full viewport toggle

   foreach item [$Frame.page.canvas find withtag CB$VP] {
      set tag [lindex [$Frame.page.canvas itemcget $item -tags] end]
      if { $ColorBar::Data(Full$tag) } {
         Shape::Full $Frame.page.canvas $tag ColorBar::Full $Frame.page.canvas $tag $VP $DX
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Viewport::MoveDepend>
# Creation : Avril 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Deplacer les items dependant de la projection.
#
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <VP>      : Identificateur du Viewport
#  <DX>      : Translation en X
#  <DY>      : Translation en Y
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Viewport::MoveDepend { Frame VP DX DY } {

   if { [llength [$Frame.page.canvas find withtag DB$VP]] && $DataBar::Data(Full$VP) } {
      Shape::Move $Frame.page.canvas DB$VP $DX $DY True
      DataBar::Move $Frame $VP DB$VP
      set DataBar::Data(Full$VP) 1
   }
   foreach item [$Frame.page.canvas find withtag CB$VP] {
      set tag [lindex [$Frame.page.canvas itemcget $item -tags] end]
      if { $ColorBar::Data(Full$tag) } {
         Shape::Move $Frame.page.canvas $tag $DX $DY True
         ColorBar::Move $Frame.page.canvas $tag
         set ColorBar::Data(Full$tag) 1
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Viewport::Setup>
# Creation : Octobre 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectue la creation des objets relatifs a une meme page
#
# Parametres   :
#  <Frame>     : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Viewport::Setup { Frame } {
   variable Data
   variable Map
   variable Resources

   if { [projection is $Frame] } {
      return
   }

   if { ![info exists Viewport::Data(Data$Frame)] } {
      set Data(Data$Frame) {}              ;#Liste des donnees
   }

   set Data(VP$Frame)               ""             ;#Viewport courant dans un frame
   set Map(Type$Frame)              [lindex [split $Map(Type) :] 0]     ;#Type de projection
   set Map(GeoRef$Frame)            [lindex [split $Map(Type) :] 1]     ;#Georef associee
   set Data(Seconds$Frame)          0              ;#Frame per second counter
   set ::Miniport::Data(Mini$Frame) {}             ;#Miniport table

   if { [lsearch -exact [font names] $Resources(Font)]==-1 } {
      font create FONT$Frame -family courier -weight bold -size -10
      set Resources(Font) FONT$Frame
   }

   #----- Creation des objects

   ProjCam::Create $Frame

   projection create $Frame
   projection configure $Frame -location $Map(Lat) $Map(Lon) -type $Map(Type$Frame) -georef $Map(GeoRef$Frame) \
      -mapres $Map(Res) -mask $Map(Mask) -scale $Map(Elev) \
      -mapcoast $Map(Coast) -maplake $Map(Lake) -mapriver $Map(River) -mappolit $Map(Polit) -mapadmin $Map(Admin) \
      -mapcity $Map(City) -maproad $Map(Road) -maprail $Map(Rail) -maptopo $Map(Topo) -mapplace $Map(Place) -perspective $Map(Perspective) \
      -maptext $Map(Text) -mapcoord [expr $Map(Coord)*$Map(CoordLoc)] $Map(CoordDef) $Map(CoordNum) -data $Data(Data$Frame) -date $Data(Seconds$Frame)
}

#----------------------------------------------------------------------------
# Nom      : <Viewport::UnSetup>
# Creation : Avril 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectue la suppression des objets relatifs a une meme page
#
# Parametres   :
#  <Frame>     : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Viewport::UnSetup { Frame } {
   variable Data
   variable Map

   if { ![projection is $Frame] } {
      return
   }

   unset Data(VP$Frame)
   unset Map(Type$Frame)
   unset Map(GeoRef$Frame)
   unset Data(Seconds$Frame)

   #----- Suppression des objects

   projection destroy $Frame
   projcam destroy $Frame
   catch { font delete FONT$Frame }
}

#----------------------------------------------------------------------------
# Nom      : <Viewport::LinkSet>
# Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialiser le viewport source pour les liens.
#
# Parametres   :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Viewport::LinkSet { } {
   variable Data

   set Data(Link) [list $Page::Data(Frame) $Data(VP)]
}

#----------------------------------------------------------------------------
# Nom      : <Viewport::Link>
# Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Lier le viewport courant avec le viewport source.
#
# Parametres   :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Viewport::Link { } {
   variable Data


   set vp [lindex $Data(Link) 1]
   lappend Data(Linked$vp) [list $Page::Data(Frame) $Data(VP)]
   set Data(Link$Data(VP)) $Data(Link)

   Viewport::UpdateData $Page::Data(Frame) $Data(VP)
}

#----------------------------------------------------------------------------
# Nom      : <Viewport::LinkDo>
# Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Affectuer la liason de donnees.
#
# Parametres   :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Viewport::LinkDo { VP } {
   variable Data

   if { $VP!="" } {
      foreach link $Data(Linked$VP) {
         set frame [lindex $link 0]
         set vpdst [lindex $link 1]
         if { [winfo exists $frame] && [llength [$frame.page.canvas find withtag $vpdst]] } {
            $frame.page.canvas itemconf $vpdst -data $Animator::Play(Data)
         }
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Viewport::UnLink>
# Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : DeLier le viewport courant.
#
# Parametres   :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Viewport::UnLink { } {
   variable Data

   if { [set vp [lindex $Data(Link$Data(VP)) 1]]!="" } {
      set idx [lsearch -exact $Data(Linked$vp) [list $Page::Data(Frame) $Data(VP)]]
      set Data(Linked$vp) [lreplace Data(Linked$vp) $idx $idx]
   }
   set Data(Link$Data(VP)) {}

   Viewport::UpdateData $Page::Data(Frame) $Data(VP)
}

#----------------------------------------------------------------------------
# Nom      : <Viewport::UpdateData>
# Creation : Janvier 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectue la mise a jours des donnees.
#
# Parametres   :
#  <Frame>     : Identificateur de Page
#  <VP>        : Liste des viewports a mette a jour
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Viewport::UpdateData { Frame { VP { } } } {
   variable Data
   variable Map

   if { ![llength $VP] } {
      set VP [Page::Registered $Frame Viewport]
   }

   if { ![winfo exists $Frame.page.canvas] } {
      return
   }

   #----- Faire un update de tous les viewports
   foreach vp $VP {
      if { [info exists ::Viewport::Data(Link$vp)] && [llength $Data(Link$vp)] } {
         set frame [lindex $Data(Link$vp) 0]
         set vpsrc [lindex $Data(Link$vp) 1]
         if { [winfo exists $frame] && [llength [$frame.page.canvas find withtag $vpsrc]] } {
            $Frame.page.canvas itemconf $vp -data [lindex [$frame.page.canvas itemconf $vpsrc -data] end]
         } else {
            $Frame.page.canvas itemconf $vp -data {}
         }
      } else {
         $Frame.page.canvas itemconf $vp -data [concat $Data(Data$vp) [FieldCalc::Operand $vp $Data(Data$vp)]]
      }
   }

   #----- Force parameter refresh (after viewport assign to get view dependent params)
   FSTD::ParamGet

   foreach mini $Miniport::Data(Mini$Frame) {
      Miniport::UpdateData $Frame $mini $VP
   }
   update idletasks

   #----- Dans le cas d'une projection "GRID", reevaluer
   Viewport::ForceGrid $Frame
}

#------------------------------------------------------------------------------
# Nom      : <Viewport::Write>
# Creation : Novembre 2003 - J.P. Gauthier - CMC/CMOE -
#
# But     : Engeristrer les parametres des Viewport dans un fichier Layout
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <File>   : Identificateur de Fichier
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Viewport::Write { Frame File } {
   variable Data
   variable Map
   variable MapDef

   set vps [Page::Registered $Frame Viewport]

   if { [llength $vps] } {
      set vp [lindex $vps 0]

      puts $File "   #----- Affichage des Viewports"
      puts $File ""

      if { $Map(GeoRef$Frame)!="" } {
         set def $Map(GeoRef$Frame)
         puts $File "   set Viewport::MapDef($def) \{ $MapDef($def) \}"
         puts $File "   set Viewport::Map(Type) \"grid:$def\""
      } else {
         puts $File "   set Viewport::Map(Type)        \"[projection configure $Frame -type]\""
      }
      puts $File "   set Viewport::Map(Perspective) \"[projection configure $Frame -perspective]\""
      puts $File "   set Viewport::Map(Data)        \"[projection configure $Frame -data]\""
      puts $File "   set Viewport::Map(Elev)        \"[projection configure $Frame -scale]\""
      puts $File "   set Viewport::Map(Delay)       $Viewport::Map(Delay)"
      puts $File "   set Viewport::Map(Damping)     $Viewport::Map(Damping)"

      set coo [projection configure $Frame -mapcoord]
      puts $File "   set Viewport::Map(Coord)       [expr abs([lindex $coo 0])]"
      puts $File "   set Viewport::Map(CoordLoc)    [expr [lindex $coo 0]<0?-1:1]"
      puts $File "   set Viewport::Map(CoordDef)    [lindex $coo 1]"
      puts $File "   set Viewport::Map(CoordNum)    [lindex $coo 2]"
      puts $File "   set Viewport::Map(Res)         [projection configure $Frame -mapres]"
      puts $File "   set Viewport::Map(Mask)        [projection configure $Frame -mask]"
      puts $File "   set Viewport::Map(Coast)       [projection configure $Frame -mapcoast]"
      puts $File "   set Viewport::Map(Lake)        [projection configure $Frame -maplake]"
      puts $File "   set Viewport::Map(River)       [projection configure $Frame -mapriver]"
      puts $File "   set Viewport::Map(Admin)       [projection configure $Frame -mapadmin]"
      puts $File "   set Viewport::Map(City)        [projection configure $Frame -mapcity]"
      puts $File "   set Viewport::Map(Polit)       [projection configure $Frame -mappolit]"
      puts $File "   set Viewport::Map(Place)       [projection configure $Frame -mapplace]"
      puts $File "   set Viewport::Map(Road)        [projection configure $Frame -maproad]"
      puts $File "   set Viewport::Map(Rail)        [projection configure $Frame -maprail]"
      puts $File "   set Viewport::Map(Topo)        [projection configure $Frame -maptopo]"
      puts $File "   set Viewport::Map(Bath)        [projection configure $Frame -mapbath]"
      puts $File "   set Viewport::Map(Text)        [projection configure $Frame -maptext]"
      puts $File "   set Viewport::Resources(Bkg)       \"[lindex [$Frame.page.canvas itemconf $vp -bg] 4]\""
      puts $File "   set Viewport::Resources(BD)        \"[lindex [$Frame.page.canvas itemconf $vp -bd] 4]\""
      puts $File "   set Viewport::Resources(FillCoast) \"[lindex [$Frame.page.canvas itemconf $vp -colorfillcoast] 4]\""
      puts $File "   set Viewport::Resources(FillLake)  \"[lindex [$Frame.page.canvas itemconf $vp -colorfilllake] 4]\""
      puts $File "   set Viewport::Resources(Coast)     \"[lindex [$Frame.page.canvas itemconf $vp -colorcoast] 4]\""
      puts $File "   set Viewport::Resources(Lake)      \"[lindex [$Frame.page.canvas itemconf $vp -colorlake] 4]\""
      puts $File "   set Viewport::Resources(River)     \"[lindex [$Frame.page.canvas itemconf $vp -colorriver] 4]\""
      puts $File "   set Viewport::Resources(Polit)     \"[lindex [$Frame.page.canvas itemconf $vp -colorpolit] 4]\""
      puts $File "   set Viewport::Resources(Place)     \"[lindex [$Frame.page.canvas itemconf $vp -colorplace] 4]\""
      puts $File "   set Viewport::Resources(Admin)     \"[lindex [$Frame.page.canvas itemconf $vp -coloradmin] 4]\""
      puts $File "   set Viewport::Resources(City)      \"[lindex [$Frame.page.canvas itemconf $vp -colorcity] 4]\""
      puts $File "   set Viewport::Resources(Road)      \"[lindex [$Frame.page.canvas itemconf $vp -colorroad] 4]\""
      puts $File "   set Viewport::Resources(Rail)      \"[lindex [$Frame.page.canvas itemconf $vp -colorrail] 4]\""
      puts $File "   set Viewport::Resources(Coord)     \"[lindex [$Frame.page.canvas itemconf $vp -colorcoord] 4]\""
      puts $File "   set Viewport::Resources(DashCoord) \"[lindex [$Frame.page.canvas itemconf $vp -dashcoord] 4]\""

      set no 1
      foreach vp $vps {
         set Data(Alias$vp) Data(Viewport[format "%03i" $no])
         puts $File "   set $Data(Alias$vp) \[Viewport::Create \$Frame $Data(X$vp) $Data(Y$vp) $Data(Width$vp) $Data(Height$vp) $Data(Active$vp) $Data(Full$vp)\]"
         incr no
      }
      puts $File ""
   }
}
