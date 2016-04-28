#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie d'objects visuel interactifs
# Fichier  : Graph.tcl
# Creation : Octobre 2003 - J.P. Gauthier - CMC/CMOE
#
# Description: Ce package s'occupe de l'affichage et de la manipulation de graphs
#
# Fonctions:
#
#    Graph::Activate           { Frame Graph Type }
#    Graph::DeActivate         { Graph Type }
#    Graph::Resize             { Frame Graph X0 Y0 X1 Y1 Limit }
#    Graph::Resolution         { Frame Type Res }
#    Graph::Configure          { }
#    Graph::Idle               { Graph Type }
#    Graph::UnIdle             { Graph Type }
#    Graph::Destroy            { Frame { Graph "" } { Type "" } }
#    Graph::LocTool            { Graph Type Array Index Op }
#    Graph::Mode               { Graph Type { Zoom False } }
#    Graph::Params             { { Graph "" } { Type "" } { Force False } }
#    Graph::ParamsOff          { Frame Type Graph }
#    Graph::ItemPos            { Frame VP Coords Desc Tag { Type POINT } { Marks {} } }
#    Graph::ItemConfigure      { Graph Type Item }
#    Graph::ItemSelect         { Item }
#    Graph::Labels             { Graph Type Title UnitX UnitY }
#    Graph::ParamsItem         { Parent }
#    Graph::ParamsPos          { Parent }
#    Graph::ParamsScaleUniform { Graph Type { Update True } }
#    Graph::PosAdd             { Graph Type }
#    Graph::PosSet             { Graph Type }
#    Graph::PosDel             { Graph Type }
#    Graph::PosDelAll          { Graph Type }
#    Graph::PosSelect          { Graph Type }
#    Graph::PosSave            { Type Graph Pos }
#    Graph::PosDelete          { Type Graph Pos }
#    Graph::ParamsGraph        { Parent }
#    Graph::ParamsObs          { Parent Type Graph }
#    Graph::ParamsObsSelect    { Type Graph Desc }
#    Graph::ParamsObsSearch    { Type Graph }
#    Graph::RangeDraw          { Type Graph Place Id Y }
#    Graph::Translate          { Frame Type Graph X Y }
#    Graph::TranslateDone      { Frame Type Graph }
#    Graph::TranslateInit      { Frame Type Graph X Y }
#    Graph::Update             { Frame }
#    Graph::UpdateItems        { Frame }
#    Graph::TimeFormat         { Sec Mode { From 0 } }
#    Graph::ValFormat          { Order Val }
#    Graph::Write              { Frame File }
#    Graph::ZoomScroll         { Type Graph X Y Incr { Centered True } }
#    Graph::Zoom               { Type Graph { Incr 0 } }
#    Graph::ZoomReset          { Type Graph }
#    Graph::ZoomBox            { Canvas X1 Y1 }
#    Graph::ZoomInit           { Canvas X0 Y0 }
#    Graph::DrawInit           { Frame VP }
#    Graph::Draw               { Frame VP }
#    Graph::DrawDone           { Frame VP }
#    Graph::MoveInit           { Frame VP }
#    Graph::Move               { Frame VP }
#    Graph::MoveDone           { Frame VP }
#    Graph::VertexAdd          { Frame VP X Y }
#    Graph::VertexDelete       { Frame VP }
#    Graph::VertexFollow       { Frame VP X Y Scan }
#    Graph::VertexSample       { Type Graph Coord { Res 0 } }
#    Graph::VertexResolution   { Type Graph  }
#
#===============================================================================

package provide Graph 2.0

catch { SPI::Splash "Loading Widget Package Graph 2.0" }

namespace eval Graph {
   variable Font
   variable Grid
   variable Color
   variable Width
   variable Data
   variable Graph
   variable Lbl
   variable Msg
   variable Error
   variable Item
   variable Bubble
   variable Param

   set Font(Select)    [font create -family courier -size -12  -weight bold]
   set Font(Axis)      [font create -family courier -size -10  -weight bold]
   set Font(Graph)     [font create -family courier -size -14  -weight bold]

   set Color(Select) #FF0000
   set Color(Axis)   #000000
   set Color(Fill)   #EEEEEE
   set Color(BG)     #FFFFFF
   set Color(FG)     #000000
   set Color(Scale)  #000000
   set Color(Footer) #000000
   set Color(Header) #000000
   set Color(Unit)   #000000
   set Color(Graph)  #FFFFFF
   set Color(Frame)  #FFFFFF

   set Grid(XColor)   #C7C7C7
   set Grid(XWidth)   0
   set Grid(XDash)    "."
   set Grid(YColor)   #C7C7C7
   set Grid(YWidth)   0
   set Grid(YDash)    "."

   set Width(Frame)  1

   set Param(Dock)        True
   set Param(Geom)        { 175x630+[winfo rootx .]+[winfo rooty .] }

   set Param(AxisFormats)     { NONE FIT INTEGER FLOAT EXPONENT }
   set Param(AxisFormatsTime) { NONE DATE TIME DATETIME TIME/DATE 00HH/DDMM 00HH/MMDD HH/DDMM HH HHMM DDMM MMDD T-HH T+HH T+HHMM }
   set Param(AxisTypes)       { LINEAR LOG LN }
   set Param(AxisZs)          { GRID PRESSURE MASL }
   set Param(SelectMode)      POINT
   set Param(SelectType)      AVG

   set Param(NONE)            ""
   set Param(LOCATION)        ""
   set Param(POINT)           { }
   set Param(LINE)            { }
   set Param(BOX)             { }
   set Param(POLYGON)         { }

   set Data(Nb)           0                      ;#Nombre de graph
   set Data(Type)         ""                     ;#Type du graph courant
   set Data(Types)        "Contingency Scatter Profile Time Section TimeSection Hovmoller" ;#Liste des types de graph
   set Data(Graph)        ""                     ;#Id du graph courant
   set Data(GraphParams)  ""                     ;#Id du graph courant
   set Data(ResBest)      True                   ;#Selection de la resolution
   set Data(Res)          100000                 ;#Resolution en metres
   set Data(Show)         False
   set Data(ShowCoord)    True
   set Data(ShowGrid)     False
   set Data(ToolMode)     Data
   set Data(Update)       True
   set Data(IP3)          False                  ;#Valider les IP3
   set Data(Item)         ""
   set Data(Pos)          ""
   set Data(PosNo)        -1
   set Data(Frame)        ""
   set Data(Stats)       { R2 VARx VARy VARxy RMSE NRMSE ME NME MNE LMNE MB NMB MNB LMNB MFB MFE a b Ea Eb AVGx AVGy MINx MAXx MINy MAXy NA RNA n }
   set Data(Funcs)       { scor svarx svary scov srmse snrmse sme snme smne slmne smb snmb smnb slmnb smfb smfe srega sregb serra serrb savgx savgy sminx smaxx sminy smaxy sna srna snb }

   set Graph(ParamsGraph) True
   set Graph(ParamsAxisX) True
   set Graph(ParamsAxisY) True
   set Graph(ParamsItem)  True
   set Graph(ParamsObs)   False
   set Graph(Identitys)   { A B C D E F G H I J K L M N O P Q R S T U V W X Y Z }
   set Graph(Dashs)       { \"\" "_" "." ".._" "..._" "...__" }

   set Graph(Stipples) "@$GDefs(Dir)/share/bitmap/raydiagleft16.xbm
                        @$GDefs(Dir)/share/bitmap/raydiagright16.xbm
                        @$GDefs(Dir)/share/bitmap/rayhor16.xbm
                        @$GDefs(Dir)/share/bitmap/rayver16.xbm
                        @$GDefs(Dir)/share/bitmap/grey06.xbm
                        @$GDefs(Dir)/share/bitmap/grey12.xbm
                        @$GDefs(Dir)/share/bitmap/raydiagleft16.xbm
                        @$GDefs(Dir)/share/bitmap/raydiagright16.xbm
                        @$GDefs(Dir)/share/bitmap/rayhor16.xbm
                        @$GDefs(Dir)/share/bitmap/rayver16.xbm"

   set Graph(Icons)    { NONE TRIANGLE SQUARE VBAR HBAR CIRCLE LOZENGE PENTAGON HEXAGON BARB }
   set Graph(Colors)   { #FF0000 #00FF00 #0000FF #FFF300 #00FFF3 #E600FF #FF8C00 #804F4F #8CFF40 #B5A36B #80F9FF #A66BFF }

   set Item(No)          0
   set Item(Outline)     #000000
   set Item(FillColor)   #FFFFFF
   set Item(Tranparency) 100
   set Item(Width)       1
   set Item(Size)        1
   set Item(Value)       False
   set Item(Type)        LINE
   set Item(Dash)        ""
   set Item(Font)        [font create -family courier -size -10  -weight bold]
   set Item(Bitmap)      ""
   set Item(Image)       ""
   set Item(Stipple)     -1
   set Item(Types)       { NONE LINE SPLINE BAR WIDEBAR HISTOGRAM RASTER }
   set Item(Icon)        NONE

   #----- Definitions des labels

   set Lbl(Add)        { "Ajouter" "Add" }
   set Lbl(Cancel)     { "Annuler" "Cancel" }
   set Lbl(Del)        { "Supprimer" "Delete" }
   set Lbl(Item)       { "Item" "Item" }
   set Lbl(Graph)      { "Graphique" "Graph" }
   set Lbl(Outline)    { "Ligne" "Line" }
   set Lbl(Fill)       { "Remplissage" "Fill" }
   set Lbl(Icon)       { "Icône" "Icon" }
   set Lbl(Type)       { "Type        " "Type        " }
   set Lbl(Title)      { "Titre" "Title" }
   set Lbl(Value)      { "Valeurs" "Values" }
   set Lbl(Update)     { "Info auto" "Info update" }
   set Lbl(Save)       { "Sauvegarder ..." "Save ..." }
   set Lbl(Del)        { "Supprimer ..." "Delete ..." }
   set Lbl(Yes)        { "Oui" "Yes" }
   set Lbl(No)         { "Non" "No" }

   set Lbl(Font)       { "Police" "Font" }
   set Lbl(Info)       { "Info" "Info" }
   set Lbl(Axis)       { "Axe" "Axis" }
   set Lbl(Grid)       { "Marqueurs" "Markers" }
   set Lbl(Unit)       { "Unité" "Unit" }
   set Lbl(Since)      { "Depuis" "Since" }
   set Lbl(FitLinear)  { "Régression Linéaire" "Linear regression" }
   set Lbl(Params)     { "Parametres" "Parameters" }

   set Lbl(Background) { "Arrière" "Background" }
   set Lbl(Frame)      { "Pourtour" "Frame" }
   set Lbl(Color)      { "Couleur" "Color" }
   set Lbl(Count)      { "Nombre" "Count" }
   set Lbl(Cut)        { "Coupes" "Cuts" }
   set Lbl(Data)       { "Données"  "Data" }
   set Lbl(Date)       { "Date"  "Date" }
   set Lbl(Day)        { "Jour"  "Day" }
   set Lbl(Disp)       { "Affichage" "Display" }
   set Lbl(From)       { "De" "From" }
   set Lbl(Hour)       { "Heure" "Hour" }
   set Lbl(IP3)        { "Valider IP3" "Validate IP3" }
   set Lbl(Level)      { "Niveau" "Level" }
   set Lbl(Limit)      { "Délimitation" "Limiting range" }
   set Lbl(Lin)        { "Linéaire" "Linear" }
   set Lbl(Log)        { "Logarithmique" "Logarithmic" }
   set Lbl(Min)        { "Min"   "Min" }
   set Lbl(Obs)        { "Observations" "Observations" }
   set Lbl(Points)     { "Points" "Vertex" }
   set Lbl(Pos)        { "Position" "Position" }
   set Lbl(WindGeo)    { "Direction géographique des vents (Nord vers le haut)" "Geographical direction of winds (North is up)" }
   set Lbl(Wind3D)     { "Direction vertical des vents" "Vertical direction of winds" }
   set Lbl(Proj)       { "Afficher dans la vue" "Display in viewport" }
   set Lbl(Projected)  { "Projeté" "Projected" }
   set Lbl(Section)    { "Section" "Section" }
   set Lbl(Res)        { "Résolution (m)" "Resolution (m)" }
   set Lbl(Same)       { "Echelle Uniforme" "Uniform scale" }
   set Lbl(Scale)      { "Échelle" "Scale" }
   set Lbl(Sec)        { "Sec"   "Sec" }
   set Lbl(Select)     { "Sélection" "Select" }
   set Lbl(Stat)       { "Statistiques" "Statistics" }
   set Lbl(To)         { "A " "To  " }
   set Lbl(Val)        { "Valeur" "Value" }
   set Lbl(Values)     { "Valeurs aux points" "Local values" }
   set Lbl(TimeMatch)  { "Valider les temps" "Validate time" }
   set Lbl(Inter)      { "Intervalles" "Intervals" }
   set Lbl(Format)     { "Format" "Format" }
   set Lbl(Decimals)   { "Décimales" "Decimals" }
   set Lbl(ZType)      { "Elévation" "Height" }
   set Lbl(Angle)      { "Angle" "Angle" }
   set Lbl(PRESSURE)   { "Pression" "Pressure" }
   set Lbl(MASL)       { "Metres au dessus du niveaux de la mer" "Meters above sea level" }
   set Lbl(Average)    { "Moyenne" "Average" }
   set Lbl(Minimum)    { "Minimum" "Minimum" }
   set Lbl(Maximum)    { "Maximum" "Maximum" }
   set Lbl(Median)     { "Mediane" "Median" }
   set Lbl(StdDev)     { "Ecart type" "Standard deviation" }
   set Lbl(Nb)         { "Nombre de valeur" "Sample number" }

   #----- Messages

   set Msg(Reading)    { "Lecture des données" "Reading data" }
   set Msg(Extracting) { "Extraction des données" "Extracting data" }
   set Msg(PosSave)    { "Veuillez spécifier le nom de la position" "Please enter the position name" }
   set Msg(PosExist)   { "Cette position existe déja, voulez-vous la remplacer ?" "This position already exist, do you want to replace it" }
   set Msg(PosDel)     { "Voulez-vous vraiment supprimer la position ?" "Do you really want to delete this position ?" }

   #----- Bulles d'aides

   set Bubble(Back)      { "Paramêtres de l'arrière plan du graph" "Graph background parameters" }
   set Bubble(Frame)     { "Paramêtres du contour du graph" "Graph frame parameters" }
   set Bubble(Pick)      { "Paramêtres de la sélection interactive" "Interactive selection parameters" }
   set Bubble(Mode)      { "Mode de sélection de coordonées dans la projection" "Coordinate selection mode" }
   set Bubble(Reset)     { "Réinitialiser le graph" "Reinitialize the graph" }
   set Bubble(Sheet)     { "Affichage des données du graph" "Display graph data" }
   set Bubble(Save)      { "Sauvegarde des données en format RPN" "Save the data in RPN format" }
   set Bubble(Update)    { "Mise-à-jour automatique des unités et information à partir des données" "Automatic update of graph units and information from data source" }

   set Bubble(ItemList)  { "Liste des items par sélection de position" "List of itmes per position selection" }
   set Bubble(ItemLine)  { "Paramêtres d'affichage des lignes" "Line parameters" }
   set Bubble(ItemFill)  { "Paramêtres de remplissage" "Polygon filling parameters" }
   set Bubble(ItemIcon)  { "Paramêtres des icônes" "Icon parameters" }
   set Bubble(ItemType)  { "Type d'affichage de l'item" "Item display type" }
   set Bubble(ObsList)   { "Liste des stations d'observations disponibles" "List of available observation stations" }
   set Bubble(ObsSearch) { "Recherche d'une station dans la liste" "Station search through the list" }
   set Bubble(PosList)   { "Liste des sélections de position" "Position selection list" }
   set Bubble(PosAdd)    { "Ajout d'une sélection de position" "Add a position selection" }
   set Bubble(PosDel)    { "Suppression de la sélection de position courante" "Delete the current position sélection" }
   set Bubble(Info)      { "Paramêtres de l'entête" "Header parameters" }
   set Bubble(Viewport)  { "Affichage des données sur la projection active" "Display the data on the active viewport" }
   set Bubble(Sample)    { "Sélection de la distance entre les profils" "Select the sampling distance in km" }
   set Bubble(Fit)       { "Affichage de la courbe de régression" "Displays the fitting curve" }
   set Bubble(Select)    { "Affichage d'un interval de sélection" "Displays an interval of selection" }
   set Bubble(Stat)      { "Affichage de statistiques relatives au graph" "Displays graph statistics" }
   set Bubble(IP3)       { "Validation du IP3 lors de la recherche des champs temporels" "Validate IP3 when looking ro temporal fields" }
   set Bubble(Date0)     { "Spécification de la date de début\nformat: YYYMMDD HHMMSS" "Specify the start date\nformat: YYYMMDD HHMMSS" }
   set Bubble(Date1)     { "Spécification de la date de fin\nformat: YYYMMDD HHMMSS" "Specify the end date\nformat: YYYMMDD HHMMSS" }
   set Bubble(Uniform)   { "Permet de fixer l'échelle des deux axes aux mêmes intervals" "Fix the two scales to the same intervals" }
   set Bubble(TimeMatch) { "Vérification de la concordandes des temps entre les paires de données" "Make sure time is matching between sets" }

   set Bubble(AxisInter)     { "Spécification des intervals de l'axe" "Specify the axis intervals" }
   set Bubble(AxisType)      { "Fonction de distribution des valeurs de l'axe" "Axis value distribution function" }
   set Bubble(AxisZType)     { "Type de coordonnée verticale" "Vertical coordinate type" }
   set Bubble(AxisFormat)    { "Format des valeurs de l'axe" "Axis value format" }
   set Bubble(AxisDecimals)  { "Nombre de décimales des valeurs de l'axe" "Number of decimals for axis values" }
   set Bubble(AxisMark)      { "Paramêtres de marquage des axes" "Axis markers parameters" }
   set Bubble(AxisAngle)     { "Angle des libelles" "Labels angle" }

   #----- Erreurs

   set Error(Pressure)  { "Impossible de calculer les niveaux de pressions. Vérifiez que les descripteurs verticaux sont bien dans le fichier." "Impossible to calculate pressure levels. Make sure vertical descriptors are in the file." }
   set Error(Meter)     { "Impossible de calculer les niveaux MASL. Vérifiez que les champs GZ sont bien dans le fichier." "Impossible to calculate MASL levels. Make sure GZ fields are in the file." }
   set Error(NbData)    { "Les pas de temps entre les items pour le calcul ne correspondent pas" "Time step don't correspond for calculus" }
}

package require MetStat

source $GDefs(Dir)/tcl/Lib/Graph_Time.tcl
source $GDefs(Dir)/tcl/Lib/Graph_Scatter.tcl
source $GDefs(Dir)/tcl/Lib/Graph_Profile.tcl
source $GDefs(Dir)/tcl/Lib/Graph_Contingency.tcl
source $GDefs(Dir)/tcl/Lib/Graph_Section.tcl
source $GDefs(Dir)/tcl/Lib/Graph_Frequence.tcl
source $GDefs(Dir)/tcl/Lib/Graph_Compare.tcl
source $GDefs(Dir)/tcl/Lib/Graph_TimeSection.tcl
source $GDefs(Dir)/tcl/Lib/Graph_Stat.tcl
source $GDefs(Dir)/tcl/Lib/Graph_Hovmoller.tcl

if { [file exists $env(HOME)/.spi/Graph] } {
   source $env(HOME)/.spi/Graph
}

#----------------------------------------------------------------------------
# Nom      : <Graph::Activate>
# Creation : Janvier 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Mettre un graph actif et desactiver le precedent
#
# Parametres :
#   <Frame>  : Indentificateur de Page
#   <Graph>  : Indentificateur du Graph
#   <Type>   : Type du Graph
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Graph::Activate { Frame Graph Type } {
   variable Data

   if { $Frame=="" } {
      return
   }

   #----- Desactiver le graph precedent

   if { $Data(Type)!=""  && $Data(Graph)!="" && $Data(Graph)!=$Graph } {
      eval Graph::DeActivate $Data(Graph) $Graph::Data(Type)
   }
   set Data(Type) $Type

   #----- Si pas de graph, selectionner celui defaut du frame

   if { $Graph!="" } {
      set Data(Graph) $Graph
    }

   #----- Activer le viewport courant

   Page::ActiveWrap $Frame $Data(Graph)

   #----- Recuperer et instaurer ses parametres

   if { $Data(Graph)!="" && $Data(Active$Graph) } {
      set Data(Show) True
      Graph::Params $Data(Graph) $Data(Type)

      if { [array exists Graph::$Data(Type)::$Data(Type)$Data(Graph)::Graph] && [info exists Graph::$Data(Type)::$Data(Type)$Data(Graph)::Data(Pos)] } {
         eval trace add variable SPI::Src(Info) write \{ Graph::LocTool \$Graph::Data(Graph) $Type \}
         Graph::PosSet $Data(Graph) $Data(Type)
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Graph::DeActivate>
# Creation : Janvier 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Mettre un graph actif et desactiver le precedent
#
# Parametres :
#   <Graph>  : Indentificateur du Graph
#   <Type>   : Type du Graph
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Graph::DeActivate { Graph Type } {

   upvar #0 Graph::${Type}::${Type}${Graph}::Data  data

   #----- Desactiver le viewport precedent

   Page::ActiveUnWrap $data(Frame) $Graph
   eval trace vdelete SPI::Src(Info) w \{ Graph::LocTool \$Graph::Data(Graph) $Type \}
}

#----------------------------------------------------------------------------
# Nom      : <Graph::Labels>
# Creation : Fevrier 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Changer les titres et unites d'un graph intercatif
#
# Parametres :
#   <Graph>  : Indentificateur du Graph
#   <Type>   : Type du Graph
#   <Title>  : Titre du Graph
#   <UnitX>  : Unite en X
#   <UnitY>  : Unite en Y
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Graph::Labels { Graph Type Title UnitX UnitY } {

   upvar #0 Graph::${Type}::${Type}${Graph}::Data  data

   $data(Canvas) itemconfigure [lindex [$data(Canvas) itemconfigure $Graph -title] end] -text $Title
   $data(Canvas) itemconfigure [graphaxis configure axisx$Graph -unit] -text $UnitX
   $data(Canvas) itemconfigure [graphaxis configure axisy$Graph -unit] -text $UnitY
}

proc Graph::WindLabel { Field } {
   global GDefs
   variable Lbl
   
   set wlbl ""
   
   #----- Wind direction label
   if { [fstdfield is $Field] && [fstdfield configure $Field -rendervector]!="NONE" } {
      switch [fstdfield stats $Field -component] {
         1 { set wlbl "" }
         2 { set wlbl "\n\n[lindex $Lbl(WindGeo) $GDefs(Lang)]" }
         3 { set wlbl "\n\n[lindex $Lbl(Wind3D) $GDefs(Lang)]" }
      }
   }
   return $wlbl
}

#----------------------------------------------------------------------------
# Nom      : <Graph::Resize>
# Creation : Avril 1998 - J.P. Gauthier - CMC/CMOE
#
# But      : Redimensionner la projection selon la grandeur de la fenetre.
#
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <Graph>   : Identificateur du Graph
#  <Width>   : Nouvelle largeur
#  <Height>  : Nouvelle hauteur
#  <Limit>   : Seulement le frame
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Graph::Resize { Frame Graph X0 Y0 X1 Y1 Limit } {
   variable Data

   set cv $Frame.page.canvas

   if { $X0==-999 } {
      set coo [$cv coords $Graph]
      set X0 [lindex $coo 0]
      set Y0 [lindex $coo 1]
   }

   if { [set dx [expr $X1-$X0]]>80 } {
      set px [expr $Data(Width$Graph)-$dx]
      set Data(Width$Graph) $dx
   }  else {
      set px $Data(Width$Graph)
      set X1 [expr $X0+$Data(Width$Graph)]
   }

   if { [set dy [expr $Y1-$Y0]]>80 } {
      set py [expr $Data(Height$Graph)-$dy]
      set Data(Height$Graph) $dy
   }  else {
      set py $Data(Height$Graph)
      set Y1 [expr $Y0+$Data(Height$Graph)]
   }

   set Data(X$Graph)      $X0
   set Data(Y$Graph)      $Y0

   catch { $cv coords $Graph $X0 $Y0 $X1 $Y1 }
   catch { $cv itemconfigure $Graph -x $X0 -y $Y0 -width $Data(Width$Graph) -height $Data(Height$Graph) }

   if { $Data(Active$Graph) } {
      $Frame.page.canvas coords BSPAGE$Graph $X1 $Y1
      $Frame.page.canvas coords BMPAGE$Graph [expr $X1-11] $Y1
      $Frame.page.canvas coords BFPAGE$Graph [expr $X1-22] $Y1
      $Frame.page.canvas coords BDPAGE$Graph $X1 $Y0
   }

   if { !$Limit } {
      $cv config -cursor watch
      update idletasks

      set type $Data(Type$Graph)
      if { [info procs ::Graph::${type}::Page]!="" } {
         Graph::${type}::Page $Graph
      }
      Graph::${type}::Graph $Graph

      update idletasks
      $cv config -cursor left_ptr
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
#  <Type>      : Type de Graph
#  <Res>       : Resolution
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Graph::Resolution { Frame Type Res } {

   glrender -resolution $Res

   if { $Res==1 } {
      foreach graph [Page::Registered $Frame Graph::${Type}] {
         $Frame.page.canvas itemconf $graph -update True
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Configure>
# Creation : Avril 2003 - J.P. Gauthier - CMC/CMOE -
#
# But      : Configure les parametres globaux des graphs.
#
# Parametres :
#
#-------------------------------------------------------------------------------

proc Graph::Configure { } {
   variable Data

   if { $Data(Type)!="" && $Data(Graph)!="" } {
      Graph::$Data(Type)::Graph $Data(Graph)
   }
}

#----------------------------------------------------------------------------
# Nom      : <Graph::Idle>
# Creation : Janvier 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Configurer le curseur pour inqiquer un travail en cours
#
# Parametres :
#   <Graph>  : Indentificateur du Graph
#   <Type>   : Type du Graph
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Graph::Idle { Graph Type } {

   upvar #0 Graph::${Type}::${Type}${Graph}::Data  data

   if { $data(FrameData)!="" } {
      . config -cursor watch
      $data(Canvas) config -cursor watch
      $data(FrameData).page.canvas config -cursor watch
   }
}

#----------------------------------------------------------------------------
# Nom      : <Graph::UnIdle>
# Creation : Janvier 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Configurer le curseur pour inqiquer un travail termine
#
# Parametres :
#   <Graph>  : Indentificateur du Graph
#   <Type>   : Type du Graph
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Graph::UnIdle { Graph Type } {

   upvar #0 Graph::${Type}::${Type}${Graph}::Data  data

   if { $data(FrameData)!="" } {
      . config -cursor left_ptr
      $data(Canvas) config -cursor left_ptr
      $data(FrameData).page.canvas config -cursor left_ptr
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Destroy>
# Creation : Avril 2003 - J.P. Gauthier - CMC/CMOE -
#
# But      : Mise a jour des graphs associees a une page
#
# Parametres :
#   <Frame>  : Identificateur de Page
#
#-------------------------------------------------------------------------------

proc Graph::Destroy { Frame { Graph "" } { Type "" } } {
   variable Data

   if { $Graph=="" && $Type=="" } {
      foreach type $Data(Types) {
         foreach graph [Page::Registered $Frame Graph::${type}] {
            Graph::Destroy $Frame $graph $type
         }
      }
   } else {

      if { [Page::Registered $Frame Graph::${Type} $Graph]==-1 } {
         return
      }

      $Frame.page.canvas configure -cursor watch
      update idletasks

      Page::ActiveUnWrapper Graph $Frame $Graph

      Graph::DeActivate $Graph $Type
      Graph::ParamsOff $Frame $Graph $Type

      #----- Cleanup graphs specifics
      if { [info procs ::Graph::${Type}::Clean]!="" } {
         Graph::${Type}::Clean $Graph
      }

      upvar #0 Graph::${Type}::${Type}${Graph}::Data data

      #----- Supprimer les pointeurs
      $Frame.page.canvas delete PAGE$Graph
      catch { $data(FrameData).page.canvas delete GRAPHSELECT$Graph }

      #----- Supprimer les items
      foreach pos $data(Pos) {
         Graph::${Type}::ItemUnDefine $Graph $pos
      }
      #----- Supprimer le namespace du graph
      namespace delete ::Graph::${Type}::${Type}$Graph

      #----- Supprimer les variables du viewport
      unset Data(Full$Graph)
      unset Data(Active$Graph)
      unset Data(X$Graph)
      unset Data(Y$Graph)
      unset Data(Width$Graph)
      unset Data(Height$Graph)
      unset Data(Type$Graph)

      update idletasks
      $Frame.page.canvas configure -cursor left_ptr
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::LocTool>
# Creation : Mai 2000 - J.P. Gauthier - CMC/CMOE -
#
# But      : Recupere la selection d'une source.
#
# Parametres :
#  <Graph>   : Indentificateur du Graph
#  <Type>    : Type du Graph
#  <Array>   : Variable array
#  <Index>   : Index dans la variable Array
#  <Op>      : Operation effectuer sur la variable
#
# Remarques :
#   -Cette procedure repond a un "trace" sur une variable a l'interieur de SPI
#    afin de recuperer l'information necessaire automatiquement (SPI::Src(Read))
#
#-------------------------------------------------------------------------------

proc Graph::LocTool { Graph Type Array Index Op } {

   if { $Graph!="" } {
      Graph::${Type}::ItemDefine $Graph $Graph::Data(Pos) [list $SPI::Src(Lat) $SPI::Src(Lon)]
   }
}

#------------------------------------------------------------------------------
# Nom      : <Graph::Mode>
# Creation : Mars 2004 - J.P. Gauthier - CMC/CMOE -
#
# But     : Activer les bindings relatifs aux graphs
#
# Parametres :
#   <Graph>  : Identificateur du graph
#   <Type>   : Type de graph
#   <Zoom>   : Activation du mode Zoom
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Graph::Mode { Graph Type { Zoom False } } {
   variable Data

   upvar #0 Graph::${Type}::${Type}${Graph}::Data  data

   $data(Canvas) bind PAGE$Graph    <ButtonPress-1> "Graph::Activate $data(Frame) $Graph $Type"

   if { $Zoom } {

      $data(Canvas) bind $Graph                <Motion>        "if { \[$Graph -header %x %y\] } { $data(Canvas) configure -cursor hand1 } else { $data(Canvas) configure -cursor left_ptr }
                                                                  Graph::${Type}::Coord $Page::Data(Frame) $Graph \[$data(Canvas) canvasx %x\] \[$data(Canvas) canvasy %y\]"
      $data(Canvas) bind $Graph                <Leave>         "set Page::Data(Coord) \"\";set Page::Data(Value) \"\""

      #----- Evenements de zoom

      $data(Canvas) bind PAGE$Graph <ButtonPress-2>   "Graph::Activate $data(Frame) $Graph $Type;\
                                                                Graph::ZoomInit $data(Canvas) \[$data(Canvas) canvasx %x\] \[$data(Canvas) canvasy %y\]"
      $data(Canvas) bind PAGE$Graph <B2-Motion>       "Graph::ZoomBox  $data(Canvas) \[$data(Canvas) canvasx %x\] \[$data(Canvas) canvasy %y\]"
      $data(Canvas) bind PAGE$Graph <ButtonRelease-2> "Graph::Zoom $Type $Graph"
      $data(Canvas) bind PAGE$Graph <ButtonRelease-3> "Graph::ZoomReset $Type $Graph"

      $data(Canvas) bind PAGE$Graph <ButtonPress-4>   "Graph::ZoomScroll $Type $Graph \[$data(Canvas) canvasx %x\] \[$data(Canvas) canvasy %y\] +0.1 True"
      $data(Canvas) bind PAGE$Graph <ButtonPress-5>   "Graph::ZoomScroll $Type $Graph \[$data(Canvas) canvasx %x\] \[$data(Canvas) canvasy %y\] -0.1"

      #----- Evenements de rotation

      $data(Canvas) bind $Graph <ButtonPress-1>   "Graph:LegendMoveInit $data(Frame) $data(Canvas) $Graph $Type %x %y"
      $data(Canvas) bind $Graph <B1-Motion>       "Graph:LegendMove $data(Frame) $data(Canvas) $Graph $Type %x %y"
      $data(Canvas) bind $Graph <ButtonRelease-1> "Graph:LegendMoveDone $data(Frame) $data(Canvas) $Graph $Type"
   }
}

#------------------------------------------------------------------------------
# Nom      : <Graph::ModeSelect>
# Creation : Juin 2012 - J.P. Gauthier - CMC/CMOE -
#
# But     : Selectionner le mode de selection par curseur
#
# Parametres :
#  <Mode>    : mode de selection (1=point, 2=carre)
#  <Valid>   : Liste des modes valides
#  <Type>    : Liste des types valides
# Remarques :
#
#-------------------------------------------------------------------------------

proc Graph::ModeSelect { Mode { Valid {} } { Type {} } } {
   global GDefs
   variable Data
   variable Param

   set Param(SelectMode) $Mode
   set Data(ToolMode)    None

   if { $Mode=="NONE" } {
      $Data(Tab).head.sel configure -state disabled
   } else {
      $Data(Tab).head.sel configure -state normal
   }

   if { [llength $Valid] } {
      $Data(Tab).head.sel.down.menu entryconfigure 0 -state disabled
      $Data(Tab).head.sel.down.menu entryconfigure 1 -state disabled
      $Data(Tab).head.sel.down.menu entryconfigure 2 -state disabled
      $Data(Tab).head.sel.down.menu entryconfigure 3 -state disabled
      foreach mode $Valid {
         switch $mode {
           "POINT"   { $Data(Tab).head.sel.down.menu entryconfigure 0 -state normal }
           "LINE"    { $Data(Tab).head.sel.down.menu entryconfigure 1 -state normal }
           "BOX"     { $Data(Tab).head.sel.down.menu entryconfigure 2 -state normal }
           "POLYGON" { $Data(Tab).head.sel.down.menu entryconfigure 3 -state normal }
         }
      }
   }

   if { [llength $Type] } {
      $Data(Tab).head.sel.down.menu entryconfigure 5 -state disabled
      $Data(Tab).head.sel.down.menu entryconfigure 6 -state disabled
      $Data(Tab).head.sel.down.menu entryconfigure 7 -state disabled
      $Data(Tab).head.sel.down.menu entryconfigure 8 -state disabled
      $Data(Tab).head.sel.down.menu entryconfigure 9 -state disabled
      foreach mode $Type {
         switch $mode {
           "AVG" { $Data(Tab).head.sel.down.menu entryconfigure 5 -state normal }
           "MIN" { $Data(Tab).head.sel.down.menu entryconfigure 6 -state normal }
           "MAX" { $Data(Tab).head.sel.down.menu entryconfigure 7 -state normal }
           "MED" { $Data(Tab).head.sel.down.menu entryconfigure 8 -state normal }
           "STD" { $Data(Tab).head.sel.down.menu entryconfigure 9 -state normal }
         }
      }
   }
   switch $Param(SelectMode) {
      "POINT"   { $Data(Tab).head.sel configure -image ARROW;       set Data(ToolMode) Data; $Data(Tab).head.sel.down.menu entryconfigure 0 -state normal }
      "LINE"    { $Data(Tab).head.sel configure -image ARROWLINE;   set Data(ToolMode) Draw; $Data(Tab).head.sel.down.menu entryconfigure 1 -state normal }
      "BOX"     { $Data(Tab).head.sel configure -image ARROWSQUARE; set Data(ToolMode) Data; $Data(Tab).head.sel.down.menu entryconfigure 2 -state normal }
      "POLYGON" { $Data(Tab).head.sel configure -image ARROWPOLY;   set Data(ToolMode) Draw; $Data(Tab).head.sel.down.menu entryconfigure 3 -state normal }
   }

   SPI::ToolMode $Page::Data(ToolMode) $Graph::Data(ToolMode) True

   #----- Insert saved locations
   if { [set idx [$Data(Tab).head.sel.down.menu index end]]>13 } {
      $Data(Tab).head.sel.down.menu delete 14 $idx
   }
   foreach item $Param($Param(SelectMode)) {
      $Data(Tab).head.sel.down.menu add radiobutton -label [lindex $item 0] -variable Graph::Param(LOCATION) \
         -command "Graph::PosSelectSaved \$Graph::Data(Graph) \$Graph::Data(Type) \$Graph::Data(Pos) \[list $item\]"
   }
}

#------------------------------------------------------------------------------
# Nom      : <Graph::PosSelectSaved>
# Creation : Juin 2012 - J.P. Gauthier - CMC/CMOE -
#
# But     : Selectionner le mode de selection par curseur
#
# Parametres :
#   <Graph>  : Identificateur du Graph
#   <Type>   : Type de graph
#   <Pos>    : Position
#   <Def>    : Saved position definition
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Graph::PosSelectSaved { Graph Type Pos Def } {
   variable Param

   upvar #0 Graph::${Type}::${Type}${Graph}::Data  data

   set coords [lindex $Def 1]

   switch $Param(SelectMode) {
      "POINT"   { set data(Lat0) [lindex $coords 0]; set data(Lon0) [lindex $coords 1] }
      "LINE"    { set data(Coords) $coords }
      "BOX"     { set data(Lat0) [lindex $coords 0]; set data(Lon0) [lindex $coords 1]; set data(Lat1) [lindex $coords 2]; set data(Lon1) [lindex $coords 3] }
      "POLYGON" { set data(Coords) $coords }
   }

   #----- Force field assignation if none is present
   if { ![fstdfield is $data(Field)] && $data(VP)!="" } {
      set data(Field)     [lindex [Viewport::Assigned $data(FrameData) $data(VP) fstdfield] 0]
   }

   Graph::VertexResolution $Type $Graph False
   Graph::${Type}::ItemDefine $Graph $Pos $coords
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Params>
# Creation : Avril 2003 - J.P. Gauthier - CMC/CMOE -
#
# But      : Fenetre de parametrage des graphs
#
# Parametres :
#   <Graph>  : Identificateur du Graph
#   <Type>   : Type de graph
#
#-------------------------------------------------------------------------------

proc Graph::Params { { Graph "" } { Type "" } { Force False } } {
   global GDefs
   variable Lbl
   variable Bubble
   variable Data
   variable Param

   if { !$Data(Show) } {
      destroy .graphparams
      Page::ModeSelect Zoom
      return
   }

   set Param(Window) .graphparams

   update idletasks

   if { ![winfo exists .graphparams] } {

      if { $Param(Dock) } {
         frame .graphparams
         SPI::Dock .graphparams
      } else {
         toplevel         .graphparams
         wm title         .graphparams [lindex $Lbl(Graph) $GDefs(Lang)]
         wm transient     .graphparams .
         eval wm geometry .graphparams $Param(Geom)
         wm protocol      .graphparams WM_DELETE_WINDOW { set Graph::Data(Show) False; Page::ModeSelect Zoom ;destroy .graphparams }
      }

      TabFrame::Create .graphparams.tab 1 ""
      set Data(Tab) [TabFrame::Add .graphparams.tab 1 [lindex $Lbl(Graph) $GDefs(Lang)] True]
      pack .graphparams.tab -side top -fill both -expand true

      frame $Data(Tab).head
         checkbutton $Data(Tab).head.sel -image ARROW -relief sunken -bd 1 -overrelief raised -offrelief flat -indicatoron False -anchor w -width 30 -state disabled \
            -variable Page::Data(ToolMode) -offvalue SPI -onvalue Graph -command { SPI::ToolMode $Page::Data(ToolMode) $Graph::Data(ToolMode) True; Graph::UpdateItems $Page::Data(Frame) }
         menubutton $Data(Tab).head.sel.down -image OPTIONS -relief flat -bd 0 -menu $Data(Tab).head.sel.down.menu -state disabled
         place $Data(Tab).head.sel.down -relx 1.0 -rely 0.0 -anchor ne -relheight 1.0
         menu $Data(Tab).head.sel.down.menu
         $Data(Tab).head.sel.down.menu add radiobutton -image ARROW       -value POINT   -variable Graph::Param(SelectMode) -command { Graph::ModeSelect POINT ;   if { $Page::Data(ToolMode)=="SPI" } { $Graph::Data(Tab).head.sel invoke } }
         $Data(Tab).head.sel.down.menu add radiobutton -image ARROWLINE   -value LINE    -variable Graph::Param(SelectMode) -command { Graph::ModeSelect LINE ;    if { $Page::Data(ToolMode)=="SPI" } { $Graph::Data(Tab).head.sel invoke } }
         $Data(Tab).head.sel.down.menu add radiobutton -image ARROWSQUARE -value BOX     -variable Graph::Param(SelectMode) -command { Graph::ModeSelect BOX ;     if { $Page::Data(ToolMode)=="SPI" } { $Graph::Data(Tab).head.sel invoke } }
         $Data(Tab).head.sel.down.menu add radiobutton -image ARROWPOLY   -value POLYGON -variable Graph::Param(SelectMode) -command { Graph::ModeSelect POLYGON ; if { $Page::Data(ToolMode)=="SPI" } { $Graph::Data(Tab).head.sel invoke } }
         $Data(Tab).head.sel.down.menu add separator
         $Data(Tab).head.sel.down.menu add radiobutton -label [lindex $Lbl(Average) $GDefs(Lang)] -variable Graph::Param(SelectType) -value AVG \
            -command { eval Graph::${Graph::Data(Type)}::Update \$Graph::${Graph::Data(Type)}::${Graph::Data(Type)}${Graph::Data(Graph)}::Data(FrameData) $Graph::Data(Graph) }
         $Data(Tab).head.sel.down.menu add radiobutton -label [lindex $Lbl(Minimum) $GDefs(Lang)] -variable Graph::Param(SelectType) -value MIN\
            -command { eval Graph::${Graph::Data(Type)}::Update \$Graph::${Graph::Data(Type)}::${Graph::Data(Type)}${Graph::Data(Graph)}::Data(FrameData) $Graph::Data(Graph) }
         $Data(Tab).head.sel.down.menu add radiobutton -label [lindex $Lbl(Maximum) $GDefs(Lang)] -variable Graph::Param(SelectType) -value MAX\
            -command { eval Graph::${Graph::Data(Type)}::Update \$Graph::${Graph::Data(Type)}::${Graph::Data(Type)}${Graph::Data(Graph)}::Data(FrameData) $Graph::Data(Graph) }
         $Data(Tab).head.sel.down.menu add radiobutton -label [lindex $Lbl(Median) $GDefs(Lang)] -variable Graph::Param(SelectType) -value MED \
            -command { eval Graph::${Graph::Data(Type)}::Update \$Graph::${Graph::Data(Type)}::${Graph::Data(Type)}${Graph::Data(Graph)}::Data(FrameData) $Graph::Data(Graph) }
         $Data(Tab).head.sel.down.menu add radiobutton -label [lindex $Lbl(StdDev) $GDefs(Lang)] -variable Graph::Param(SelectType) -value STD \
            -command { eval Graph::${Graph::Data(Type)}::Update \$Graph::${Graph::Data(Type)}::${Graph::Data(Type)}${Graph::Data(Graph)}::Data(FrameData) $Graph::Data(Graph) }
         $Data(Tab).head.sel.down.menu add separator
         $Data(Tab).head.sel.down.menu add command -label [lindex $Lbl(Save) $GDefs(Lang)] -command { Graph::PosSave $Graph::Data(Type) $Graph::Data(Graph) $Graph::Data(Pos) }
         $Data(Tab).head.sel.down.menu add command -label [lindex $Lbl(Del) $GDefs(Lang)] -command { Graph::PosDelete $Graph::Data(Type) $Graph::Data(Graph) $Graph::Data(Pos) }
         $Data(Tab).head.sel.down.menu add separator

         button $Data(Tab).head.reset -image GRAPHRESET -relief flat -bd 0 -overrelief raised -state disabled -command { Graph::ZoomReset $Graph::Data(Type) $Graph::Data(Graph) }
         button $Data(Tab).head.data -image GRID -relief flat -bd 0 -overrelief raised -state disabled -command {  Graph::DataSheet $Graph::Data(Type) $Graph::Data(Graph) }
         button $Data(Tab).head.save -image GRIDSAVE -relief flat -bd 0 -overrelief raised -state disabled -command {  Graph::DataSave $Graph::Data(Type) $Graph::Data(Graph) [FileBox::Create . "" Save [list $FileBox::Type(FSTD)]] }
         pack $Data(Tab).head.sel $Data(Tab).head.reset $Data(Tab).head.data $Data(Tab).head.save -side left -fill y -padx 2
      pack $Data(Tab).head -side top -fill x

      frame .graphparams.dock -relief raised -bd 1
         button .graphparams.dock.sel -image DOCK -anchor w -relief flat -bd 0 -overrelief raised -command { SPI::DockTool Graph { Graph::Params $Graph::Data(Graph) $Graph::Data(Type) True } }
         button .graphparams.dock.del -image DOCKDELETE -anchor w -relief flat -bd 0 -overrelief raised -command { set Graph::Data(Show) False ; Page::ModeSelect Zoom ;destroy .graphparams }
         label .graphparams.dock.info -text "" -relief sunken -bd 1 -anchor w -width 21 -bg $GDefs(ColorLight)
         pack .graphparams.dock.sel .graphparams.dock.del -side left
         pack .graphparams.dock.info -side left -fill x -expand true
      pack .graphparams.dock -side bottom -fill x

      Bubble::Create $Data(Tab).head.sel   $Bubble(Mode)
      Bubble::Create $Data(Tab).head.reset $Bubble(Reset)
      Bubble::Create $Data(Tab).head.data  $Bubble(Sheet)
      Bubble::Create $Data(Tab).head.save  $Bubble(Save)
   }

   #----- Inserer les parametres du graph

   if { $Type!="" && ($Data(GraphParams)!=$Graph || $Force) } {
      eval .graphparams.dock.info configure -text \[lindex \$Graph::${Type}::Lbl(Title) $GDefs(Lang)\]

      $Data(Tab).head.sel      configure -state normal
      $Data(Tab).head.reset    configure -state normal
      $Data(Tab).head.data     configure -state normal
      $Data(Tab).head.sel.down configure -state normal

      if { $Type=="Section" } {
         $Data(Tab).head.save     configure -state normal
      } else {
         $Data(Tab).head.save     configure -state disabled
      }

      destroy $Data(Tab).graph
      frame $Data(Tab).graph
      pack $Data(Tab).graph -after $Data(Tab).head -side top -fill both -expand true

      #----- Force le mode selection du graph actif si besoin est
      set Data(GraphParams) $Graph

      if { [info exists Graph::Data(Full$Graph)] } {
         Graph::${Type}::Params $Data(Tab).graph $Graph
         SPI::ToolMode $Page::Data(ToolMode) $Graph::Data(ToolMode) True
      }
      Graph::ParamsGraph $Data(Tab).graph
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::ParamsAxis>
# Creation : Avril 2003 - J.P. Gauthier - CMC/CMOE -
#
# But      : Fenetre de parametrage des axes
#
# Parametres :
#   <Parent> : Fenetre parent
#   <Graph>  : Identificateur du Graph
#   <Type>   : Type de graph
#   <Axis>   : Axe X ou Y
#   <Mode>   : Type d'axe (MARK,TIME,INTERVAL,VERTICAL)
#
#-------------------------------------------------------------------------------

proc Graph::ParamsAxis { Parent Graph Type Axis { Mode "" } } {
   global GDefs
   variable Lbl
   variable Bubble
   variable Data
   variable Param

   if  { ![winfo exists  $Parent.scale${Axis}] } {
      checkbutton $Parent.scale${Axis}toggle -text "[lindex $Lbl(Scale) $GDefs(Lang)] $Axis" -compound right -variable Graph::Graph(ParamsAxis$Axis) -onvalue True -offvalue False \
         -command "Graph::ParamsAxis $Parent $Graph $Type $Axis $Mode" -indicatoron False  -bd 0 -activebackground $GDefs(ColorHighLight) -selectcolor $GDefs(ColorFrame)
      labelframe $Parent.scale${Axis} -labelwidget $Parent.scale${Axis}toggle
   }

   if { $Graph::Graph(ParamsAxis$Axis) } {

      $Parent.scale${Axis}toggle configure -bitmap @$GDefs(Dir)/share/bitmap/up.xbm
      destroy $Parent.scale${Axis}.lbl

      if { $Mode!="MARK" } {
         if { $Mode=="TIME" } {

            frame $Parent.scale${Axis}.t0
               label $Parent.scale${Axis}.t0.lbl -text [lindex $Graph::Lbl(From) $GDefs(Lang)] -width 12 -anchor w
#               Calendar::Create $Parent.scale${Axis}.t0.val [lindex $Graph::Lbl(From) $GDefs(Lang)] Graph::${Type}::${Type}${Graph}::Data(Date0) -1 "Graph::${Type}::Graph $Graph"
               entry $Parent.scale${Axis}.t0.val -textvariable Graph::${Type}::${Type}${Graph}::Data(Date0) -bg $GDefs(ColorLight) -relief sunken -bd 1
               pack  $Parent.scale${Axis}.t0.lbl -side left
               pack  $Parent.scale${Axis}.t0.val  -side left -fill x -expand true

            frame $Parent.scale${Axis}.t1
               label $Parent.scale${Axis}.t1.lbl -text [lindex $Graph::Lbl(To) $GDefs(Lang)] -width 12 -anchor w
               entry $Parent.scale${Axis}.t1.val -textvariable Graph::${Type}::${Type}${Graph}::Data(Date1) -bg $GDefs(ColorLight) -relief sunken -bd 1
               pack  $Parent.scale${Axis}.t1.lbl -side left
               pack  $Parent.scale${Axis}.t1.val  -side left -fill x -expand true

            frame $Parent.scale${Axis}.format
               label $Parent.scale${Axis}.format.lbl -text [lindex $Lbl(Format) $GDefs(Lang)] -width 12 -anchor w
               ComboBox::Create $Parent.scale${Axis}.format.val Graph::${Type}::${Type}${Graph}::Graph(${Axis}Format) noedit unsorted nodouble -1 $Graph::Param(AxisFormatsTime) 10 5 \
                  Graph::${Type}::Graph $Graph
               pack $Parent.scale${Axis}.format.lbl -side left
               pack $Parent.scale${Axis}.format.val -side left -fill x -expand True -padx 2
               Bubble::Create $Parent.scale${Axis}.format.val $Graph::Bubble(AxisFormat)
            pack $Parent.scale${Axis}.format -side top -fill x

            pack $Parent.scale${Axis}.t0 $Parent.scale${Axis}.t1 -side top -fill x
#            Bubble::Create $Parent.scale${Axis}.t0.val  $Graph::Bubble(Date0)
            Bubble::Create $Parent.scale${Axis}.t1.val  $Graph::Bubble(Date1)

            bind $Parent.scale${Axis}.t0.val <Return>    "Graph::${Type}::Graph $Graph"
            bind $Parent.scale${Axis}.t1.val <Return>    "Graph::${Type}::Graph $Graph"
         } else {
            frame $Parent.scale${Axis}.inter
               label $Parent.scale${Axis}.inter.lbl -text "[lindex $Lbl(Inter) $GDefs(Lang)]" -width 12 -anchor w
               entry $Parent.scale${Axis}.inter.val -textvariable Graph::${Type}::${Type}${Graph}::Graph(${Axis}Inter) -bg $GDefs(ColorLight) -relief sunken -width 1
               pack $Parent.scale${Axis}.inter.lbl -side left
               pack $Parent.scale${Axis}.inter.val -side left -fill x -expand True -padx 2
            Bubble::Create $Parent.scale${Axis}.inter.val $Graph::Bubble(AxisInter)
            bind $Parent.scale${Axis}.inter.val <Return> "Graph::${Type}::Graph $Graph"
            pack $Parent.scale${Axis}.inter -side top -fill x
         }

         if { $Mode!="INTERVAL" && $Mode!="TIME" } {
            frame $Parent.scale${Axis}.type
               label $Parent.scale${Axis}.type.lbl -text [lindex $Lbl(Type) $GDefs(Lang)] -width 12 -anchor w
               ComboBox::Create $Parent.scale${Axis}.type.val Graph::${Type}::${Type}${Graph}::Graph(${Axis}Scale) noedit unsorted nodouble -1 $Graph::Param(AxisTypes) 10 5 \
                  Graph::${Type}::Graph $Graph
               pack $Parent.scale${Axis}.type.lbl -side left
               pack $Parent.scale${Axis}.type.val -side left -fill x -expand True -padx 2
               Bubble::Create $Parent.scale${Axis}.type.val $Graph::Bubble(AxisType)
            pack $Parent.scale${Axis}.type -side top -fill x

            frame $Parent.scale${Axis}.format
               label $Parent.scale${Axis}.format.lbl -text [lindex $Lbl(Format) $GDefs(Lang)] -width 12 -anchor w
               ComboBox::Create $Parent.scale${Axis}.format.val Graph::${Type}::${Type}${Graph}::Graph(${Axis}Format) noedit unsorted nodouble -1 $Graph::Param(AxisFormats) 10 5 \
                  Graph::${Type}::Graph $Graph
               pack $Parent.scale${Axis}.format.lbl -side left
               pack $Parent.scale${Axis}.format.val -side left -fill x -expand True -padx 2
               Bubble::Create $Parent.scale${Axis}.format.val $Graph::Bubble(AxisFormat)
            pack $Parent.scale${Axis}.format -side top -fill x

            frame $Parent.scale${Axis}.dec
               label $Parent.scale${Axis}.dec.lbl -text [lindex $Lbl(Decimals) $GDefs(Lang)] -width 12 -anchor w
               spinbox $Parent.scale${Axis}.dec.val -textvariable Graph::${Type}::${Type}${Graph}::Graph(${Axis}Decimals) -from 0 -to 10 -increment 1 -wrap True -width 5 \
                  -bg $GDefs(ColorLight) -command "Graph::${Type}::Graph $Graph"
               pack $Parent.scale${Axis}.dec.lbl -side left
               pack $Parent.scale${Axis}.dec.val -side left -fill x -expand True -padx 2
               Bubble::Create $Parent.scale${Axis}.dec.val $Graph::Bubble(AxisDecimals)
            pack $Parent.scale${Axis}.dec -side top -fill x
         }

         if { $Mode!="INTERVAL" && $Mode=="VERTICAL" } {
            frame $Parent.scale${Axis}.z
               label $Parent.scale${Axis}.z.lbl -text [lindex $Lbl(ZType) $GDefs(Lang)] -width 12 -anchor w
               ComboBox::Create $Parent.scale${Axis}.z.val Graph::${Type}::${Type}${Graph}::Graph(ZType) noedit unsorted nodouble -1 $Graph::Param(AxisZs) 10 5 \
                  Graph::${Type}::Update \$Graph::${Type}::${Type}${Graph}::Data(FrameData)
               pack $Parent.scale${Axis}.z.lbl -side left
               pack $Parent.scale${Axis}.z.val -side left -fill x -expand True -padx 2
            pack $Parent.scale${Axis}.z -side top -fill x
            Bubble::Create $Parent.scale${Axis}.z.val $Graph::Bubble(AxisZType)
         }
      }

      if { $Mode!="INTERVAL" } {
         frame $Parent.scale${Axis}.grid
            label $Parent.scale${Axis}.grid.lbl -text [lindex $Lbl(Grid) $GDefs(Lang)] -width 12 -anchor w
            ColorBox::CreateSel $Parent.scale${Axis}.grid.col Graph::Grid(${Axis}Color) "Graph::${Type}::Graph $Graph"
            IcoMenu::Create $Parent.scale${Axis}.grid.sz $GDefs(Dir)/share/bitmap \
               "zeroth.xbm width1.xbm width2.xbm width3.xbm width4.xbm width5.xbm" "0 1 2 3 4 5" \
               Graph::Grid(${Axis}Width) { Graph::Configure } $Graph::Grid(${Axis}Width) -relief groove -bd 2
            IcoMenu::CreateDef $Parent.scale${Axis}.grid.dash $GDefs(Dir)/share/bitmap \
               { dash0.xbm dash1.xbm dash2.xbm dash3.xbm dash4.xbm dash5.xbm } { "" . - .- .-- .-. } \
               Graph::Grid(${Axis}Dash) { Graph::Configure } $Graph::Grid(${Axis}Dash) -relief groove -bd 2
            pack $Parent.scale${Axis}.grid.lbl $Parent.scale${Axis}.grid.col $Parent.scale${Axis}.grid.sz $Parent.scale${Axis}.grid.dash -side left
            pack $Parent.scale${Axis}.grid.dash -side left -fill x -expand True
         pack $Parent.scale${Axis}.grid -side top -fill x
         Bubble::Create $Parent.scale${Axis}.grid $Graph::Bubble(AxisMark)

         frame $Parent.scale${Axis}.angle
            label $Parent.scale${Axis}.angle.lbl -text [lindex $Lbl(Angle) $GDefs(Lang)] -width 12 -anchor w
            scale $Parent.scale${Axis}.angle.val -from -90 -to 90 -resolution 10 -variable Graph::${Type}::${Type}${Graph}::Graph(${Axis}Angle) -showvalue false \
               -relief flat -bd 1 -orient horizontal -width 15 -sliderlength 10 -length 50 -command "Graph::${Type}::Graph $Graph; catch "
            pack $Parent.scale${Axis}.angle.lbl -side left
            pack $Parent.scale${Axis}.angle.val -side left -fill x -expand True
         pack $Parent.scale${Axis}.angle -side top -fill x
         Bubble::Create $Parent.scale${Axis}.angle $Graph::Bubble(AxisAngle)
      }
   } else {
      $Parent.scale${Axis}toggle configure -bitmap @$GDefs(Dir)/share/bitmap/down.xbm
      destroy $Parent.scale${Axis}.inter $Parent.scale${Axis}.type $Parent.scale${Axis}.format $Parent.scale${Axis}.dec $Parent.scale${Axis}.z \
         $Parent.scale${Axis}.grid $Parent.scale${Axis}.t0 $Parent.scale${Axis}.t1 $Parent.scale${Axis}.angle
      label $Parent.scale${Axis}.lbl -text "" -width 12 -anchor w
      pack $Parent.scale${Axis}.lbl -side left
   }

   pack $Parent.scale${Axis} -side top -fill x -padx 5 -pady 5 -anchor n
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::ParamsAxisUniform>
# Creation : Fevrier 2003 - J.P. Gauthier - CMC/CMOE -
#
# But      : Desactiver certains widgets dnas le cas d'echelle uniforme
#
# Parametres :
#   <Type>   : Type de graph
#   <Graph>  : Indentificateur du Graph
#   <Update> : Mettre a jour le graph
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Graph::ParamsAxisUniform { Type Graph { Update True }  } {
   variable Data

   upvar #0 Graph::${Type}::${Type}${Graph}::Graph graph
   upvar #0 Graph::${Type}::${Type}${Graph}::Data  data

   if { $graph(Uniform) } {
      pack forget $Data(Tab).graph.scaleY
   } else {
      pack $Data(Tab).graph.scaleY -after $Data(Tab).graph.scaleX -side top -fill x -padx 5 -pady 5
   }

   if { $Update } {
      Graph::${Type}::Update $data(FrameData) $Graph
   }
   Graph::${Type}::Graph $Graph
}

#----------------------------------------------------------------------------
# Nom      : <Graph::ParamsItem>
# Creation : Janvier 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Afficher les options de parametres des items
#
# Parametres :
#   <Parent> : Fenetre parent
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Graph::ParamsItem { Parent } {
   global GDefs
   variable Lbl
   variable Bubble
   variable Data
   variable Graph

   set Data(Frame) $Parent

   if  { ![winfo exists $Parent.item] } {
      checkbutton $Parent.itemtoggle -text [lindex $Lbl(Item) $GDefs(Lang)] -compound right -variable Graph::Graph(ParamsItem) -onvalue True -offvalue False\
         -command "Graph::ParamsItem $Parent" -indicatoron False -bd 0 -activebackground $GDefs(ColorHighLight) -selectcolor $GDefs(ColorFrame)
      labelframe $Parent.item -labelwidget $Parent.itemtoggle
   }

   if { $Graph::Graph(ParamsItem) } {

      destroy $Parent.item.lbl
      $Parent.itemtoggle configure -bitmap @$GDefs(Dir)/share/bitmap/up.xbm

      frame $Parent.item.sel
         listbox $Parent.item.sel.list -yscrollcommand [list $Parent.item.sel.scroll set] -listvariable Graph::Data(Items)\
            -height 3  -bd 1 -relief sunken -bg $GDefs(ColorLight) -exportselection 0 -selectmode single
         scrollbar $Parent.item.sel.scroll -orient vertical -command [list $Parent.item.sel.list yview] -bd 1 -width 10
         pack $Parent.item.sel.list -side left -fill both -expand true
         pack $Parent.item.sel.scroll -side right -fill y
      frame $Parent.item.line
         label $Parent.item.line.lbl -text [lindex $Lbl(Outline) $GDefs(Lang)] -width 12 -anchor w
         ColorBox::CreateSel $Parent.item.line.col Graph::Item(Outline) Graph::ItemConfigure \$Graph::Data(Graph) \$Graph::Data(Type) \$Graph::Data(Item)
         IcoMenu::Create $Parent.item.line.width $GDefs(Dir)/share/bitmap { zeroth.xbm width1.xbm width2.xbm width3.xbm width4.xbm width5.xbm }\
             "0 1 2 3 4 5" Graph::Item(Width) { Graph::ItemConfigure $Graph::Data(Graph) $Graph::Data(Type) $Graph::Data(Item) } $Graph::Item(Width) -relief groove -bd 2
         IcoMenu::CreateDef $Parent.item.line.dash $GDefs(Dir)/share/bitmap \
            { dash0.xbm dash1.xbm dash2.xbm dash3.xbm dash4.xbm dash5.xbm } { "" . - .- .-- .-. } \
             Graph::Item(Dash) { Graph::ItemConfigure $Graph::Data(Graph) $Graph::Data(Type) $Graph::Data(Item) } $Graph::Item(Dash) -relief groove -bd 2
         pack  $Parent.item.line.lbl $Parent.item.line.col $Parent.item.line.width $Parent.item.line.dash -side left
      frame $Parent.item.fill
         label $Parent.item.fill.lbl -text [lindex $Lbl(Fill) $GDefs(Lang)] -width 12 -anchor w
         ColorBox::CreateSel $Parent.item.fill.col Graph::Item(FillColor) Graph::ItemConfigure \$Graph::Data(Graph) \$Graph::Data(Type) \$Graph::Data(Item)
         IcoMenu::CreateDef $Parent.item.fill.stipple $GDefs(Dir)/share/bitmap \
            { zeroth.xbm stipple0.xbm stipple1.xbm stipple2.xbm stipple3.xbm stipple4.xbm stipple5.xbm stipple6.xbm stipple7.xbm stipple8.xbm } \
            { -1 "" @$GDefs(Dir)/share/bitmap/stipple1-32.xbm @$GDefs(Dir)/share/bitmap/stipple2-32.xbm @$GDefs(Dir)/share/bitmap/stipple3-32.xbm @$GDefs(Dir)/share/bitmap/stipple4-32.xbm @$GDefs(Dir)/share/bitmap/stipple5-32.xbm @$GDefs(Dir)/share/bitmap/stipple6-32.xbm @$GDefs(Dir)/share/bitmap/stipple7-32.xbm @$GDefs(Dir)/share/bitmap/stipple8-32.xbm } \
             Graph::Item(Stipple) { Graph::ItemConfigure $Graph::Data(Graph) $Graph::Data(Type) $Graph::Data(Item) } $Graph::Item(Stipple) -relief groove -bd 2
         pack  $Parent.item.fill.lbl $Parent.item.fill.col $Parent.item.fill.stipple -side left
      frame $Parent.item.icon
         label $Parent.item.icon.lbl -text [lindex $Lbl(Icon) $GDefs(Lang)] -width 12 -anchor w
         IcoMenu::Create $Parent.item.icon.sel $GDefs(Dir)/share/bitmap \
            { zeroth.xbm stri.xbm ssquare.xbm svbar.xbm shbar.xbm scircle.xbm slos.xbm spenta.xbm shexa.xbm wind1.xbm } \
            $Graph::Graph(Icons) Graph::Item(Icon) { Graph::ItemConfigure $Graph::Data(Graph) $Graph::Data(Type) $Graph::Data(Item) } \
            0 -relief groove -bd 2
         scale $Parent.item.icon.size -from 1 -to 20 -resolution 1 -variable Graph::Item(Size) -showvalue false \
               -relief flat -bd 1 -orient horizontal -width 15 -sliderlength 10 -length 50 -command { Graph::ItemConfigure $Graph::Data(Graph) $Graph::Data(Type) $Graph::Data(Item); catch }
         pack $Parent.item.icon.lbl $Parent.item.icon.sel -side left
         pack $Parent.item.icon.size -side left -fill x -expand True
      frame $Parent.item.value
         label $Parent.item.value.lbl -text [lindex $Lbl(Value) $GDefs(Lang)] -width 12 -anchor w
         checkbutton $Parent.item.value.sel -variable Graph::Item(Value) -relief raised -bd 1 -onvalue True -offvalue False  -selectcolor "" -relief groove -bd 1\
            -bitmap @$GDefs(Dir)/share/bitmap/zeroth.xbm -indicatoron false -command { Graph::ItemConfigure $Graph::Data(Graph) $Graph::Data(Type) $Graph::Data(Item) }
         button $Parent.item.value.font -bitmap @$GDefs(Dir)/share/bitmap/font.ico  -relief groove \
            -command "FontBox::Create $Parent.item.value.font { Graph::ItemConfigure \$Graph::Data(Graph) \$Graph::Data(Type) \$Graph::Data(Item) } $Graph::Item(Font)"
         pack $Parent.item.value.lbl $Parent.item.value.sel $Parent.item.value.font -side left

      Option::Create $Parent.item.type [lindex $Lbl(Type) $GDefs(Lang)] ::Graph::Item(Type) 0 -1 \
         $Graph::Item(Types) { Graph::ItemConfigure $Graph::Data(Graph) $Graph::Data(Type) $Graph::Data(Item) }

      pack $Parent.item.sel $Parent.item.type $Parent.item.line $Parent.item.fill $Parent.item.icon $Parent.item.value -side top -fill x -padx 2

      Bubble::Create $Parent.item.sel.list $Bubble(ItemList)
      Bubble::Create $Parent.item.line     $Bubble(ItemLine)
      Bubble::Create $Parent.item.fill     $Bubble(ItemFill)
      Bubble::Create $Parent.item.icon     $Bubble(ItemIcon)
      Bubble::Create $Parent.item.type     $Bubble(ItemType)

      bind $Parent.item.sel.list <ButtonRelease-1>  { set Graph::Data(Item) [%W get [%W nearest %y]] ; Graph::ItemSelect $Graph::Data(Item) }
   } else {
      $Parent.itemtoggle configure -bitmap @$GDefs(Dir)/share/bitmap/down.xbm
      destroy $Parent.item.sel $Parent.item.type $Parent.item.line $Parent.item.fill $Parent.item.icon $Parent.item.value
      label $Parent.item.lbl -text "" -width 12 -anchor w
      pack $Parent.item.lbl -side left
   }
   pack $Parent.item -side top -fill both -padx 5 -pady 5 -anchor n
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::DataSheet>
# Creation : Avril 2003 - J.P. Gauthier - CMC/CMOE -
#
# But      :Afficher les donnees du graph
#
# Parametres :
#   <Type>   : Type de graph
#   <Graph>  : Identificateur du Graph
#
#-------------------------------------------------------------------------------

proc Graph::DataSheet { Type Graph } {
   global GDefs
   variable Lbl

   upvar #0 Graph::${Type}::${Type}${Graph}::Data  data
   upvar #0 Graph::${Type}::${Type}${Graph}::Graph graph

   eval set title \[lindex \$Graph::${Type}::Lbl(Title) $GDefs(Lang)\]
   set text [Dialog::Text .graphdata "[lindex $Lbl(Data) $GDefs(Lang)]: $title" "" 60 20]

   $text insert end "Title      : $title\n"

   foreach item $data(Items) {
      if { [graphitem is $item] } {

         $text insert end "Item       : [lindex [$data(Canvas) itemconfigure [graphitem configure $item -desc] -text] end]\n"
         set pos [join [lrange [split $item _]  0 end-1] _]
         $text insert end "Unit X     : $graph(UnitX)\n"
         $text insert end "Unit Y     : $graph(UnitY)\n"
         $text insert end "Coordinates: $data(Pos$pos)\n"

         #----- Graph Raster
         set ditem [graphitem configure $item -data]
         if { $ditem!="" } {
            if { [fstdfield define $ditem -GRTYP]=="V" } {
               $text insert end "Levels     :[fstdfield stats $ditem -levels]\n"
            }
            for { set j 0 } { $j < [fstdfield define $ditem -NJ] } { incr j } {
               $text insert end "\n"
               for { set i 0 } { $i < [fstdfield define $ditem -NI] } { incr i } {
                  $text insert end [format "%-20e" [fstdfield stats $ditem -gridvalue $i $j]]
               }
            }
         } else {
            #----- Graph 2D
            set xitem [graphitem configure $item -xdata]
            set yitem [graphitem configure $item -ydata]
            set t ""
            if { $xitem!="" && $yitem!="" } {
               $text insert end [format "%-20s %-20s" X Y]\n
               foreach x [vector get $xitem] y [vector get $yitem] {
                  if { $Type=="Time" } {
                     set t [Graph::TimeFormat $x $data(Time) $data(XMin)]
                  }
                  $text insert end [format "%-20.8e %-20.8e $t" $x $y]\n
               }
               $text insert end "\n"
            }
         }
      }
   }
}

proc Graph::DataSave { Type Graph File } {
   global GDefs
   variable Lbl

   upvar #0 Graph::${Type}::${Type}${Graph}::Data  data
   upvar #0 Graph::${Type}::${Type}${Graph}::Graph graph

   if { $File!="" } {

      fstdfile open GRAPHFILE write $File

      set n 0
      foreach item $data(Items) {
         if { [graphitem is $item] } {

            #----- Graph Raster
            if { [set ditem [graphitem configure $item -data]]!="" } {

               if { [fstdfield define $ditem -GRTYP]=="V" } {

                  if { !$n } {
                     incr n

                     set ni     [fstdfield define $ditem -NI]
                     set nj     [fstdfield define $ditem -NJ]
                     set etiket [fstdfield define $ditem -ETIKET]
                     set typvar [fstdfield define $ditem -TYPVAR]

                     fstdfield create GRAPHTIC $ni 1 1 Float32
                     fstdfield define GRAPHTIC -NOMVAR >> -TYPVAR $typvar -GRTYP L 0 0 1.0 1.0 -ETIKET $etiket -IP1 $ni -IP2 $nj -IP3 $n
                     fstdfield define GRAPHTIC -DATA 0 [fstdfield stats $ditem -gridlon]

                     fstdfield create GRAPHTAC 1 $ni 1 Float32
                     fstdfield define GRAPHTAC -NOMVAR ^^ -TYPVAR $typvar -GRTYP L 0 0 1.0 1.0 -ETIKET $etiket -IP1 $ni -IP2 $nj -IP3 $n
                     fstdfield define GRAPHTAC -DATA 0 [fstdfield stats $ditem -gridlat]

                     fstdfield create GRAPHTOC $nj 1 1 Float32
                     fstdfield define GRAPHTOC -NOMVAR ^> -TYPVAR $typvar -GRTYP X -ETIKET $etiket -IP1 $ni -IP2 $nj -IP3 $n
                     fstdfield define GRAPHTOC -DATA 0 [fstdfield stats $ditem -levels]


                     fstdfield write GRAPHTIC GRAPHFILE -32 True
                     fstdfield write GRAPHTAC GRAPHFILE -32 True
                     fstdfield write GRAPHTOC GRAPHFILE -32 True

                     fstdfield free GRAPHTIC GRAPHTAC GRAPHTOC
                  }

                  fstdfield define $ditem -IG1 $ni -IG2 $nj -IG3 $n -IG4 0
                  fstdfield write $ditem   GRAPHFILE -32 True
               }
            }
         }
      }
      fstdfile close GRAPHFILE
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::ParamsOff>
# Creation : Avril 2003 - J.P. Gauthier - CMC/CMOE -
#
# But      : Fenetre de parametrage des graphs
#
# Parametres :
#   <Frame>  : Page courante
#   <Graph>  : Identificateur du Graph
#   <Type>   : Type de graph
#
#-------------------------------------------------------------------------------

proc Graph::ParamsOff { Frame Graph Type } {
   variable Data

   if { $Data(GraphParams)==$Graph } {
      destroy $Data(Tab).graph

      if { [winfo exist .graphparams] } {
        $Data(Tab).head.sel      configure -state disabled
        $Data(Tab).head.reset    configure -state disabled
        $Data(Tab).head.data     configure -state disabled
        $Data(Tab).head.sel.down configure -state disabled
      }

      if { $Page::Data(ToolMode)=="Graph" } {
         SPI::ToolMode SPI Zoom
      }
   }
   set Data(Graph) [Page::UnRegister $Frame Graph::$Type $Graph]
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Dock>
# Creation : Avril 2003 - J.P. Gauthier - CMC/CMOE -
#
# But      : Inserer la fenetre dans l'interface
#
# Parametres :
#
#-------------------------------------------------------------------------------

proc Graph::Dock { } {
   variable Data
   variable Param

   if { $Param(Dock) } {
      set Param(Dock) False
   } else {
      set Param(Dock) True
   }
   destroy .graphparams

   catch { Graph::Params $Data(Graph) $Data(Type) True }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::SaveParams>
# Creation : Juillet 2012 - J.P. Gauthier - CMC/CMOE -
#
# But      : Sauvegarder les parametres des graphs
#
# Parametres :
#
#-------------------------------------------------------------------------------

proc Graph::SaveParams { } {
   global env
   variable Param

   catch { file copy -force $env(HOME)/.spi/Graph $env(HOME)/.spi/Graph.old }

   set f [open $env(HOME)/.spi/Graph w]
   foreach mode { POINT LINE BOX POLYGON } {
      puts $f "set Graph::Param($mode) { $Param($mode) }"
   }
   close $f
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::ItemPos>
# Creation : Juillet 2012 - J.P. Gauthier - CMC/CMOE -
#
# But      : Afficher la position dans la vue
#
# Parametres :
#   <Frame>  : Identificateur de page
#   <VP>     : Identificateur de la vue
#   <Coords> : Coordonnes
#   <Desc>   : Descripteur
#   <Tag>    : Tag a apposer pour le canvas
#   <Type>   : Type de position (POINT,BOX,LINE,POLYGON)
#   <Marks>  : Marqueurs
#
#-------------------------------------------------------------------------------

proc Graph::ItemPos { Frame VP Coords Desc Tag { Type POINT } { Marks {} } } {
   variable Graph
   variable Data

   if { ![llength $Coords] || [lindex $Coords 0]==-999 } {
      return
   }

   if { [Page::Registered $Frame Viewport $VP]==-1 } {
      return
   }

   switch $Type {
      "POINT" {
            if { [set xy [$VP -project [lindex $Coords 0] [lindex $Coords 1] 0]]!="" && [lindex $xy 2]>0 } {
               set x [lindex $xy 0]
               set y [lindex $xy 1]

               if { $Data(ShowCoord) } {
                  set Desc "$Desc\n[format "(%.3f,%.3f)" [lindex $Coords 0] [lindex $Coords 1]]"
               }

               $Frame.page.canvas create text [expr $x+6] $y -text $Desc \
                  -fill $Graph::Color(Select) -font $Graph::Font(Select) -tags "PAGE$VP $Tag" -anchor w
               $Frame.page.canvas create oval [expr $x-2] [expr $y-2] [expr $x+2] [expr $y+2] -fill $Graph::Color(Select) \
                  -tags "PAGE$VP $Tag" -outline $Graph::Color(Select)
            }
      }
      "BOX" {
            if { [llength $Coords]==4 } {
               set la0 [lindex $Coords 0]
               set lo0 [lindex $Coords 1]
               set la1 [lindex $Coords 2]
               set lo1 [lindex $Coords 3]
               Viewport::DrawLine $Frame $VP "$la0 $lo0 0 $la1 $lo0 0 $la1 $lo1 0 $la0 $lo1 0 $la0 $lo0 0" $Tag $Graph::Color(Select) 2

               if { [set xy [$VP -project $la0 $lo1 0]]!= "" && [lindex $xy 2]>0 } {
                  set x [lindex $xy 0]
                  set y [lindex $xy 1]

                  if { $Data(ShowCoord) } {
                     set Desc "$Desc\n[format "(%.3f,%.3f - %.3f,%.3f)" $la0 $lo0 $la1 $lo1]"
                  }
                  $Frame.page.canvas create text [expr [lindex $xy 0]-2] [expr [lindex $xy 1]-2] -text $Desc \
                     -fill $Graph::Color(Select) -font $Graph::Font(Select) -tags "PAGE$VP $Tag" -anchor se
               }
            }
      }
      "POLYGON" {
            set coords {}
            foreach { lat lon } $Coords {
               lappend coords $lat $lon 0.0
            }
            lappend coords [lindex $Coords 0] [lindex $Coords 1] 0.0
            lappend Tag PAGE$VP

#            Viewport::DrawArea $Frame $VP $coords $Tag $Tag $Graph::Color(Select) $Graph::Color(Select) "" False 2
            Viewport::DrawLine $Frame $VP $coords $Tag $Graph::Color(Select) 2
      }
      "LINE" {
            set coords { }
            set i -1
            foreach { lat lon } $Marks  {
               if { [set xy [$VP -project $lat $lon 0]]!="" && [lindex $xy 2]>0 } {
                  lappend coords [lindex $xy 0] [lindex $xy 1]
                  set id [lindex $Graph(Identitys) [incr i]]
                  if { $Data(ShowCoord) } {
                     set id "$id\n[format "(%.3f,%.3f)" $lat $lon]"
                  }
                  $Frame.page.canvas create text [expr [lindex $xy 0]-2] [expr [lindex $xy 1]-2] -anchor se -text $id -font $Graph::Font(Select) \
                     -fill $Graph::Color(Select) -tags "PAGE$VP $Tag"
               }
            }

            foreach { lat lon } $Coords {
               if { [set xy [$VP -project $lat $lon 0]]!="" && [lindex $xy 2]>0 } {
                  set x [lindex $xy 0]
                  set y [lindex $xy 1]
                  #----- Check close to cursor position cause it breaks in follow mode
                  if { [expr hypot($x-$Viewport::Map(X),$y-$Viewport::Map(Y))]>2 } {
                     $Frame.page.canvas create rectangle [expr $x-1] [expr $y-1] [expr $x+1] [expr $y+1] -width 1 -outline "" \
                        -fill $Graph::Color(Select) -tags "PAGE$VP $Tag"
                  }
               }
            }
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::ItemConfigure>
# Creation : Juillet 2012 - J.P. Gauthier - CMC/CMOE -
#
# But      : Appliquer les parametres aux item du graph
#
# Parametres :
#   <Graph>  : Identificateur du Graph
#   <Type>   : Type de graph
#   <Item>   : Item de graph
#
#-------------------------------------------------------------------------------

proc Graph::ItemConfigure { Graph Type Item } {
   variable Data
   global  GDefs

   upvar #0 Graph::${Type}::${Type}${Graph}::Data data

   if { ![graphitem is $Item] } {
      return
   }

   if { $Graph::Item(Stipple)!=-1 } {
      set fill    $Graph::Item(FillColor)
      set stipple $Graph::Item(Stipple)
   } else {
      set fill    ""
      set stipple ""
   }

   if { $Graph::Item(Icon)!="BARB" } {
      set icon $Graph::Item(Icon)
   } else {
      set icon ""
   }
#   set Graph::Item(Bitmap) @$GDefs(Dir)/share/bitmap/CLEAR.xbm
#   set Graph::Item(Image)  MODEL

   graphitem configure $Item -outline $Graph::Item(Outline) -fill $fill -iconoutline $Graph::Item(Outline) -iconfill $fill -transparency $Graph::Item(Tranparency) \
      -width $Graph::Item(Width) -size $Graph::Item(Size) -value $Graph::Item(Value) -dash $Graph::Item(Dash) \
      -type $Graph::Item(Type) -font $Graph::Item(Font) -icon $icon \
      -bitmap $Graph::Item(Bitmap) -stipple $stipple -image $Graph::Item(Image)

   set item [graphitem configure $Item -desc]
   if { $item!="" } {
      $data(Canvas) itemconfigure [graphitem configure $Item -desc] -font $Graph::Item(Font) -fill $Graph::Color(FG)
   }
   $data(Canvas) itemconfigure $Graph -bg $Graph::Color(BG)
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::ItemSelect>
# Creation : Juillet 2012 - J.P. Gauthier - CMC/CMOE -
#
# But      : Recuperer les parametres d'un item du graph pour l'interface
#
# Parametres :
#   <Item>   : Item de graph
#
#-------------------------------------------------------------------------------

proc Graph::ItemSelect { Item } {
   global GDefs
   variable Data
   variable Graph

   if { ![graphitem is $Item] || ![winfo exist $Data(Frame).item] } {
      return
   }

   set Graph::Item(Outline)     [graphitem configure $Item -outline]
   set Graph::Item(FillColor)   [graphitem configure $Item -fill]
   set Graph::Item(Tranparency) [graphitem configure $Item -transparency]
   set Graph::Item(Width)       [graphitem configure $Item -width]
   set Graph::Item(Size)        [expr int([graphitem configure $Item -size])]
   set Graph::Item(Value)       [graphitem configure $Item -value]
   set Graph::Item(Type)        [graphitem configure $Item -type]
   set Graph::Item(Dash)        [graphitem configure $Item -dash]
   set Graph::Item(Icon)        [graphitem configure $Item -icon]
   set Graph::Item(Font)        [graphitem configure $Item -font]
   set Graph::Item(Image)       [graphitem configure $Item -image]
   set Graph::Item(Bitmap)      [graphitem configure $Item -bitmap]
   set Graph::Item(Stipple)     [graphitem configure $Item -stipple]

   if { $Graph::Item(Outline)!="" } {
      $Data(Frame).item.line.col configure -fg $Graph::Item(Outline)
   }
   if { $Graph::Item(FillColor)!="" } {
      $Data(Frame).item.fill.col configure -fg $Graph::Item(FillColor)
   } else {
      set Graph::Item(Stipple) -1
   }

   catch { IcoMenu::Set $Data(Frame).item.line.dash $Graph::Item(Dash) }

   IcoMenu::Set $Data(Frame).item.fill.stipple $Graph::Item(Stipple)
   IcoMenu::Set $Data(Frame).item.line.width $Graph::Item(Width)
   IcoMenu::Set $Data(Frame).item.icon.sel $Graph::Item(Icon)

#   $Data(Frame).item.line.width configure -bitmap @$GDefs(Dir)/share/bitmap/[lindex $Graph(WidthBitmap) $Graph::Item(Width)]
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::ParamsPos>
# Creation : Juillet 2012 - J.P. Gauthier - CMC/CMOE -
#
# But      : Interface des parametres de positions
#
# Parametres :
#   <Parent> : Fenetre parent
#
#-------------------------------------------------------------------------------

proc Graph::ParamsPos { Parent } {
   global GDefs
   variable Lbl
   variable Data
   variable Graph
   variable Bubble

   set Data(Frame) $Parent

   labelframe $Parent.pos -text [lindex $Lbl(Pos) $GDefs(Lang)]
      ComboBox::Create $Parent.pos.sel Graph::Data(Pos) edit unsorted nodouble -1 { } 10 5 Graph::PosSelect \$Graph::Data(Graph) \$Graph::Data(Type)
      button $Parent.pos.add -image ARROWADD -relief flat -bd 0 -overrelief raised -command { Graph::PosAdd $Graph::Data(Graph) $Graph::Data(Type) }
      button $Parent.pos.del -image ARROWDEL -relief flat -bd 0 -overrelief raised -command { Graph::PosDel $Graph::Data(Graph) $Graph::Data(Type) }
      pack $Parent.pos.sel -side left -fill x -expand true -padx 2
      pack $Parent.pos.add $Parent.pos.del -side left
   pack $Parent.pos -side top -fill x -padx 5 -pady 5

   Bubble::Create $Parent.pos.sel $Bubble(PosList)
   Bubble::Create $Parent.pos.add $Bubble(PosAdd)
   Bubble::Create $Parent.pos.del $Bubble(PosDel)
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::PosSave>
# Creation : Juillet 2012 - J.P. Gauthier - CMC/CMOE -
#
# But      : Sauvegarder la position courante (nom et coordonnees)
#
# Parametres :
#   <Type>   : Type de graph
#   <Graph>  : Identificateur du Graph
#   <Pos>    : Position courante
#
#-------------------------------------------------------------------------------

proc Graph::PosSave { Type Graph Pos } {
   variable Param
   variable Msg
   variable Lbl

   upvar #0 Graph::${Type}::${Type}${Graph}::Data data

   if { [llength $data(Pos$Pos)] && [set name [Dialog::Get . $Lbl(Save) $Msg(PosSave)]]!="" } {
      if { [set idx [lsearch -exact -index 0 $Param($Param(SelectMode)) $name]]!=-1 } {
         if { ![Dialog::Default . 300 WARNING $Msg(PosExist) "\n\n\t$name\n" 1 $Lbl(Yes) $Lbl(No)] } {
            lset Param($Param(SelectMode)) $idx [list $name $data(Coords$Pos)]
         } else {
            return
         }
      } else {
         lappend Param($Param(SelectMode)) [list $name $data(Coords$Pos)]
      }
      Graph::SaveParams
      Graph::ModeSelect $Param(SelectMode)
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::PosDelete>
# Creation : Juillet 2012 - J.P. Gauthier - CMC/CMOE -
#
# But      : Supprimer la position courante
#
# Parametres :
#   <Type>   : Type de graph
#   <Graph>  : Identificateur du Graph
#   <Pos>    : Position courante
#
#-------------------------------------------------------------------------------

proc Graph::PosDelete { Type Graph Pos } {
   variable Param
   variable Msg
   variable Lbl

   if { $Param(LOCATION)!="" && [set idx [lsearch -exact -index 0 $Param($Param(SelectMode)) $Param(LOCATION)]]!=-1 } {
      if { ![Dialog::Default . 300 WARNING $Msg(PosDel) "\n\n\t$Param(LOCATION)\n" 1 $Lbl(Yes) $Lbl(No)] } {
         set Param($Param(SelectMode)) [lreplace $Param($Param(SelectMode)) $idx $idx]
         Graph::SaveParams
         Graph::ModeSelect $Param(SelectMode)
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::PosAdd>
# Creation : Juillet 2012 - J.P. Gauthier - CMC/CMOE -
#
# But      : Appliquer les parametres aux item du graph
#
# Parametres :
#   <Graph>  : Identificateur du Graph
#   <Type>   : Type de graph
#   <Item>   : Item de graph
#
#-------------------------------------------------------------------------------

proc Graph::PosAdd { Graph Type } {
   variable Data

   upvar #0 Graph::${Type}::${Type}${Graph}::Data data

   if { [winfo exist $Data(Frame).pos.sel] } {
      ComboBox::DelAll $Data(Frame).pos.sel
   }

   set Data(Pos)  Position[incr Data(PosNo)]

   Graph::${Type}::ItemDefine $Graph $Data(Pos) { }

   if { [winfo exist $Data(Frame).pos.sel] } {
      ComboBox::AddList $Data(Frame).pos.sel $data(Pos)
   }

   set Data(Item) [lindex [set Data(Items) $data(Items$Data(Pos))] 0]

   if { [winfo exist $Data(Frame).item.sel.list] } {
      $Data(Frame).item.sel.list selection set 0
   }
   Graph::ItemSelect $Graph::Data(Item)
}

proc Graph::PosSet { Graph Type } {
   variable Data

   upvar #0 Graph::${Type}::${Type}${Graph}::Data data
   upvar #0 Graph::${Type}::${Type}${Graph}::Graph graph

   if { [winfo exist $Data(Frame).pos.sel] } {
      ComboBox::DelAll $Data(Frame).pos.sel
      ComboBox::AddList $Data(Frame).pos.sel $data(Pos)
   }

   if { [set Data(Pos) [lindex $data(Pos) end]]!="" } {
      if { [winfo exist $Data(Frame).scaleY.z.val] } {
         ComboBox::DelAll $Data(Frame).scaleY.z.val False
         ComboBox::AddList $Data(Frame).scaleY.z.val $data(ZTypes$Data(Pos))
      }

      set Data(Item) [lindex [set Data(Items) $data(Items$Data(Pos))] 0]
      if { [winfo exist $Data(Frame).item.sel.list] } {
         $Data(Frame).item.sel.list selection set 0
      }
      Graph::ItemSelect $Graph::Data(Item)
   }
}

proc Graph::PosDel { Graph Type } {
   variable Data

   upvar #0 Graph::${Type}::${Type}${Graph}::Data  data

   Graph::${Type}::ItemUnDefine $Graph $Data(Pos)
   set Data(Item) [lindex [set Data(Items) $data(Items$Data(Pos))] 0]
   if { [winfo exist $Data(Frame).item.sel.list] } {
      $Data(Frame).item.sel.list selection set 0
   }
   Graph::ItemSelect $Graph::Data(Item)

   if { [winfo exist $Data(Frame).pos.sel] } {
      ComboBox::DelAll $Data(Frame).pos.sel
      ComboBox::AddList $Data(Frame).pos.sel $data(Pos)
   }
   set Data(Pos) [lindex $data(Pos) end]
}

proc Graph::PosDelAll { Graph Type } {
   variable Data

   upvar #0 Graph::${Type}::${Type}${Graph}::Data  data

   foreach pos $data(Pos) {
      Graph::${Type}::ItemUnDefine $Graph $pos
   }
   set Data(Item) ""
   if { [winfo exist $Data(Frame).item.sel.list] } {
      $Data(Frame).item.sel.list selection set 0
   }
   Graph::ItemSelect $Graph::Data(Item)

   if { [winfo exist $Data(Frame).pos.sel] } {
      ComboBox::DelAll $Data(Frame).pos.sel
   }
   set Data(Pos) ""
}

proc Graph::PosSelect { Graph Type } {
   variable Data

   upvar #0 Graph::${Type}::${Type}${Graph}::Data  data
   upvar #0 Graph::${Type}::${Type}${Graph}::Graph graph

   set Data(Item) [lindex [set Data(Items) $data(Items$Data(Pos))] 0]
   if { [winfo exist $Data(Frame).item.sel.list] } {
      $Data(Frame).item.sel.list selection clear 0 end
      $Data(Frame).item.sel.list selection set 0
   }

   if { [winfo exist $Data(Frame).scaleY.z.val] } {
      ComboBox::DelAll $Data(Frame).scaleY.z.val False
      ComboBox::AddList $Data(Frame).scaleY.z.val $data(ZTypes$Data(Pos))
   }

   Graph::ItemSelect $Graph::Data(Item)
}

#----------------------------------------------------------------------------
# Nom      : <Graph::ParamsGraph>
# Creation : Mars 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer le frame des options generales
#
# Parametres :
#   <Parent> : Frame Parent
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Graph::ParamsGraph { Parent } {
   global GDefs
   variable Lbl
   variable Bubble

   if  { ![winfo exists  $Parent.graph] } {
      checkbutton $Parent.graphtoggle -text [lindex $Lbl(Graph) $GDefs(Lang)] -compound right -variable Graph::Graph(ParamsGraph) -onvalue True -offvalue False \
         -command "Graph::ParamsGraph $Parent" -indicatoron False -bd 0 -activebackground $GDefs(ColorHighLight) -selectcolor $GDefs(ColorFrame)
      labelframe $Parent.graph -labelwidget $Parent.graphtoggle
   }

   if { $Graph::Graph(ParamsGraph) } {

      $Parent.graphtoggle configure -bitmap @$GDefs(Dir)/share/bitmap/up.xbm
      destroy $Parent.graph.lbl

      frame $Parent.graph.info
         label $Parent.graph.info.lbl -text [lindex $Lbl(Info) $GDefs(Lang)] -width 12 -anchor w
         button $Parent.graph.info.sel -bitmap @$GDefs(Dir)/share/bitmap/font.ico  -relief groove \
            -command "FontBox::Create $Parent.graph.info.sel Graph::Configure $Graph::Font(Graph)"
         ColorBox::CreateSel $Parent.graph.info.col ::Graph::Color(FG) Graph::Configure
         pack $Parent.graph.info.lbl $Parent.graph.info.col $Parent.graph.info.sel -side left
      frame $Parent.graph.axis
         label $Parent.graph.axis.lbl -text [lindex $Lbl(Axis) $GDefs(Lang)] -width 12 -anchor w
         button $Parent.graph.axis.sel -bitmap @$GDefs(Dir)/share/bitmap/font.ico  -relief groove \
            -command "FontBox::Create $Parent.graph.axis.sel Graph::Configure $Graph::Font(Axis)"
         ColorBox::CreateSel $Parent.graph.axis.col ::Graph::Color(Axis) Graph::Configure
         pack $Parent.graph.axis.lbl $Parent.graph.axis.col $Parent.graph.axis.sel -side left
      frame $Parent.graph.select
         label $Parent.graph.select.lbl -text [lindex $Lbl(Select) $GDefs(Lang)] -width 12 -anchor w
         button $Parent.graph.select.sel -bitmap @$GDefs(Dir)/share/bitmap/font.ico -relief groove \
            -command "FontBox::Create $Parent.graph.select.sel Graph::Configure $Graph::Font(Select)"
         ColorBox::CreateSel $Parent.graph.select.col Graph::Color(Select) Graph::Configure
         checkbutton $Parent.graph.select.coord -bitmap @$GDefs(Dir)/share/bitmap/coord.ico -relief groove \
            -variable Graph::Data(ShowCoord) -indicatoron false -command Graph::Configure -onvalue True -offvalue False
         pack $Parent.graph.select.lbl $Parent.graph.select.col $Parent.graph.select.sel $Parent.graph.select.coord -side left
      pack $Parent.graph.info $Parent.graph.axis $Parent.graph.select -side top

      frame $Parent.graph.color
         label $Parent.graph.color.lbl -text [lindex $Lbl(Background) $GDefs(Lang)] -width 12 -anchor w
         ColorBox::CreateSel $Parent.graph.color.col Graph::Color(Fill) Graph::Configure
         pack $Parent.graph.color.lbl $Parent.graph.color.col -side left
      frame $Parent.graph.frame
         label $Parent.graph.frame.lbl -text [lindex $Lbl(Frame) $GDefs(Lang)] -width 12 -anchor w
         ColorBox::CreateSel $Parent.graph.frame.col Graph::Color(BG) Graph::Configure
         IcoMenu::Create $Parent.graph.frame.sz $GDefs(Dir)/share/bitmap \
            "zeroth.xbm width1.xbm width2.xbm width3.xbm width4.xbm width5.xbm" "0 1 2 3 4 5" \
            Graph::Width(Frame) "Graph::Configure" $Graph::Width(Frame) -relief groove -bd 2
         pack $Parent.graph.frame.lbl  $Parent.graph.frame.col $Parent.graph.frame.sz -side left
      frame $Parent.graph.update -relief sunken -bd 1
         checkbutton $Parent.graph.update.sel -indicatoron False -text [lindex $Lbl(Update) $GDefs(Lang)] -bd 1 -onvalue True -variable Graph::Data(Update) \
            -command { Graph::${Graph::Data(Type)}::Graph $Graph::Data(Graph) }
         pack $Parent.graph.update.sel -side left -fill x -expand true
      frame $Parent.graph.ip3 -relief sunken -bd 1
         checkbutton  $Parent.graph.ip3.sel -text [lindex $Lbl(IP3) $GDefs(Lang)] -indicatoron false \
            -command "" -bd 1 -variable Graph::Data(IP3) -onvalue True -offvalue False
         pack $Parent.graph.ip3.sel -side left -fill x -expand true
      pack $Parent.graph.info $Parent.graph.axis $Parent.graph.select $Parent.graph.color $Parent.graph.frame $Parent.graph.update $Parent.graph.ip3 -side top  -fill x

      Bubble::Create $Parent.graph.info   $Bubble(Info)
      Bubble::Create $Parent.graph.select $Bubble(Pick)
      Bubble::Create $Parent.graph.color  $Bubble(Back)
      Bubble::Create $Parent.graph.frame  $Bubble(Frame)
      Bubble::Create $Parent.graph.update $Bubble(Update)
      Bubble::Create $Parent.graph.ip3    $Bubble(IP3)
   } else {
      $Parent.graphtoggle configure -bitmap @$GDefs(Dir)/share/bitmap/down.xbm
      destroy $Parent.graph.info $Parent.graph.axis $Parent.graph.select $Parent.graph.color $Parent.graph.frame $Parent.graph.update $Parent.graph.ip3
      label $Parent.graph.lbl -text "" -width 12 -anchor w
      pack $Parent.graph.lbl -side left
   }
   pack $Parent.graph -side top -fill x -pady 5 -padx 5 -anchor n
}

#----------------------------------------------------------------------------
# Nom      : <Graph::ParamsObs>
# Creation : Mars 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer le frame de selection des observations
#
# Parametres :
#   <Parent> : Frame Parent
#   <Type>   : Type de graph
#   <Graph>  : Indentificateur du Graph
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Graph::ParamsObs { Parent Graph Type } {
   global GDefs
   variable Lbl
   variable Bubble

   if  { ![winfo exists  $Parent.obs] } {
      checkbutton $Parent.obstoggle -text [lindex $Lbl(Obs) $GDefs(Lang)] -compound right -variable Graph::Graph(ParamsObs) -onvalue True -offvalue False \
         -command "Graph::ParamsObs $Parent $Graph $Type" -indicatoron False  -bd 0 -activebackground $GDefs(ColorHighLight) -selectcolor $GDefs(ColorFrame)
      labelframe $Parent.obs -labelwidget $Parent.obstoggle
   }

   if { $Graph::Graph(ParamsObs) } {

      $Parent.obstoggle configure -bitmap @$GDefs(Dir)/share/bitmap/up.xbm
      destroy $Parent.obs.lbl

         entry $Parent.obs.sel  -relief sunken -bd 1 -bg $GDefs(ColorLight) -textvariable Graph::${Type}::${Type}${Graph}::Data(ObsToken)
         frame $Parent.obs.list
            listbox $Parent.obs.list.box -relief sunken -bd 1 -bg $GDefs(ColorLight) -height 4 \
               -yscrollcommand [list $Parent.obs.list.scroll set] -width 20
            scrollbar $Parent.obs.list.scroll -command "$Parent.obs.list.box yview" -width 10 -bd 1
            pack $Parent.obs.list.box -side left -fill both -expand true
            pack $Parent.obs.list.scroll -side left -fill y
         pack $Parent.obs.sel  -side top -fill x -padx 2
         pack $Parent.obs.list -side top  -fill both -expand true -padx 2

      Bubble::Create $Parent.obs.sel  $Bubble(ObsSearch)
      Bubble::Create $Parent.obs.list $Bubble(ObsList)

      Graph::ParamsObsSearch $Type $Graph

      bind $Parent.obs.list.box <B1-ButtonRelease> "Graph::ParamsObsSelect $Type $Graph \[%W get \[%W nearest %y\]\]"
      bind $Parent.obs.sel <Any-KeyRelease> "Graph::ParamsObsSearch $Type $Graph"
      pack $Parent.obs -side top -fill both -expand true -pady 5 -padx 5 -anchor n
   } else {
      $Parent.obstoggle configure -bitmap @$GDefs(Dir)/share/bitmap/down.xbm
      destroy $Parent.obs.sel $Parent.obs.list
      label $Parent.obs.lbl -text "" -width 12 -anchor w
      pack $Parent.obs.lbl -side left
      pack $Parent.obs -side top -fill x -pady 5 -padx 5 -anchor n
   }
}

#----------------------------------------------------------------------------
# Nom      : <Graph::ParamsObsSelect>
# Creation : Mars 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectuer la selection d'une observation
#
# Parametres :
#   <Type>   : Type de graph
#   <Graph>  : Indentificateur du Graph
#   <Desc>   : Identification de l'obs
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Graph::ParamsObsSelect { Type Graph Desc } {

   upvar #0 Graph::${Type}::${Type}${Graph}::Data  data

   foreach item $data(Data) {
      if { [observation is $item] } {
         set idx [lindex [observation define $item -IDX $Desc] 0]
         set coords [lrange [observation define $item -COORD $idx] 0 1]
         set data(Obs$Graph::Data(Pos)) $Desc
      }
   }
   Graph::${Type}::ItemDefine $Graph $Graph::Data(Pos) $coords
}

#----------------------------------------------------------------------------
# Nom      : <Graph::ParamsObsSearch>
# Creation : Mars 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Rechercher les observations selon les criteres
#
# Parametres :
#   <Type>   : Type de graph
#   <Graph>  : Indentificateur du Graph
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Graph::ParamsObsSearch { Type Graph } {
   variable Data

   upvar #0 Graph::${Type}::${Type}${Graph}::Data  data

   if { [winfo exists .graphparams] && [winfo exists $Data(Tab).graph.obs.list.box] } {

      $Data(Tab).graph.obs.list.box delete 0 end
      foreach obs $data(ObsIds) {
         if { [string match -nocase *$data(ObsToken)* $obs] } {
            $Data(Tab).graph.obs.list.box insert end $obs
         }
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::RangeDraw>
# Creation : Octobre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche les delimitateurs sur le graphe.
#
# Parametres :
#   <Type>   : Type de graph
#   <Graph>  : Indentificateur du Graph
#   <Place>  : Positionnement par coordonnee
#   <Id>     : Id du delimitateur (0,1,...)
#   <Y>      : Coordonnee du positionnement
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Graph::RangeDraw { Type Graph Place Id Y } {

   upvar #0 Graph::${Type}::${Type}${Graph}::Data  data
   upvar #0 Graph::${Type}::${Type}${Graph}::Graph graph

   if { $graph(Range) } {
      if { $Place && $data(YIncr)!=0 } {
         set y  [expr ($Y-$graph(Y0))/$data(YIncr)]

         if { $graph(YScale)=="Log" } {
            set data(Range$Id) [expr pow(10,-$y + $data(Y0))]
         } else {
            set data(Range$Id) [expr -$y+$data(Y0)]
         }
      }

      set y [${Type}::DrawY $Graph $data(Range$Id)]

      if { $y<$graph(Y0) && $y>$graph(Y1) } {
         if { [$data(Canvas) find withtag RANGE$Id$Graph]=="" } {
            $data(Canvas) create line $graph(X0) $y $graph(X1) $y -fill #FF0000 -tags "PAGE$Graph RANGE$Graph RANGE$Id$Graph"
            $data(Canvas) bind RANGE$Id$Graph <Enter> "$data(Canvas) configure -cursor hand1"
            $data(Canvas) bind RANGE$Id$Graph <B1-Motion> "Graph::RangeDraw $Type $Graph 1 $Id %y"
            $data(Canvas) bind RANGE$Id$Graph <Leave> "$data(Canvas) configure -cursor left_ptr"
         } else {
            $data(Canvas) coords RANGE$Id$Graph $graph(X0) $y $graph(X1) $y
         }
      }
   } else {
      $data(Canvas) delete RANGE$Id$Graph
   }
}

#------------------------------------------------------------------------------
# Nom      : <Graph::Translate>
# Creation : Mars 2004 - J.P. Gauthier - CMC/CMOE -
#
# But     : Effectuer la translation dans le graph
#
# Parametres :
#   <Frame>  : Page
#   <Type>   : Type de graph
#   <Graph>  : Identificateur du Graph
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Graph::Translate { Frame Type Graph X Y } {
   variable Data

   upvar #0 Graph::${Type}::${Type}${Graph}::Data  data
   upvar #0 Graph::${Type}::${Type}${Graph}::Graph graph

   #----- Process the coords

   set coords [lrange [$Graph -unproject $X $Y False $Graph::Data(Item)] 0 1]

   if { [llength $coords] } {

      if { [llength $graph(ZXInter)] && $Data(X0)!="" } {
         set dx [expr $Data(X0)-[lindex $coords 0]]
         set graph(ZXInter) [list [expr [lindex $graph(ZXInter) 0]+$dx] [expr [lindex $graph(ZXInter) 1]+$dx]]
      }

      if { [llength $graph(ZYInter)] && $Data(Y0)!=""  } {
         set dy [expr $Data(Y0)-[lindex $coords 1]]
         set graph(ZYInter) [list [expr [lindex $graph(ZYInter) 0]+$dy] [expr [lindex $graph(ZYInter) 1]+$dy]]
      }

      #----- Refresh the graph

      Graph::${Type}::Graph $Graph
   }
}

#------------------------------------------------------------------------------
# Nom      : <Graph::TranslateDone>
# Creation : Mars 2004 - J.P. Gauthier - CMC/CMOE -
#
# But     : Finaliser la fonction de Translation des Graph
#
# Parametres :
#   <Frame>  : Page
#   <Type>   : Type de graph
#   <Graph>  : Identificateur du Graph
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Graph::TranslateDone { Frame Type Graph } {
   Graph::Resolution $Frame $Type 1
}

#------------------------------------------------------------------------------
# Nom      : <Graph::TranslateInit>
# Creation : Mars 2004 - J.P. Gauthier - CMC/CMOE -
#
# But     : Initialiser la fonction de Translation des Graph
#
# Parametres :
#   <Frame>  : Page
#   <Type>   : Type de graph
#   <Graph>  : Identificateur du Graph
#   <X>      : Coordonnee X initiale
#   <Y>      : Coordonnee Y initiale
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Graph::TranslateInit { Frame Type Graph X Y } {
   variable Data

   set coords [lrange [$Graph -unproject $X $Y False $Graph::Data(Item)] 0 1]
   set Data(X0) [lindex $coords 0]
   set Data(Y0) [lindex $coords 1]

   Graph::Resolution $Frame $Type 2
}

#------------------------------------------------------------------------------
# Nom      : <LegendMoveInit>
# Creation : Fevrier 2009 - J.P. Gauthier - CMC/CMOE -
#
# But     : Initialiser les fonctions de deplacement de la legende ou dans le graph
#
# Parametres :
#   <Frame>  : Identificateur de page
#   <Canvas> : Identificateur de canvas
#   <Graph>  : Identificateur du graph
#   <Type>   : Type de graph
#   <X>      : Coordonne X du pointeur
#   <Y>      : Coordonne Y du pointeur
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Graph:LegendMoveInit { Frame Canvas Graph Type X Y } {
   variable Data

   if { [set Data(Legend) [$Graph -header $X $Y]] } {

      set Data(XLegend) [lindex [$Canvas itemconfigure $Graph -xlegend] end]
      set Data(YLegend) [lindex [$Canvas itemconfigure $Graph -ylegend] end]

      set Data(X) $X
      set Data(Y) $Y
   } else {
      Graph::Activate $Frame $Graph $Type
      Graph::TranslateInit $Frame $Type $Graph [$Canvas canvasx $X] [$Canvas canvasy $Y]
   }
   $Canvas config -cursor hand1
}

#------------------------------------------------------------------------------
# Nom      : <LegendMove>
# Creation : Fevrier 2009 - J.P. Gauthier - CMC/CMOE -
#
# But     : Effectuer les fonctions de deplacement de la legende ou dans le graph
#
# Parametres :
#   <Frame>  : Identificateur de page
#   <Canvas> : Identificateur de canvas
#   <Graph>  : Identificateur du graph
#   <Type>   : Type de graph
#   <X>      : Coordonne X du pointeur
#   <Y>      : Coordonne Y du pointeur
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Graph:LegendMove { Frame Canvas Graph Type X Y } {
   variable Data

   if { $Data(Legend) } {
      $Canvas itemconfigure $Graph -xlegend [expr $Data(XLegend)+($X-$Data(X))] -ylegend [expr $Data(YLegend)+($Y-$Data(Y))]
   } else {
      Graph::Translate $Frame $Type $Graph [$Canvas canvasx $X] [$Canvas canvasy $Y]
   }
}

#------------------------------------------------------------------------------
# Nom      : <LegendMoveDone>
# Creation : Fevrier 2009 - J.P. Gauthier - CMC/CMOE -
#
# But     : Finaliser les fonctions de deplacement de la legende ou dans le graph
#
# Parametres :
#   <Frame>  : Identificateur de page
#   <Canvas> : Identificateur de canvas
#   <Graph>  : Identificateur du graph
#   <Type>   : Type de graph
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Graph:LegendMoveDone { Frame Canvas Graph Type } {
   variable Data

   if { $Data(Legend) } {
      set Data(Legend) False
   } else {
      Graph::TranslateDone $Frame $Type $Graph;
   }
   $Canvas config -cursor left_ptr
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Update>
# Creation : Avril 2003 - J.P. Gauthier - CMC/CMOE -
#
# But      : Mise a jour des graphs associees a une page
#
# Parametres :
#   <Frame>  : Identificateur de Page
#
#-------------------------------------------------------------------------------

proc Graph::Update { Frame } {
   variable Data

   foreach type $Data(Types) {
      Graph::${type}::Update $Frame
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::UpdateItems>
# Creation : Avril 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectuer le "Refresh" des items relatifs station sur
#            la projection.
#
# Parametres :
#   <Frame>  : Identificateur de Page
#
# Remarques :
#    - Cette fonctions est appele par le package Viewport au besoin.
#
#-------------------------------------------------------------------------------

proc Graph::UpdateItems { Frame } {
   variable Data

   foreach type $Data(Types) {
      if { [info procs ::Graph::${type}::UpdateItems]!="" } {
         Graph::${type}::UpdateItems $Frame
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::TimeFormat>
# Creation : Octobre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Format du temps en secondes.
#
# Parametres :
#   <Sec>    : Secondes depuis l'origine
#   <Mode>   : Type de format (S, M, H, D ou format ("%Y%m%d%H%M"))
#   <From>   : Secondes du point de depart
#
# Retour     :
#   <Time>   : Temps formatte
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Graph::TimeFormat { Sec Mode { From 0 } } {
   variable Data

   switch $Mode {
      "S"     { return [expr int($Sec-$From)] }
      "M"     { return [expr int($Sec-$From)/60] }
      "H"     { return [expr int($Sec-$From)/3600] }
      "D"     { return [expr int($Sec-$From)/86400] }
      "DATE"  { return [clock format [expr int($Sec)] -format "%d/%m %H:%M" -timezone :UTC] }
      default { return [clock format [expr int($Sec)] -format $Mode -timezone :UTC] }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::ValFormat>
# Creation : Octobre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Formatte une valeur selon l'ordre de grandeur specifie
#
# Parametres :
#   <Order>  : Ordre de grandeur de la valeur
#   <Val>    : Valeur
#
# Retour     :
#   <Time>   : Temps formatte
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Graph::ValFormat { Order Val } {

   if { $Order>6 || $Order<-3 } {
     set val [format "%.1e" $Val]
   } elseif { $Order<1 } {
      set d [expr int(ceil(abs($Order)+1))]
      set val [format "%.${d}f" $Val]
   } else {
      set val [format "%.0f" $Val]
   }

   return $val
}

#------------------------------------------------------------------------------
# Nom      : <Graph::Write>
# Creation : Novembre 2003 - J.P. Gauthier - CMC/CMOE -
#
# But     : Enregistrer les parametres des Graph dans un fichier Layout
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <File>   : Identificateur de Fichier
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Graph::Write { Frame File } {
   variable Data


   set no 1
   foreach type $Data(Types) {
      set grs [Page::Registered $Frame Graph::${type}]

      if { [llength $grs] } {
         puts $File "   #----- Affichage des graphs $type"
         puts $File ""
         foreach gr $grs {
            eval puts $File \"     set Data(Graph[format "%03i" $no]) \\\[Graph::${type}::Create \\\$Frame \$Graph::Data(X$gr) \$Graph::Data(Y$gr) \
               \$Graph::Data(Width$gr) \$Graph::Data(Height$gr) \$Graph::Data(Active$gr) \$Graph::Data(Full$gr)\\\]\"
            incr no
         }
         puts $File ""
      }
   }
}

#------------------------------------------------------------------------------
# Nom      : <Graph::ZoomScroll>
# Creation : Mars 2004 - J.P. Gauthier - CMC/CMOE -
#
# But     : Effectuer le zoom dans le graph avec la roulette
#
# Parametres   :
#   <Type>     : Type de graph
#   <Graph>    : Identificateur du Graph
#   <X>        : Coordonnee X du curseur
#   <Y>        : Coordonnee Y du curseur
#   <Incr>     : Increment
#   <Centered> : Centrer sur le curseur
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Graph::ZoomScroll { Type Graph X Y Incr { Centered True } } {
   variable Data

   upvar #0 Graph::${Type}::${Type}${Graph}::Data  data
   upvar #0 Graph::${Type}::${Type}${Graph}::Graph graph

   #----- Get current limits
   set xmin [graphaxis configure axisx$Graph -min]
   set xmax [graphaxis configure axisx$Graph -max]
   set ymin [graphaxis configure axisy$Graph -min]
   set ymax [graphaxis configure axisy$Graph -max]

   #----- Define scaling
   set nan [catch {
      set sx [expr ($xmax-$xmin)*$Incr]
      set sy [expr ($ymax-$ymin)*$Incr] } ]

   if { $nan } {
      return
   }

   if { $Centered } {

      #----- Get value under cursor
      if { ![llength [set coords [lrange [$Graph -unproject $X $Y False $Graph::Data(Item)] 0 1]]] } {
         return
      }
      set x [lindex $coords 0]
      set y [lindex $coords 1]

      #----- Define translation
      set dx [expr ($x-($xmax+$xmin)*0.5)*$Incr*2]
      set dy [expr ($y-($ymax+$ymin)*0.5)*$Incr*2]
   } else {
      set dx 0
      set dy 0
   }

   #----- Apply transform
   set xmin [expr $xmin+$sx+$dx]
   set xmax [expr $xmax-$sx+$dx]
   set ymin [expr $ymin+$sy+$dy]
   set ymax [expr $ymax-$sy+$dy]

   #----- Don't overshoot on zoom out
   if { $Incr<0 && ([expr abs($xmax-$xmin)] > [expr abs($data(XMax)-$data(XMin))] || [expr abs($ymax-$ymin)] > [expr abs($data(YMax)-$data(YMin))]) } {
      set graph(ZXInter) {}
      set graph(ZYInter) {}
   } else {
      set graph(ZXInter) [list $xmin $xmax]
      set graph(ZYInter) [list $ymin $ymax]
   }

   #----- Refresh the graph
   Graph::${Type}::Graph $Graph
}

#------------------------------------------------------------------------------
# Nom      : <Graph::Zoom>
# Creation : Mars 2004 - J.P. Gauthier - CMC/CMOE -
#
# But     : Effectuer le zoom dans le graph
#
# Parametres :
#   <Type>   : Type de graph
#   <Graph>  : Identificateur du Graph
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Graph::Zoom { Type Graph } {
   variable Data

   upvar #0 Graph::${Type}::${Type}${Graph}::Data  data
   upvar #0 Graph::${Type}::${Type}${Graph}::Graph graph

   $data(Canvas) delete GRAPHZOOM

   #----- Order the coords

   if { $Data(X0)>$Data(X1) } {
      set t $Data(X0)
      set Data(X0) $Data(X1)
      set Data(X1) $t
   }

   if { $Data(Y0)>$Data(Y1) } {
      set t $Data(Y0)
      set Data(Y0) $Data(Y1)
      set Data(Y1) $t
   }

   #----- Check the size of the zoombox (>10)

   if { [expr $Data(X1)-$Data(X0)]<10 } {
      return
   }
   if { [expr $Data(Y1)-$Data(Y0)]<10 } {
      return
   }

   #----- Process the coords

   set coords0 [lindex [$Graph -unproject $Data(X0) $Data(Y1) True] 0]
   set coords1 [lindex [$Graph -unproject $Data(X1) $Data(Y0) True] 0]

   set graph(ZXInter) [list [lindex $coords0 0] [lindex $coords1 0]]
   set graph(ZYInter) [list [lindex $coords0 1] [lindex $coords1 1]]

   #----- Refresh the graph

   Graph::${Type}::Graph $Graph
}

#------------------------------------------------------------------------------
# Nom      : <Graph::ZoomReset>
# Creation : Mars 2004 - J.P. Gauthier - CMC/CMOE -
#
# But     : Reinitialiser le zoom
#
# Parametres :
#   <Type>   : Type de graph
#   <Graph>  : Identificateur du Graph

# Remarques :
#
#-------------------------------------------------------------------------------

proc Graph::ZoomReset { Type Graph } {

   upvar #0 Graph::${Type}::${Type}${Graph}::Graph graph

   set graph(ZXInter) {}
   set graph(ZYInter) {}

   #----- Refresh the graph

   Graph::${Type}::Graph $Graph
}

#------------------------------------------------------------------------------
# Nom      : <Graph::ZoomBox>
# Creation : Mars 2004 - J.P. Gauthier - CMC/CMOE -
#
# But     : Generer la boite de zoom des Graph
#
# Parametres :
#   <Canvas> : Identificateur du canvas
#   <X1>     : Coordonnee X finale
#   <Y1>     : Coordonnee Y finale
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Graph::ZoomBox { Canvas X1 Y1 } {
   variable Data

   set Data(X1) $X1
   set Data(Y1) $Y1

   $Canvas coords GRAPHZOOM $Data(X0) $Data(Y0) $Data(X1) $Data(Y1)
}

#------------------------------------------------------------------------------
# Nom      : <Graph::ZoomInit>
# Creation : Mars 2004 - J.P. Gauthier - CMC/CMOE -
#
# But     : Initialiser la fonction de Zoom des Graph
#
# Parametres :
#   <Canvas> : Identificateur du canvas
#   <X0>     : Coordonnee X initiale
#   <Y0>     : Coordonnee Y initiale
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Graph::ZoomInit { Canvas X0 Y0 } {
   variable Data

   set Data(X0) $X0
   set Data(X1) $X0
   set Data(Y0) $Y0
   set Data(Y1) $Y0

   $Canvas create rectangle $X0 $Y0 $X0 $Y0 -fill "" -outline red -width 2 -tags GRAPHZOOM
}

#----------------------------------------------------------------------------
# Nom      : <Graph::Time::Draw...>
# Creation : Octobre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Fonctions de manipulation sur la projection
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <VP>      : Identificateur du Viewport
#  <Type>    : Type de graph
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Graph::DrawInit { Frame VP } {
   variable Param
   variable Data

   set Param(LOCATION) ""

   upvar #0 Graph::${Graph::Data(Type)}::${Graph::Data(Type)}${Graph::Data(Graph)}::Data data

   set data(Lat0) $Viewport::Map(LatCursor)
   set data(Lon0) $Viewport::Map(LonCursor)
   set data(Lat1) $Viewport::Map(LatCursor)
   set data(Lon1) $Viewport::Map(LonCursor)

   if { $data(FrameData)!="" } {
      $data(FrameData).page.canvas delete GRAPHSELECT$Graph::Data(Graph)
   }

   if { $VP!=$data(VP) } {
      set data(VP)        $VP
      set data(FrameData) $Frame
      Graph::${Graph::Data(Type)}::Update $Frame $Graph::Data(Graph)
   } else {
      set coords [list $data(Lat0) $data(Lon0)]
      switch [lindex $Viewport::Data(Picked) 0] {
         "observation" {
            set obj [lindex $Viewport::Data(Picked) 1]
            set idx [lindex $Viewport::Data(Picked) 2]
            set coords [lrange [observation define $obj -COORD $idx] 0 1]
            set data(Obs$Graph::Data(Pos)) [observation define $obj -ID $idx]
         }
         "metobs" {
            set obj [lindex $Viewport::Data(Picked) 1]
            set idx [lindex $Viewport::Data(Picked) 2]
            set coords [lrange [metobs define $obj -COORD $idx] 0 1]
            set data(Obs$Graph::Data(Pos)) [metobs define $obj -ID $idx]
            set data(Elem$Graph::Data(Pos)) [lindex [lindex [metmodel define [metobs define $obj -MODEL] -items] [lindex $Viewport::Data(Picked) 3]] 2]
         }
      }

      switch $Param(SelectMode) {
         "POINT" { Graph::${Graph::Data(Type)}::ItemDefine $Graph::Data(Graph) $Graph::Data(Pos) $coords }
      }
   }
}

proc Graph::Draw { Frame VP } {
   variable Param

   upvar #0 Graph::${Graph::Data(Type)}::${Graph::Data(Type)}${Graph::Data(Graph)}::Data data

   set data(Lat1) $Viewport::Map(LatCursor)
   set data(Lon1) $Viewport::Map(LonCursor)

   switch $Param(SelectMode) {
      "POINT"     { Graph::${Graph::Data(Type)}::ItemDefine $Graph::Data(Graph) $Graph::Data(Pos) [list $data(Lat1) $data(Lon1)] }
      "BOX"       { Graph::${Graph::Data(Type)}::ItemDefine $Graph::Data(Graph) $Graph::Data(Pos) [list $data(Lat0) $data(Lon0) $data(Lat1) $data(Lon1)] False }
   }
}

proc Graph::DrawDone { Frame VP } {
   variable Param

   if { $Param(SelectMode)=="POINT" } {
      return
   }

   upvar #0 Graph::${Graph::Data(Type)}::${Graph::Data(Type)}${Graph::Data(Graph)}::Data data

   #----- Sort corners (lowerleft/upperright)
   if { $data(Lat0)>$data(Lat1) } {
      set tmp $data(Lat1)
      set data(Lat1) $data(Lat0)
      set data(Lat0) $tmp
   }

   if { $data(Lon0)>$data(Lon1) } {
      set tmp $data(Lon1)
      set data(Lon1) $data(Lon0)
      set data(Lon0) $tmp
   }
   
   #----- If length>180, invert longitude to register as -180,180 wrapover
   if { [expr abs($data(Lon1)-$data(Lon0))]>180 } {
       set tmp $data(Lon1)
       set data(Lon1) $data(Lon0)
       set data(Lon0) $tmp  
   }  

   if { $data(Lat0)==$data(Lat1) || $data(Lon0)==$data(Lon1) } {
      set data(Lat0) 0
      set data(Lat1) 0
      set data(Lon0) 0
      set data(Lon1) 0
      set data(Pos$Graph::Data(Graph)$Graph::Data(Item)) {}
   } else {
      set data(Pos$Graph::Data(Graph)$Graph::Data(Item)) [list $data(Lat0) $data(Lon0) $data(Lat1) $data(Lon1)]
   }

   .                  config -cursor watch
   $Frame.page.canvas config -cursor watch
   $data(Canvas)      config -cursor watch
   $data(Frame)       config -cursor watch
   update idletasks

   Graph::${Graph::Data(Type)}::ItemDefine $Graph::Data(Graph) $Graph::Data(Pos) [list $data(Lat0) $data(Lon0) $data(Lat1) $data(Lon1)]

   .                  config -cursor left_ptr
   $Frame.page.canvas config -cursor left_ptr
   $data(Canvas)      config -cursor left_ptr
   $data(Frame)       config -cursor left_ptr
}

proc Graph::MoveInit { Frame VP } {
   variable Param

   set Param(LOCATION) ""

   if { $Param(SelectMode)=="POINT" } {
      return
   }

   upvar #0 Graph::${Graph::Data(Type)}::${Graph::Data(Type)}${Graph::Data(Graph)}::Data  data

   set data(LonD)   $Viewport::Map(LonCursor)
   set data(LatD)   $Viewport::Map(LatCursor)
   set data(Coords) $data(Pos$Graph::Data(Pos))

   if { $data(FrameData)!="" } {
      $data(FrameData).page.canvas delete GRAPHSELECT$Graph::Data(Graph)
   }

   set data(VP)        $VP
   set data(FrameData) $Frame
}

proc Graph::Move { Frame VP } {
   variable Param

   if { $Param(SelectMode)=="POINT" } {
      return
   }

   upvar #0 Graph::${Graph::Data(Type)}::${Graph::Data(Type)}${Graph::Data(Graph)}::Data  data

   #----- Effectuer la translation

   set lat0 [expr $data(Lat0) + $Viewport::Map(LatCursor) - $data(LatD)]
   set lat1 [expr $data(Lat1) + $Viewport::Map(LatCursor) - $data(LatD)]

   if { $lat0 > -90.0 && $lat0 < 90.0 && $lat1 > -90.0 && $lat1 < 90.0 } {

      set data(Lat0) $lat0
      set data(Lat1) $lat1
      eval set data(Lon0) [Viewport::CheckCoord [expr $data(Lon0) + $Viewport::Map(LonCursor) - $data(LonD)]]
      eval set data(Lon1) [Viewport::CheckCoord [expr $data(Lon1) + $Viewport::Map(LonCursor) - $data(LonD)]]
   }

   #----- Reaffecter le point de reference de translation

   set data(LonD) $Viewport::Map(LonCursor)
   set data(LatD) $Viewport::Map(LatCursor)

   Graph::${Graph::Data(Type)}::ItemDefine $Graph::Data(Graph) $Graph::Data(Pos) [list $data(Lat0) $data(Lon0) $data(Lat1) $data(Lon1)] False
}

proc Graph::MoveDone { Frame VP } {
   Graph::DrawDone $Frame $VP
}

namespace eval Shape {}

proc Shape::BindMoveGeo { Canvas Tags Var { Command "" } } {

   set tag [lindex $Tags 0]

   $Canvas bind $tag <Enter>           "$Canvas configure -cursor fleur"
   $Canvas bind $tag <Leave>           "$Canvas configure -cursor left_ptr"
   $Canvas bind $tag <ButtonPress-1>   { set Shape::Data(LonD) $Viewport::Map(LonCursor);  set Shape::Data(LatD)  $Viewport::Map(LatCursor) }
   $Canvas bind $tag <ButtonRelease-1> "set $Var \$Shape::Data(Coords)"

   if { $Command!="" } {
#      $Canvas bind $tag <B1-Motion>    "Shape::MoveGeo $Canvas \"$Tags\" $Var; $Command"
      $Canvas bind $tag <B1-Motion>    "if { \[Viewport::Follow $Page::Data(Frame) $Page::Data(VP) \[$Canvas canvasx %x\] \[$Canvas canvasy %y\]\] } { Shape::MoveGeo $Canvas \"$Tags\" $Var; break }"
   } else {
      $Canvas bind $tag <B1-Motion>    "if { \[Viewport::Follow $Page::Data(Frame) $Page::Data(VP) \[$Canvas canvasx %x\] \[$Canvas canvasy %y\]\] } { Shape::MoveGeo $Canvas \"$Tags\" $Var; break }"
   }
}

proc Shape::MoveGeo { Canvas Tags Var } {
   variable Data

   upvar #0 $Var coords

   set lat $Viewport::Map(LatCursor)
   set lon $Viewport::Map(LonCursor)

   set dist [projection function $Page::Data(Frame) -dist [list $Data(LatD) $Data(LonD) $lat $lon]]
   set bear [projection function $Page::Data(Frame) -bearing $Data(LatD) $Data(LonD) $lat $lon]

catch {
   $Canvas delete TOTO
   eval $Canvas create line [lindex [$Page::Data(VP) -projectline TRUE [list $Data(LatD) $Data(LonD) 0.0 $lat $lon 0.0]] 0] -tag TOTO -width 2 -fill black
}

   if { $dist>10 } {
      set Data(Coords) [projection function $Page::Data(Frame) -circle $lat $lon $dist $bear $coords]
      foreach { lat lon } $Data(Coords) {
         lappend coos $lat $lon 0.0
       }
      $Canvas coords $Tags [lindex [$Page::Data(VP) -projectline NONE $coos] 0]
   }
}

#----------------------------------------------------------------------------
# Nom      : <Graph::VertexAdd>
# Creation : Ocotbre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Ajout d'un point a la coupe.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <VP>      : Identificateur du Viewport
#  <X>       : Coordonnee X de la souris
#  <Y>       : Coordonnee Y de la souris
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Graph::VertexAdd { Frame VP X Y } {
   variable Param

   upvar #0 Graph::${Graph::Data(Type)}::${Graph::Data(Type)}${Graph::Data(Graph)}::Data  data

   if { $VP==-1 } {
      return
   }

   if { $VP!=$data(VP) } {
      set data(VP)        $VP
      set data(FrameData) $Frame
      Graph::${Graph::Data(Type)}::Update $Frame $Graph::Data(Graph)
   }
   set data(Field) [lindex [Viewport::Assigned $Frame $VP fstdfield] 0]

   if { $VP==-1 || $data(Field)=="" } {
      return
   }

   #----- Si la grille et le vertex est valide on l'ajoute a la liste
   set grtyp [fstdfield define $data(Field) -GRTYP]
   if { $grtyp!="V" && $grtyp!="X" && $Viewport::Map(LatCursor)>-999 && $Viewport::Map(LonCursor)>-999 } {

      Graph::VertexResolution $Graph::Data(Type) $Graph::Data(Graph) False
      lappend data(Coords) $Viewport::Map(LatCursor) $Viewport::Map(LonCursor)
      set coords $data(Coords)

      #----- Fermer le polygone
      if { $Param(SelectMode)=="POLYGON" } {
         lappend coords [lindex $coords 0] [lindex $coords 1]
      }

      #----- Afficher la base de la coupes et en recuperer les coordonnees lat-lon
      Graph::${Graph::Data(Type)}::ItemDefine $Graph::Data(Graph) $Graph::Data(Pos) $coords
   }

#   Shape::BindMoveGeo $Frame.page.canvas GRAPHSELECT$Graph::Data(Graph) Graph::${Graph::Data(Type)}::${Graph::Data(Type)}${Graph::Data(Graph)}::Data(Pos$Graph::Data(Pos)) "Graph::Time::UpdateItems $data(FrameData) $Graph::Data(Graph)"
}

#----------------------------------------------------------------------------
# Nom      : <Graph::VertexDelete>
# Creation : Octobre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      :Suppression d'un point a la coupe.
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

proc Graph::VertexDelete { Frame VP } {

   upvar #0 Graph::${Graph::Data(Type)}::${Graph::Data(Type)}${Graph::Data(Graph)}::Data  data

   if { $VP!=-1 } {
      set data(Coords) [lreplace $data(Coords) end-1 end]

      Graph::${Graph::Data(Type)}::ItemDefine $Graph::Data(Graph) $Graph::Data(Pos) $data(Coords)
   }
   $data(Canvas) delete VERTEXFOLLOW
}

#----------------------------------------------------------------------------
# Nom      : <Graph::VertexFollow>
# Creation : Octobre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche une ligne entre le dernier vertex creer et la position du
#            curseur de la souris.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <VP>      : Identificateur du Viewport
#  <X>       : Coordonnee X de la souris
#  <Y>       : Coordonnee Y de la souris
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Graph::VertexFollow { Frame VP X Y Scan } {
   global GDefs
   variable Param
   variable Lbl

   upvar #0 Graph::${Graph::Data(Type)}::${Graph::Data(Type)}${Graph::Data(Graph)}::Data  data

   if { $VP==-1 } {
      if { $data(VP)=="" } {
         return
      } else {
         set VP $data(VP)
      }
   }

   if { $data(FrameData)!="" && [llength $data(Items$Graph::Data(Pos))] } {
      set coords $data(Coords)

      if { $Viewport::Map(LatCursor)>-999 && $Viewport::Map(LonCursor)>-999 } {
         lappend coords $Viewport::Map(LatCursor) $Viewport::Map(LonCursor)
      }

      if { $Param(SelectMode)=="POLYGON" } {
         lappend coords [lindex $coords 0] [lindex $coords 1]
      }

      $Frame.page.canvas delete GRAPHSELECT$Graph::Data(Graph)
      set smpl [Graph::VertexSample $Graph::Data(Type) $Graph::Data(Graph) $coords]
      set id   [graphitem configure [lindex $data(Items$Graph::Data(Pos)) 0] -desc]
      set desc [lindex [$data(Canvas) itemconfigure $id -text] end]

      Graph::ItemPos $Frame $VP $smpl "[lindex $Lbl(Title) $GDefs(Lang)]\n$desc" GRAPHSELECT$Graph::Data(Graph) $Param(SelectMode) $coords

      if { $Scan && [llength $coords]>2 } {
         Graph::${Graph::Data(Type)}::ItemDefine $Graph::Data(Graph) $Graph::Data(Pos) $coords
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Graph::VertexSample>
# Creation : Octobre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Calculer le path de coupe et points intermediaire selon la resolution.
#
# Parametres :
#  <Type>    : Type de graph
#  <Graph>   : Identificateur du graph
#  <Coord>   : Liste des coordonnee
#  <Res>     : Resolution des points intermediaire (Defaut 0 = aucun)
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Graph::VertexSample  { Type Graph Coord { Res 0 } } {
   variable Data

   upvar #0 Graph::${Type}::${Type}${Graph}::Data  data

   if { [llength $Coord]==2 } {
      return $Coord
   }

   set res  [expr $Res==0?$Data(Res):$Res]
   set plat [lindex $Coord 0]
   set plon [lindex $Coord 1]

   set coords {}
   set data(DCoords) { 0 }

   foreach { lat lon } $Coord {

      if { $plat!=$lat || $plon!=$lon } {
         set coords [concat $coords [projection function $data(FrameData) -path [list $plat $plon $lat $lon] $res]]
         lappend data(DCoords) [expr [llength $coords]/2-1]
      }
      set plat $lat
      set plon $lon
   }

   return $coords
}

#----------------------------------------------------------------------------
# Nom      : <Graph::VertexResolution>
# Creation : Janvier 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Determiner la resolution optimale du sampling des points de coupes.
#
# Parametres :
#  <Type>    : Type de graph
#  <Graph>   : Identificateur du graph
#  <Update>  : Update graph
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Graph::VertexResolution { Type Graph { Update True } } {
   variable Data

   upvar #0 Graph::${Type}::${Type}${Graph}::Data data

   if { $data(VP)=="" || $data(Field)=="" } {
      return
   }

   if { $Data(ResBest) } {
      set i [expr int([fstdfield define $data(Field) -NI]/2)]
      set j [expr int([fstdfield define $data(Field) -NJ]/2)]

      if { [fstdfield define $data(Field) -GRTYP]=="R" } {
         set Data(Res) 1000
      } else {
         #----- Calculate distances in gridpoint for grid projection otherwise, use meters
         if { [projection configure $data(FrameData) -type]=="grid" } {
            set Data(Res) 1
         } else {
            set c0 [fstdfield stats $data(Field) -gridpoint $i $j]
            set c1 [fstdfield stats $data(Field) -gridpoint [incr i] $j]
            eval set Data(Res) \[$data(VP) -distll $c0 $c1 0.0\]
         }
      }
      set Data(Res) [format "%0.0f" $Data(Res)]
   }

   if { $Update } {
      Graph::${Type}::ItemDefine $Graph $Graph::Data(Pos) $data(Coords)
      Graph::${Type}::Graph $Graph
   }
}
