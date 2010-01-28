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
#    Graph::Activate           { Frame GR Type }
#    Graph::DeActivate         { GR Type }
#    Graph::Resize             { Frame GR X0 Y0 X1 Y1 Limit }
#    Graph::Resolution         { Frame Type Full }
#    Graph::Configure          { }
#    Graph::Idle               { GR Type }
#    Graph::UnIdle             { GR Type }
#    Graph::Destroy            { Frame { GR "" } { Type "" } }
#    Graph::LocTool            { GR Type Array Index Op }
#    Graph::Mode               { Type GR { Zoom False } }
#    Graph::Params             { { GR "" } { Type "" } { Force False } }
#    Graph::ParamsOff          { Frame Type GR }
#    Graph::ItemPos            { Frame VP Coords Desc Tag { Type POINT } { Marks {} } }
#    Graph::ItemConfigure      { GR Type Item }
#    Graph::ItemSelect         { Item }
#    Graph::Labels             { Type GR Title UnitX UnitY }
#    Graph::ParamsItem         { Parent }
#    Graph::ParamsPos          { Parent }
#    Graph::ParamsScaleUniform { GR Type { Update True } }
#    Graph::PosAdd             { GR Type }
#    Graph::PosSet             { GR Type }
#    Graph::PosDel             { GR Type }
#    Graph::PosDelAll          { GR Type }
#    Graph::PosSelect          { GR Type }
#    Graph::ParamsGraph        { Parent }
#    Graph::ParamsObs          { Parent Type GR }
#    Graph::ParamsObsSelect    { Type GR Desc }
#    Graph::ParamsObsSearch    { Type GR }
#    Graph::RangeDraw          { Type GR Place Id Y }
#    Graph::Translate          { Frame Type GR X Y }
#    Graph::TranslateDone      { Frame Type GR }
#    Graph::TranslateInit      { Frame Type GR X Y }
#    Graph::Update             { Frame }
#    Graph::UpdateItems        { Frame }
#    Graph::TimeFormat         { Sec Mode { From 0 } }
#    Graph::ValFormat          { Order Val }
#    Graph::Write              { Frame File }
#    Graph::Zoom               { Type GR }
#    Graph::ZoomReset          { Type GR }
#    Graph::ZoomBox            { Canvas X1 Y1 }
#    Graph::ZoomInit           { Canvas X0 Y0 }
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
   variable Item
   variable Bubble
   variable Param

   set Font(Select)    [font create -family courier -size -8   -weight bold]
   set Font(Axis)      [font create -family courier -size -10  -weight bold]
   set Font(Graph)     [font create -family courier -size -14  -weight bold]
   set Font(Angle)     0

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

   set Grid(Color)   #C7C7C7
   set Grid(Width)   1
   set Grid(Dash)    "."

   set Width(Frame)  1

   set Param(Dock)        True
   set Param(Geom)        { 175x630+[winfo rootx .]+[winfo rooty .] }

   set Data(Nb)           0                      ;#Nombre de graph
   set Data(Type)         ""                     ;#Type du graph courant
   set Data(Types)        "Compare Contingency Frequence Profile Scatter Section Time TimeSection" ;#Liste des types de graph
   set Data(Types)        "Contingency Scatter Profile Time Section TimeSection" ;#Liste des types de graph
   set Data(Graph)        ""                     ;#Id du graph courant
   set Data(GraphParams)  ""                     ;#Id du graph courant
   set Data(Show)         False
   set Data(ShowCoord)    False
   set Data(ShowGrid)     False
   set Data(ToolMode)     Data
   set Data(Update)       True
   set Data(Pos)          ""
   set Data(PosNo)        -1
   set Data(Frame)        ""
   set Data(Stats)       { R2 VARx VARy VARxy RMSE NRMSE ME NME MNE LMNE MB NMB MNB LMNB MFB MFE a b Ea Eb AVGx AVGy NA RNA n }
   set Data(Funcs)       { scor svarx svary scov srmse snrmse sme snme smne slmne smb snmb smnb slmnb smfb smfe srega sregb serra serrb savgx savgy sna srna snb }

   set Graph(Identitys)   { A B C D E F G H I J K L M N O P Q R S T U V W X Y Z }
   set Graph(Dashs)       { \"\" "_" "." ".._" "..._" "...__" }

   set Graph(Stipples) "@$GDefs(Dir)/Resources/Bitmap/raydiagleft16.xbm
                        @$GDefs(Dir)/Resources/Bitmap/raydiagright16.xbm
                        @$GDefs(Dir)/Resources/Bitmap/rayhor16.xbm
                        @$GDefs(Dir)/Resources/Bitmap/rayver16.xbm
                        @$GDefs(Dir)/Resources/Bitmap/grey06.xbm
                        @$GDefs(Dir)/Resources/Bitmap/grey12.xbm
                        @$GDefs(Dir)/Resources/Bitmap/raydiagleft16.xbm
                        @$GDefs(Dir)/Resources/Bitmap/raydiagright16.xbm
                        @$GDefs(Dir)/Resources/Bitmap/rayhor16.xbm
                        @$GDefs(Dir)/Resources/Bitmap/rayver16.xbm"

   set Graph(Icons)    { NONE TRIANGLE SQUARE VBAR HBAR CIRCLE LOZENGE PENTAGON HEXAGON }
   set Graph(Colors)   { #FF0000 #00FF00 #0000FF #FFF300 #00FFF3 #E600FF #FF8C00 #804F4F #8CFF40 #B5A36B #80F9FF #A66BFF }

   set Item(No)          0
   set Item(Outline)     #000000
   set Item(FillColor)   #FFFFFF
   set Item(Tranparency) 100
   set Item(Width)       1
   set Item(Size)        0
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
   set Lbl(Fit)        { "Régression" "Regression" }
   set Lbl(Item)       { "Item" "Item" }
   set Lbl(Graph)      { "Graphique" "Graph" }
   set Lbl(Outline)    { "Ligne" "Line" }
   set Lbl(Fill)       { "Remplissage" "Fill" }
   set Lbl(Icon)       { "Icone" "Icon" }
   set Lbl(Type)       { "Type        " "Type        " }
   set Lbl(Linear)     { "Linéaire" "Linear" }
   set Lbl(Title)      { "Titre" "Title" }
   set Lbl(Value)      { "Valeurs" "Values" }
   set Lbl(Update)     { "Info auto" "Info update" }

   set Lbl(Font)       { "Police" "Font" }
   set Lbl(Info)       { "Info" "Info" }
   set Lbl(Axis)       { "Axe" "Axis" }
   set Lbl(Grid)       { "Grille" "Grid" }
   set Lbl(Unit)       { "Unité" "Unit" }
   set Lbl(Since)      { "Depuis" "Since" }

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
   set Lbl(Proj)       { "Projection" "Projection" }
   set Lbl(Res)        { "Résolution" "Resolution" }
   set Lbl(Same)       { "Uniforme" "Uniform" }
   set Lbl(Scale)      { "Echelle" "Scale" }
   set Lbl(Sec)        { "Sec"   "Sec" }
   set Lbl(Select)     { "Sélection" "Select" }
   set Lbl(Stat)       { "Statistiques" "Statistics" }
   set Lbl(To)         { "A " "To  " }
   set Lbl(Val)        { "Valeur" "Value" }
   set Lbl(Values)     { "Valeurs aux points" "Local values" }
   set Lbl(TimeMatch)  { "Valider les temps" "Validate time" }

   #----- Bulles d'aides

   set Bubble(Back)      { "Paramêtres de l'arrière plan du graph" "Graph background parameters" }
   set Bubble(Frame)     { "Paramêtres du contour du graph" "Graph frame parameters" }
   set Bubble(Grid)      { "Paramêtres de grillage des axes" "Axis grid parameters" }
   set Bubble(Pick)      { "Paramêtres de la sélection interactive" "Interactive selection parameters" }
   set Bubble(Axis)      { "Paramêtres des axes" "Axis parameters" }
   set Bubble(Mode)      { "Mode de selection de coordonees dans la projection" "Coordinate selection mode" }
   set Bubble(Reset)     { "Reinitialiser le graph" "Reinitialize the graph" }
   set Bubble(Sheet)     { "Affichage des données du graph" "Display graph data" }
   set Bubble(Update)    { "Mise-à-jour automatique des unités et information à partir des données" "Automatic update of graph units and information from data source" }

   set Bubble(ItemList)  { "Liste des items par sélection de position" "List of itmes per position selection" }
   set Bubble(ItemLine)  { "Paramtres d'affichage des lignes" "Line parameters" }
   set Bubble(ItemFill)  { "Paramêtres de remplissage" "Polygon filling parameters" }
   set Bubble(ItemIcon)  { "Paramêtres des icones" "Icon parameters" }
   set Bubble(ItemType)  { "Type d'affichage de l'item" "Item display type" }
   set Bubble(ObsList)   { "Liste des stations d'observations disponibles" "List of available observation stations" }
   set Bubble(ObsSearch) { "Recherche d'une station dans la liste" "Station search through the list" }
   set Bubble(PosList)   { "Liste des sélections de position" "Position selection list" }
   set Bubble(PosAdd)    { "Ajout d'une sélection de position" "Add a position selection" }
   set Bubble(PosDel)    { "Suppression de la sélection de position courante" "Delete the current position sélection" }
   set Bubble(Info)      { "Paramêtres de l'entête" "Header parameters" }
   set Bubble(Viewport)  { "Affichage des données sur la projection active" "Display the data on the active viewport" }
   set Bubble(Sample)    { "Selection de la distance entre les profils" "Select the sampling distance in km" }
   set Bubble(Fit)       { "Affichage de la courbe de regression" "Displays the fitting curve" }
   set Bubble(Select)    { "Affichage d'un interval de sélection" "Displays an interval of selection" }
   set Bubble(Stat)      { "Affichage de statistiques relatives au graph" "Displays graph statistics" }
   set Bubble(IP3)       { "Validation du IP3 lors de la recherche des champs temporels" "Validate IP3 when looking ro temporal fields" }
   set Bubble(Date)      { "Affichage des dates ou du temps écoulé depuis le debut" "Display date or time elapsed since the beginning" }
   set Bubble(ScaleY)    { "Spécification des intervals de l'échelle en Y" "Specify the Y axis intervals" }
   set Bubble(ScaleX)    { "Spécification des intervals de l'échelle en X" "Specify the X axis intervals" }
   set Bubble(Date0)     { "Spécification de la date de début\nformat: YYYMMDD HHMMSS" "Specify the start date\nformat: YYYMMDD HHMMSS" }
   set Bubble(Date1)     { "Spécification de la date de fin\nformat: YYYMMDD HHMMSS" "Specify the end date\nformat: YYYMMDD HHMMSS" }
   set Bubble(Uniform)   { "Permet de fixer l'échelle des deux axes aux mêmes intervals" "Fix the two scales to the same intervals" }
}

package require MetStat

source $GDefs(Dir)/Apps/Lib/Graph_Time.tcl
source $GDefs(Dir)/Apps/Lib/Graph_Scatter.tcl
source $GDefs(Dir)/Apps/Lib/Graph_Profile.tcl
source $GDefs(Dir)/Apps/Lib/Graph_Contingency.tcl
source $GDefs(Dir)/Apps/Lib/Graph_Section.tcl
source $GDefs(Dir)/Apps/Lib/Graph_Frequence.tcl
source $GDefs(Dir)/Apps/Lib/Graph_Compare.tcl
source $GDefs(Dir)/Apps/Lib/Graph_TimeSection.tcl
source $GDefs(Dir)/Apps/Lib/Graph_Stat.tcl

#----------------------------------------------------------------------------
# Nom      : <Graph::Activate>
# Creation : Janvier 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Mettre un graph actif et desactiver le precedent
#
# Parametres :
#   <Frame>  : Indentificateur de Page
#   <GR>     : Indentificateur du Graph
#   <Type>   : Type du Graph
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Graph::Activate { Frame GR Type } {
   variable Data

   #----- Si le viewport actif est le courant, out

   if { $Frame=="" } {
      return
   }

   #----- Desactiver le graph precedent

   if { $Data(Type)!=""  && $Data(Graph)!="" && $Data(Graph)!=$GR } {
      eval Graph::DeActivate $Data(Graph) $Graph::Data(Type)
   }
   set Data(Type) $Type

   #----- Si pas de graph, selectionner celui defaut du frame

   if { $GR!="" } {
      set Data(Graph) $GR
    }

   #----- Activer le viewport courant

   Page::ActiveWrap $Frame $Data(Graph)

   #----- Recuperer et instaurer ses parametres

   if { $Data(Graph)!="" && $Data(Active$GR) } {
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
#   <GR>     : Indentificateur du Graph
#   <Type>   : Type du Graph
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Graph::DeActivate { GR Type } {

   upvar #0 Graph::${Type}::${Type}${GR}::Data  data

   #----- Desactiver le viewport precedent

   Page::ActiveUnWrap $data(Frame) $GR
   eval trace vdelete SPI::Src(Info) w \{ Graph::LocTool \$Graph::Data(Graph) $Type \}
}

#----------------------------------------------------------------------------
# Nom      : <Graph::Labels>
# Creation : Fevrier 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Changer les titres et unites d'un graph intercatif
#
# Parametres :
#   <GR>     : Indentificateur du Graph
#   <Title>  : Titre du Graph
#   <UnitX>  : Unite en X
#   <UnitY>  : Unite en Y
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Graph::Labels { Type GR Title UnitX UnitY } {

   upvar #0 Graph::${Type}::${Type}${GR}::Data  data

   $data(Canvas) itemconfigure [lindex [$data(Canvas) itemconfigure GRAPH$GR -title] end] -text $Title
   $data(Canvas) itemconfigure [graphaxis configure axisx$GR -unit] -text $UnitX
   $data(Canvas) itemconfigure [graphaxis configure axisy$GR -unit] -text $UnitY
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
#  <GR>      : Identificateur du Graph
#  <Width>   : Nouvelle largeur
#  <Height>  : Nouvelle hauteur
#  <Limit>   : Seulement le frame
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Graph::Resize { Frame GR X0 Y0 X1 Y1 Limit } {
   variable Data

   if { $X0==-999 } {
      set coo [$Frame.page.canvas coords GRAPH$GR]
      set X0 [lindex $coo 0]
      set Y0 [lindex $coo 1]
   }

   if { [expr $X1-$X0]>25 && [expr $Y1-$Y0]>25 } {
      set Data(X$GR)      $X0
      set Data(Y$GR)      $Y0
      set Data(Width$GR)  [expr $X1-$X0]
      set Data(Height$GR) [expr $Y1-$Y0]

      catch { $Frame.page.canvas coords GRAPH$GR $X0 $Y0 $X1 $Y1 }
      catch { $Frame.page.canvas itemconfigure GRAPH$GR -x $X0 -y $Y0 -width $Data(Width$GR) -height $Data(Height$GR) }

      if { $Data(Active$GR) } {
         $Frame.page.canvas coords BS$Page::Data(Tag)$GR $X1 [expr $Y1-1]
         $Frame.page.canvas coords BM$Page::Data(Tag)$GR [expr $X1-11] [expr $Y1-1]
         $Frame.page.canvas coords BF$Page::Data(Tag)$GR [expr $X1-22] [expr $Y1-1]
         $Frame.page.canvas coords BD$Page::Data(Tag)$GR $X1 $Y0
      }
   }

   if { !$Limit } {
      $Frame.page.canvas config -cursor watch
      update idletasks

      set type $Data(Type$GR)
      if { [info procs ::Graph::${type}::Page]!="" } {
         Graph::${type}::Page $GR
      }
      Graph::${type}::Graph $GR

      update idletasks
      $Frame.page.canvas config -cursor left_ptr
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
         $Frame.page.canvas itemconf GRAPH$graph -update True
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

proc Graph::Idle { GR Type } {

   upvar #0 Graph::${Type}::${Type}${GR}::Data  data

   if { $data(FrameData)!="" } {
      . config -cursor watch
      $data(Canvas) config -cursor watch
      $data(FrameData).page.canvas config -cursor watch
   }
}

proc Graph::UnIdle { GR Type } {

   upvar #0 Graph::${Type}::${Type}${GR}::Data  data

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

proc Graph::Destroy { Frame { GR "" } { Type "" } } {
   variable Data

   if { $GR=="" && $Type=="" } {
      foreach type $Data(Types) {
         foreach graph [Page::Registered $Frame Graph::${type}] {
            Graph::Destroy $Frame $graph $type
         }
      }
   } else {

      if { [Page::Registered $Frame Graph::${Type} $GR]==-1 } {
         return
      }

      $Frame.page.canvas configure -cursor watch
      update idletasks

      Page::ActiveUnWrapper Graph $Frame $GR

      Graph::DeActivate $GR $Type
      Graph::ParamsOff $Frame $Type $GR
      Graph::${Type}::Destroy $Frame $GR

      #----- Supprimer les variables du viewport

      unset Data(Full$GR)
      unset Data(Active$GR)
      unset Data(X$GR)
      unset Data(Y$GR)
      unset Data(Width$GR)
      unset Data(Height$GR)
      unset Data(Type$GR)

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
#  <GR>      : Indentificateur du Graph
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

proc Graph::LocTool { GR Type Array Index Op } {

   if { $GR!="" } {
      Graph::${Type}::ItemDefine $GR $Graph::Data(Pos) [list $SPI::Src(Lat) $SPI::Src(Lon)]
   }
}

#------------------------------------------------------------------------------
# Nom      : <Graph::Mode>
# Creation : Mars 2004 - J.P. Gauthier - CMC/CMOE -
#
# But     : Activer les bindings relatifs aux graphs
#
# Parametres :
#   <Type>   : Type de graph
#   <GR>     : Identificateur du graph
#   <Zoom>   : Activation du mode Zoom
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Graph::Mode { Type GR { Zoom False } } {
   variable Data

   upvar #0 Graph::${Type}::${Type}${GR}::Data  data

   $data(Canvas) bind $Page::Data(Tag)$GR    <ButtonPress-1> "Graph::Activate $data(Frame) $GR $Type"

   if { $Zoom } {

      $data(Canvas) bind GRAPH$GR                <Motion>        "if { \[$GR -header %x %y\] } { $data(Canvas) configure -cursor hand1 } else { $data(Canvas) configure -cursor left_ptr }
                                                                  Graph::${Type}::Coord $Page::Data(Frame) $GR \[$data(Canvas) canvasx %x\] \[$data(Canvas) canvasy %y\]"
      $data(Canvas) bind GRAPH$GR                <Leave>         "set Page::Data(Coord) \"\";set Page::Data(Value) \"\""

      #----- Evenements de zoom

      $data(Canvas) bind $Page::Data(Tag)$GR <ButtonPress-2>   "Graph::Activate $data(Frame) $GR $Type;\
                                                                Graph::ZoomInit $data(Canvas) \[$data(Canvas) canvasx %x\] \[$data(Canvas) canvasy %y\]"
      $data(Canvas) bind $Page::Data(Tag)$GR <B2-Motion>       "Graph::ZoomBox  $data(Canvas) \[$data(Canvas) canvasx %x\] \[$data(Canvas) canvasy %y\]"
      $data(Canvas) bind $Page::Data(Tag)$GR <ButtonRelease-2> "Graph::Zoom $Type $GR"
      $data(Canvas) bind $Page::Data(Tag)$GR <ButtonRelease-3> "Graph::ZoomReset $Type $GR"

#      $data(Canvas) bind $Page::Data(Tag)$GR <ButtonPress-4>   "Graph::Zoom $Type $GR"
#      $data(Canvas) bind $Page::Data(Tag)$GR <ButtonPress-5>   "Graph::Zoom $Type $GR"

      #----- Evenements de rotation

      $data(Canvas) bind GRAPH$GR <ButtonPress-1>   "Graph:LegendMoveInit $data(Frame) $data(Canvas) $GR $Type %x %y"
      $data(Canvas) bind GRAPH$GR <B1-Motion>       "Graph:LegendMove $data(Frame) $data(Canvas) $GR $Type %x %y"
      $data(Canvas) bind GRAPH$GR <ButtonRelease-1> "Graph:LegendMoveDone $data(Frame) $data(Canvas) $GR $Type"
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Params>
# Creation : Avril 2003 - J.P. Gauthier - CMC/CMOE -
#
# But      : Fenetre de parametrage des graphs
#
# Parametres :
#   <Type>   : Type de graph
#   <GR>     : Identificateur du Graph
#
#-------------------------------------------------------------------------------

proc Graph::Params { { GR "" } { Type "" } { Force False } } {
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
         wm protocol      .graphparams WM_DELETE_WINDOW { set Graph::Data(Show) False ; Page::ModeSelect Zoom ;destroy .graphparams }
      }

      TabFrame::Create .graphparams.tab 1 ""
      set Data(Tab) [TabFrame::Add .graphparams.tab 1 [lindex $Lbl(Graph) $GDefs(Lang)] True]
      pack .graphparams.tab -side top -fill both -expand true

      frame $Data(Tab).head
         checkbutton $Data(Tab).head.sel -image ARROW -relief sunken -bd 1 -overrelief raised -offrelief flat -indicatoron False \
            -variable Page::Data(ToolMode) -offvalue SPI -command { SPI::ToolMode $Page::Data(ToolMode) $Graph::Data(ToolMode) True }
         button $Data(Tab).head.reset -image GRAPHRESET -relief flat -bd 0 -overrelief raised -command ""
         button $Data(Tab).head.data -image GRAPHDATA -relief flat -bd 0 -overrelief raised -command ""
         pack $Data(Tab).head.sel $Data(Tab).head.reset $Data(Tab).head.data -side left -fill y -padx 2
      pack $Data(Tab).head -side top -fill x

      frame .graphparams.dock -relief raised -bd 1
         button .graphparams.dock.sel -image DOCK -anchor w -relief flat -bd 0 -overrelief raised -command { SPI::DockTool Graph { Graph::Params $Graph::Data(Graph) $Graph::Data(Type) True } }
         button .graphparams.dock.del -image DOCKDELETE -anchor w -relief flat -bd 0 -overrelief raised -command { set Graph::Data(Show) False ; Page::ModeSelect Zoom ;destroy .graphparams }
         label .graphparams.dock.info -text "" -relief sunken -bd 1 -anchor w -width 21 -bg $GDefs(ColorLight)
         pack .graphparams.dock.sel .graphparams.dock.del -side left
         pack .graphparams.dock.info -side left -fill x -expand true
      pack .graphparams.dock -side bottom -fill x

      frame $Data(Tab).font
      pack $Data(Tab).font -side bottom -fill x -anchor s

      Bubble::Create $Data(Tab).head.sel   $Bubble(Mode)
      Bubble::Create $Data(Tab).head.reset $Bubble(Reset)
      Bubble::Create $Data(Tab).head.data  $Bubble(Sheet)

      Graph::ParamsGraph $Data(Tab).font
   }

   #----- Inserer les parametres du graph

   if { $Type!="" && ($Data(GraphParams)!=$GR || $Force) } {
       eval .graphparams.dock.info configure -text \[lindex \$Graph::${Type}::Lbl(Title) $GDefs(Lang)\]

      $Data(Tab).head.sel configure -onvalue Graph::${Type} -command "SPI::ToolMode \$Page::Data(ToolMode) \$Graph::Data(ToolMode) True; catch { Graph::Section::UpdateItems \$Graph::Section::Section${GR}::Data(FrameData) $GR }"
      $Data(Tab).head.reset configure -command "Graph::ZoomReset $Type $GR"
      $Data(Tab).head.data  configure -command "Graph::DataSheet $Type $GR"

      destroy $Data(Tab).graph
      frame $Data(Tab).graph
      pack $Data(Tab).graph -before $Data(Tab).font -side top -fill both -expand true

      #----- Force le mode selection du graph actif si besoin est

      set Data(ToolMode)    $Graph::Data(ToolMode$GR)
      set Data(GraphParams) $GR

      if { [info exists Graph::Data(Full$GR)] } {
         Graph::${Type}::Params $Data(Tab).graph $GR
         if { $Graph::Data(ToolMode$GR)!="None" } {
            $Data(Tab).head.sel configure -state normal
            if { [lindex [split $Page::Data(ToolMode) ":"] 0]=="Graph" } {
               set Page::Data(ToolMode) Graph::${Type}
               SPI::ToolMode $Page::Data(ToolMode) $Graph::Data(ToolMode) True
            }
         } else {
            $Data(Tab).head.sel configure -state disabled
            SPI::ToolMode SPI Zoom
         }
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::DataSheet>
# Creation : Avril 2003 - J.P. Gauthier - CMC/CMOE -
#
# But      :Afficher les donnees du graph
#
# Parametres :
#   <Type>   : Type de graph
#   <GR>     : Identificateur du Graph
#
#-------------------------------------------------------------------------------

proc Graph::DataSheet { Type GR } {
   global GDefs
   variable Lbl

   upvar #0 Graph::${Type}::${Type}${GR}::Data  data
   upvar #0 Graph::${Type}::${Type}${GR}::Graph graph

   eval set title \[lindex \$Graph::${Type}::Lbl(Title) $GDefs(Lang)\]
   set text [Dialog::Text .graphdata "[lindex $Lbl(Data) $GDefs(Lang)]: $title" "" 60 20]

   $text insert end "Title : $title\n"

   foreach item $data(Items) {
      if { [graphitem is $item] } {

         $text insert end "Item  : [lindex [$data(Canvas) itemconfigure [graphitem configure $item -desc] -text] end]\n"
         set pos [lindex [split $item _] 0]
         $text insert end "Coordinates: $data(Pos$pos)\n"
         $text insert end "Unit X: $graph(UnitX)\n"
         $text insert end "Unit Y: $graph(UnitY)\n"

         #----- Graph Raster
         set ditem [graphitem configure $item -data]
         if { $ditem!="" } {
            for { set j 0 } { $j < [fstdfield define $ditem -NJ] } { incr j } {
               $text insert end "\n[format "%-20e" [lindex [fstdfield stats $ditem -levels] $j]]"
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
                     set t [Graph::Time::Format $GR $x]
                  }
                  $text insert end [format "%-20.8e %-20.8e $t" $x $y]\n
               }
               $text insert end "\n"
            }
         }
      }
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
#   <Type>   : Type de graph
#   <GR>     : Identificateur du Graph
#
#-------------------------------------------------------------------------------

proc Graph::ParamsOff { Frame Type GR } {
   variable Data

   if { $Data(GraphParams)==$GR } {
      destroy $Data(Tab).graph

      if { [winfo exist .graphparams] } {
        $Data(Tab).head.sel configure -command ""
        $Data(Tab).head.reset configure -command ""
      }

      if { [lindex [split $Page::Data(ToolMode) ":"] 0]=="Graph" } {
         SPI::ToolMode SPI Zoom
      }
   }
   set Data(Graph) [Page::UnRegister $Frame Graph::$Type $GR]
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::ParamsScaleUniform>
# Creation : Fevrier 2003 - J.P. Gauthier - CMC/CMOE -
#
# But      : Desactiver certains widgets dnas le cas d'echelle uniforme
#
# Parametres :
#   <Type>   : Type de graph
#   <GR>     : Indentificateur du Graph
#   <Update> : Mettre a jour le graph
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Graph::ParamsScaleUniform { Type GR { Update True }  } {
   variable Data

   upvar #0 Graph::${Type}::${Type}${GR}::Graph graph
   upvar #0 Graph::${Type}::${Type}${GR}::Data  data

   if { $graph(Uniform) } {
      pack forget $Data(Tab).graph.scale.valy
   } else {
      pack $Data(Tab).graph.scale.valy -side top -padx 2 -pady 2 -fill x
   }

   if { $Update } {
      Graph::${Type}::Update $data(FrameData) $GR
   }
   Graph::${Type}::Graph $GR
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Dock>
# Creation : Avril 2003 - J.P. Gauthier - CMC/CMOE -
#
# But      : Inserer la fenetre dans l'interface
#
# Parametres :
#   <Type>   : Type de graph
#   <GR>     : Identificateur du Graph
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

proc Graph::ItemPos { Frame VP Coords Desc Tag { Type POINT } { Marks {} } } {
   variable Graph
   variable Data

   if { ![llength $Coords] || [lindex $Coords 0]==-999 } {
      return
   }

   if { [Page::Registered $Frame Viewport $VP]==-1 } {
      return
   }

   if { $Type=="POINT" } {
      if { [set xy [$VP -project [lindex $Coords 0] [lindex $Coords 1] 0]]!="" && [lindex $xy 2]>0 } {
         set x [lindex $xy 0]
         set y [lindex $xy 1]

         if { $Data(ShowCoord) } {
            set Desc "$Desc\n[format "(%.3f,%.3f)" [lindex $Coords 0] [lindex $Coords 1]]"
         }

         $Frame.page.canvas create text [expr $x+6] $y -text $Desc \
            -fill $Graph::Color(Select) -font $Graph::Font(Select) -tags "$Page::Data(Tag)$VP $Tag" -anchor w
         $Frame.page.canvas create oval [expr $x-2] [expr $y-2] [expr $x+2] [expr $y+2] -fill $Graph::Color(Select) \
            -tags "$Page::Data(Tag)$VP $Tag" -outline $Graph::Color(Select)
      }
   } elseif { $Type=="RECTANGLE" } {
      if { [llength $Coords]==4 } {
         set la0 [lindex $Coords 0]
         set lo0 [lindex $Coords 1]
         set la1 [lindex $Coords 2]
         set lo1 [lindex $Coords 3]
         Viewport::DrawLine $Frame $VP "$la0 $lo0 0 $la1 $lo0 0 $la1 $lo1 0 $la0 $lo1 0 $la0 $lo0 0" $Tag  $Graph::Color(Select) 2

         if { [set xy [$VP -project $la0 $lo1 0]]!= "" && [lindex $xy 2]>0 } {
            set x [lindex $xy 0]
            set y [lindex $xy 1]

            if { $Data(ShowCoord) } {
               set Desc "$Desc\n[format "(%.3f,%.3f - %.3f,%.3f)" $la0 $lo0 $la1 $lo1]"
            }
            $Frame.page.canvas create text [expr [lindex $xy 0]-2] [expr [lindex $xy 1]-2] -text $Desc \
               -fill $Graph::Color(Select) -font $Graph::Font(Select) -tags "$Page::Data(Tag)$VP $Tag" -anchor se
         }
      }
   } else {
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
               -fill $Graph::Color(Select) -tags "$Page::Data(Tag)$VP $Tag"
         }
      }

      foreach { lat lon } $Coords {
         if { [set xy [$VP -project $lat $lon 0]]!="" && [lindex $xy 2]>0 } {
            set x [lindex $xy 0]
            set y [lindex $xy 1]
            #----- Check close to cursor position cause it breaks in follow mode
            if { [expr hypot($x-$Viewport::Map(X),$y-$Viewport::Map(Y))]>2 } {
               $Frame.page.canvas create rectangle [expr $x-1] [expr $y-1] [expr $x+1] [expr $y+1] -width 1 -outline "" \
                  -fill $Graph::Color(Select) -tags "$Page::Data(Tag)$VP $Tag"
            }
         }
      }
   }
}

proc Graph::ItemConfigure { GR Type Item } {
   variable Data
   global  GDefs

   upvar #0 Graph::${Type}::${Type}${GR}::Data data

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

#   set Graph::Item(Bitmap) @$GDefs(Dir)/Resources/Bitmap/CLEAR.xbm
#   set Graph::Item(Image)  MODEL

   graphitem configure $Item -outline $Graph::Item(Outline) -fill $fill -iconoutline $Graph::Item(Outline) -iconfill $fill -transparency $Graph::Item(Tranparency) \
      -width $Graph::Item(Width) -size $Graph::Item(Size) -value $Graph::Item(Value) -dash $Graph::Item(Dash) \
      -type $Graph::Item(Type) -font $Graph::Item(Font) -icon $Graph::Item(Icon) \
      -bitmap $Graph::Item(Bitmap) -stipple $stipple -image $Graph::Item(Image)

   set item [graphitem configure $Item -desc]
   if { $item!="" } {
      $data(Canvas) itemconfigure [graphitem configure $Item -desc] -font $Graph::Item(Font) -fill $Graph::Item(Outline)
   }
   $data(Canvas) itemconfigure GRAPH$GR -bg $Graph::Color(BG)
}

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
   set Graph::Item(Size)        [graphitem configure $Item -size]
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
   IcoMenu::Set $Data(Frame).item.icon.size $Graph::Item(Size)
   IcoMenu::Set $Data(Frame).item.icon.sel $Graph::Item(Icon)

#   $Data(Frame).item.line.width configure -bitmap @$GDefs(Dir)/Resources/Bitmap/[lindex $Graph(WidthBitmap) $Graph::Item(Width)]
}

proc Graph::ParamsItem { Parent } {
   global GDefs
   variable Lbl
   variable Bubble
   variable Data
   variable Graph

   set Data(Frame) $Parent

   labelframe $Parent.item -text [lindex $Graph::Lbl(Item) $GDefs(Lang)]

      frame $Parent.item.sel
         listbox $Parent.item.sel.list -yscrollcommand [list $Parent.item.sel.scroll set] -listvariable Graph::Data(Items)\
            -height 3  -bd 1 -relief sunken -bg $GDefs(ColorLight) -exportselection 0 -selectmode single
         scrollbar $Parent.item.sel.scroll -orient vertical -command [list $Parent.item.sel.list yview] -bd 1 -width 10
         pack $Parent.item.sel.list -side left -fill both -expand true
         pack $Parent.item.sel.scroll -side right -fill y
      frame $Parent.item.line
         label $Parent.item.line.lbl -text [lindex $Lbl(Outline) $GDefs(Lang)] -width 12 -anchor w
         ColorBox::CreateSel $Parent.item.line.col Graph::Item(Outline) Graph::ItemConfigure \$Graph::Data(Graph) \$Graph::Data(Type) \$Graph::Data(Item)
         IcoMenu::Create $Parent.item.line.width $GDefs(Dir)/Resources/Bitmap { zeroth.xbm width1.xbm width2.xbm width3.xbm width4.xbm width5.xbm }\
             "0 1 2 3 4 5" Graph::Item(Width) { Graph::ItemConfigure $Graph::Data(Graph) $Graph::Data(Type) $Graph::Data(Item) } $Graph::Item(Width) -relief groove -bd 2
         IcoMenu::CreateDef $Parent.item.line.dash $GDefs(Dir)/Resources/Bitmap \
            { dash0.xbm dash1.xbm dash2.xbm dash3.xbm dash4.xbm dash5.xbm } { "" . - .- .-- .-. } \
             Graph::Item(Dash) { Graph::ItemConfigure $Graph::Data(Graph) $Graph::Data(Type) $Graph::Data(Item) } $Graph::Item(Dash) -relief groove -bd 2
         pack  $Parent.item.line.lbl $Parent.item.line.col $Parent.item.line.width $Parent.item.line.dash -side left
      frame $Parent.item.fill
         label $Parent.item.fill.lbl -text [lindex $Lbl(Fill) $GDefs(Lang)] -width 12 -anchor w
         ColorBox::CreateSel $Parent.item.fill.col Graph::Item(FillColor) Graph::ItemConfigure \$Graph::Data(Graph) \$Graph::Data(Type) \$Graph::Data(Item)
         IcoMenu::CreateDef $Parent.item.fill.stipple $GDefs(Dir)/Resources/Bitmap \
            { zeroth.xbm stipple0.xbm stipple1.xbm stipple2.xbm stipple3.xbm stipple4.xbm stipple5.xbm stipple6.xbm stipple7.xbm stipple8.xbm } \
            { -1 "" @$GDefs(Dir)/Resources/Bitmap/stipple1-32.xbm @$GDefs(Dir)/Resources/Bitmap/stipple2-32.xbm @$GDefs(Dir)/Resources/Bitmap/stipple3-32.xbm @$GDefs(Dir)/Resources/Bitmap/stipple4-32.xbm @$GDefs(Dir)/Resources/Bitmap/stipple5-32.xbm @$GDefs(Dir)/Resources/Bitmap/stipple6-32.xbm @$GDefs(Dir)/Resources/Bitmap/stipple7-32.xbm @$GDefs(Dir)/Resources/Bitmap/stipple8-32.xbm } \
             Graph::Item(Stipple) { Graph::ItemConfigure $Graph::Data(Graph) $Graph::Data(Type) $Graph::Data(Item) } $Graph::Item(Stipple) -relief groove -bd 2
         pack  $Parent.item.fill.lbl $Parent.item.fill.col $Parent.item.fill.stipple -side left
                  frame $Parent.item.icon
         label $Parent.item.icon.lbl -text [lindex $Lbl(Icon) $GDefs(Lang)] -width 12 -anchor w
         IcoMenu::Create $Parent.item.icon.sel $GDefs(Dir)/Resources/Bitmap \
            { zeroth.xbm stri.xbm ssquare.xbm svbar.xbm shbar.xbm scircle.xbm slos.xbm spenta.xbm shexa.xbm } \
            $Graph::Graph(Icons) Graph::Item(Icon) { Graph::ItemConfigure $Graph::Data(Graph) $Graph::Data(Type) $Graph::Data(Item) } \
            0 -relief groove -bd 2
         IcoMenu::Create $Parent.item.icon.size $GDefs(Dir)/Resources/Bitmap \
            "zeroth.xbm size1.xbm size2.xbm size3.xbm size4.xbm size5.xbm" "0 2 4 6 8 10" \
             Graph::Item(Size) { Graph::ItemConfigure $Graph::Data(Graph) $Graph::Data(Type) $Graph::Data(Item) } $Graph::Item(Size) -relief groove -bd 2
         pack $Parent.item.icon.lbl $Parent.item.icon.sel $Parent.item.icon.size -side left
      frame $Parent.item.value
         label $Parent.item.value.lbl -text [lindex $Lbl(Value) $GDefs(Lang)] -width 12 -anchor w
         checkbutton $Parent.item.value.sel -variable Graph::Item(Value) -relief raised -bd 1 -onvalue True -offvalue False  -selectcolor "" -relief groove -bd 1\
            -bitmap @$GDefs(Dir)/Resources/Bitmap/zeroth.xbm -indicatoron false -command { Graph::ItemConfigure $Graph::Data(Graph) $Graph::Data(Type) $Graph::Data(Item) }
         button $Parent.item.value.font -bitmap @$GDefs(Dir)/Resources/Bitmap/font.ico  -relief groove \
            -command "FontBox::Create $Parent.item.value.font { Graph::ItemConfigure \$Graph::Data(Graph) \$Graph::Data(Type) \$Graph::Data(Item) } $Graph::Item(Font)"
         pack $Parent.item.value.lbl $Parent.item.value.sel $Parent.item.value.font -side left

     Option::Create $Parent.item.type [lindex $Lbl(Type) $GDefs(Lang)] ::Graph::Item(Type) 0 -1 \
         $Graph::Item(Types) { Graph::ItemConfigure $Graph::Data(Graph) $Graph::Data(Type) $Graph::Data(Item) }

      pack $Parent.item.sel $Parent.item.type $Parent.item.line $Parent.item.fill $Parent.item.icon $Parent.item.value -side top -fill x -padx 2
   pack $Parent.item -side top -fill both -padx 5 -pady 5

   Bubble::Create $Parent.item.sel.list $Bubble(ItemList)
   Bubble::Create $Parent.item.line     $Bubble(ItemLine)
   Bubble::Create $Parent.item.fill     $Bubble(ItemFill)
   Bubble::Create $Parent.item.icon     $Bubble(ItemIcon)
   Bubble::Create $Parent.item.type     $Bubble(ItemType)

   bind $Parent.item.sel.list <ButtonRelease-1>  { set Graph::Data(Item) [%W get [%W nearest %y]] ; Graph::ItemSelect $Graph::Data(Item) }
}

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

proc Graph::PosAdd { GR Type } {
   variable Data

   upvar #0 Graph::${Type}::${Type}${GR}::Data data

   if { [winfo exist $Data(Frame).pos.sel] } {
      ComboBox::DelAll $Data(Frame).pos.sel
   }

   set Data(Pos)  Position[incr Data(PosNo)]

   Graph::${Type}::ItemDefine $GR $Data(Pos) { }

   if { [winfo exist $Data(Frame).pos.sel] } {
      ComboBox::AddList $Data(Frame).pos.sel $data(Pos)
   }

   set Data(Item) [lindex [set Data(Items) $data(Items$Data(Pos))] 0]

   if { [winfo exist $Data(Frame).item.sel.list] } {
      $Data(Frame).item.sel.list selection set 0
   }
   Graph::ItemSelect $Graph::Data(Item)
}

proc Graph::PosSet { GR Type } {
   variable Data

   upvar #0 Graph::${Type}::${Type}${GR}::Data data

   if { [winfo exist $Data(Frame).pos.sel] } {
      ComboBox::DelAll $Data(Frame).pos.sel
      ComboBox::AddList $Data(Frame).pos.sel $data(Pos)
   }

   set Data(Pos)  [lindex $data(Pos) end]
   set Data(Item) [lindex [set Data(Items) $data(Items$Data(Pos))] 0]

   if { [winfo exist $Data(Frame).item.sel.list] } {
      $Data(Frame).item.sel.list selection set 0
   }
   Graph::ItemSelect $Graph::Data(Item)
}

proc Graph::PosDel { GR Type } {
   variable Data

   upvar #0 Graph::${Type}::${Type}${GR}::Data  data

   Graph::${Type}::ItemUnDefine $GR $Data(Pos)
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

proc Graph::PosDelAll { GR Type } {
   variable Data

   upvar #0 Graph::${Type}::${Type}${GR}::Data  data

   foreach pos $data(Pos) {
      Graph::${Type}::ItemUnDefine $GR $pos
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

proc Graph::PosSelect { GR Type } {
   variable Data

   upvar #0 Graph::${Type}::${Type}${GR}::Data  data

   set Data(Item) [lindex [set Data(Items) $data(Items$Data(Pos))] 0]
   if { [winfo exist $Data(Frame).item.sel.list] } {
      $Data(Frame).item.sel.list selection clear 0 end
      $Data(Frame).item.sel.list selection set 0
   }
   Graph::ItemSelect $Graph::Data(Item)
}

proc Graph::ParamsGraph { Parent } {
   global GDefs
   variable Lbl
   variable Bubble

   labelframe $Parent.graph -text [lindex $Lbl(Graph) $GDefs(Lang)]
      frame $Parent.graph.info
         label $Parent.graph.info.lbl -text [lindex $Lbl(Info) $GDefs(Lang)] -width 12 -anchor w
         button $Parent.graph.info.sel -bitmap @$GDefs(Dir)/Resources/Bitmap/font.ico  -relief groove \
            -command "FontBox::Create $Parent.graph.info.sel Graph::Configure $Graph::Font(Graph)"
         ColorBox::CreateSel $Parent.graph.info.col ::Graph::Color(FG) Graph::Configure
         pack $Parent.graph.info.lbl $Parent.graph.info.col $Parent.graph.info.sel -side left
      frame $Parent.graph.axis
         label $Parent.graph.axis.lbl -text [lindex $Lbl(Axis) $GDefs(Lang)] -width 12 -anchor w
         button $Parent.graph.axis.sel -bitmap @$GDefs(Dir)/Resources/Bitmap/font.ico  -relief groove \
            -command "FontBox::Create $Parent.graph.axis.sel Graph::Configure $Graph::Font(Axis)"
         ColorBox::CreateSel $Parent.graph.axis.col ::Graph::Color(Axis) Graph::Configure
         scale  $Parent.graph.axis.angle -from -45 -to 45 -resolution 10 -variable Graph::Font(Angle) -showvalue false \
            -relief flat -bd 1 -orient horizontal -width 15 -sliderlength 10 -length 50 -command "Graph::Configure ; catch "
         pack $Parent.graph.axis.lbl $Parent.graph.axis.col $Parent.graph.axis.sel -side left
         pack $Parent.graph.axis.angle -side left -fill x
      frame $Parent.graph.select
         label $Parent.graph.select.lbl -text [lindex $Lbl(Select) $GDefs(Lang)] -width 12 -anchor w
         button $Parent.graph.select.sel -bitmap @$GDefs(Dir)/Resources/Bitmap/font.ico -relief groove \
            -command "FontBox::Create $Parent.graph.select.sel Graph::Configure $Graph::Font(Select)"
         ColorBox::CreateSel $Parent.graph.select.col Graph::Color(Select) Graph::Configure
         checkbutton $Parent.graph.select.coord -bitmap @$GDefs(Dir)/Resources/Bitmap/coord.ico -relief groove \
            -variable Graph::Data(ShowCoord) -indicatoron false -command Graph::Configure
#         checkbutton $Parent.graph.select.grid -bitmap @$GDefs(Dir)/Resources/Bitmap/grid.ico -relief groove \
#            -variable Graph::Data(ShowGrid) -indicatoron false -command Graph::Configure
         pack $Parent.graph.select.lbl $Parent.graph.select.col $Parent.graph.select.sel $Parent.graph.select.coord -side left
      pack $Parent.graph.info $Parent.graph.axis $Parent.graph.select -side top

      frame $Parent.graph.grid
         label $Parent.graph.grid.lbl -text [lindex $Lbl(Grid) $GDefs(Lang)] -width 12 -anchor w
         ColorBox::CreateSel $Parent.graph.grid.col Graph::Grid(Color) Graph::Configure
         IcoMenu::Create $Parent.graph.grid.sz $GDefs(Dir)/Resources/Bitmap \
            "zeroth.xbm width1.xbm width2.xbm width3.xbm width4.xbm width5.xbm" "0 1 2 3 4 5" \
            Graph::Grid(Width) { Graph::Configure } $Graph::Grid(Width) -relief groove -bd 2
         IcoMenu::CreateDef $Parent.graph.grid.dash $GDefs(Dir)/Resources/Bitmap \
            { dash0.xbm dash1.xbm dash2.xbm dash3.xbm dash4.xbm dash5.xbm } { "" . - .- .-- .-. } \
            Graph::Grid(Dash) { Graph::Configure } $Graph::Grid(Dash) -relief groove -bd 2
         pack $Parent.graph.grid.lbl $Parent.graph.grid.col $Parent.graph.grid.sz $Parent.graph.grid.dash -side left
         pack $Parent.graph.grid.dash -side left -fill x -expand True
      frame $Parent.graph.color
         label $Parent.graph.color.lbl -text [lindex $Lbl(Background) $GDefs(Lang)] -width 12 -anchor w
         ColorBox::CreateSel $Parent.graph.color.col Graph::Color(Fill) Graph::Configure
         pack $Parent.graph.color.lbl $Parent.graph.color.col -side left
      frame $Parent.graph.frame
         label $Parent.graph.frame.lbl -text [lindex $Lbl(Frame) $GDefs(Lang)] -width 12 -anchor w
         ColorBox::CreateSel $Parent.graph.frame.col Graph::Color(BG) Graph::Configure
         IcoMenu::Create $Parent.graph.frame.sz $GDefs(Dir)/Resources/Bitmap \
            "zeroth.xbm width1.xbm width2.xbm width3.xbm width4.xbm width5.xbm" "0 1 2 3 4 5" \
            Graph::Width(Frame) "Graph::Configure" $Graph::Width(Frame) -relief groove -bd 2
         pack $Parent.graph.frame.lbl  $Parent.graph.frame.col $Parent.graph.frame.sz -side left
      frame $Parent.graph.update -relief sunken -bd 1
         checkbutton $Parent.graph.update.sel -indicatoron False -text [lindex $Lbl(Update) $GDefs(Lang)] -bd 1 -onvalue True -variable Graph::Data(Update) \
            -command { Graph::${Graph::Data(Type)}::Graph $Graph::Data(Graph) }
         pack $Parent.graph.update.sel -side left -fill x -expand true
      pack $Parent.graph.info $Parent.graph.axis $Parent.graph.select $Parent.graph.grid $Parent.graph.color $Parent.graph.frame $Parent.graph.update -side top  -fill x
   pack $Parent.graph -side top -fill both -padx 5 -pady 5

   Bubble::Create $Parent.graph.info   $Bubble(Info)
   Bubble::Create $Parent.graph.axis   $Bubble(Axis)
   Bubble::Create $Parent.graph.select $Bubble(Pick)
   Bubble::Create $Parent.graph.grid   $Bubble(Grid)
   Bubble::Create $Parent.graph.color  $Bubble(Back)
   Bubble::Create $Parent.graph.frame  $Bubble(Frame)
   Bubble::Create $Parent.graph.update $Bubble(Update)
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
#   <GR>     : Indentificateur du Graph
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Graph::ParamsObs { Parent Type GR } {
   global GDefs
   variable Lbl
   variable Bubble

   labelframe $Parent.obs -text [lindex $Lbl(Obs) $GDefs(Lang)]
      entry $Parent.obs.sel  -relief sunken -bd 1 -bg $GDefs(ColorLight) -textvariable Graph::${Type}::${Type}${GR}::Data(ObsToken)
      frame $Parent.obs.list
         listbox $Parent.obs.list.box -relief sunken -bd 1 -bg $GDefs(ColorLight) -height 4 \
            -yscrollcommand [list $Parent.obs.list.scroll set] -width 20
         scrollbar $Parent.obs.list.scroll -command "$Parent.obs.list.box yview" -width 10 -bd 1
         pack $Parent.obs.list.box -side left -fill both -expand true
         pack $Parent.obs.list.scroll -side left -fill y
      pack $Parent.obs.sel  -side top -fill x -padx 2
      pack $Parent.obs.list -side top  -fill both -expand true -padx 2
   pack $Parent.obs -side top -fill both -expand true -pady 5 -padx 5

   Bubble::Create $Parent.obs.sel  $Bubble(ObsSearch)
   Bubble::Create $Parent.obs.list $Bubble(ObsList)

   Graph::ParamsObsSearch $Type $GR

   bind $Parent.obs.list.box <B1-ButtonRelease> "Graph::ParamsObsSelect $Type $GR \[%W get \[%W nearest %y\]\]"
   bind $Parent.obs.sel <Any-KeyRelease> "Graph::ParamsObsSearch $Type $GR"
}

#----------------------------------------------------------------------------
# Nom      : <Graph::ParamsObsSelect>
# Creation : Mars 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectuer la selection d'une observation
#
# Parametres :
#   <Type>   : Type de graph
#   <GR>     : Indentificateur du Graph
#   <Desc>   : Identification de l'obs
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Graph::ParamsObsSelect { Type GR Desc } {

   upvar #0 Graph::${Type}::${Type}${GR}::Data  data

   foreach item $data(Data) {
      if { [observation is $item] } {
         set idx [lindex [observation define $item -IDX $Desc] 0]
         set coords [lrange [observation define $item -COORD $idx] 0 1]
         set data(Obs$Graph::Data(Pos)) $Desc
      }
   }
   Graph::${Type}::ItemDefine $GR $Graph::Data(Pos) $coords
}

#----------------------------------------------------------------------------
# Nom      : <Graph::ParamsObsSearch>
# Creation : Mars 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Rechercher les observations selon les criteres
#
# Parametres :
#   <Type>   : Type de graph
#   <GR>     : Indentificateur du Graph
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Graph::ParamsObsSearch { Type GR } {
   variable Data

   upvar #0 Graph::${Type}::${Type}${GR}::Data  data

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
#   <GR>     : Indentificateur du Graph
#   <Place>  : Positionnement par coordonnee
#   <Id>     : Id du delimitateur (0,1,...)
#   <Y>      : Coordonnee du positionnement
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Graph::RangeDraw { Type GR Place Id Y } {

   upvar #0 Graph::${Type}::${Type}${GR}::Data  data
   upvar #0 Graph::${Type}::${Type}${GR}::Graph graph

   if { $graph(Range) } {
      if { $Place && $data(YIncr)!=0 } {
         set y  [expr ($Y-$graph(Y0))/$data(YIncr)]

         if { $graph(YScale)=="Log" } {
            set data(Range$Id) [expr pow(10,-$y + $data(Y0))]
         } else {
            set data(Range$Id) [expr -$y+$data(Y0)]
         }
      }

      set y [${Type}::DrawY $GR $data(Range$Id)]

      if { $y<$graph(Y0) && $y>$graph(Y1) } {
         if { [$data(Canvas) find withtag RANGE$Id$GR]=="" } {
            $data(Canvas) create line $graph(X0) $y $graph(X1) $y -fill #FF0000 -tags "$Page::Data(Tag)$GR RANGE$GR RANGE$Id$GR"
            $data(Canvas) bind RANGE$Id$GR <Enter> "$data(Canvas) configure -cursor hand1"
            $data(Canvas) bind RANGE$Id$GR <B1-Motion> "Graph::RangeDraw $Type $GR 1 $Id %y"
            $data(Canvas) bind RANGE$Id$GR <Leave> "$data(Canvas) configure -cursor left_ptr"
         } else {
            $data(Canvas) coords RANGE$Id$GR $graph(X0) $y $graph(X1) $y
         }
      }
   } else {
      $data(Canvas) delete RANGE$Id$GR
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
#   <GR>     : Identificateur du Graph
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Graph::Translate { Frame Type GR X Y } {
   variable Data

   upvar #0 Graph::${Type}::${Type}${GR}::Data  data
   upvar #0 Graph::${Type}::${Type}${GR}::Graph graph

   #----- Process the coords

   set coords [lrange [$GR -unproject $X $Y False $Graph::Data(Item)] 0 1]

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

      Graph::${Type}::Graph $GR
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
#   <GR>     : Identificateur du Graph
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Graph::TranslateDone { Frame Type GR } {
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
#   <GR>     : Identificateur du Graph
#   <X>      : Coordonnee X initiale
#   <Y>      : Coordonnee Y initiale
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Graph::TranslateInit { Frame Type GR X Y } {
   variable Data

   set coords [lrange [$GR -unproject $X $Y False $Graph::Data(Item)] 0 1]
   set Data(X0) [lindex $coords 0]
   set Data(Y0) [lindex $coords 1]

   Graph::Resolution $Frame $Type $OpenGL::Param(Res)
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
#   <GR>     : Identificateur du graph
#   <Type>   : Type de graph
#   <X>      : Coordonne X du pointeur
#   <Y>      : Coordonne Y du pointeur
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Graph:LegendMoveInit { Frame Canvas GR Type X Y } {
   variable Data

   if { [set Data(Legend) [$GR -header $X $Y]] } {

      set Data(XLegend) [lindex [$Canvas itemconfigure GRAPH$GR -xlegend] end]
      set Data(YLegend) [lindex [$Canvas itemconfigure GRAPH$GR -ylegend] end]

      set Data(X) $X
      set Data(Y) $Y
   } else {
      Graph::Activate $Frame $GR $Type;
      Graph::TranslateInit $Frame $Type $GR [$Canvas canvasx $X] [$Canvas canvasy $Y]
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
#   <GR>     : Identificateur du graph
#   <Type>   : Type de graph
#   <X>      : Coordonne X du pointeur
#   <Y>      : Coordonne Y du pointeur
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Graph:LegendMove { Frame Canvas GR Type X Y } {
   variable Data

   if { $Data(Legend) } {
      $Canvas itemconfigure GRAPH$GR -xlegend [expr $Data(XLegend)+($X-$Data(X))] -ylegend [expr $Data(YLegend)+($Y-$Data(Y))]
   } else {
      Graph::Translate $Frame $Type $GR [$Canvas canvasx $X] [$Canvas canvasy $Y]
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
#   <GR>     : Identificateur du graph
#   <Type>   : Type de graph
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Graph:LegendMoveDone { Frame Canvas GR Type } {
   variable Data

   if { $Data(Legend) } {
      set Data(Legend) False
   } else {
      Graph::TranslateDone $Frame $Type $GR;
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
      "DATE"  { return [clock format [expr int($Sec)] -format "%d/%m %H:%M" -gmt True] }
      default { return [clock format [expr int($Sec)] -format $Mode -gmt true] }
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
# Nom      : <Graph::Zoom>
# Creation : Mars 2004 - J.P. Gauthier - CMC/CMOE -
#
# But     : Effectuer le zoom dans le graph
#
# Parametres :
#   <Type>   : Type de graph
#   <GR>     : Identificateur du Graph
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Graph::Zoom { Type GR } {
   variable Data

   upvar #0 Graph::${Type}::${Type}${GR}::Data  data
   upvar #0 Graph::${Type}::${Type}${GR}::Graph graph

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

   set coords0 [lindex [$GR -unproject $Data(X0) $Data(Y1) Limit] 0]
   set coords1 [lindex [$GR -unproject $Data(X1) $Data(Y0) Limit] 0]

   set graph(ZXInter) [list [lindex $coords0 0] [lindex $coords1 0]]
   set graph(ZYInter) [list [lindex $coords0 1] [lindex $coords1 1]]

   #----- Refresh the graph

   Graph::${Type}::Graph $GR
}

#------------------------------------------------------------------------------
# Nom      : <Graph::ZoomReset>
# Creation : Mars 2004 - J.P. Gauthier - CMC/CMOE -
#
# But     : Reinitialiser le zoom
#
# Parametres :
#   <Type>   : Type de graph
#   <GR>     : Identificateur du Graph

# Remarques :
#
#-------------------------------------------------------------------------------

proc Graph::ZoomReset { Type GR } {

   upvar #0 Graph::${Type}::${Type}${GR}::Graph graph

   set graph(ZXInter) {}
   set graph(ZYInter) {}

   #----- Refresh the graph

   Graph::${Type}::Graph $GR
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
