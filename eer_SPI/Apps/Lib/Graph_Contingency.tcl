#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie d'objects visuel interactifs
# Fichier  : Graph_Contingency.tcl
# Creation : Octobre 2003 - J.P. Gauthier - CMC/CMOE
#
# Description: Ce package s'occupe de l'affichage et de la manipulation de graphs
#              de profil verticaux
#
# Fonctions:
#
#    Graph::Contingency::Create         { Frame X0 Y0 Width Height Active Full }
#    Graph::Contingency::Coord          { Frame GR X Y }
#    Graph::Contingency::DrawInit       { Frame VP }
#    Graph::Contingency::Draw           { Frame VP }
#    Graph::Contingency::DrawDone       { Frame VP }
#    Graph::Contingency::MoveInit       { Frame VP }
#    Graph::Contingency::Move           { Frame VP }
#    Graph::Contingency::MoveDone       { Frame VP }
#    Graph::Contingency::Graph          { GR { Update True } }
#    Graph::Contingency::Init           { Frame }
#    Graph::Contingency::Page           { GR }
#    Graph::Contingency::Params         { Parent GR }
#    Graph::Contingency::ItemAdd        { GR Item }
#    Graph::Contingency::ItemDel        { GR Item }
#    Graph::Contingency::ItemDefine     { GR Pos Coords { Update True } }
#    Graph::Contingency::ItemUnDefine   { GR Pos }
#    Graph::Contingency::ItemData       { GR Item Data }
#    Graph::Contingency::ItemDataObs    { GR Data0 Data1 }
#    Graph::Contingency::ItemDataField  { GR Data0 Data1 }
#    Graph::Contingency::ItemDataVector { GR Data0 Data1 }
#    Graph::Contingency::Update         { Frame { GR {} } }
#    Graph::Contingency::UpdateItems    { Frame { GR { } } }
#    Graph::Contingency::Data           { GR { Data { } } }
#    Graph::Contingency::DrawData       { GR }
#    Graph::Contingency::DrawStat       { GR }
#    Graph::Contingency::DrawScale      { GR }
#    Graph::Contingency::Select         { GR  I J }
#    Graph::Contingency::Stat           { GR }
#
#===============================================================================

namespace eval Graph::Contingency { } {
   variable Lbl

   #----- Definitions des labels

   set Lbl(Title)    { "Table de contingence" "Contingency table" }

   set Lbl(PC)        { "Pourcentage correct"                   "Percent correct" }
   set Lbl(HSS)       { "Indice de comparaison de Heidke"       "Heidke skill score" }
   set Lbl(BUST)      { "Proportion des erreurs des categories" "Proportion of errors of the categories" }
   set Lbl(BIAS)      { "Biais"                                 "Bias" }
   set Lbl(POD)       { "Probabilite de detection d'evenements" "Probability of detection of an event" }
   set Lbl(FAR)       { "Proportions de fausses alertes"        "False alarm ratio" }
   set Lbl(REL)       { "Indice de confiance"                   "Reliability index" }
   set Lbl(CSI)       { "Succes critique"                       "Critical success index" }
}

#----------------------------------------------------------------------------
# Nom      : <Graph::Contingency::Create>
# Creation : Octobre 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Crrer l'object graph et ses structures
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <X0>     : Coordonee X du coin superieur gauche
#   <Y0>     : Coordonee Y du coin superieur gauche
#   <Width>  : Largeur du Graph
#   <Height> : Hauteur du Graph
#   <Active> : Fonction active (Deplacement,Agrandisement)
#   <Full>   : Mode Full screen
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Graph::Contingency::Create { Frame X0 Y0 Width Height Active Full } {
   global GDefs
   variable Data

   set gr [Graph::Contingency::Init $Frame]
   set tag $Page::Data(Tag)$gr

   if { $Full } {
      set X0       0
      set Y0       0
      set Width   [winfo width  $Page::Data(Canvas)]
      set Height  [winfo height $Page::Data(Canvas)]
   }

   set Graph::Data(Full$gr)     $Full       ;#Mode FullCanvas
   set Graph::Data(Active$gr)   $Active     ;#Mode Active (Manipulation in place)
   set Graph::Data(X$gr)        $X0         ;#Offset en x
   set Graph::Data(Y$gr)        $Y0         ;#Offset en y
   set Graph::Data(Width$gr)    $Width      ;#Largeur du graph
   set Graph::Data(Height$gr)   $Height     ;#Hauteur du graph
   set Graph::Data(ToolMode$gr) Data        ;#Mode de selection
   set Graph::Data(Type$gr)     Contingency ;#Type de graph

   upvar #0 Graph::Contingency::Contingency${gr}::Data  data
   upvar #0 Graph::Contingency::Contingency${gr}::Graph graph

   set data(Canvas)    $Frame.page.canvas
   set data(Frame)     $Frame
   set data(TimeMatch) True

   set data(Select)    ""
   set data(Obs)       ""

   if { $Viewport::Data(VP)!="" } {
      set data(VP)        $Viewport::Data(VP)
      set data(FrameData) $Viewport::Data(Frame$data(VP))
      Graph::Contingency::Update $data(FrameData) $gr
   } else {
      set data(VP)        ""
      set data(FrameData) ""
   }

   Graph::Contingency::Page $gr

   Graph::Activate $Frame $gr Contingency
   Graph::Mode Contingency $gr False
   Graph::PosAdd $gr Contingency

   #----- Creer les fonction du mode actif

   if { $Active } {
      Page::ActiveWrapper Graph $Frame $gr $X0 $Y0 [expr $Width+$X0] [expr $Height+$Y0] Contingency
   }
   Page::Register $Frame Graph::Contingency $gr

   return $gr
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Contingency::Coord>
# Creation : Octobre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Evalue les valeurs de concentrations et de date a la position du curseur
#            dans le graph.
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <GR>     : Indentificateur du Graph
#   <X>      : Coordonnee X a l'interieur du canvas
#   <Y>      : Coordonnee Y a l'interieur du canvas
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Graph::Contingency::Coord { Frame GR X Y } {
}

#----------------------------------------------------------------------------
# Nom      : <Graph::Contingency::Draw...>
# Creation : Octobre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Fonctions de manipulation sur la projection
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

proc Graph::Contingency::DrawInit { Frame VP } {

   upvar #0 Graph::Contingency::Contingency${Graph::Data(Graph)}::Data  data

   set data(Lat0) $Viewport::Map(LatCursor)
   set data(Lon0) $Viewport::Map(LonCursor)
   set data(Lat1) $Viewport::Map(LatCursor)
   set data(Lon1) $Viewport::Map(LonCursor)

   if { $data(FrameData)!="" } {
      $data(FrameData).page.canvas delete GRAPHCONTINGENCY$Graph::Data(Graph)
   }

   if { $VP!=$data(VP) } {
      set data(VP)        $VP
      set data(FrameData) $Frame
      Graph::Contingency::Update $Frame $Graph::Data(Graph)
   }
}

proc Graph::Contingency::Draw { Frame VP } {
   global   GDefs
   variable Lbl

   upvar #0 Graph::Contingency::Contingency${Graph::Data(Graph)}::Data  data

   set data(Lat1)   $Viewport::Map(LatCursor)
   set data(Lon1)   $Viewport::Map(LonCursor)

   Graph::Contingency::ItemDefine $Graph::Data(Graph) $Graph::Data(Pos) [list $data(Lat0) $data(Lon0) $data(Lat1) $data(Lon1)] False
}

proc Graph::Contingency::DrawDone { Frame VP } {

   upvar #0 Graph::Contingency::Contingency${Graph::Data(Graph)}::Data  data

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

   if { $data(Lat0)==$data(Lat1) || $data(Lon0)==$data(Lon1) } {
      set data(Lat0) 0
      set data(Lat1) 0
      set data(Lon0) 0
      set data(Lon1) 0
      set data(Pos$Graph::Data(Graph)$Graph::Data(Item)) {}
   } else {
      set data(Pos$Graph::Data(Graph)$Graph::Data(Item)) [list $data(Lat0) $data(Lon0) $data(Lat1) $data(Lon1)]
   }

   Graph::Contingency::ItemDefine $Graph::Data(Graph) $Graph::Data(Pos) [list $data(Lat0) $data(Lon0) $data(Lat1) $data(Lon1)]
}

proc Graph::Contingency::MoveInit { Frame VP } {

   upvar #0 Graph::Contingency::Contingency${Graph::Data(Graph)}::Data  data

   set data(LonD) $Viewport::Map(LonCursor)
   set data(LatD) $Viewport::Map(LatCursor)

   if { $data(FrameData)!="" } {
      $data(FrameData).page.canvas delete GRAPHCONTINGENCY$Graph::Data(Graph)
   }

   set data(VP)        $VP
   set data(FrameData) $Frame
}

proc Graph::Contingency::Move { Frame VP } {
   global   GDefs
   variable Lbl

   upvar #0 Graph::Contingency::Contingency${Graph::Data(Graph)}::Data  data

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

   Graph::Contingency::ItemDefine $Graph::Data(Graph) $Graph::Data(Pos) [list $data(Lat0) $data(Lon0) $data(Lat1) $data(Lon1)] False
}

proc Graph::Contingency::MoveDone { Frame VP } {

   Graph::Contingency::DrawDone $Frame $VP
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Contingency::Graph>
# Creation : Fevrier 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche le graphique des series temporelles.
#
# Parametres :
#   <GR>     : Indentificateur du Graph
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Graph::Contingency::Graph { GR { Update True } } {
   variable Data

   upvar #0 Graph::Contingency::Contingency${GR}::Graph graph
   upvar #0 Graph::Contingency::Contingency${GR}::Data  data

   if { ![llength $data(Items)] } {
      return
   }

   #----- Recalculer les limites du graph
   set graph(X0) [expr 110+$Graph::Data(X$GR)]
   set graph(X1) [expr $Graph::Data(X$GR)+$Graph::Data(Width$GR)-70]
   set graph(Y1) [expr 105+$Graph::Data(Y$GR)]

   set rx [llength $graph(XInter)]
   set ry [llength $graph(YInter)]

   set graph(DX) [expr double($graph(X1)-$graph(X0))/($rx-1)]
   if { $rx==$ry } {
      set graph(DY) [expr ($Graph::Data(Height$GR)-105-35-5)/($ry+8)]
      set graph(Y0) [expr $graph(Y1)+($ry-1)*$graph(DY)]
   } else {
      set graph(DY) [expr ($Graph::Data(Height$GR)-105-35)/($ry-1)]
      set graph(Y0) [expr $graph(Y1)+($ry-1)*$graph(DY)]
   }

   #----- This graph need to update data on some specific occasion
   if { $Update } {
      Graph::Contingency::Update $data(FrameData) $GR
   }

   #----- Afficher le graph
   Graph::Contingency::Page      $GR
   Graph::Contingency::DrawScale $GR
   Graph::Contingency::DrawData  $GR
   Graph::Contingency::DrawStat  $GR
}

#----------------------------------------------------------------------------
# Nom      : <Graph::Contingency::Init>
# Creation : Octobre 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialisation des variables du graph.
#
# Parametres :
#   <Frame>  : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Graph::Contingency::Init { Frame } {
   global   GDef
   variable Data

   set gr GR[incr Graph::Data(Nb)]

   namespace eval Contingency$gr {
      variable Data
      variable Graph

      #----- Constantes relatives aux series temporelles

      set Data(Items)   {}         ;#Liste des items
      set Data(Pos)     {}         ;#Liste des positions
      set Data(Data)    {}        ;#Liste des donnees du graph
      set Data(Select)  ""        ;#Case selectionnee
      set Data(Lat0)    0         ;#Rectangle de selection
      set Data(Lat1)    0         ;#Rectangle de selection
      set Data(Lon0)    0         ;#Rectangle de selection
      set Data(Lon1)    0         ;#Rectangle de selection
      set Data(TIJ)     0         ;#Total en X-Y

      #----- Constantes relatives au Graph

      set Graph(Uniform)  True                                                   ;#Echelle equivalente
      set Graph(XInter)   "0 40 60 80 200"                                       ;#Groupes en X
      set Graph(YInter)   "0 40 60 80 200"                                       ;#Groupes en Y
      set Graph(UnitY)    "Desc Y"                                               ;#Descriptif de l'echelle des valeur en Y
      set Graph(UnitX)    "Desc X"                                               ;#Descriptif de l'echelle des valeur en X
      set Graph(Title)   [lindex $Graph::Contingency::Lbl(Title) $GDefs(Lang)] ;#Entete du graph
   }
   return $gr
}

#----------------------------------------------------------------------------
# Nom      : <Graph::Contingency::Page>
# Creation : Octobre 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectue l'affichage des parties du graphique.
#
# Parametres :
#   <GR>     : Identificateur du Graph
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Graph::Contingency::Page { GR } {
   variable Data

   upvar #0 Graph::Contingency::Contingency${GR}::Data  data
   upvar #0 Graph::Contingency::Contingency${GR}::Graph graph

   set tag $Page::Data(Tag)$GR
   $data(Canvas) delete $tag

   #----- Recalculer les limites du graph

   set graph(X0) [expr 110+$Graph::Data(X$GR)]
   set graph(X1) [expr $Graph::Data(X$GR)+$Graph::Data(Width$GR)-70]
   set graph(Y1) [expr 105+$Graph::Data(Y$GR)]

   set rx [llength $graph(XInter)]
   set ry [llength $graph(YInter)]

   set graph(DX) [expr double($graph(X1)-$graph(X0))/($rx-1)]
   if { $rx==$ry } {
      set graph(DY) [expr ($Graph::Data(Height$GR)-105-35-5)/($ry+8)]
      set graph(Y0) [expr $graph(Y1)+($ry-1)*$graph(DY)]
   } else {
      set graph(DY) [expr ($Graph::Data(Height$GR)-105-35)/($ry-1)]
      set graph(Y0) [expr $graph(Y1)+($ry-1)*$graph(DY)]
   }

   #----- Creation des partie fixes du graph

   if { $Graph::Width(Frame) } {
      $data(Canvas) create rectangle $Graph::Data(X$GR) $Graph::Data(Y$GR) [expr $Graph::Data(Width$GR)+$Graph::Data(X$GR)] [expr $Graph::Data(Height$GR)+$Graph::Data(Y$GR)] -outline black  -fill $Graph::Color(Frame) -width $Graph::Width(Frame) -tags "$tag $GR"
   } else {
      $data(Canvas) create rectangle $Graph::Data(X$GR) $Graph::Data(Y$GR) [expr $Graph::Data(Width$GR)+$Graph::Data(X$GR)] [expr $Graph::Data(Height$GR)+$Graph::Data(Y$GR)] -outline "" -fill "" -width 0 -tags "$tag $GR"
   }
   $data(Canvas) create rectangle $graph(X0) $graph(Y0) $graph(X1) $graph(Y1) -outline $Graph::Color(Scale) -fill $Graph::Color(Graph) -tags "$tag"

   $data(Canvas) bind $tag     <ButtonPress-1> "Graph::Activate $data(Frame) $GR Contingency"

   $data(Canvas) create polygon [expr $graph(X0)-5] [expr $graph(Y1)-8] [expr $graph(X0)-92] [expr $graph(Y1)-45] [expr $graph(X0)-5] [expr $graph(Y1)-45] \
      -outline $Graph::Color(Scale) -tags "$tag" -fill ""
   $data(Canvas) create polygon [expr $graph(X0)-8] [expr $graph(Y1)-5] [expr $graph(X0)-95] [expr $graph(Y1)-42] [expr $graph(X0)-95] [expr $graph(Y1)-5] \
      -outline $Graph::Color(Scale) -tags "$tag" -fill ""

   $data(Canvas) create rectangle $graph(X0) [expr $graph(Y1)-5] $graph(X1) [expr $graph(Y1)-45] -outline $Graph::Color(Scale) -tags "$tag" -fill #DDDDDD
   $data(Canvas) create rectangle [expr $graph(X0)-5] $graph(Y0) [expr $graph(X0)-95] $graph(Y1) -outline $Graph::Color(Scale) -tags "$tag" -fill #DDDDDD

   $data(Canvas) create rectangle $graph(X0) [expr $graph(Y0)+5] $graph(X1) [expr $graph(Y0)+25] -outline $Graph::Color(Scale) -tags "$tag" -fill #F5F5F5
   $data(Canvas) create rectangle [expr $graph(X1)+5] $graph(Y0) [expr $graph(X1)+55] $graph(Y1) -outline $Graph::Color(Scale) -tags "$tag" -fill #F5F5F5
   $data(Canvas) create rectangle [expr $graph(X1)+5] [expr $graph(Y0)+5] [expr $graph(X1)+55] [expr $graph(Y0)+25] -outline $Graph::Color(Scale) -tags "$tag " -fill #F5F5F5

   $data(Canvas) create text [expr $Graph::Data(X$GR)+$Graph::Data(Width$GR)/2] [expr $Graph::Data(Y$GR)+5] -text "$graph(Title)" -fill $Graph::Color(FG) -tags "$tag CVTEXT HEADER$GR" \
      -font $Graph::Font(Graph) -justify center -anchor n

   #----- Creation des unite de l'echelle

   $data(Canvas) create text [expr $graph(X0)-6] [expr $graph(Y1)-44] -fill $Graph::Color(Unit) -tags "$tag CVTEXT UNITX$GR" \
      -text "$graph(UnitX)" -font $Graph::Font(Axis) -justify left -anchor ne
   $data(Canvas) create text [expr $graph(X0)-90] [expr $graph(Y1)-6] -fill $Graph::Color(Unit) -tags "$tag CVTEXT UNITY$GR" \
      -text "$graph(UnitY)" -font $Graph::Font(Axis) -justify left -anchor sw
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Contingency::Params>
# Creation : Octobre 2002 - J.P. Gauthier - CMC/CMOE -
#
# But      : Creer le frame des widgets des options
#
# Parametres :
#   <Parent> : Frame dans lequel creer les widgets
#   <GR>     : Identificateur du Graph
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Graph::Contingency::Params { Parent GR } {
   global GDefs

   labelframe $Parent.par -text [lindex $Graph::Lbl(Params) $GDefs(Lang)]
      frame $Parent.par.sel -relief sunken -bd 1
         checkbutton $Parent.par.sel.same -text [lindex $Graph::Lbl(Same) $GDefs(Lang)] -indicatoron false -onvalue True -offvalue False\
            -command "Graph::ParamsAxisUniform Contingency $GR" -bd 1 -variable Graph::Contingency::Contingency${GR}::Graph(Uniform)
         checkbutton $Parent.par.sel.time -text [lindex $Graph::Lbl(TimeMatch) $GDefs(Lang)] -indicatoron false \
            -command "Graph::Contingency::Graph $GR" -bd 1 -onvalue True -offvalue False \
            -variable Graph::Contingency::Contingency${GR}::Data(TimeMatch)
         pack $Parent.par.sel.same $Parent.par.sel.time -side top -fill x
      pack $Parent.par.sel -side top -fill x
   pack $Parent.par -side top -fill x -padx 5 -pady 5

   Graph::ParamsAxis $Parent $GR Contingency X INTERVAL
   Graph::ParamsAxis $Parent $GR Contingency Y INTERVAL
   Graph::ParamsAxisUniform Contingency $GR

   Bubble::Create $Parent.par.sel.same       $Graph::Bubble(Uniform)
   Bubble::Create $Parent.par.sel.fitlinear  $Graph::Bubble(TimeMatch)
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Contingency::ItemAdd>
# Creation : Avril 2005 - J.P. Gauthier - CMC/CMOE -
#
# But      : Creer un nouvel item de graph
#
# Parametres :
#   <GR>     : Identificateur du Graph
#   <Item>   : Identificateur de l'item positionnel a ajouter
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Graph::Contingency::ItemAdd { GR Item } {

   upvar #0 Graph::Contingency::Contingency${GR}::Data  data

   if { [lsearch -exact $data(Items) $Item]==-1 } {
      vector free   $Item
      vector create $Item
      vector dim    $Item { X Y }

      lappend data(Items) $Item
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Contingency::ItemDel>
# Creation : Avril 2005 - J.P. Gauthier - CMC/CMOE -
#
# But      : Supprimer un item de graph et ses resources
#
# Parametres :
#   <GR>     : Identificateur du Graph
#   <Item>   : Identificateur de l'item positionnel a ajouter
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Graph::Contingency::ItemDel { GR Item } {

   upvar #0 Graph::Contingency::Contingency${GR}::Data  data

   if { [set idx [lsearch -exact $data(Items) $Item]]!=-1 } {
      set data(Items) [lreplace $data(Items) $idx $idx]

      vector free $Item
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Contingency::ItemDefine>
# Creation : Avril 2005 - J.P. Gauthier - CMC/CMOE -
#
# But      : Gestion des items. Generer les itmes necessaires selon les donnees
#
# Parametres :
#   <GR>     : Identificateur du Graph
#   <Pos>    : Position
#   <Coords> : Coordonnees
#   <Update> : Mise a jour du graph
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Graph::Contingency::ItemDefine { GR Pos Coords { Update True } } {

   upvar #0 Graph::Contingency::Contingency${GR}::Data  data

   if { $Pos=="" } {
      return
   }

   if { [info exists Graph::Contingency::Contingency${GR}::Data(Items$Pos)] } {
      foreach item [lrange $data(Items$Pos) [llength $data(Data)] end] {
         Graph::Contingency::ItemDel $GR $item
      }
   }

   if { [lsearch -exact $data(Pos) $Pos]==-1 } {
      lappend data(Pos) $Pos
   }
   set data(Items$Pos) ${Pos}_Item0

   if { ![llength $Coords] } {
      set data(Pos$Pos)  { 0 0 0 0 }
   } else {
      set data(Pos$Pos)   $Coords
   }

   set item $Pos

   Graph::Idle $GR Contingency
   Graph::Contingency::ItemAdd $GR $item
   Graph::Contingency::UpdateItems $data(FrameData) $GR

   if { $Update } {
      Graph::Contingency::ItemData $GR $Pos $item $data(Data)
      Graph::Contingency::Graph $GR False
   }
   Graph::UnIdle $GR Contingency
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Contingency::ItemUnDefine>
# Creation : Avril 2005 - J.P. Gauthier - CMC/CMOE -
#
# But      : Gestion des items. Supression des items relatif a une position
#
# Parametres :
#   <GR>     : Identificateur du Graph
#   <Pos>    : Position
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Graph::Contingency::ItemUnDefine { GR Pos } {

   upvar #0 Graph::Contingency::Contingency${GR}::Data  data

   if { [set idx [lsearch -exact $data(Pos) $Pos]]!=-1 } {
      foreach item $data(Items$Pos) {
         Graph::Contingency::ItemDel $GR $item
      }

      set data(Pos) [lreplace $data(Pos)  $idx $idx]
      set Graph::Data(Pos) [lindex $data(Pos) end]
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Contingency::ItemData>
# Creation : Avril 2005 - J.P. Gauthier - CMC/CMOE -
#
# But      : Recuperer les donnees d'un item
#
# Parametres :
#   <GR>     : Identificateur du Graph
#   <Pos>    : Position
#   <Item>   : Identificateur de l'item positionnel a ajouter
#   <Data>   : Donnees a extraire
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Graph::Contingency::Clear { GR } {

   upvar #0 Graph::Contingency::Contingency${GR}::Data  data
   upvar #0 Graph::Contingency::Contingency${GR}::Graph graph

   set data(TIJ)    0
   set data(Select) ""

   #----- Initialiser les listes

   set rx [expr [llength $graph(XInter)]-1]
   set ry [expr [llength $graph(YInter)]-1]

   for { set i 0 } { $i <=$rx } { incr i } {
      for { set j 0 } { $j<=$ry } { incr j } {
          set data(R$i,$j) {}
          set data(TI$j) 0
      }
      set data(TJ$i) 0
   }
}

proc Graph::Contingency::ItemData { GR Pos Item Data } {

   upvar #0 Graph::Contingency::Contingency${GR}::Data  data
   upvar #0 Graph::Contingency::Contingency${GR}::Graph graph

   SPI::Progress 0

   if { $graph(Uniform) } {
      set graph(YInter) $graph(XInter)
   }

   Graph::Contingency::Clear $GR

   #----- Pour tout les couples de donnees

   foreach duo $data(Data) {

       set data0 [lindex $duo 0]
       set data1 [lindex $duo 1]

       if { [fstdfield is $data0] && [fstdfield is $data1] } {
          Graph::Contingency::ItemDataField $GR $data0 $data1
       } elseif { [observation is $data0] || [observation is $data1] } {
          Graph::Contingency::ItemDataObs $GR $data0 $data1
       }
   }

   SPI::Progress 80
   Graph::Contingency::Stat $GR
   SPI::Progress 0
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Contingency::ItemDataObs>
# Creation : Mars 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Calculer les blocs de donnees pour des observations.
#
# Parametres :
#   <GR>     : Identificateur du Graph
#   <Data0>  : Champs 1
#   <Data1>  : Champs 2
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Graph::Contingency::ItemDataObs { GR Data0 Data1 } {

   upvar #0 Graph::Contingency::Contingency${GR}::Data  data
   upvar #0 Graph::Contingency::Contingency${GR}::Graph graph

   set rx [expr [llength $graph(XInter)]-1]
   set ry [expr [llength $graph(YInter)]-1]

   #----- Dans le cas d'un champs, on en extrait les valeurs aux position de l'observation

   if { [fstdfield is $Data1] } {
      observation copy $Data1 $Data0
      observation extract $Data1 $Data1
      set data(Obs) $Data0
      set graph(UnitX) "[observation configure $Data0 -desc] Obs"
      set graph(UnitY) "[fstdfield configure $Data1 -desc] Model"
   }
   if { [fstdfield is $Data0] } {
      observation copy $Data0 $Data1
      observation extract $Data0 $Data0
      set data(Obs) $Data1
      set graph(UnitX) "[fstdfield configure $Data0 -desc] Model"
      set graph(UnitY) "[observation configure $Data1 -desc] Obs"
   }

   SPI::Progress 5

   foreach idx [observation stats $Data0 -within [list $data(Lat0) $data(Lon0) $data(Lat1) $data(Lon1)]] {

      set coo [observation define $Data0 -COORD $idx]
      set val0 [observation define $Data0 -DATA $idx]
      set val1 [observation define $Data1 -DATA $idx]

      if { $val0!="-" && $val1!="-" } {

         set ik 0
         for { set i 0 } { $i < $rx } { incr i } {
            if { $val0>=[lindex $graph(XInter) $i] && $val0<[lindex $graph(XInter) [expr $i+1]] } {
               set ik 1
               break
            }
         }

         set jk 0
         for { set j 0 } { $j < $ry } { incr j } {
            if { $val1>=[lindex $graph(YInter) $j] && $val1<[lindex $graph(YInter) [expr $j+1]] } {
               set jk 1
               break
            }
         }

         if { $ik && $jk } {
            lappend data(R$i,$j) $idx
            incr data(TJ$i)
            incr data(TI$j)
            incr data(TIJ)
         }
      }
   }

   SPI::Progress 60

   #----- Dans le cas d'un champs, suprimer l'observation temporaire

   if { [fstdfield is $Data1] } {
      observation free $Data1
   }
   if { [fstdfield is $Data0] } {
      observation free $Data0
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Contingency::ItemDataField>
# Creation : Mars 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Calculer les blocs de donnees pour deux champs.
#
# Parametres :
#   <GR>     : Identificateur du Graph
#   <Data0>  : Champs 1
#   <Data1>  : Champs 2
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Graph::Contingency::ItemDataField { GR Data0 Data1 } {

   upvar #0 Graph::Contingency::Contingency${GR}::Data  data
   upvar #0 Graph::Contingency::Contingency${GR}::Graph graph

   set graph(UnitX) "[fstdfield configure $Data0 -desc] Model"
   set graph(UnitY) "[fstdfield configure $Data1 -desc] Model"

   set rx [expr [llength $graph(XInter)]-1]
   set ry [expr [llength $graph(YInter)]-1]

   SPI::Progress 5

   set grids [fstdfield stats $Data0 -within [list $data(Lat0) $data(Lon0) $data(Lat1) $data(Lon1)]]
   foreach grid $grids {

      set ni [lindex $grid 0]
      set nj [lindex $grid 1]
      set coo [fstdfield stats $Data0 -gridpoint $ni $nj]
      set lat [lindex $coo 0]
      set lon [lindex $coo 1]

      set val0 [lindex [fstdfield stats $Data0 -gridvalue $ni $nj] 0]
      set val1 [lindex [fstdfield stats $Data1 -coordvalue $lat $lon] 0]

      if { $val0!="-" && $val1!="-" } {

         set ik 0
         for { set i 0 } { $i < $rx } { incr i } {
            if { $val0>=[lindex $graph(XInter) $i] && $val0<[lindex $graph(XInter) [expr $i+1]] } {
               set ik 1
               break
            }
         }

         set jk 0
         for { set j 0 } { $j < $ry } { incr j } {
            if { $val1>=[lindex $graph(YInter) $j] && $val1<[lindex $graph(YInter) [expr $j+1]] } {
               set jk 1
               break
            }
         }

         if { $ik && $jk } {
            lappend data(R$i,$j) $ni.$nj
            incr data(TJ$i)
            incr data(TI$j)
            incr data(TIJ)
         }
      }
   }
   SPI::Progress 65
}

proc Graph::Contingency::ItemDataVector { GR Data0 Data1 } {

   upvar #0 Graph::Contingency::Contingency${GR}::Data  data
   upvar #0 Graph::Contingency::Contingency${GR}::Graph graph

   set graph(UnitX) ""
   set graph(UnitY) ""

   set rx [expr [llength $graph(XInter)]-1]
   set ry [expr [llength $graph(YInter)]-1]

   SPI::Progress 5

   for { set n 0 } { $n < [vector length $Data0] } { incr n } {

      set ik 0
      if { [set val0 [vector get $Data0 $n]]!="-" } {
         for { set i 0 } { $i < $rx } { incr i } {
            if { $val0>=[lindex $graph(XInter) $i] && $val0<[lindex $graph(XInter) [expr $i+1]] } {
               set ik 1
               break
            }
         }
      }

      set jk 0
      if { [set val1 [vector get $Data1 $n]]!="-" } {
         for { set j 0 } { $j < $ry } { incr j } {
            if { $val1>=[lindex $graph(YInter) $j] && $val1<[lindex $graph(YInter) [expr $j+1]] } {
               set jk 1
               break
            }
         }
      }

      if { $ik && $jk } {
         lappend data(R$i,$j) $n
         incr data(TJ$i)
         incr data(TI$j)
         incr data(TIJ)
      }
   }
   SPI::Progress 65
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Contingency::Update>
# Creation : Octobre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectuer le "Refresh" des items sur la projection.
#
# Parametres :
#   <Frame > : Identificateur de page
#   <GR>     : Liste des graphs a mettre a jour
#
# Remarques :
#    - Cette fonctions est appele par le package Page au besoin.
#
#-------------------------------------------------------------------------------

proc Graph::Contingency::Update { Frame { GR {} } } {
   variable Data

   if { ![llength $GR] } {
      set GR [Page::Registered All Graph::Contingency]
   }

   foreach gr $GR {

      upvar #0 Graph::Contingency::Contingency${gr}::Data  data

      if { $data(FrameData)==$Frame && $Frame!="" } {

         catch {
            $data(FrameData).page.canvas configure -cursor watch
            $data(Canvas) configure -cursor watch
            update idletasks
         }

         if { [Page::Registered All Viewport $data(VP)]!=-1 } {
            Graph::Contingency::Data $gr [Viewport::Assigned $Viewport::Data(Frame$data(VP)) $data(VP) { fstdfield observation }]
         }
         #----- Update des items

         foreach pos $data(Pos) {
            Graph::Contingency::ItemDefine $gr $pos $data(Pos$pos)
         }

         catch {
            $data(Canvas) configure -cursor left_ptr
            $data(FrameData).page.canvas configure -cursor left_ptr
         }
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Contingency::UpdateItems>
# Creation : Octobre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectuer le "Refresh" des items sur la projection.
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <GR>     : Liste des graphs a mettre a jour
#
# Remarques :
#    - Cette fonctions est appele par le package Page au besoin.
#
#-------------------------------------------------------------------------------

proc Graph::Contingency::UpdateItems { Frame { GR { } } } {
   global GDefs
   variable Lbl
   variable Data

   if { ![llength $GR] } {
      set GR [Page::Registered All Graph::Contingency]
   }

   foreach gr $GR {

      upvar #0 Graph::Contingency::Contingency${gr}::Data  data

      if { $data(VP)!="" && $data(FrameData)==$Frame } {
         $Frame.page.canvas delete GRAPHCONTINGENCY$gr
         foreach pos $data(Pos) {
            Graph::ItemPos $Frame $data(VP) $data(Pos$pos) "[lindex $Lbl(Title) $GDefs(Lang)]" GRAPHCONTINGENCY$gr RECTANGLE
         }

         if { $data(Select)!="" && $data(Obs)!="" } {
            foreach idx $data($data(Select)) {

               set coo [observation define $data(Obs) -COORD $idx]
               if { [set pix [$data(VP) -project [lindex $coo 0] [lindex $coo 1] 0]]!="" && [lindex $pix 2]>0 } {
                  $data(FrameData).page.canvas create bitmap [expr [lindex $pix 0]-$Obs::Param(Size)] [lindex $pix 1] \
                     -bitmap @$GDefs(Dir)/Resources/Bitmap/arrow.ico -tags "$Page::Data(Tag)$data(VP) GRAPHCONTINGENCY$gr" \
                     -anchor e -foreground $Graph::Color(Select)
               }
            }
         }
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Contingency::Data>
# Creation : Mars 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer les couples de donnees a evaluer
#
# Parametres :
#   <GR>     : Identificateur du Graph
#   <Data>   : Liste des donnees
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Graph::Contingency::Data { GR { Data { } } } {

   upvar #0 Graph::Contingency::Contingency${GR}::Data  data
   upvar #0 Graph::Contingency::Contingency${GR}::Graph graph

   set data(Data)   {}

   if { $data(TimeMatch) } {
      for { set i 0 } { $i<[llength $Data] } { incr i } {
         set dataa [lindex $Data $i]

         if { [fstdfield is $dataa] } {
            set dateva [fstdstamp toseconds [fstdfield define $dataa -DATEV]]
         }
         if { [observation is $dataa] } {
            set dateva [observation define $dataa -DATE]
         }

         for { set j [expr $i+1] } { $j<[llength $Data] } { incr j } {
            set datab [lindex $Data $j]

            if { [fstdfield is $datab] } {
               set datevb [fstdstamp toseconds [fstdfield define $datab -DATEV]]
            }
            if { [observation is $datab] } {
               set datevb [observation define $datab -DATE]
            }

            if { $dateva==$datevb } {
               lappend data(Data) [list $dataa $datab]
            }
         }
      }
   } else {
      foreach { a b }  $Data {

         if { $a!=$b } {
            lappend data(Data) [list $a $b]
         }
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Contingency::DrawData>
# Creation : Mars 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche les donnees sur le graphique de contingences.
#
# Parametres :
#   <GR>     : Identificateur du Graph
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Graph::Contingency::DrawData { GR } {

   upvar #0 Graph::Contingency::Contingency${GR}::Data  data
   upvar #0 Graph::Contingency::Contingency${GR}::Graph graph

   $data(Canvas) delete DATA$GR
   set tag $Page::Data(Tag)$GR

   if { ![info exists data(R0,0)] } {
      return
   }

   set rx [expr [llength $graph(XInter)]-1]
   set ry [expr [llength $graph(YInter)]-1]

   #----- Inscrire les donnees du tableau

   for { set i 0 } { $i < $rx } { incr i } {
      for { set j 0 } { $j < $ry } { incr j } {
         set x [expr $graph(X0)+($i+1)*$graph(DX)]
         set y [expr $graph(Y1)+($j+1)*$graph(DY)]

         #----- Determiner si la case est selectionnee

         if { $data(Select)=="R$i,$j" } {
            set col yellow
         } else {
            set col white
         }
         $data(Canvas) create rectangle $x $y [expr $x-$graph(DX)] [expr $y-$graph(DY)] -outline black -fill $col -tags "$tag DATA$GR DATA$GR$i$j"
         $data(Canvas) create text [expr $x-$graph(DX)/2.0] [expr $y-$graph(DY)/2.0] -fill black -tags "$tag DATA$GR" -text [llength $data(R$i,$j)] -font $Graph::Font(Axis)

         $data(Canvas) bind DATA$GR$i$j <Enter>            "$data(Canvas) configure -cursor hand1"
         $data(Canvas) bind DATA$GR$i$j <Leave>            "$data(Canvas) configure -cursor left_ptr"
         $data(Canvas) bind DATA$GR$i$j <ButtonPress-1>    "Graph::Contingency::Select $GR $i $j"
      }
   }

   #----- Inscrire les totaux

   for { set i 0 } { $i < $rx } { incr i } {
      $data(Canvas) create text [expr $graph(X0)+($i+1)*$graph(DX)-$graph(DX)/2.0] [expr $graph(Y0)+15] -fill black -tags "$tag DATA$GR" \
         -text $data(TJ$i) -font $Graph::Font(Axis)
   }


   for { set j 0 } { $j < $ry } { incr j } {
      $data(Canvas) create text [expr $graph(X1)+30] [expr $graph(Y1)+($j+1)*$graph(DY)-$graph(DY)/2.0] -fill black -tags "$tag DATA$GR" \
         -text $data(TI$j) -font $Graph::Font(Axis)
   }

   $data(Canvas) create text [expr $graph(X1)+30] [expr $graph(Y0)+15] -fill black -tags "$tag DATA$GR" -text $data(TIJ) -font $Graph::Font(Axis)

   $data(Canvas) itemconfigure HEADER$GR -text "$graph(Title)"
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Contingency::DrawScale>
# Creation : Mars 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche les echelles du graphique.
#
# Parametres :
#   <GR>     : Identificateur du Graph
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Graph::Contingency::DrawScale { GR } {

   upvar #0 Graph::Contingency::Contingency${GR}::Data  data
   upvar #0 Graph::Contingency::Contingency${GR}::Graph graph

   $data(Canvas) delete SCALE$GR
   set tag $Page::Data(Tag)$GR

   #----- Aficher les boites
   if { [set rx [expr [llength $graph(XInter)]-1]]>0 } {

      for { set i 0 } { $i < $rx } { incr i } {

         set x [expr $graph(X0)+($i+1)*$graph(DX)]
         $data(Canvas) create line $x [expr $graph(Y1)-5] $x [expr $graph(Y1)-45] -fill black -tags "$tag SCALE$GR"
         $data(Canvas) create line $x [expr $graph(Y0)+5] $x [expr $graph(Y0)+25] -fill black -tags "$tag SCALE$GR"

         set lbl "[lindex $graph(XInter) $i] - [lindex $graph(XInter) [expr $i+1]]"
         $data(Canvas) create text [expr $x-$graph(DX)/2.0] [expr $graph(Y1)-20] -text $lbl -fill $Graph::Color(Scale) -font $Graph::Font(Axis) -tags "$tag SCALE$GR"
      }
   }

   if { [set ry [expr [llength $graph(YInter)]-1]]>0 } {

      for { set j 0 } { $j < $ry } { incr j } {
         set y [expr $graph(Y1)+($j+1)*$graph(DY)]
         $data(Canvas) create line [expr $graph(X0)-5] $y [expr $graph(X0)-95] $y -fill black -tags "$tag SCALE$GR"
         $data(Canvas) create line [expr $graph(X1)+5] $y [expr $graph(X1)+55] $y -fill black -tags "$tag SCALE$GR"

         set lbl "[lindex $graph(YInter) $j] - [lindex $graph(YInter) [expr $j+1]]"
         $data(Canvas) create text [expr $graph(X0)-45] [expr $y-$graph(DY)/2.0] -text $lbl -fill $Graph::Color(Scale) -font $Graph::Font(Axis) -tags "$tag SCALE$GR"
      }
   }

   $data(Canvas) itemconfigure UNITX$GR -text "$graph(UnitX)"
   $data(Canvas) itemconfigure UNITY$GR -text "$graph(UnitY)"
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Contingency::DrawStat>
# Creation : Mars 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche les statistiques.
#
# Parametres :
#   <GR>     : Identificateur du Graph
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Graph::Contingency::DrawStat { GR } {
   global GDefs
   variable Lbl

   upvar #0 Graph::Contingency::Contingency${GR}::Data  data
   upvar #0 Graph::Contingency::Contingency${GR}::Graph graph

   set tag $Page::Data(Tag)$GR

   set rx [expr [llength $graph(XInter)]-1]
   set ry [expr [llength $graph(YInter)]-1]

   if { $rx!=$ry || $data(TIJ)==0 } {
      return
   }

   set stats { BUST BIAS POD FAR REL CSI }
   set len   [llength $stats]
   set rx    [expr [llength $graph(XInter)]-1]

   $data(Canvas) create rectangle $graph(X0) [expr $graph(Y0)+30] $graph(X1) [expr $graph(Y0)+30+$graph(DY)*($len+3)] \
      -outline black -tags "$tag"
   $data(Canvas) create rectangle [expr $graph(X0)-5] [expr $graph(Y0)+30] [expr $graph(X0)-95] [expr $graph(Y0)+30+$graph(DY)*($len+3)] \
      -outline black -tags "$tag" -fill #DDDDDD

   #----- BUST BIAS POD FAR REL et CSI

   set j [expr $graph(DY)/2.0]
   foreach stat $stats {

      set yt [expr $graph(Y0)+30+$j]
      set yl [expr $yt+$graph(DY)/2.0]

      $data(Canvas) create text [expr $graph(X0)-45] $yt -text $stat -fill black -tags "$tag DATA$GR" -font $Graph::Font(Axis)
      $data(Canvas) create line [expr $graph(X0)-5] $yl [expr $graph(X0)-95] $yl -fill black -tags "$tag"
      $data(Canvas) create line $graph(X0) $yl $graph(X1) $yl -fill black -tags "$tag"

      set j [expr $j+$graph(DY)]

      for { set i 0 } { $i < $rx } { incr i } {
         set xl [expr $graph(X0)+($i+1)*$graph(DX)]
         set xt [expr $xl-$graph(DX)/2.0]

         $data(Canvas) create text $xt $yt -text [format "%.3f" $data($stat$i)] -fill black -tags "$tag DATA$GR" -font $Graph::Font(Axis)
         $data(Canvas) create line $xl [expr $graph(Y0)+30] $xl [expr $graph(Y0)+30+$graph(DY)*$len] -fill black -tags "$tag"
      }
   }

   #----- PC HSS et BUSTT

   set yt [expr $graph(Y0)+30+$j]
   set yl [expr $yt+$graph(DY)/2.0]
   $data(Canvas) create text [expr $graph(X0)-45] $yt -text PC -fill black -tags "$tag DATA$GR" -font $Graph::Font(Axis)
   $data(Canvas) create text [expr $graph(X0)+($graph(X1)-$graph(X0))/2.0] $yt -text [format "%.3f" $data(PC)] -fill black -tags "$tag DATA$GR" -font $Graph::Font(Axis)
   $data(Canvas) create line [expr $graph(X0)-5] $yl [expr $graph(X0)-95] $yl -fill black -tags "$tag"
   $data(Canvas) create line $graph(X0) $yl $graph(X1) $yl -fill black -tags "$tag"
   set j [expr $j+$graph(DY)]
   set yt [expr $graph(Y0)+30+$j]
   set yl [expr $yt+$graph(DY)/2.0]
   $data(Canvas) create text [expr $graph(X0)-45] $yt -text HSS -fill black -tags "$tag DATA$GR" -font $Graph::Font(Axis)
   $data(Canvas) create text [expr $graph(X0)+($graph(X1)-$graph(X0))/2.0] $yt -text [format "%.3f" $data(HSS)] -fill black -tags "$tag DATA$GR" -font $Graph::Font(Axis)
   $data(Canvas) create line [expr $graph(X0)-5] $yl [expr $graph(X0)-95] $yl -fill black -tags "$tag"
   $data(Canvas) create line $graph(X0) $yl $graph(X1) $yl -fill black -tags "$tag"
   set j [expr $j+$graph(DY)]
   set yt [expr $graph(Y0)+30+$j]
   $data(Canvas) create text [expr $graph(X0)-45] $yt -text BUSTT -fill black -tags "$tag DATA$GR" -font $Graph::Font(Axis)
   $data(Canvas) create text [expr $graph(X0)+($graph(X1)-$graph(X0))/2.0] $yt -text [format "%.3f" $data(BUSTT)] -fill black -tags "$tag DATA$GR" -font $Graph::Font(Axis)

#   set yt [expr $yt+30]
#   foreach stat { PC HSS BUST BIAS POD FAR REL CSI } {
#      $data(Canvas) create text [expr $graph(X0)+($graph(X1)-$graph(X0))/2.0-200] $yt -text [format "%-5s: %s" $stat [lindex $Lbl($stat) $GDefs(Lang)]] \
#         -anchor w -fill $Graph::Color(Footer) -tags "$tag DATA$GR FOOTER$GR" -font $Graph::Font(Footer)
#      set yt [expr $yt+10]
#   }
}

#----------------------------------------------------------------------------
# Nom      : <Graph::Contingency::Select>
# Creation : Mars 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectue l'affichage des stations incluses dans la case selectionnee.
#
# Parametres :
#   <GR>     : Identificateur du Graph
#   <I>      : Position en X de la case
#   <J>      : Position en Y de la case
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Graph::Contingency::Select { GR  I J } {

   upvar #0 Graph::Contingency::Contingency${GR}::Data  data
   upvar #0 Graph::Contingency::Contingency${GR}::Graph graph

   set data(Select) "R$I,$J"

   Graph::Contingency::DrawData $GR
   Graph::Contingency::DrawStat $GR
   Graph::Contingency::UpdateItems $data(FrameData) $GR
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Contingency::Stat>
# Creation : Mars 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Calculer les statistiques associes a une table de contingence.
#
# Parametres :
#   <GR>     : Identificateur du Graph
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Graph::Contingency::Stat { GR } {

   upvar #0 Graph::Contingency::Contingency${GR}::Data  data
   upvar #0 Graph::Contingency::Contingency${GR}::Graph graph

   set rx [expr [llength $graph(XInter)]-1]
   set ry [expr [llength $graph(YInter)]-1]

   if { $rx!=$ry || $data(TIJ)==0 } {
      return
   }

   set data(PC)    0.0                         ;#Pourcentage Correct
   set data(HSS)   0.0                         ;#Indice de comparaison de Heidke
   set data(BUSTT) 0.0                         ;#Proportions des erreurs total

   for { set i 0 } { $i <$rx } { incr i } {
      set data(BIAS$i)  0.0                    ;#Bias
      set data(POD$i)   0.0                    ;#Probabilite de detection d'evenement
      set data(REL$i)   0.0                    ;#Indice de confiance
      set data(CSI$i)   0.0                    ;#Succes critique
      set data(FAR$i)   0.0                    ;#Proportions de fausses alertes
      set data(BUST$i)  0.0                    ;#Proportions des erreurs des categories
   }

   set c 0.0
   set n [expr [llength $graph(XInter)]-1]

   for { set i 0 } { $i <$rx } { incr i } {
      set diag         [expr double([llength $data(R$i,$i)])]
      set c            [expr $c+$data(TI$i)*$data(TJ$i)]
      set data(PC)     [expr $data(PC)+$diag]

      if { $data(TI$i)!=0.0 } {
         set data(BIAS$i) [expr double($data(TJ$i))/$data(TI$i)]
         set data(POD$i)  [expr $diag/$data(TI$i)]
      }

      if { $data(TJ$i)!=0.0 } {
         set no            [expr ([llength $graph(XInter)]-1)-($i+1)]
         set data(BUST$i)  [expr [llength $data(R$i,$no)]/double($data(TJ$i))]
         set data(REL$i)   [expr $diag/$data(TJ$i)]
      }

      if { [expr $data(TI$i)+$data(TJ$i)-$diag]!=0.0 } {
         set data(CSI$i)  [expr $diag/($data(TI$i)+$data(TJ$i)-$diag)]
      }
      set data(FAR$i)  [expr 1.0-$data(REL$i)]
   }

   set c         [expr $c/$data(TIJ)]

   if { $data(TIJ)!=$c } {
      set data(HSS) [expr ($data(PC)-$c)/($data(TIJ)-$c)]
   }

   if { $data(TJ0)!=$data(TJ$n) } {
      set data(BUSTT) [expr ([llength $data(R0,$n)]+double([llength $data(R$n,0)]))/($data(TJ0)+$data(TJ$n))]
   }

   set data(PC)  [expr 100.0*$data(PC)/$data(TIJ)]
}
